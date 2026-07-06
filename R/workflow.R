# workflow.R — generate a targets pipeline from `msens:` YAML frontmatter ----
# Ported from CalCOFI's calcofi4db::build_targets_list() (which drives the
# CalCOFI/workflows pipeline). Each pipeline notebook declares its dependencies
# and output in a `msens:` frontmatter block; these two functions parse those
# blocks and emit the `list()` of targets that becomes the body of `_targets.R`.
# No per-target boilerplate, no tarchetypes.

#' Parse `msens:` frontmatter from pipeline notebooks
#'
#' Globs the `.qmd` files in `workflows_dir`, reads the YAML frontmatter of each
#' (the block between the first two `---` fences), and keeps only those carrying
#' a top-level `msens:` key. Returns one row per pipeline notebook with the
#' fields needed to wire the `targets` DAG.
#'
#' The `msens:` block vocabulary:
#' \describe{
#'   \item{target_name}{the `targets` node name (a legal R symbol)}
#'   \item{workflow_type}{one of `grid`, `ingest`, `merge`, `score`, `publish`,
#'     `release`, `test`}
#'   \item{dependency}{list of upstream `target_name`s, or `[auto]` to depend on
#'     every `grid` + `ingest` target}
#'   \item{output}{the file/dir the notebook produces (tracked via
#'     `format = "file"`; may contain a `*` glob)}
#' }
#'
#' @param workflows_dir directory holding the pipeline `.qmd`s (default
#'   [here::here()])
#' @param pattern glob for notebooks (default `"*.qmd"`)
#' @return a tibble with columns `qmd_file`, `target_name`, `workflow_type`,
#'   `dependency` (list column), `output`
#' @export
#' @concept workflow
#' @examples
#' \dontrun{
#' wf <- parse_qmd_frontmatter("~/Github/MarineSensitivity/workflows")
#' dplyr::filter(wf, workflow_type == "ingest")
#' }
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom rlang %||%
parse_qmd_frontmatter <- function(
    workflows_dir = here::here(),
    pattern       = "*.qmd") {

  stopifnot(requireNamespace("yaml", quietly = TRUE))

  qmd_files <- Sys.glob(file.path(workflows_dir, pattern))

  results <- Filter(Negate(is.null), lapply(qmd_files, function(f) {
    # read the whole file: frontmatter can be long (dataset metadata blocks),
    # so a fixed line cap could miss the closing ---
    lines <- readLines(f, warn = FALSE)

    delims <- which(trimws(lines) == "---")
    if (length(delims) < 2) return(NULL)

    yaml_text <- paste(lines[(delims[1] + 1):(delims[2] - 1)], collapse = "\n")
    meta <- tryCatch(yaml::yaml.load(yaml_text), error = function(e) NULL)
    if (is.null(meta) || is.null(meta$msens)) return(NULL)

    m <- meta$msens
    tibble::tibble(
      qmd_file      = basename(f),
      target_name   = m$target_name   %||% NA_character_,
      workflow_type = m$workflow_type %||% NA_character_,
      # YAML `[]` parses to list(); normalize deps to a character vector
      dependency    = list(as.character(m$dependency %||% character(0))),
      output        = m$output         %||% NA_character_)
  }))

  if (length(results) == 0) {
    return(tibble::tibble(
      qmd_file      = character(),
      target_name   = character(),
      workflow_type = character(),
      dependency    = list(),
      output        = character()))
  }

  dplyr::bind_rows(results)
}

#' Build the targets list from `msens:` frontmatter
#'
#' Reads every pipeline notebook's `msens:` block (via [parse_qmd_frontmatter()])
#' and returns a `list()` of `targets::tar_target_raw()` objects for use as the
#' body of `_targets.R`. Each target's body is a `{}` block whose leading
#' statements are bare references to its upstream target names (so `targets`
#' draws the DAG edges), followed by a `quarto::quarto_render()` of the notebook
#' and the `output` path (tracked with `format = "file"` for hash-based
#' invalidation).
#'
#' `dependency: [auto]` (typically the `release` caboose) resolves to every
#' `grid` + `ingest` target.
#'
#' @param workflows_dir directory holding the pipeline `.qmd`s (default
#'   [here::here()])
#' @param exclude character vector of target names (or `.qmd` filenames) to drop
#'   from the pipeline; excluded targets are also stripped from other targets'
#'   dependency lists
#' @param verbose print the parsed workflow table (default `TRUE`)
#' @return a `list()` of `tar_target_raw()` objects for `_targets.R`
#' @export
#' @concept workflow
#' @examples
#' \dontrun{
#' # in _targets.R:
#' library(targets)
#' library(msens)          # or devtools::load_all("../msens")
#' build_targets_list()
#' }
#' @importFrom glue glue
build_targets_list <- function(
    workflows_dir = here::here(),
    exclude       = NULL,
    verbose       = TRUE) {

  stopifnot(
    "package 'targets' is required" = requireNamespace("targets", quietly = TRUE))

  wf <- parse_qmd_frontmatter(workflows_dir)
  if (nrow(wf) == 0)
    stop("no .qmd files with `msens:` frontmatter found in ", workflows_dir)

  # validate: every target has a name + type + output
  bad <- wf$qmd_file[is.na(wf$target_name) | is.na(wf$workflow_type) | is.na(wf$output)]
  if (length(bad) > 0)
    stop("`msens:` block missing target_name/workflow_type/output in: ",
         paste(bad, collapse = ", "))

  # exclude requested targets (accept qmd filenames or hyphenated names)
  if (length(exclude) > 0) {
    exclude_targets <- gsub("-", "_", gsub("\\.qmd$", "", exclude))
    n_before <- nrow(wf)
    wf <- wf[!wf$target_name %in% exclude_targets, ]
    if (verbose && nrow(wf) < n_before)
      message("excluded ", n_before - nrow(wf), " target(s): ",
              paste(exclude_targets, collapse = ", "))
    wf$dependency <- lapply(wf$dependency, function(d) setdiff(d, exclude_targets))
  }

  if (verbose) {
    message("parsed ", nrow(wf), " pipeline workflows:")
    for (i in seq_len(nrow(wf))) {
      deps <- paste(wf$dependency[[i]], collapse = ", ")
      message(glue::glue(
        "  {wf$target_name[i]} ({wf$workflow_type[i]}) -> {wf$output[i]}",
        "{if (nchar(deps) > 0) paste0(' [deps: ', deps, ']') else ''}"))
    }
  }

  # [auto] → all grid + ingest targets
  auto_targets <- wf$target_name[wf$workflow_type %in% c("grid", "ingest")]

  # verify every named dependency resolves to a defined target (catch typos early)
  defined <- wf$target_name
  for (i in seq_len(nrow(wf))) {
    deps <- wf$dependency[[i]]
    if (length(deps) == 1 && identical(deps, "auto")) next
    missing <- setdiff(deps, defined)
    if (length(missing) > 0)
      stop(glue::glue(
        "target '{wf$target_name[i]}' depends on undefined target(s): ",
        "{paste(missing, collapse = ', ')}"))
  }

  target_list <- list()
  for (i in seq_len(nrow(wf))) {
    row  <- wf[i, ]
    deps <- row$dependency[[1]]
    if (length(deps) == 1 && identical(deps, "auto"))
      deps <- setdiff(auto_targets, row$target_name)

    # body: bare dependency symbols (→ DAG edges), then render, then output path
    body_parts <- lapply(deps, as.symbol)
    body_parts <- c(body_parts, list(
      bquote(quarto::quarto_render(.(row$qmd_file)))))
    body_parts <- c(body_parts, list(
      if (grepl("\\*", row$output)) bquote(Sys.glob(.(row$output))) else row$output))
    body_expr <- as.call(c(list(as.symbol("{")), body_parts))

    target_list <- c(target_list, list(
      targets::tar_target_raw(row$target_name, body_expr, format = "file")))
  }

  target_list
}
