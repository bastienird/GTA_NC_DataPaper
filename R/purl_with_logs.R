#' Purl an R Markdown file and inject log statements around chunks
#'
#' This function is a wrapper around [knitr::purl()] that:
#' * extracts the code from an R Markdown file,
#' * injects a log statement at the beginning and the end of each chunk,
#' * and writes a small header at the top of the generated R script indicating
#'   that it was generated with `purl_with_logs()` from a given source file.
#'
#' It forwards the `documentation` argument to [knitr::purl()], so you can
#' control how text outside of code chunks is treated (ignored, roxygen-style,
#' or kept as comments), exactly as in `knitr::purl()`.
#'
#' @param input Path to the input R Markdown file (`.Rmd`).
#' @param output Optional path to the output R script (`.R`). If `NULL`,
#'   the output file name is obtained by replacing the `.Rmd` extension
#'   of `input` with `.R`.
#' @param log_fun Character string specifying the logging function to use.
#'   Must be either `"cat"` or `"message"`. The function is called with a
#'   single character string argument.
#' @param prefix_begin Character string prefix for the “beginning of chunk”
#'   log message. The chunk label is appended automatically.
#' @param prefix_end Character string prefix for the “end of chunk”
#'   log message. The chunk label is appended automatically.
#' @param documentation Passed to [knitr::purl()]. Can be `0`, `1`, `2` or
#'   `"none"`, `"roxygen"`, `"all"`:
#'   * `0` / `"none"`: drop all documentation (code only),
#'   * `1` / `"roxygen"`: keep text as roxygen comments,
#'   * `2` / `"all"`: keep all text outside chunks as plain comments.
#'
#' @return Invisibly, the normalized path to the generated R script.
#' @export
#'
#' @examples
#' \dontrun{
#' # Code only (default, like documentation = 0)
#' purl_with_logs("Paper_WPDCS_COMP_FSJ_IOTC_Rev2.Rmd",
#'                output = "Paper_WPDCS_Comp_FSJ_IOTC_Rev2.R")
#'
#' # Keep all prose from the Rmd as comments in the .R file
#' purl_with_logs("Paper_WPDCS_COMP_FSJ_IOTC_Rev2.Rmd",
#'                output = "Paper_WPDCS_Comp_FSJ_IOTC_Rev2_with_text.R",
#'                documentation = 2)
#' }
purl_with_logs <- function(input,
                           output = NULL,
                           log_fun = c("cat", "message"),
                           prefix_begin = "---- Beginning of the chunk: ",
                           prefix_end = "---- End of the chunk: ",
                           documentation = 0) {
  log_fun <- match.arg(log_fun)
  
  # 1. Run knitr::purl() into a temporary R file
  tmp_r <- knitr::purl(
    input,
    output        = tempfile(fileext = ".R"),
    documentation = documentation
  )
  
  # 2. Read the temporary R script
  lines <- readLines(tmp_r, warn = FALSE)
  res <- character(0)
  
  current_label <- NULL
  started <- FALSE
  
  is_chunk_header <- function(x) grepl("^## ----", x)
  get_label <- function(x) sub("^## ----([^, ]*).*", "\\1", x)
  
  for (line in lines) {
    if (is_chunk_header(line)) {
      # Close previous chunk with an "end" log if it has started
      if (!is.null(current_label) && started) {
        res <- c(
          res,
          sprintf('%s("%s%s\\n")', log_fun, prefix_end, current_label)
        )
      }
      # Start a new chunk
      current_label <- get_label(line)
      started <- FALSE
      res <- c(res, line)
      
    } else {
      # First non-empty, non-comment line in the chunk: insert "begin" log
      if (!is.null(current_label) &&
          !started &&
          !grepl("^\\s*($|#)", line)) {
        res <- c(
          res,
          sprintf('%s("%s%s\\n")', log_fun, prefix_begin, current_label)
        )
        started <- TRUE
      }
      res <- c(res, line)
    }
  }
  
  # Close the last chunk if needed
  if (!is.null(current_label) && started) {
    res <- c(
      res,
      sprintf('%s("%s%s\\n")', log_fun, prefix_end, current_label)
    )
  }
  
  # 3. Determine output path if not provided
  if (is.null(output)) {
    output <- sub("\\.Rmd$", ".R", input)
  }
  
  # 4. Prepend header to the script
  header <- c(
    sprintf("# Generated with purl_with_logs() from '%s'", basename(input)),
    sprintf("# Source Rmd: %s", normalizePath(input, winslash = "/")),
    sprintf("# Generated on: %s", as.character(Sys.time())),
    ""
  )
  
  writeLines(c(header, res), con = output)
  
  invisible(normalizePath(output, winslash = "/"))
}
