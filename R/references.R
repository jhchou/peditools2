#' The publications behind each chart
#'
#' Each chart and measure's references, as on the PediTools Sources and methods page: the chart's publication, a
#' separate source of its parameters, the source of its Z-score method, and more information.
#'
#' @param chart Chart ids; `NULL` (default) for all.
#' @param measure Measures; `NULL` (default) for every measure of the charts.
#' @return A data frame, one row per chart, measure and reference, with columns:
#'   \describe{
#'     \item{chart, measure}{The chart and measure.}
#'     \item{role}{`"clinical"` (the chart's publication), `"lms_data"` (a separate source of the parameters),
#'       `"method"` (the source prescribing its Z-score method) or `"resource"` (more information, the charts).}
#'     \item{position}{The order within the role.}
#'     \item{id, short, citation}{The reference's id, a short form and the full citation.}
#'     \item{doi, pmid}{Its DOI and PubMed id (`NA` when it has none).}
#'     \item{url}{Its PubMed page, or its web address.}
#'     \item{sources_url}{The chart's section of the PediTools Sources and methods page, where its sources and
#'       methods are described in full.}
#'   }
#' @seealso [growth_charts()]; `citation("peditools2")` to cite the package.
#' @export
#' @examples
#' growth_references("who_2006_infant", "weight")[c("role", "short", "url")]
growth_references <- function(chart = NULL, measure = NULL) {
  data <- pt_data()
  tables <- if (is.null(chart)) data$tables else select_tables(chart, measure)
  if (is.null(chart) && !is.null(measure)) {
    tables <- tables[tables$measure %in% measure, , drop = FALSE]
  }
  refs <- data$references
  refs <- refs[table_key(refs$chart, refs$measure) %in% table_key(tables$chart, tables$measure), , drop = FALSE]
  links <- unique(tables[c("chart", "measure", "sources_url")])
  refs$sources_url <- links$sources_url[match(
    table_key(refs$chart, refs$measure),
    table_key(links$chart, links$measure)
  )]
  rownames(refs) <- NULL
  refs
}

#' The version of the chart data
#'
#' The date and the commit of the last change to the PediTools parameter files the package's data were built from.
#'
#' @return A named character vector: `date` (YYYY-MM-DD) and `commit`.
#' @export
#' @examples
#' growth_data_version()
growth_data_version <- function() {
  pt_data()$version
}
