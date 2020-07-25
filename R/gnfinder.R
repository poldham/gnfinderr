#' Find names with gnfinder
#' @description Extract names from a text by passing a string or strings and
#'   returning a data frame. Optionally check names and retrieve taxon ids and
#'   classification from different data sources.
#' @param string A string or strings to be queried for a species name (character)
#' @param id A document identifier (e.g. df$id). If NULL added as 1:n
#' @param nobayes Whether to turn off the bayes algorithm (default = FALSE)
#' @param check_names check identified names against taxonomic databases?
#'   (logical). Defaults to TRUE for the Catalogue of Life
#' @param sources flag for sources
#' @param source_ids list of taxonomic databases. See list at
#'   [https://index.globalnames.org/datasource](https://index.globalnames.org/datasource)
#'   (integer) * 1 = Catalogue of Life * 2 = Wikispecies * 3 = ITIS * 9 = World
#'   Register of Marine Species * 11 = GBIF Backbone Taxonomy * 12 =
#'   Encyclopedia of Life * 179 = Open Tree of Life Reference Taxonomy
#' @return data.frame
#' @export
#' @importFrom purrr map
#' @importFrom purrr compact
#' @importFrom purrr set_names
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom glue double_quote
#' @importFrom tibble add_column
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom utf8 utf8_valid
#' @importFrom usethis ui_info
#' @importFrom usethis ui_warn
#' @importFrom usethis ui_stop
#' @examples single <- gnfinder("Lepidium meyenii is a hot plant. Escherichia coli is not")
#' hotplants <- gnfinder(c("Lepidium meyenii is a hot plant",
#' "Capsicum annuum is a hot plant for a different reason"))
#' five <- gnfinder(fivetexts$text, fivetexts$id, check_names = FALSE)
gnfinder <- function(string, id = NULL, nobayes = NULL, check_names = NULL, sources = NULL, source_ids = NULL) {


# gnfinder terminal flags -------------------------------------------------

  # check_names <- "-c"
  # nobayes <- "-n"
  # lang <- "-l" # "eng" or "deu" only
  # odds_details <- "-o"
  # sources <- "-s"

multi_ids <- glue_collapse(source_ids, ",")


# Fail Fast ---------------------------------------------------------------

if(is.null(string) || !is.character(string)){
  ui_stop("Function is expecting a character string or vector")
}

validate_string <- utf8::utf8_valid(string) %>%
  all(. == TRUE)

if(isFALSE(validate_string)) {
  ui_stop("non-utf8 text found in string. gnfinder will fail on non-utf8.
  Consider preprocessing strings with stringr, utf8, textclean or similar packages.")
}

if(!is.null(check_names) && !is.null(source_ids)) {

  ui_stop("Review query: check_names indicates you don't want to check names, but source_ids are provided.")
}

# With bayes

if(is.null(nobayes) && is.null(check_names) && is.null(source_ids)) {

  ui_info("Running search: Checking names against Catalogue of Life")
  input <- glue::glue('echo {double_quote(string)} | gnfinder find -c -l eng')

}

if(is.null(nobayes) && !is.null(check_names)) {

  ui_info("Running search: not checking names")
  input <- glue::glue('echo {double_quote(string)} | gnfinder find -l eng')

}

if(is.null(nobayes) && is.null(check_names) && !is.null(source_ids)) {

  ui_info("Running search: Checking against the Catalogue of Life and the source_ids provided")
  input <- glue::glue('echo {double_quote(string)} | gnfinder find -c -l eng -s {multi_ids}')

}

# No bayes

if(!is.null(nobayes) && is.null(check_names) && is.null(source_ids)) {

    ui_info("Running search: no bayes, checking names against default Catalogue of Life")
    input <- glue::glue('echo {double_quote(string)} | gnfinder find -n -c -l eng')

  }

if(!is.null(nobayes) && !is.null(check_names)) {

    ui_info("Running search: no bayes, not checking names")
    input <- glue::glue('echo {double_quote(string)} | gnfinder find -n -l eng')

  }

if (!is.null(nobayes) && !is.null(source_ids)) {

    ui_info("Running search: no bayes checking against Catalogue of Life and the source_ids provided")
    input <- glue::glue('echo {double_quote(string)} | gnfinder find -n -c -l eng -s {multi_ids}')

  }

  out <- map(input, system, intern = TRUE) %>%
     map(., fromJSON) %>%
     set_names(., nm = id)

  names <- map(out, `[`, "names") %>%
    compact() %>%
    bind_rows(., .id = "id")

  names_out <- names$names %>%
    add_column(id = names$id, .before = "cardinality")  # not keen on this


}
