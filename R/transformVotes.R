if (getRversion() >= "2.15.1")  utils::globalVariables(c(".data", ".cols"))
#' Transform the database of rollcall votes
#' 
#' Transform a database of rollcall votes, use only after downloading data with `buildRollcallDataset`
#'
#' @param .data the data frame
#' @param filter a logical, if TRUE
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringi stri_trans_general
#'
#' @export
transformVotes <- function(.data, filter = TRUE) {
 .data <- .data %>%
dplyr::mutate(GOV_orientation = stringi::stri_trans_general(GOV_orientation, "Latin-ASCII")) %>% 
dplyr::mutate(legislator_vote = stringi::stri_trans_general(legislator_vote, "Latin-ASCII"))

.data <- .data %>%
     dplyr::mutate(GOV_orientation = ifelse(stringr::str_detect(GOV_orientation, "Sim"), 1, 
                                        ifelse(stringr::str_detect(GOV_orientation, "Nao"), 0, 
                                               ifelse(stringr::str_detect(GOV_orientation, "Liberado"), NA, GOV_orientation)))) 
  
  .data <- .data %>%
    dplyr::mutate(legislator_vote = 
                    ifelse( stringr::str_detect(legislator_vote, "Sim"), 1, 
                        ifelse(stringr::str_detect(legislator_vote, "Nao"), 0, 
                        ifelse( stringr::str_detect(legislator_vote, "Obstrucao") && !is.na(GOV_orientation), abs(GOV_orientation - 1), legislator_vote))))
  if (!filter) {
    .data <- dplyr::filter(.data, GOV_orientation == "1" |
                                 GOV_orientation == "0", legislator_vote =="0" |
                                 legislator_vote == "1" )
  }
  .data <- dplyr::mutate(.data, governismo = ifelse(GOV_orientation == legislator_vote, 1, 0))
return(.data)
}
NULL