if (getRversion() >= "2.15.1")  utils::globalVariables(c(".data", ".cols"))
#' Transform the database of rollcall votes
#' 
#' Transform a database of rollcall votes, use only after downloading data with `buildRollcallDataset`
#'
#' @param .data the data frame
#' @param filter a logical, if TRUE, only rollcall votes with government orientation is returned. So "liberado" or missing cases are filtered from data output.
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringi stri_trans_general
#'
#' @export
transformVotes <- function(.data,  filter = FALSE) {
  
  if (is.null(.data)) {
    stop("Need a data frame to perform vote transformation")
  }

.data <- dplyr::mutate(.data, Governo = stringi::stri_trans_general(Governo, "Latin-ASCII"))

.data <- dplyr::mutate(.data, legislator_vote = stringi::stri_trans_general(legislator_vote, "Latin-ASCII"))

.data <- dplyr::mutate(.data, Governo = ifelse(Governo == "Sim", 1, 
                                        ifelse(Governo == "Nao", 0, 
                                        ifelse(Governo == "Liberado", NA_integer_, Governo))) %>% as.integer()) 

.data <- dplyr::mutate(.data, legislator_vote = ifelse(legislator_vote == "Sim", 1, 
                                    ifelse(legislator_vote == "Nao", 0, 
ifelse( (legislator_vote == "Obstrucao" && !is.na(Governo) ), 
        abs(Governo - 1), legislator_vote))) %>% as.integer())

if (filter) {
.dados <- .data %>% dplyr::filter(Governo == 1 | Governo == "1" | Governo == 0 | Governo == "0")
}

.dados <- dplyr::mutate(.data, governismo = ifelse(Governo == legislator_vote, 1, 0))

return(.dados)
}
NULL