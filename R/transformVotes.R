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

.data <- dplyr::mutate(.data, ori_Governo = stringi::stri_trans_general(ori_Governo, "Latin-ASCII"))
.data <- dplyr::mutate(.data, legislator_vote = stringi::stri_trans_general(legislator_vote, "Latin-ASCII"))

.data <- dplyr::mutate(.data, ori_Governo = ifelse(ori_Governo == "Sim", 1, 
                                        ifelse(ori_Governo == "Nao", 0, 
                                          ifelse(ori_Governo == "Liberado", NA_integer_, ori_Governo)))) 

.data <- dplyr::mutate(.data, legislator_vote = ifelse(legislator_vote == "Sim", 1, 
                                    ifelse(legislator_vote == "Nao", 0, 
ifelse( (legislator_vote == "Obstrucao" && !is.na(ori_Governo) ), 
        abs(ori_Governo - 1), legislator_vote))))
  if (filter) {
.data <- dplyr::filter(.data, ori_Governo == "1" | ori_Governo == "0", legislator_vote == "0" |
legislator_vote == "1")
  }
.data <- dplyr::mutate(.data, governismo = ifelse(ori_Governo == legislator_vote, 1, 0))
return(.data)
}
NULL