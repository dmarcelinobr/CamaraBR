if (getRversion() >= "2.15.1")  utils::globalVariables(c(".data", ".cols"))
#' Transform the database of rollcall votes
#' 
#' Transform a database of rollcall votes, use only after downloading data with `buildRollcallDataset`
#'
#' @param .data the data frame
#' @param filter a logical, if TRUE, only rollcall votes with strictly government orientation and legislator vote cast will be returned. So "liberado" cases are filtered from data.
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringi stri_trans_general
#'
#' @export
transformVotes <- function(.data,  filter = TRUE) {
  
  if (is.null(.data)) {
    stop("Need a data frame to perform vote transformation")
  }

.data <- dplyr::mutate(.data, ori_GOV = stringi::stri_trans_general(ori_GOV, "Latin-ASCII"))
.data <- dplyr::mutate(.data, legislator_vote = stringi::stri_trans_general(legislator_vote, "Latin-ASCII"))

.data <- dplyr::mutate(.data, ori_GOV = ifelse(ori_GOV == "Sim", 1, 
                                        ifelse(ori_GOV == "Nao", 0, 
                                          ifelse(ori_GOV == "Liberado", NA_integer_, ori_GOV)))) 

.data <- dplyr::mutate(.data, legislator_vote = ifelse(legislator_vote == "Sim", 1, 
                                    ifelse(legislator_vote == "Nao", 0, 
ifelse( (legislator_vote == "Obstrucao" && !is.na(ori_GOV) ), 
        abs(ori_GOV - 1), legislator_vote))))
  if (filter) {
.data <- dplyr::filter(.data, ori_GOV == "1" | ori_GOV == "0", legislator_vote == "0" |
legislator_vote == "1")
  }
.data <- dplyr::mutate(.data, governismo = ifelse(ori_GOV == legislator_vote, 1, 0))
return(.data)
}
NULL