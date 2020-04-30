if (getRversion() >= "2.15.1")  utils::globalVariables(c(".votacoesPlenarioCamara", ".votacoesPlenarioVotos", ".pivot", ".data", ".cols", ".proposalDetails", "ano", "decision_date"))

#' Fetch bills discussed and voted on the plenary
#' 
#' @param year an itneger the year querying
#' @param type a string for querying a particular type of bill
#' @param download  a logical, if TRUE it will download a database of bills before combining all data 
#' @param ascii a logical
#' @param filter a logical, if TRUE, it will filter data to a few types of bills, such as PEC, PL, PLP, PDL, PDC, MPV etc 
#'
#'
#' @importFrom stringi stri_trans_general
#' @importFrom purrr map_df
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate_if
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom lubridate year
#' @importFrom dplyr left_join
#' @importFrom dplyr pull
#' @importFrom dplyr rename_at
#' @importFrom dplyr vars
#' @importFrom dplyr everything
#' @importFrom dplyr if_else
#' @importFrom tidyr pivot_wider
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom utils data
#' @examples 
#' # data <- buildRollcallDataset(year=2020)
#' 
#' @export
#' @rdname buildRollcallDataset
buildRollcallDataset <- function (year = 2020, type = "", download = TRUE, ascii = FALSE, filter = TRUE) {

  "This function lists every bill voted on in plenary."
  if (is.null(year)) {
    stop("Lacking arguments. year is mandatory")
  }
  
  if (year < 2003) {
    stop("Orientation information is only available from 2003 onwards")
  }

  if (download == TRUE) {
    .proposalDetails <- purrr::map_df(1988:year,~{loadCamaraProposals(.x)})
  }

.votacoesPlenarioCamara <- loadVotacoesOrientacoesCamara(year);

.votacoesPlenarioVotos <- loadVotacoesCamara(year);

.votacoesPlenarioVotos <- dplyr::full_join(.votacoesPlenarioCamara, .votacoesPlenarioVotos) %>%
  dplyr::distinct(rollcall_id, legislator_id, .keep_all = TRUE)

.data <- dplyr::left_join(.votacoesPlenarioVotos, .proposalDetails) %>% 
  dplyr::mutate(sigla_orgao = ifelse(is.na(sigla_orgao) & legislator_vote != "Simbolico", "PLEN", sigla_orgao)) %>% 
  dplyr::mutate(legislator_vote = ifelse(legislator_vote == "", NA, legislator_vote)) %>% 
  dplyr::select(bill_id, rollcall_id, type_bill, number_bill, year_bill, decision_summary, decision_date, decision_time, rollcall_subject, legislator_id, legislator_name, legislator_party, legislator_state, legislator_vote, sigla_orgao, orientation, ori_GOV, everything()) 

if (filter == TRUE) {
  .data <- dplyr::filter(.data, sigla_orgao == "PLEN")
# type_bill %in% c("PL", "PEC", "PLP", "MPV", "REQ", "REC", "PRC", "PDC", "PDL", "PLN", "PFC", "PLV", "PLC")
}


if (ascii == TRUE) {
.data <- .data %>% dplyr::mutate_if(is.character, function(x) stringi::stri_trans_general(x, "Latin-ASCII"))
}

.data <- .data %>%
  dplyr::mutate(ano = lubridate::year(decision_date)) %>%
  dplyr::filter(ano == year)
return(.data)
}
NULL