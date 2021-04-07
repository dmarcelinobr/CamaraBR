if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      ".votacoesPlenarioCamara",
      ".votacoesPlenarioVotos",
      ".pivot",
      ".data",
      ".cols",
      ".proposalDetails",
      "ano",
      "decision_date",
      "dat",
      "Governo"
    )
  )

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
#' @importFrom dplyr bind_rows
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
#' data <- buildRollcallDataset(year = 2021)
#'
#' @export
#' @rdname buildRollcallDataset
buildRollcallDataset <- function (year = 2021,
                                  type = "",
                                  download = TRUE,
                                  ascii = FALSE,
                                  filter = TRUE) {
  "This function lists every bill voted on in plenary."
  if (is.null(year)) {
    stop("Lacking arguments. year is mandatory")
  }
  
  if (year < 2003) {
    stop("Orientation information is only available from 2003 onwards")
  }
  
  if (download == TRUE) {
    .proposalDetails <- loadCamaraProposals(year)
    # .proposalDetails <- purrr::map_df(1988:year,~{loadCamaraProposals(.x)})
  }
  
  
  .votacoesPlenarioVotos <- loadVotacoesCamara(year)
  
  
  
  ids <- .votacoesPlenarioVotos %>%
    dplyr::pull(rollcall_id) %>% unique()
  
  
  .votacoesPlenarioCamara <- tibble()
  
  for (i in ids) {
    dat <- fetchVotacoesOrientacoesCamara(i)
    .votacoesPlenarioCamara <-
      dplyr::bind_rows(.votacoesPlenarioCamara, dat)
    message(paste0("\nFetching vote orientation of ", i))
  }
  
  # To be implemented
  # in case the API is not working, you can still try download from the repository
  # .votacoesPlenarioCamara <- loadVotacoesOrientacoesCamara(year);
  
  .votacoesPlenarioVotos <-
    dplyr::full_join(.votacoesPlenarioCamara, .votacoesPlenarioVotos) %>%
    dplyr::distinct(rollcall_id, legislator_id, .keep_all = TRUE)
  
  .data <-
    dplyr::left_join(.votacoesPlenarioVotos, .proposalDetails) %>%
    # dplyr::mutate(sigla_orgao = ifelse(is.na(sigla_orgao) & legislator_vote != "Simbolico", "PLEN", sigla_orgao)) %>%
    dplyr::mutate(legislator_vote = ifelse(legislator_vote == "", NA, legislator_vote)) %>%
    dplyr::select(
      bill_name,
      bill_id,
      rollcall_id,
      type_bill,
      number_bill,
      year_bill,
      decision_date,
      decision_time,
      rollcall_subject,
      legislator_id,
      legislator_name,
      legislator_party,
      legislator_state,
      legislator_vote,
      orientation,
      Governo,
      everything()
    )
  # decision_summary, sigla_orgao
  
  
  if (filter == TRUE) {
    .data <-
      dplyr::filter(
        .data,
        type_bill %in% c(
          "PL",
          "PEC",
          "PLP",
          "MPV",
          "REQ",
          "REC",
          "PRC",
          "PDC",
          "PDL",
          "PLN",
          "PFC",
          "PLV",
          "PLC",
          "CMC"
        )
      )
    #  sigla_orgao == "PLEN"
  }
  
  
  if (ascii == TRUE) {
    .data <-
      .data %>% dplyr::mutate_if(is.character, function(x)
        stringi::stri_trans_general(x, "Latin-ASCII"))
  }
  
  .data <- .data %>%
    dplyr::mutate(ano = lubridate::year(decision_date)) %>%
    dplyr::filter(ano == year)
  return(.data)
}
NULL
