if (getRversion() >= "2.15.1")  utils::globalVariables(c("bill_id", "type_bill", "year_bill", "number_bill", "rollcall_keywords", "rollcall_subject", "rollcall_id", "legislator_id", "legislator_name", "legislator_party", "legislator_state", "legislator_vote", "uri_votacao", "orientation", "decision_summary", "sigla_bancada", "sigla_orgao", ".proposals ", ".file", ".votacoesOrientacoes", ".votacoesVotos", "siglaTipo", "id", "siglaTipo", "ementa", "numero", "keywords", "dataHoraVoto", "idVotacao", "deputado_id", "deputado_nome", "deputado_siglaUf", "deputado_siglaPartido", "orientacao", "descricao", "siglaOrgao", "siglaBancada", "voto", "uriVotacao"))

#' Load deputie's roll-call votes 
#'
#'
#' @param year An integer
#'
#' @examples 
#' 
#' # dados1 <- loadCamaraVotes(2020)
#'
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate_if
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom lubridate year
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang :=
#' @rdname loadCamaraProposals
#' @export
`loadCamaraProposals` <- function(year){
  
  if (is.null(year)) {
    stop("Lacking arguments. year is mandatory")
  }
  
  .file <- paste0("https://dadosabertos.camara.leg.br/arquivos/proposicoes/csv/proposicoes-", year, ".csv")
  
  message(paste0("\nDownloading detailed information for proposals from: ", year))
  
  .proposals <- data.table::fread(.file, colClasses = 'character', data.table = FALSE) %>%
    dplyr::rename(bill_id = id,
                  type_bill = siglaTipo, 
                  year_bill = ano,
                  number_bill = numero,
                  rollcall_keywords = keywords,
                  rollcall_subject = ementa) %>% 
    dplyr::select(bill_id, type_bill, year_bill, rollcall_subject, rollcall_keywords)
  return(.proposals)
}


#' Load orietation of deputie's roll-call votes 
#'
#'
#' @param year An integer
#'
#' @examples 
#' 
#' # dados2 <- loadVotacoesOrientacoesCamara(2020)
#'
#' @importFrom data.table fread
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#' @importFrom magrittr "%>%"
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom stringr str_replace
#' @rdname loadVotacoesOrientacoesCamara
#' @export
`loadVotacoesOrientacoesCamara` <- function(year){

  if (is.null(year)) {
    stop("Lacking arguments. year is mandatory")
  }
  
  if (year < 2003) {
    stop("Orientation information is only available from 2003 onwards")
  }

  .file <- paste0("https://dadosabertos.camara.leg.br/arquivos/votacoesOrientacoes/csv/votacoesOrientacoes-", year, ".csv")
  
.votacoesOrientacoes <- data.table::fread(.file, colClasses = 'character', data.table = FALSE) %>% 
    dplyr::rename(rollcall_id = idVotacao,
                 uri_votacao = uriVotacao, 
                 orientation = orientacao,
                decision_summary = descricao,
                sigla_orgao = siglaOrgao,
                sigla_bancada = siglaBancada) %>%
  dplyr::select(rollcall_id, decision_summary, sigla_orgao, sigla_bancada, orientation, uri_votacao)
return(.votacoesOrientacoes)
  }
NULL




#' Load deputie's roll-call votes 
#'
#' @param year An integer
#' 
#' @examples 
#' # dados3 <- loadVotacoesCamara(2020)
#' 
#' @importFrom data.table fread
#' @importFrom tibble tibble
#' @importFrom purrr map_df
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate_if
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom stringr str_replace
#' 
#' @rdname loadVotacoesCamara
#' @export
`loadVotacoesCamara` <- function(year) {

  if (is.null(year)) {
    stop("Lacking arguments. year is mandatory")
  }
  
  if (year < 2001) {
    stop("Orientation information is only available from 2001 onwards")
  }
  
  .file <-
    paste0(
      "https://dadosabertos.camara.leg.br/arquivos/votacoesVotos/csv/votacoesVotos-",
      year,
      ".csv"
    )

.votacoesVotos <- data.table::fread(.file, colClasses = 'character', data.table = FALSE) %>%
  dplyr::mutate(decision_date = as.Date(stringr::str_replace(dataHoraVoto, 'T+', ' '))) %>% # dplyr::mutate(decision_time = (stringr::str_replace(dataHoraVoto, 'T+', ' '))) %>%
    dplyr::rename(rollcall_id = idVotacao,
                  legislator_id = deputado_id,
                  legislator_name = deputado_nome,
                  legislator_party = deputado_siglaPartido, 
                  legislator_state = deputado_siglaUf,
                  legislator_vote = voto) %>% 
  dplyr::select(rollcall_id, decision_date, legislator_vote, legislator_id, legislator_name, legislator_party, legislator_state)
return(.votacoesVotos)
}
NULL