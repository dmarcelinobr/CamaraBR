if (getRversion() >= "2.15.1")  utils::globalVariables(c("bill_id", "type_bill", "year_bill", "number_bill", "rollcall_keywords", "rollcall_subject", "rollcall_id", "legislator_id", "decision_time", "legislator_name", "legislator_party", "legislator_state", "legislator_vote", "uri_votacao", "orientation", "decision_summary", "sigla_bancada", "sigla_orgao", ".proposals ", ".file", ".votacoesOrientacoes", ".votacoesVotos", "siglaTipo", "id", "siglaTipo", "ementa", "numero", "keywords", "dataHoraVoto", "idVotacao", "deputado_id", "deputado_nome", "deputado_siglaUf", "deputado_siglaPartido", "orientacao", "descricao", "siglaOrgao", "siglaBancada", "voto", "uriVotacao", "GOV_orientation", "Oposicao_orientation", "Minoria_orientation", "Maioria_orientation"))

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
    dplyr::select(bill_id, type_bill, year_bill, number_bill, rollcall_subject, rollcall_keywords)
  return(.proposals)
}
NULL











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
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace_all
#' @importFrom stringi stri_trans_general
#' @importFrom stats na.omit
#' @rdname loadVotacoesOrientacoesCamara
#' @export
`loadVotacoesOrientacoesCamara` <- function(year) {
  if (is.null(year)) {
    stop("Lacking arguments. year is mandatory")
  }
  
  if (year < 2003) {
    stop("Orientation information is only available from 2003 onwards")
  }
  
  .file <-
    paste0(
      "https://dadosabertos.camara.leg.br/arquivos/votacoesOrientacoes/csv/votacoesOrientacoes-",
      year,
      ".csv"
    )
  
  message(paste0("\nDownloading rollcall orientation for proposals of ", year))
  
  .votacoesOrientacoes <-
    data.table::fread(.file, colClasses = 'character', data.table = FALSE) %>%
    dplyr::mutate(sigla_bancada = stringi::stri_trans_general(siglaBancada, "Latin-ASCII")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^MINORIA$", "Minoria"))  %>%
    dplyr::mutate(sigla_bancada =  stringr::str_replace_all(sigla_bancada, "^MAIORIA$", "Maioria"))  %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^AVANTE$", "Avante"))  %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada,  "^CIDADANIA$", "Cidadania")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PODEMOS$", "Podemos")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PODE$", "Podemos")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^REDE$", "Rede")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PC do B$|^PC DO B$", "PCdoB")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PCDOB$|^PCDoB$", "PCdoB")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^GOV.$", "GOV")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^GOV$", "GOV")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^Oposicao$", "Oposicao")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^NOVO$", "Novo")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PATRIOTA$", "Patriota")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PATRI$", "Patriota")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^NOVO$", "Novo")) %>%
    dplyr::mutate(
      sigla_bancada = stringr::str_replace_all(sigla_bancada, "^SOLIDARIEDADE$", "Solidaried")
    ) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^SDD$", "Solidaried")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^REPUBLICANO$", "Republican")) %>%
    dplyr::mutate(bill_id = stringr::str_extract(idVotacao, ".+?(?=-)")) %>%
    dplyr::rename(
      rollcall_id = idVotacao,
      uri_votacao = uriVotacao,
      orientation = orientacao,
      decision_summary = descricao,
      sigla_orgao = siglaOrgao
    ) %>%
    dplyr::select(
      bill_id,
      rollcall_id,
      decision_summary,
      sigla_orgao,
      sigla_bancada,
      orientation
      )
  
  .cols <-.votacoesOrientacoes %>% pull(sigla_bancada) %>% unique %>% na.omit %>% as.vector()
  .data <- .votacoesOrientacoes %>% 
    tidyr::pivot_wider(names_from = sigla_bancada, values_from = orientation) %>%
    dplyr::select(-bill_id, -decision_summary, -sigla_orgao)
  
  .votacoesOrientacoes <- left_join(.votacoesOrientacoes, .data, by =  'rollcall_id') %>% rename_at(vars(.cols), ~paste0(., '_orientation'))
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
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace_all
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
  
  message(paste0("\nDownloading rollcall votes for proposals of ", year))
  
  .votacoesVotos <-
    data.table::fread(.file, colClasses = 'character', data.table = FALSE) %>%
    dplyr::mutate(decision_date = as.Date(stringr::str_replace(dataHoraVoto, 'T+', ' '))) %>%
    dplyr::mutate(decision_time = stringr::str_extract(dataHoraVoto, '(?<=T).+')) %>%
    dplyr::mutate(bill_id = stringr::str_extract(idVotacao, ".+?(?=-)")) %>%
    dplyr::mutate(
      deputado_siglaPartido = stringr::str_replace_all(deputado_siglaPartido, "^PATRIOTA$", "PATRI")
    ) %>%
    dplyr::mutate(
      deputado_siglaPartido = stringr::str_replace_all(deputado_siglaPartido, "^SD$", "SOLIDARIEDADE")
    ) %>%
    dplyr::mutate(
      deputado_siglaPartido = stringr::str_replace_all(deputado_siglaPartido, "^SDD$", "SOLIDARIEDADE")
    ) %>%
    dplyr::rename(
      rollcall_id = idVotacao,
      legislator_id = deputado_id,
      legislator_name = deputado_nome,
      legislator_party = deputado_siglaPartido,
      legislator_state = deputado_siglaUf,
      legislator_vote = voto
    ) %>%
    dplyr::select(
      bill_id,
      rollcall_id,
      decision_date,
      decision_time,
      legislator_vote,
      legislator_id,
      legislator_name,
      legislator_party,
      legislator_state
    )
  return(.votacoesVotos)
}
NULL