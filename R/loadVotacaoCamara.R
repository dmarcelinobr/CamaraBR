if (getRversion() >= "2.15.1")  utils::globalVariables(c("bill_id", "type_bill", "year_bill", "number_bill", "rollcall_keywords", "rollcall_subject", "rollcall_id", "legislator_id", "decision_time", "legislator_name", "legislator_party", "legislator_state", "legislator_vote", "uri_votacao", "orientation", "decision_summary", "sigla_bancada", "sigla_orgao", ".proposals ", ".file", ".votacoesOrientacoes", ".votacoesVotos", "siglaTipo", "id", "siglaTipo", "ementa", "numero", "keywords", "dataHoraVoto", "idVotacao", "deputado_id", "deputado_nome", "deputado_siglaUf", "deputado_siglaPartido", "orientacao", "descricao", "siglaOrgao", "siglaBancada", "voto", "uriVotacao", "ori_Governo", "ori_GOV", "ori_Oposicao", "ori_Minoria", "ori_Maioria", "proposicao_ano", "proposicao_ementa", "proposicao_siglaTipo", "proposicao_titulo", "proposicao_id", "proposicao_numero", "bill_name", "dataRegistroVoto", "codPartidoBloco", "codPartido", "siglaPartidoBloco", "orientacaoVoto", "tipoVoto"))

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
  
  .file <- paste0("https://dadosabertos.camara.leg.br/arquivos/votacoesProposicoes/csv/votacoesProposicoes-", year, ".csv")
  
  message(paste0("\nDownloading detailed information for proposals from: ", year))
  
  .proposals <- data.table::fread(.file, colClasses = 'character', data.table = FALSE) %>%
    dplyr::rename(rollcall_id = idVotacao,
                  bill_id = proposicao_id,
                  bill_name = proposicao_titulo,
                  type_bill = proposicao_siglaTipo, 
                  year_bill = proposicao_ano,
                  number_bill = proposicao_numero,
                  rollcall_subject = proposicao_ementa) %>% 
    dplyr::select(rollcall_id, bill_id, bill_name, type_bill, year_bill, number_bill, rollcall_subject)
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
#' @importFrom tidyr pivot_wider
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
    dplyr::mutate(orientacao = stringi::stri_trans_general(orientacao, "Latin-ASCII")) %>%   dplyr::mutate(sigla_bancada = stringi::stri_trans_general(siglaBancada, "Latin-ASCII")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^MINORIA$", "Minoria"))  %>%
    dplyr::mutate(sigla_bancada =  stringr::str_replace_all(sigla_bancada, "^MAIORIA$", "Maioria"))  %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^AVANTE$", "Avante"))  %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada,  "^CIDADANIA$", "Cidadania")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PODEMOS$", "Podemos")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PODE$", "Podemos")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^REDE$", "Rede")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PC do B$|^PC DO B$", "PCdoB")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PCDOB$|^PCDoB$", "PCdoB")) %>%
    dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^GOV.$", "Governo")) %>%
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
  
  .data <- .votacoesOrientacoes %>%
    tidyr::pivot_wider(names_from = sigla_bancada, values_from = orientation, names_prefix = 'ori_') %>% left_join(.votacoesOrientacoes %>% select(-bill_id, -sigla_orgao, -decision_summary), by = 'rollcall_id')
  
  return(.data)
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
    dplyr::mutate(voto = stringi::stri_trans_general(voto, "Latin-ASCII")) %>%  
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



#' Fetch roll-call party/bloc vote orientation 
#' 
#' @param id A string for the roll-call id
#'
#'
#' @examples 
#'
#'  dat <- fetchVotacoesOrientacoesCamara("2206395-58")
#'  
#' @importFrom tibble tibble
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate_if
#' @importFrom dplyr rename
#' @importFrom stringr str_replace
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace_all
#' @importFrom stringi stri_trans_general
#' @importFrom stats na.omit
#' @importFrom tidyr pivot_wider
#' @rdname fetchVotacoesOrientacoesCamara
#' @export 
fetchVotacoesOrientacoesCamara <- function(id){

# url <- "https://dadosabertos.camara.leg.br/api/v2/votacoes/","2251392-38","/orientacoes/"
  url <- paste0("https://dadosabertos.camara.leg.br/api/v2/votacoes/",id,"/orientacoes/")

  status_code <- 500
  while(status_code != 200){
    try(resp <- httr::GET(url, httr::accept_json()))
    status_code = httr::status_code(resp)
  }
  
  dados <- httr::content(resp,  type = "text", encoding = "UTF-8") %>% 
    jsonlite::fromJSON() %>%
    .[["dados"]]
  
   if(!is.null(dados)){
     dados <- dados %>% 
     dplyr::mutate(rollcall_id = id) %>%
     dplyr::mutate(bill_id = stringr::str_extract(id, ".+?(?=-)")) %>%
     dplyr::select(bill_id, rollcall_id, codPartidoBloco, siglaPartidoBloco, orientacaoVoto)

dados <- dados %>%
 dplyr::mutate(orientation = stringi::stri_trans_general(orientacaoVoto, "Latin-ASCII")) %>%      dplyr::mutate(sigla_bancada = stringi::stri_trans_general(siglaPartidoBloco, "Latin-ASCII")) %>%
  dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^MINORIA$", "Minoria"))  %>%
  dplyr::mutate(sigla_bancada =  stringr::str_replace_all(sigla_bancada, "^MAIORIA$", "Maioria"))  %>%
  dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PODEMOS|Podemos$", "PODE")) %>%
  dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PC do B$|^PC DO B$", "PCdoB")) %>%
  dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PCDOB$|^PCDoB$", "PCdoB")) %>%
  dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^GOV.$", "Governo")) %>%
  dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^PATRI$", "Patriota")) %>%
  dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^SDD$", "SOLIDARIEDADE")) %>%
  dplyr::mutate(sigla_bancada = stringr::str_replace_all(sigla_bancada, "^Republican$", "REPUBLICANOS")) %>%
  dplyr::select(
    bill_id,
    rollcall_id,
   sigla_bancada,
    orientation
  )

dados %>% tidyr::pivot_wider(names_from = sigla_bancada, values_from = orientation, names_prefix = 'ori_') %>% left_join(dados %>% select(-bill_id), by = 'rollcall_id')

  } else {
    return(NULL)
  }

}
NULL





#' Fetch deputie's roll-call vote given a roll-call id
#' 
#' @param id A string for the rollcall id
#'
#' @examples 
#'  # dat <- fetchVotacoesCamara("2206395-58")
#' 
# fetchVotacoesCamara <- function(id){
#  library(tidyverse)
  # url <- paste0("https://dadosabertos.camara.leg.br/api/v2/votacoes/2206395-58/votos/")
#  url <- paste0("https://dadosabertos.camara.leg.br/api/v2/votacoes/",id,"/votos/")
  
  
#  tryCatch({
#    resp <- httr::GET(url, httr::accept_json())
#    status_code = httr::status_code(resp)
#    data <- httr::content(resp, type = "text", encoding = "UTF-8") %>% 
#      jsonlite::fromJSON() %>%
#      .[["dados"]]
    
#    data <- data %>% 
#      dplyr::mutate(rollcall_id = id) %>%
#      dplyr::mutate(bill_id = stringr::str_extract(id, ".+?(?=-)")) %>%
#      dplyr::rename(siglaUf = deputado_.siglaUf, siglaPartido = deputado_.siglaPartido, nome = #deputado_.nome, idParlamentar = deputado_.id) %>%
#      dplyr::select(bill_id, rollcall_id, codPartido, siglaPartido, siglaUf, nome, tipoVoto, #dataRegistroVoto)
#  })
  
#  return(data)
# }