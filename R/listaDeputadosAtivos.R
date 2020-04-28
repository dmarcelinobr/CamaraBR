if (getRversion() >= "2.15.1")  utils::globalVariables(c("id", "nome", "variable", "siglaPartido", "siglaUf", ".dados"))
#' Fetch a list of actual deputies 
#'
#' @param url the web address 
#'
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate 
#' @importFrom dplyr select
#' @importFrom dplyr mutate_if
#' @export
#' 
listaDeputadosAtivos <- function(url="https://dadosabertos.camara.leg.br/api/v2/deputados"){

  print(url)
  
  .dados <- tryCatch({
    jsonlite::fromJSON(url, flatten = TRUE)$dados
  },error = function(e) {
    return(NULL)
  })
  
  # checa se tem resultados
  if (length(.dados) == 0) {
    return(NULL)
  }
  
  .dados <- .dados %>%
    dplyr::select(legislator_id = id,
           legislator_cham_name = nome,
           legislator_party = siglaPartido,
           legislator_state = siglaUf) %>%
    dplyr::mutate(legislator_id = as.character(legislator_id)) %>%
    dplyr::mutate_if(is.factor, as.character)
}