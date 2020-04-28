if (getRversion() >= "2.15.1")  utils::globalVariables(c(".votacoesPlenarioCamara", ".votacoesPlenarioVotos", ".data", "ano", "decision_date"))

#' Fetch bills discussed and voted on the plenary
#' 
#' @param year an itneger the year querying
#' @param type a string for querying a particular type of bill
#' @param ascii a logical
#' @param outfile folder and name for the file
#'
#'
#' @importFrom stringi stri_trans_general
#' @importFrom purrr map_df
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate_if
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom lubridate year
#' @importFrom dplyr left_join
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom rlang :=
#' @examples 
#' # data <- buildRollcallDataset(year=2020)
#' 
#' @export
#' @rdname buildRollcallDataset
buildRollcallDataset <- function (year = 2020, type = "", ascii = FALSE, outfile = "cham_data_votes/data_") {

  "This function lists every bill voted on in plenary."
  if (is.null(year)) {
    stop("Lacking arguments. year is mandatory")
  }
  
  if (year < 2003) {
    stop("Orientation information is only available from 2003 onwards")
  }
  
.votacoesPlenarioCamara <- loadVotacoesOrientacoesCamara(year = year);
.votacoesPlenarioVotos <- loadVotacoesCamara(year = year);

.data <- dplyr::left_join(.votacoesPlenarioVotos, .votacoesPlenarioCamara)

if (ascii == TRUE) {
.data <- .data %>% dplyr::mutate_if(is.character, function(x) stringi::stri_trans_general(x, "Latin-ASCII"))
}

.data <- .data %>%
  dplyr::mutate(ano = lubridate::year(decision_date)) %>%
  dplyr::filter(ano == year)
# Saving data  
# saveRDS(.data, file=outfile %p% year %p% ".rds" )
# 
return(.data)
}
NULL