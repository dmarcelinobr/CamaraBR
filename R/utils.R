if (getRversion() >= "2.15.1")  utils::globalVariables(".")

# Emulate '+' python function for piping string joins
`%p%` <- function(e1, e2) return(paste0(e1, e2))

'%ni%' <- Negate('%in%')

# discard NA in vector
discard_na <- function(x){
  x <- as.character(x) %>% purrr::discard(is.na)
  if(purrr::is_empty(x)){
    x <- NA
  }
  return(x)
}
