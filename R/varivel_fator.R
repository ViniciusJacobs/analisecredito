#' Transformar a variavel em fator
#'
#'
#' @param df data.frame em analise
#' @param variavel nome da coluna entre (" ")
#'
#' @return variaveis tipo fator
#' @export
#'
#' @examples

to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}
