#' Normalização
#'
#'
#' @param data.frame
#' @param variaveis
#'
#' @return
#' @export
#'
#' @examples
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}
