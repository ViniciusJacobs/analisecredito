#' Contagem de NA's
#' A funcao retorna o numero de NAs dentro de um vetor do data.frame
#' @description: usethis::use_pipe()
#' @param base_dados data.frame em analise
#' @param variavel nome da coluna entre (" ")
#'
#' @return Quantidade de NAs
#' @export
#'
#' @examples

contagem_nas <- function(df, variavel){

       df %>%
       dplyr::filter(dplyr::across(
      .cols = c(variavel),
      .fns = ~is.na(.x)
    )) %>%
    count()
}
























