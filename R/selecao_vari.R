#' Função para seleção de variáveis
#'
#'
#' @param variaveis
#' @param variaveis
#'
#' @return
#' @export
#'
#' @examples
run.feature.selection <- function(num.iters=10, feature.vars, class.var){
  set.seed(10)
  variable.sizes <- 1:10
  control <- rfeControl(functions = rfFuncs, method = "cv",
                        verbose = FALSE, returnResamp = "all",
                        number = num.iters)
  results.rfe <- rfe(x = feature.vars, y = class.var,
                     sizes = variable.sizes,
                     rfeControl = control)
  return(results.rfe)
}
