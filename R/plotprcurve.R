#' plot.pr.curve
#'
#' @description: auxilia curva roc
#' @param predict
#' @param text
#'
#' @return
#' @export
#'
#' @examples
#'
#'
plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf, col = "black", lty = 1, lwd = 2,
       main = title.text, cex.main = 0.6, cex.lab = 0.8, xaxs = "i", yaxs = "i")
}
