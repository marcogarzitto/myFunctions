#' give_mean_confidence_intervals
#'
#' Funzione per ottenere l'intervallo di confidenza della media.
#'
#' @param x Vettore numerico.
#' @param confidence Ampiezza dell'intervallo di confidenza voluto.
#' @param distribution Usare una distribuzione z o t (per campioni piccoli o mal distribuiti).
#' @return Un vettore (utile per inserirlo in un data-frame).
#' @export
give_mean_confidence_intervals <- function (x, confidence = 0.950, distribution = 'z')
{
 x <- psych::describe(x)
 if (distribution == 'z')
 {
  plus_minus <- (abs(qnorm((1 - confidence) / 2)) * x$se)
 } else if (distribution == 't')
 {
  plus_minus <- (qt(confidence + (1 - confidence) / 2, df = (x$n - 1)) * x$se)
 } else
 {
  plus_minus <- 0
 }
 return(c(y = x$mean,
          ymin = x$mean - plus_minus,
          ymax = x$mean + plus_minus))
}

#
