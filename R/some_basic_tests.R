#' give_chisquared
#'
#' Test del Chi quadrato.
#'
#' @param x Vettore di fattori.
#' @param y Vettore di fattori.
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @return Il p risultante.
#' @export
give_chisquared <- function (x = NA, y = NA, void_string = '-')
{
 XY <- na.omit(data.frame(X = x, Y = y))
 result <- void_string
 try (result <- chisq.test(XY$X, XY$Y)$p.value, silent = TRUE)
 return(result)
}

#' give_fisher
#'
#' Test esatto di Fisher.
#'
#' @param x Vettore di fattori.
#' @param y Vettore di fattori.
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @return Il p risultante.
#' @export
give_fisher <- function (x = NA, y = NA, void_string = '-')
{
 if (!is.factor(x)) { x <- ordered(x) } 
 if (!is.factor(y)) { y <- ordered(y) } 
 XY <- na.omit(data.frame(X = x, Y = y))
 result <- void_string
 if ((levels(XY$X)) == 2 & (levels(XY$Y) == 2))
 {
  try (result <- fisher.test(XY$X, XY$Y)$p.value, silent = TRUE)
 }
 return(result)
}

#
