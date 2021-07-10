#' give_continuous_description
#'
#' Funzione rappresentare le percentuali.
#'
#' @param value Valore numerico.
#' @param decimals Numero di decimali da mostrare.
#' @param text Testo da mostrare prima del valore.
#' @param percent_sign Segno di percentuale da usare.
#' @param min_value Punteggio minimo rappresentabile.
#' @param max_value Punteggio massimo rappresentabile.
#' @param with_equal_sign Segno di uguale (sempre presente se c'è il testo).
#' @param with_sign Segno di +/- davanti al numero.
#' @param void_string Stringa da usare se il numero non c'è.
#' @return Una stringa rappresentante il valore T, con vari abbellimenti.
#' @export
give_continuous_description <- function (x = NA, decimals = c(2, 3), text = '', void_string = '-')
{
 x <- na.omit(x)
 if ((length(x) < 3) | is.na(x) | !is.numeric(x) | is.infinite(x)) { return(void_string) }
 out_n <- length()
 out_mean <- 
 out_sd <- 
 out_md <- 
 out_iqr <- 
 out_min <- 
 out_max <- 
 return(result)
}

#
