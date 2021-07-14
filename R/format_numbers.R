#' Arrotonda
#'
#' Funzione per rappresentare un numero arrotondato ai decimali richiesti.
#' A partire da un numero si ottiene una stringa.
#'
#' @param value Valore numerico.
#' @param decimals Numero di decimali da mostrare.
#' @return Una stringa rappresentante un numero arrotondato con i decimali indicati.
#' @export
arrotonda <- function (value, decimals = 3)
{
 return(format(round(value, decimals), nsmall = decimals))
}

#' give_nice
#'
#' Funzione per rappresentare un numero arrotondato ai decimali richiesti.
#' A partire da un numero si ottiene una stringa.
#'
#' @param value Valore numerico.
#' @param decimals Numero di decimali da mostrare.
#' @param text Testo da mostrare prima del valore.
#' @param with_equal_sign Segno di uguale (sempre presente se c'è il testo).
#' @param with_sign Segno di +/- davanti al numero.
#' @param min_value Minimo valore possibile.
#' @param max_value Massimo valore possibile.
#' @param void_string Stringa da usare se il numero non c'è.
#' @return Una stringa rappresentante un numero, con eventuale nome del valore e vari abbellimenti.
#' @export
give_nice <- function (value = NA, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = TRUE, min_value = -Inf, max_value = Inf, void_string = '-')
{
 if (text != '') { with_equal_sign = TRUE }
 if (with_equal_sign) { equal_sign <- '=' } else { equal_sign <- '' }
 if (is.na(value) | !is.numeric(value) | is.infinite(value)) { return(paste(text, equal_sign, void_string, sep = '')) }
 if (is.infinite(value))
 {
  if (is.infinite(max_value) & is.infinite(min_value))
  {
   return(paste(text, equal_sign, void_string, sep = ''))
  } else
  {
     if (value > max_value) { value <- max_value + 1 }
     if (value < min_value) { value <- min_value - 1 }
  }
 }
 if (with_sign)
 {
  if (value >= 0) { plus_sign <- '+' } else { plus_sign <- '' }
 } else { plus_sign <- '' }
 if (is.na(min_value) & is.na(max_value))
 {
  result <- arrotonda(value, decimals = decimals)
 } else if (!is.na(min_value))
 {
  if (value <= min_value)
  {
   result <- arrotonda(min_value, decimals = decimals)
  } else if (round(value, decimals) <= round(min_value, decimals))
  {
   result <- arrotonda(min_value + 10**(-1 * decimals), decimals = decimals)
   equal_sign <- '<'
  } else if (!is.na(max_value))
  {
   if (value >= max_value)
   {
    result <- arrotonda(max_value, decimals = decimals)
   } else if (round(value, decimals) >= round(max_value, decimals))
   {
    result <- arrotonda(max_value - 10**(-1 * decimals), decimals = decimals)
    equal_sign <- '>'
   } else
   {
    result <- arrotonda(value, decimals = decimals)
   }
  } else
  {
   result <- arrotonda(value, decimals = decimals)
  }
 }
 result <- paste(text, equal_sign, plus_sign, result, sep = '')
 return(result)
}

#' give_nice_p
#'
#' Funzione per rappresentare i valori di p.
#' Comprende gli asterischi per la significatività statistica.
#'
#' @param value Valore numerico.
#' @param decimals Numero di decimali da mostrare.
#' @param with_p Mostra il 'p', prima del numero.
#' @param with_equal_sign Segno di uguale (sempre presente se c'è il testo).
#' @param with_stars Mostra un asterisco se statisticamente significativo.
#' @param multiple_stars Mostra da uno a tre asterischi se statisticamente significativo.
#' @param alpha Significatività statistica (0.050)
#' @param multiple_alphas Vettore con tre livelli di significatività statistica da asteriscare.
#' @param give_only_stars Non restituisce il valore numerico, ma solo gli asterischi.
#' @param void_string Stringa da usare se il numero non c'è.
#' @return Una stringa rappresentante il valore di p, con vari abbellimenti.
#' @export
give_nice_p <- function (value = NA, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), give_only_stars = FALSE, void_string = '-')
{
 if (with_p) { text <- 'p' ; with_equal_sign = TRUE } else { text <- '' }
 if (with_stars & !multiple_stars & (value < alpha)) { stars = '*' } else { stars = '' }
 if (multiple_stars)
 {
  stars <- ''
  if (value < multiple_alphas[1]) { stars <- paste(stars, '*', sep = '') }
  if (value < multiple_alphas[2]) { stars <- paste(stars, '*', sep = '') }
  if (value < multiple_alphas[3]) { stars <- paste(stars, '*', sep = '') }
 }
 min_value = 0 - 10**(-1 * (decimals + 1))
 max_value = 1
 result <- give_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = FALSE, min_value = min_value, max_value = max_value, void_string = void_string)
 if (!give_only_stars) { result <- paste(result, stars, sep = '') } else { result <- stars }
 return(result)
}

#' give_nice_r
#'
#' Funzione per rappresentare i valori di r.
#'
#' @param value Valore numerico.
#' @param decimals Numero di decimali da mostrare.
#' @param with_r Mostra il 'r', prima del numero.
#' @param with_equal_sign Segno di uguale (sempre presente se c'è il testo).
#' @param void_string Stringa da usare se il numero non c'è.
#' @return Una stringa rappresentante il valore di r, con vari abbellimenti.
#' @export
give_nice_r <- function (value = NA, decimals = 3, with_r = TRUE, with_equal_sign = FALSE, void_string = '-')
{
 if (with_r) { text <- 'r' ; with_equal_sign = TRUE } else { text <- '' }
 min_value = -1
 max_value = 1
 result <- give_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = TRUE, min_value = min_value, max_value = max_value, void_string = void_string)
 return(result)
}

#' give_nice_z
#'
#' Funzione per rappresentare i valori z (ma applicabile anche ad altre standardizzazioni).
#'
#' @param value Valore numerico.
#' @param decimals Numero di decimali da mostrare.
#' @param text Testo da mostrare prima del valore.
#' @param standard_mean Media del punteggio standard.
#' @param standard_sd SD del punteggio standard.
#' @param min_z Minimo in SD dalla media.
#' @param max_z Massimo in SD dalla media.
#' @param with_equal_sign Segno di uguale (sempre presente se c'è il testo).
#' @param with_sign Segno di +/- davanti al numero.
#' @param void_string Stringa da usare se il numero non c'è.
#' @return Una stringa rappresentante il valore z, con vari abbellimenti.
#' @export
give_nice_z <- function (value = NA, decimals = 3, text = '', standard_mean = 0, standard_sd = 1, min_z = NA, max_z = NA, with_equal_sign = FALSE, with_sign = TRUE, void_string = '-')
{
 if (text != '') { with_equal_sign = TRUE }
 if (!is.na(min_z)) { min_value <- standard_mean - (abs(min_z) * standard_sd) } else { min_value <- NA }
 if (!is.na(max_z)) { max_value <- standard_mean + (abs(max_z) * standard_sd) } else { max_value <- NA }
 result <- give_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = TRUE, min_value = min_value, max_value = max_value, void_string = void_string)
 return(result)
}

#' give_nice_t
#'
#' Funzione per rappresentare i valori T (50, 10).
#'
#' @param value Valore numerico.
#' @param decimals Numero di decimali da mostrare.
#' @param text Testo da mostrare prima del valore.
#' @param standard_mean Media del punteggio standard.
#' @param standard_sd SD del punteggio standard.
#' @param min_value Punteggio minimo rappresentabile.
#' @param max_value Punteggio massimo rappresentabile.
#' @param with_equal_sign Segno di uguale (sempre presente se c'è il testo).
#' @param with_sign Segno di +/- davanti al numero.
#' @param void_string Stringa da usare se il numero non c'è.
#' @return Una stringa rappresentante il valore T, con vari abbellimenti.
#' @export
give_nice_t <- function (value = NA, decimals = 1, text = 'T', standard_mean = 50, standard_sd = 10, min_value = 0, max_value = 100, with_equal_sign = FALSE, with_sign = FALSE, void_string = '-')
{
 if (text != '') { with_equal_sign = TRUE }
 result <- give_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = TRUE, min_value = min_value, max_value = max_value, void_string = void_string)
 return(result)
}

#' give_nice_standard
#'
#' Funzione per rappresentare i valori Standard (100, 15).
#'
#' @param value Valore numerico.
#' @param decimals Numero di decimali da mostrare.
#' @param text Testo da mostrare prima del valore.
#' @param standard_mean Media del punteggio standard.
#' @param standard_sd SD del punteggio standard.
#' @param min_value Punteggio minimo rappresentabile.
#' @param max_value Punteggio massimo rappresentabile.
#' @param with_equal_sign Segno di uguale (sempre presente se c'è il testo).
#' @param with_sign Segno di +/- davanti al numero.
#' @param void_string Stringa da usare se il numero non c'è.
#' @return Una stringa rappresentante il valore T, con vari abbellimenti.
#' @export
give_nice_standard <- function (value = NA, decimals = 1, text = 'Standard', standard_mean = 100, standard_sd = 15, min_value = 0, max_value = 200, with_equal_sign = FALSE, with_sign = FALSE, void_string = '-')
{
 if (text != '') { with_equal_sign = TRUE }
 result <- give_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = TRUE, min_value = min_value, max_value = max_value, void_string = void_string)
 return(result)
}

#' give_nice_percent
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
give_nice_percent <- function (value = NA, decimals = 2, text = '', percent_sign = TRUE, min_value = 0, max_value = 100, with_equal_sign = FALSE, with_sign = FALSE, void_string = '-')
{
 if (text != '') { with_equal_sign = TRUE }
 if (percent_sign) { percent_sign = '%' } else { percent_sign = '' }
 result <- give_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = with_sign, min_value = min_value, max_value = max_value, void_string = void_string)
 result <- paste(result, percent_sign, sep = '')
 return(result)
}

#
