#' give_chisquare
#'
#' Test del Chi quadrato.
#'
#' @param x Vettore di fattori.
#' @param y Vettore di fattori.
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @param alpha_value Valore di alpha per la significatività del test.
#' @param multiple_alphas Valori di alpha per gli asterischi sulla significatività del test.
#' @return Un vettore con il risultato del test, con il valore di p risultante e con la comparazione fra i gruppi.
#' @export
give_chisquare <- function (x = NA, y = NA, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
{
 out <- void_string
 p_value <- NA
 comparison <- '-'
 note <- ''
 if (length(x) != length(y)) { return(c(out, p_value)) }
 if (!is.factor(x)) { x <- ordered(x) }
 if (!is.factor(y)) { y <- ordered(y) }
 XY <- na.omit(data.frame(X = x, Y = y))
 EXPECTED <- ((as.matrix(apply(table(XY), 2, sum)) %*% apply(table(XY), 1, sum)) / sum(table(XY)))
 if (sum(!apply(EXPECTED, 1, is.na)) > 0)
 {
  if (min(EXPECTED) < 5) { note <- ' (not-applicable)' }
 }
 if ((length(levels(XY$X)) >= 2) & (length(levels(XY$Y)) >= 2)
     &
     (length(levels(ordered(as.character(XY$X)))) >= 2) & (length(levels(ordered(as.character(XY$Y)))) >= 2))
 {
  TEST <- chisq.test(XY$X, XY$Y)
  out <- paste('\u03C7', '\u00B2',
               '(',
               TEST$parameter,
               ')',
               '=',
               give_nice(value = TEST$statistic, decimals = 2, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
               note,
               ',',
               ' ',
               give_nice_p(value = TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
               sep = '')
  p_value <- TEST$p.value
  if (p_value < alpha_value)
  {
   if (prop.table(table(XY$X, XY$Y), 2)[2, 1] > prop.table(table(XY$X, XY$Y), 2)[2, 2]) { comparison <- paste(levels(XY$X)[2], ': ', levels(XY$Y)[1], '>', levels(XY$Y)[2], sep = '') }
   if (prop.table(table(XY$X, XY$Y), 2)[2, 1] < prop.table(table(XY$X, XY$Y), 2)[2, 2]) { comparison <- paste(levels(XY$X)[2], ': ', levels(XY$Y)[1], '<', levels(XY$Y)[2], sep = '') }
  } else
  {
   comparison <- paste(levels(XY$X)[2], ': ', levels(XY$Y)[1], '=', levels(XY$Y)[2], sep = '')
  }
 }
 return(c(out, p_value, comparison))
}

#' give_fisher
#'
#' Test esatto di Fisher.
#'
#' @param x Vettore di fattori.
#' @param y Vettore di fattori.
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @param alpha_value Valore di alpha per la significatività del test.
#' @param multiple_alphas Valori di alpha per gli asterischi sulla significatività del test.
#' @return Un vettore con il risultato del test, con il valore di p risultante e con la comparazione fra i gruppi.
#' @export
give_fisher <- function (x = NA, y = NA, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
{
 out <- void_string
 p_value <- NA
 comparison <- '-'
 if (length(x) != length(y)) { return(c(out, p_value)) }
 if (!is.factor(x)) { x <- ordered(x) }
 if (!is.factor(y)) { y <- ordered(y) }
 XY <- na.omit(data.frame(X = x, Y = y))
 if ((length(levels(XY$X)) == 2) & (length(levels(XY$Y)) == 2)
     &
     (length(levels(ordered(as.character(XY$X)))) == 2) & (length(levels(ordered(as.character(XY$Y)))) == 2))
 {
  TEST <- fisher.test(XY$X, XY$Y)
  out <- paste('OR',
               '=',
               give_nice(value = abs(TEST$estimate), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
               ' ',
               '(',
               give_nice(value = abs(TEST$conf.int[1]), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
               ', ',
               give_nice(value = abs(TEST$conf.int[2]), decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0.001, max_value = 1000, void_string = void_string),
               ')',
               ',',
               ' ',
               give_nice_p(value = TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
               sep = '')
  p_value <- TEST$p.value
  if (p_value < alpha_value)
  {
   if (prop.table(table(XY$X, XY$Y), 2)[2, 1] > prop.table(table(XY$X, XY$Y), 2)[2, 2]) { comparison <- paste(levels(XY$X)[2], ': ', levels(XY$Y)[1], '>', levels(XY$Y)[2], sep = '') }
   if (prop.table(table(XY$X, XY$Y), 2)[2, 1] < prop.table(table(XY$X, XY$Y), 2)[2, 2]) { comparison <- paste(levels(XY$X)[2], ': ', levels(XY$Y)[1], '<', levels(XY$Y)[2], sep = '') }
  } else
  {
   comparison <- paste(levels(XY$X)[2], ': ', levels(XY$Y)[1], '=', levels(XY$Y)[2], sep = '')
  }
 }
 return(c(out, p_value, comparison))
}

#' give_categorical_test
#'
#' Test di frequenza per 2 categorie incrociate (Chi quadro o test di Fisher).
#'
#' @param x Vettore di fattori.
#' @param y Vettore di fattori.
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @param alpha_value Valore di alpha per la significatività del test.
#' @param multiple_alphas Valori di alpha per gli asterischi sulla significatività del test.
#' @return Un vettore con il risultato del test, con il valore di p risultante e con la comparazione fra i gruppi.
#' @export
give_categorical_test <- function (x = NA, y = NA, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
{
 if (!is.factor(x)) { x <- ordered(x) } 
 if (!is.factor(y)) { y <- ordered(y) } 
 XY <- na.omit(data.frame(X = x, Y = y))
 result <- void_string
 fisher <- give_fisher(x = XY$X, y = XY$Y, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas)
 chisquare <- give_chisquare(x = XY$X, y = XY$Y, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas)
 if (is.na(fisher[2])) { result <- chisquare } else { result <- fisher }
 return(result)
}

#' give_ttest
#'
#' Test t.
#'
#' @param y Vettore numerico.
#' @param group Vettore di fattori, a 2 livelli.
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @param alpha_value Valore di alpha per la significatività del test.
#' @param multiple_alphas Valori di alpha per gli asterischi sulla significatività del test.
#' @return Un vettore con il risultato del test, con il valore di p risultante e con la comparazione fra i gruppi.
#' @export
give_ttest <- function (y = NA, group = NA, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
{
 if (!is.factor(group)) { group <- ordered(group) }  
 DATA <- na.omit(data.frame(Y = y, G = group))
 if ((min(table(DATA$G)) >= 3) & (sd(DATA$Y) > 0))
 {
  LEVENE <- car::leveneTest(Y ~ G, data = DATA, center = median)
  note <- ''
  if (LEVENE$'Pr(>F)'[1] < 0.050) { note <- ' (not-applicable)' }
  TEST <- t.test(Y ~ G, data = DATA)
  result <- paste('t',
                  '(',
                  give_nice(value = TEST$parameter, decimals = 1, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = 0, max_value = Inf, void_string = void_string),
                  ')',
                  give_nice(value = TEST$statistic, decimals = 2, text = '', with_equal_sign = TRUE, with_sign = FALSE, min_value = -1000, max_value = 1000, void_string = void_string),
                  note,
                  ', ',
                  give_nice_p(value = TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
                  sep = '')
  if (TEST$p.value < alpha_value)
  {
   if (mean(DATA$Y[DATA$G == levels(group)[1]], na.rm = TRUE) > mean(DATA$Y[DATA$G == levels(group)[2]], na.rm = TRUE)) { comparison <- paste(levels(group)[1], '>', levels(group)[2], sep = '') }
   if (mean(DATA$Y[DATA$G == levels(group)[1]], na.rm = TRUE) < mean(DATA$Y[DATA$G == levels(group)[2]], na.rm = TRUE)) { comparison <- paste(levels(group)[1], '<', levels(group)[2], sep = '') }
  } else
  {
   comparison <- paste(levels(group)[1], '=', levels(group)[2], sep = '')
  }
  result <- c(result, TEST$p.value, comparison)
  return(result)
 } else { return(c(void_string, NA, NA)) }
}

#' give_mannwhitney
#'
#' Test di Mann-Whitney.
#'
#' @param y Vettore numerico.
#' @param group Vettore di fattori, a 2 livelli.
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @param alpha_value Valore di alpha per la significatività del test.
#' @param multiple_alphas Valori di alpha per gli asterischi sulla significatività del test.
#' @return Un vettore con il risultato del test, con il valore di p risultante e con la comparazione fra i gruppi.
#' @export
give_mannwhitney <- function (y = NA, group = NA, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
{
 if (!is.factor(group)) { group <- ordered(group) }  
 DATA <- na.omit(data.frame(Y = y, G = group))
 if ((min(table(DATA$G)) >= 3) & (sd(DATA$Y) > 0))
 {
  TEST <- wilcox.test(Y ~ G, data = DATA, exact = TRUE, correct = TRUE)
  result <- paste(give_nice(value = TEST$statistic, decimals = 1, text = 'U', with_equal_sign = TRUE, with_sign = FALSE, min_value = 0, max_value = 99999.9, void_string = void_string),
                  ', ',
                  give_nice_p(value = TEST$p.value, decimals = 3, with_p = TRUE, with_equal_sign = FALSE, with_stars = TRUE, multiple_stars = TRUE, alpha = alpha_value, multiple_alphas = multiple_alphas, give_only_stars = FALSE, void_string = void_string),
                  sep = '')
  if (TEST$p.value < alpha_value)
  {
   if (mean(DATA$Y[DATA$G == levels(group)[1]], na.rm = TRUE) > mean(DATA$Y[DATA$G == levels(group)[2]], na.rm = TRUE)) { comparison <- paste(levels(group)[1], '>', levels(group)[2], sep = '') }
   if (mean(DATA$Y[DATA$G == levels(group)[1]], na.rm = TRUE) < mean(DATA$Y[DATA$G == levels(group)[2]], na.rm = TRUE)) { comparison <- paste(levels(group)[1], '<', levels(group)[2], sep = '') }
  } else
  {
   comparison <- paste(levels(group)[1], '=', levels(group)[2], sep = '')
  }
  result <- c(result, TEST$p.value, comparison)
  return(result)
 } else { return(c(void_string, NA, NA)) }
}

#' give_continuous_test_2group_b
#'
#' Test per variabile numerica rispetto ad un gruppo (test t o test di Mann-Whitney).
#'
#' @param y Vettore numerico.
#' @param group Vettore di fattori, a 2 livelli.
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @param alpha_value Valore di alpha per la significatività del test.
#' @param multiple_alphas Valori di alpha per gli asterischi sulla significatività del test.
#' @return Un vettore con il risultato del test, con il valore di p risultante e con la comparazione fra i gruppi.
#' @export
give_continuous_test_2group_b <- function (y = NA, group = NA, void_string = '-', alpha_value = 0.050, multiple_alphas = c(0.050, 0.010, 0.001))
{
 if (!is.factor(group)) { group <- ordered(group) }  
 DATA <- na.omit(data.frame(Y = y, G = group))
 if ((min(table(DATA$G)) >= 3) & (sd(DATA$Y) > 0))
 {
  LEV <- car::leveneTest(Y ~ G, data = DATA, center = median)
  PAR <- give_ttest(y = DATA$Y, group = DATA$G, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas)
  NPAR <- give_mannwhitney(y = DATA$Y, group = DATA$G, void_string = void_string, alpha_value = alpha_value, multiple_alphas = multiple_alphas)
  if (LEV$'Pr(>F)'[1] < 0.050) { result <- NPAR } else { result <- PAR }
  return(result)
 } else { return(c(void_string, NA, NA)) }
}

#
