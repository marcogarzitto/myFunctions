#' give_continuous_description
#'
#' Funzione rappresentare le percentuali.
#'
#' @param x Un vettore numerico (rappresentante una variabile continua).
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @return Una stringa rappresentante il valore T, con vari abbellimenti.
#' @export
give_continuous_description <- function (x = NA, void_string = '-')
{
 X <- na.omit(x)
 if ((length(X) < 3)) { return(void_string) }
 out_n <- length(X)
          out_n <- give_nice(value = out_n, decimals = 0, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 out_miss <- 100 * (1 - length(X)) / length(x)
             out_miss <- give_nice_percent(value = out_miss, decimals = 1, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 out_mean <- mean(X, na.rm = TRUE)
             out_mean <- give_nice(value = out_mean, decimals = 2, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 out_sd <- sd(X, na.rm = TRUE)
           out_sd <- give_nice(value = out_sd, decimals = 3, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 out_md <- median(X, na.rm = TRUE)
           out_md <- give_nice(value = out_md, decimals = 1, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 out_iqr <- IQR(X, na.rm = TRUE)
            out_iqr <- give_nice(value = out_iqr, decimals = 1, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 out_min <- min(X, na.rm = TRUE)
            out_min <- give_nice(value = out_min, decimals = 2, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 out_max <- max(X, na.rm = TRUE)
            out_max <- give_nice(value = out_max, decimals = 2, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 #
 result <- c(paste(out_n, '(', out_miss, ')', sep = ''),
             paste(out_mean, ' ', '\u00B1', out_sd, sep = ''),
             paste(out_md, ' ', '(', out_iqr, ')', sep = ''),
             paste('[', out_min, ' ', ',', ' ', out_max, ']', sep = ''))
           names(result) <- c('N (missing %)', 'Mean \u00B1SD', 'Md (IQR)', '[min , Max]')
 return(result)
}

#
