#' give_continuous_description
#'
#' Funzione per descrivere una variabile continua (con un vettore).
#'
#' @param x Un vettore numerico (rappresentante una variabile continua).
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @return Un vettore con i risultati descrittivi.
#' @export
give_continuous_description <- function (x = NA, void_string = '-')
{
 X <- na.omit(x)
 if ((length(X) < 3)) { return(void_string) }
 out_n <- length(X)
          out_n <- give_nice(value = out_n, decimals = 0, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 out_miss <- 100 * (1 - (length(X) / length(x)))
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
 result <- c(paste(out_n, ' ', '(', out_miss, ')', sep = ''),
             paste(out_mean, ' ', '\u00B1', out_sd, sep = ''),
             paste(out_md, ' ', '(', out_iqr, ')', sep = ''),
             paste('[', out_min, ' ', ',', ' ', out_max, ']', sep = ''))
           names(result) <- c('N (missing %)', 'Mean \u00B1SD', 'Md (IQR)', '[min , Max]')
 return(result)
}

#' give_categorical_description
#'
#' Funzione per descrivere una variabile categoriale (con una tabella relativa a tutti i livelli possibili).
#'
#' @param x Un vettore di fattori, convertito come tale se numerico.
#' @param name Stringa da usare come nome della variabile (altrimenti è utilizzata l'etichetta, se presente).
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @param list_marker Marcatore di punto elenco (per la lista dei livelli).
#' @return Un data.frame con i risultati descrittivi.
#' @export
give_categorical_description <- function (x = NA, name = '', void_string = '-', list_marker = '-')
{
 X <- x[!is.na(x)]
 if (name == '' | is.na(name)) { name <- Hmisc::label(X) }
 if (!is.factor(X)) { X <- ordered(X) } 
 out_n <- length(X)
          out_n <- give_nice(value = out_n, decimals = 0, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 out_miss <- 100 * (1 - (length(X) / length(x)))
             out_miss <- give_nice_percent(value = out_miss, decimals = 1, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
 out_primary <- paste(out_n, ' ', '(', 'Missing: ', out_miss, ')', sep = '')
 result <- c(name, out_primary)
 #
 for (x_level in levels(X))
 {
  out_positive <- sum(X == x_level)
                  out_positive <- give_nice(value = out_positive, decimals = 0, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)
  out_positive_perc <- 100 * (sum(X == x_level) / length(X))
                      out_positive_perc <- give_nice_percent(value = out_positive_perc, decimals = 1, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string)                
  out_level <- paste(out_positive, ' ', '(', out_positive_perc, ')', sep = '')
  result <- rbind(result,
                  c(paste(list_marker, ' ', x_level, sep = ''), out_level))
 }
 row.names(result) <- NULL
 result <- as.data.frame(result)
 names(result) <- c('Variable', 'N (%)')
 return(result)
}

#' give_categorical_crosstable
#'
#' Funzione per descrivere una variabile categoriale e il suo incrocio con un'altra di cui interessa la divisione in gruppi (con una tabella relativa a tutti i livelli possibili).
#'
#' @param x Un vettore di fattori, convertito come tale se numerico.
#' @param y Un vettore di fattori, convertito come tale se numerico, con il gruppo di interesse. 
#' @param name_x Stringa da usare come nome della variabile (altrimenti è utilizzata l'etichetta, se presente).
#' @param void_string Stringa da usare se il valore non c'è o non è calcolabile.
#' @param list_marker Marcatore di punto elenco (per la lista dei livelli).
#' @return Un data.frame con i risultati descrittivi (una colonna per livello della variabile con i gruppi).
#' @export
give_categorical_crosstable <- function (x = NA, y = NA, name_x = '', void_string = '-', list_marker = '-')
{
 XY <- na.omit(data.frame(X = x, Y = y))
 if (name_x == '' | is.na(name_x)) { name_x <- Hmisc::label(XY$X) }
 if (!is.factor(XY$X)) { XY$X <- ordered(XY$X) }
 if (!is.factor(XY$Y)) { XY$Y <- ordered(XY$Y) }
 #
 CROSS <- c(rep(c(''), length(levels(XY$Y))),
            give_categorical_test(x = XY$X, y = XY$Y, void_string = void_string))
 for (x_level in c(1:length(levels(XY$X))))
 {
  #
  CROSS <- rbind(CROSS,
                 paste(sapply(table(XY)[x_level, ], give_nice, decimals = 0, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string),
                       ' ', '(',
                       sapply(100 * prop.table(table(XY), 1)[x_level, ], give_nice_percent, decimals = 1, text = '', with_equal_sign = FALSE, with_sign = FALSE, min_value = -Inf, max_value = Inf, void_string = void_string),
                       ')',
                       sep = ''),
                 c(''))
  CROSS <- data.frame(CROSS)
  row.names(CROSS) <- NULL
  names(CROSS) <- c(c(levels(XY$Y)), 'Test')
 }
 #
 result <- cbind(give_categorical_description(x = x, name = name_x, void_string = void_string, list_marker = list_marker),
                 CROSS)
 names(result) <- c('Variable', 'N (%)', levels(XY$Y), 'Test')
 return(result)
}

#
