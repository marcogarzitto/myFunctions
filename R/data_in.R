#' add_categorical_variable_from_raw_input
#'
#' Funzione per ottenere una variabile categoriale a partire da un input di inserimento.
#'
#' @param value Valore numerico.
#' @param decimals Numero di decimali da mostrare.
#' @param acronym Nome (sigla) della variabile nel data-frame di input.
#' @param name_in Nome lungo (descrizione) della variabile nel data-frame di input.
#' @param variable_description Nome lungo o descrizione della variabile.
#' @param levels_in Vettore con i livelli nella variabile di input.
#' @param levels_out Vettore con i livelli per la variabile di output.
#' @param df_in Data-frame di inpunt.
#' @return Un vettore (utile per inserirlo in un data-frame).
#' @export
add_categorical_variable_from_raw_input <- function (name_in = '', acronym = '', variable_description = '', levels_in = c(), levels_out = c(), df_in = IN)
{
 if (is.null(levels_in)) { levels_in <- levels(ordered(df_in[, c(name_in)])) }
 if (is.null(levels_out)) { levels_out <- levels_in }
 OUT <- ordered(df_in[, c(name_in)], levels = levels_in, exclude = c('', NA))
        levels(OUT) <- levels_out
        levels_out <- levels(OUT)
        if (length(levels_out) > 1) { contrasts(OUT) <- contr.treatment(length(levels_out), base = 1) }
        if (length(levels_out) == 2) { options <- paste(levels_out, collapse = '/') } else { options <- 'options' }
        Hmisc::label(OUT) <- paste(acronym, if (acronym != '') { ', ' }, variable_description, ' ', '[', options, ']', sep = '')
        #
        Hmisc::label(OUT) <- paste(acronym, if (acronym != '') { ', ' }, variable_description, sep = '')
 return(OUT)
}

#' add_date_variable_from_raw_input
#'
#' Funzione per ottenere una variabile data (formattata come stringa) a partire da un input di inserimento (xlsx).
#'
#' @param value Valore numerico.
#' @param decimals Numero di decimali da mostrare.
#' @param acronym Nome (sigla) della variabile nel data-frame di input.
#' @param name_in Nome lungo (descrizione) della variabile nel data-frame di input.
#' @param variable_description Nome lungo o descrizione della variabile.
#' @param df_in Data-frame di inpunt.
#' @return Un vettore (utile per inserirlo in un data-frame).
#' @export
add_date_variable_from_raw_input <- function (name_in = '', acronym = '', variable_description = '', df_in = IN)
{
 OUT <- as.Date(df_in[, c(name_in)], '%Y-%m-%d')
 min_date <- as.character(format(min(OUT, na.rm = TRUE), '%d.%m.%Y'))
 max_date <- as.character(format(max(OUT, na.rm = TRUE), '%d.%m.%Y'))
 OUT <- as.character(format(OUT, '%d.%m.%Y'))
        Hmisc::label(OUT) <- paste(acronym, if (acronym != '') { ', ' }, variable_description, ' ', '[', min_date, ', ', max_date, ']', sep = '')
 return(OUT)
}

#' save_table
#'
#' Funzione per ottenere una variabile data (formattata come stringa) a partire da un input di inserimento (xlsx).
#'
#' @param what .
#' @param file_name .
#' @param where .
#' @param row_names .
#' @param with_csv .
#' @param fileEncoding .
#' @return .
#' @export
save_table <- function (what, file_name = 'DF', where = getwd(), row_names = FALSE, with_csv = FALSE, fileEncoding = 'latin1')
{
 save(what, file = paste(where, '/', file_name, '.RData', sep = ''))
 if (with_csv)
 {
  write.table(what, file = paste(where, '/', file_name, '.csv', sep = ''), sep = ',', dec = '.', row.names = row_names, quote = TRUE, fileEncoding = fileEncoding)
  write.table(what, file = paste(where, '/', file_name, '_IT', '.csv', sep = ''), sep = ';', dec = ',', row.names = row_names, quote = TRUE, fileEncoding = fileEncoding)  
 }
 xlsx::write.xlsx(what, file = paste(where, '/', file_name, '.xlsx', sep = ''), sheetName = file_name, row.names = row_names, showNA = FALSE)
}

#
