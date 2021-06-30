#' add_categorical_variable_from_raw_input
#'
#' Funzione per ottenere una variabile categorica a partire da un input di inserimento.
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
 OUT <- ordered(df_in[, c(name_in)], levels = levels_in)
        levels(OUT) <- levels_out
        if (length(levels_out) > 1) { contrasts(OUT) <- contr.treatment(length(levels_out), base = 1) }
        Hmisc::label(OUT) <- paste(acronym, if (acronym != '') { ', ' }, variable_description, '[', paste(levels_out, collapse = '/'), ']', sep = '')
 return(OUT)
}
