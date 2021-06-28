#' @export
arrotonda <- function (value, decimals = 3)
{
 return(format(round(value, decimals), nsmall = decimals))
}

#' @export
give_nice <- function (text = '', value = NA, decimals = 3, with_equal_sign = FALSE, with_sign = FALSE, min_value = NA, max_value = NA, void_string = '-')
{
 if (text != '' | !is.na(text)) { with_equal_sign = TRUE }
 if (with_equal_sign) { equal_sign <- '=' } else { equal_sign <- '' }
 if (is.na(value) | !is.numeric(value) | is.infinite(value)) { return(paste(text, equal_sign, void_string, sep = '')) }
 if (with_sign)
 {
  if (value > 0) { sign <- '+' } else if (value < 0) { sign <- '-' } else { sign <- '' }
 } else { sign <- '' }
 if (!is.na(min_value))
 {
  if (value <= min_value)
  {
   value <- min_value
  } else
  {
   if (round(value, decimals) <= round(min_value, decimals))
   {
    result <- paste('<', sign, floor(min_value), '.', paste(rep(0, decimals - 1), collapse = ''), '1', sep = '')
    equal_sign <- ''
   }
  }
 }
 if (!is.na(max_value))
 {
  if (value >= max_value)
  {
   value <- max_value
  } else
  {
   if (round(value, decimals) >= round(max_value, decimals))
   {
    result <- paste('>', sign, floor(max_value), '.', paste(rep(0, decimals - 1), collapse = ''), '1', sep = '')
    equal_sign <- ''
   }
  }
 }
 result <- arrotonda(value, decimals = decimals)
 result <- paste(text, equal_sign, sign, result, sep = '')
 return(result)
}


give_nice(text = 'r', value = 0.9999, decimals = 3, with_equal_sign = TRUE, with_sign = TRUE, min_value = -1, max_value = 1, void_string = '-')

