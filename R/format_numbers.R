#' @export
arrotonda <- function (value, decimals = 3)
{
 return(format(round(value, decimals), nsmall = decimals))
}

#' @export
give_nice <- function (value = NA, decimals = 3, text = '', with_equal_sign = TRUE, with_sign = TRUE, min_value = -Inf, max_value = Inf, void_string = '-')
{
 if (text != '') { with_equal_sign = TRUE }
 if (with_equal_sign) { equal_sign <- '=' } else { equal_sign <- '' }
 if (is.na(value) | !is.numeric(value) | is.infinite(value)) { return(paste(text, equal_sign, void_string, sep = '')) }
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

#' @export
give_nice_p <- function (value = NA, decimals = 3, with_p = TRUE, with_equal_sign = TRUE, with_stars = TRUE, multiple_stars = TRUE, alpha = 0.050, multiple_alphas = c(0.050, 0.010, 0.001), give_only_stars = FALSE, void_string = '-')
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

#' @export
give_nice_r <- function (value = NA, decimals = 3, with_r = TRUE, with_equal_sign = TRUE, void_string = '-')
{
 if (with_r) { text <- 'r' ; with_equal_sign = TRUE } else { text <- '' }
 min_value = -1
 max_value = 1
 result <- give_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = TRUE, min_value = min_value, max_value = max_value, void_string = void_string)
 return(result)
}

#' @export
give_nice_z <- function (value = NA, decimals = 3, text = '', standard_mean = 0, standard_sd = 1, min_z = NA, max_z = NA, with_equal_sign = TRUE, with_sign = TRUE, void_string = '-')
{
 if (text != '') { with_equal_sign = TRUE }
 if (!is.na(min_z)) { min_value <- standard_mean - (abs(min_z) * standard_sd) } else { min_value <- NA }
 if (!is.na(max_z)) { max_value <- standard_mean + (abs(max_z) * standard_sd) } else { max_value <- NA }
 result <- give_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = TRUE, min_value = min_value, max_value = max_value, void_string = void_string)
 return(result)
}

#' @export
give_nice_t <- function (value = NA, decimals = 1, text = 'T', standard_mean = 50, standard_sd = 10, min_value = 0, max_value = 100, with_equal_sign = TRUE, with_sign = FALSE, void_string = '-')
{
 if (text != '') { with_equal_sign = TRUE }
 result <- give_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = TRUE, min_value = min_value, max_value = max_value, void_string = void_string)
 return(result)
}

#' @export
give_nice_standard <- function (value = NA, decimals = 1, text = 'Standard', standard_mean = 100, standard_sd = 15, min_value = 0, max_value = 200, with_equal_sign = TRUE, with_sign = TRUE, void_string = '-')
{
 if (text != '') { with_equal_sign = TRUE }
 result <- give_nice(text = text, value = value, decimals = decimals, with_equal_sign = with_equal_sign, with_sign = TRUE, min_value = min_value, max_value = max_value, void_string = void_string)
 return(result)
}

#
