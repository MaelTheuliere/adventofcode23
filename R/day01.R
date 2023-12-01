#' Day 01: Trebuchet?!
#'
#' [Trebuchet?!](https://adventofcode.com/2023/day/1)
#'
#' @name day01
#' @rdname day01
#' @details
#'
#' **Part One**
#'
#' Something is wrong with global snow production, and you\'ve been
#' selected to take a look. The Elves have even given you a map; on it,
#' they\'ve used stars to mark the top fifty locations that are likely to
#' be having problems.
#'
#' You\'ve been doing this long enough to know that to restore snow
#' operations, you need to check all *fifty stars* by December 25th.
#'
#' Collect stars by solving puzzles. Two puzzles will be made available on
#' each day in the Advent calendar; the second puzzle is unlocked when you
#' complete the first. Each puzzle grants *one star*. Good luck!
#'
#' You try to ask why they can\'t just use a [weather machine](/2015/day/1)
#' (\"not powerful enough\") and where they\'re even sending you (\"the
#' sky\") and why your map looks mostly blank (\"you sure ask a lot of
#' questions\")
#' [and]{title="My hope is that this abomination of a run-on sentence somehow conveys the chaos of being hastily loaded into a trebuchet."}
#' hang on did you just say the sky (\"of course, where do you think snow
#' comes from\") when you realize that the Elves are already loading you
#' into a
#' [trebuchet](https://en.wikipedia.org/wiki/Trebuchet){target="_blank"}
#' (\"please hold still, we need to strap you in\").
#'
#' As they\'re making the final adjustments, they discover that their
#' calibration document (your puzzle input) has been *amended* by a very
#' young Elf who was apparently just excited to show off her art skills.
#' Consequently, the Elves are having trouble reading the values on the
#' document.
#'
#' The newly-improved calibration document consists of lines of text; each
#' line originally contained a specific *calibration value* that the Elves
#' now need to recover. On each line, the calibration value can be found by
#' combining the *first digit* and the *last digit* (in that order) to form
#' a single *two-digit number*.
#'
#' For example:
#'
#'     1abc2
#'     pqr3stu8vwx
#'     a1b2c3d4e5f
#'     treb7uchet
#'
#' In this example, the calibration values of these four lines are `12`,
#' `38`, `15`, and `77`. Adding these together produces *`142`*.
#'
#' Consider your entire calibration document. *What is the sum of all of
#' the calibration values?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f01a(x)` returns .... For Part Two,
#'   `f01b(x)` returns ....
#' @importFrom stringr str_sub str_replace_all
#' @export
#' @examples
#' f01a(example_data_01())
f01a <- function(x) {
  res <- x %>%
    keep_digit %>%
    first_last %>%
    as.numeric() %>%
    sum(na.rm = TRUE)
  return(res)
}

f01b <- function(x) {
  res <- x %>%
    replace_digit_letter %>%
    keep_digit %>%
    first_last %>%
    as.numeric() %>%
    sum(na.rm = TRUE)
  return(res)
}

#' @rdname day01
#' @export
first_last <- function(x) {
  res <- paste0(stringr::str_sub(x,1,1),stringr::str_sub(x,-1,-1))
  return(res)
}

#' @rdname day01
#' @export
keep_digit <- function(x) {
  res <-  stringr::str_replace_all(x,'\\D','')
  return(res)
}

#' @rdname day01
#' @export
replace_digit_letter <- function(x) {
  res <- stringr::str_replace_all(x,c("nei"="neei","vei"="veei","reei"="reeei","wone"="woone","htw"="httw","hth"="htth")) %>% #gestion de doublons
    stringr::str_replace_all(c("one" = "1", "two" = "2", "three" = "3", "four" = "4","five" = "5","six" = "6","seven" = "7","eight" = "8","nine" = "9"))
  return(res)
}


#' @rdname day01
#' @export
example_data_01 <- function() {
  res <- c('1abc2',
           'pqr3stu8vwx',
           'a1b2c3d4e5f',
           'treb7uchet',
           'abc'
  )
  return(res)
}

#' @rdname day01
#' @export
example_data_01b <- function() {
  res <- c("two1nine",
           "eightwothree",
           "abcone2threexyz",
           "xtwone3four",
           "4nineeightseven2",
           "zoneight234",
           "7pqrstsixteen")
  return(res)
}
