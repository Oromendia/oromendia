#' Print linear model after using tidy
#'
#' This function allows you format the tidy output from broom in a more readable way. This has been deprecated and replaced with format_tidy_gen, which also has more options.
#' @param rslt Output from lm() %>% tidy()
#' @param d Decimal places used for HR and confidence interval
#' @param d_pVal Decimal places used for p value.
#' @keywords tidy
#' @export
#' @examples
#' format_tidy_lm()
#'
format_tidy_lm <- function(rslt,d=2,d_pVal=3){
  digi <- paste0("%.",d,"f")
  cbind(
    Variable = rslt$term,
    Beta = paste0(sprintf(digi,rslt$estimate)," (",sprintf(digi,rslt$conf.low),",",sprintf(digi,rslt$conf.high),")"),
    Pvalue = format_pval(rslt$p.value,equal="",d=d_pVal)
  )
}
