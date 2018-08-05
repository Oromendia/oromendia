#' Print coxph after using tidy
#'
#' This function allows you format the tidy output from broom in a more readable way
#' @param rslt Output from tidy(coxph(),exponentiate=T)
#' @param d Decimal places used for HR and confidence interval.
#' @param d_pVal Decimal places used for p value.
#' @keywords tidy
#' @export
#' @examples
#' \dontrun{
#' #format_tidy_cox()
#'}
format_tidy_cox <- function(rslt,d=2,d_pVal=3){
  digi <- paste0("%.",d,"f")
  cbind(
    Variable = rslt$term,
    HazardRatio = paste0(sprintf(digi,rslt$estimate)," (",sprintf(digi,rslt$conf.low),",",sprintf(digi,rslt$conf.high),")"),
    Pvalue = format_pval(rslt$p.value,equal="",d=d_pVal)
  )
}
