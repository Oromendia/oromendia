#' Format p value
#'
#' This function allows you format the o values including <.001
#' @param raw_p Number to be treated as p value
#' @param d Decimal places used for p value.
#' @param equal Symbol to include if not <
#' @export
#' @examples
#' format_pval(0.0001)
#' format_pval(0.95,equal = "")
#'
format_pval <- function(raw_p, d=3,equal="="){
  p_string= ifelse(raw_p < (1/10^d),paste0("<0.",paste(rep(0,d-1),collapse=""),"1"),paste(equal,sprintf(paste0("%.",d,"f"),raw_p)))
  return(p_string)
}
