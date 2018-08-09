#'  Extract nice coef table for  binomial model
#'
#' This is a wrapper around glm to print nicely. Could use broom, but keeping here for backwards compatibility.
#' @param glm_log Output from glm, family = "binomial"
#' @export
#' @examples
#' myLogistic_coefTable()
#'
#'
myLogistic_coefTable <- function(glm_log){
  temp = data.frame(exp(suppressMessages(confint(glm_log))))
  rslt = data.frame(OR = exp(coef(glm_log)))
  rslt$CI = paste0("(",sprintf("%.2f",temp[,1]),",",sprintf("%.2f",temp[,2]),")")
  rslt$p_val = sprintf("%.3f",coef(summary(glm_log))[,4])
  rslt = rslt[-which(rownames(rslt)=="(Intercept)"),]
  rslt
}
