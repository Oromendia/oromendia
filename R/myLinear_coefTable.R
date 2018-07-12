#'  Extract nice coef table for linear model
#'
#' This is a wrapper around glm to print nicely. Could use broom, but keeping here for backwards compatibility.
#' @param glm_linear Output from glm(, family = "gaussian")
#' @examples
#' myLinear_coefTable()
#'
#'
################################################
# Print one linear regression file nicely
################################################
myLinear_coefTable <- function(glm_linear){
  temp = data.frame(suppressMessages(confint(glm_linear)))
  rslt = data.frame(Est = coef(glm_linear))
  rslt$CI = paste0("(",sprintf("%.2f",temp[,1]),",",sprintf("%.2f",temp[,2]),")")
  rslt$p_val = sprintf("%.3f",coef(summary(glm_linear))[,4])
  rslt = rslt[-which(rownames(rslt)=="(Intercept)"),]
  rslt
}
