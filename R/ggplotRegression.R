#' Make a crude scatterplot with regression line printed on it
#'
#' This is a wrapper around broom:::tidy that adds formatted confidence intervals and p values
#' @param fit Linear model with a single continuous predictor
#' @param d Decimal places used for HR and confidence interval
#' @param d_pval Decimal places used for p value.
#' @export
#' ggplotRegression()
ggplotRegression <- function(fit,d=3,d_pval=3){
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, d),
                       #"Intercept =",signif(fit$coef[[1]],d ),
                       " Slope =",signif(fit$coef[[2]], d),
                       " P =",signif(summary(fit)$coef[2,4],d_pval)))
}