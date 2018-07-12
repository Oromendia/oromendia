#'  Extract nice coef table for negative binomial model
#'
#' This is a wrapper around univAndMultiGLM which presents both binomial and Poisson coefficients
#' @param mod_matrix Output from mod_zeroinf
#' @examples
#' extract_mod_zeroinf_count()
#'
#'

extract_mod_zeroinf_count <- function(mod_zeroinf,digits=2){
  # Extract factor levels in each model
  factors = names(mod_zeroinf$levels)
  factors_pois = factors_binom = NULL
  if(length(factors > 0)){
    for (i in 1:length(factors)){
      if( length(grep(factors[i], grep("count_",names(coef(mod_zeroinf)),value=T))) >0) factors_pois = c(factors_pois,factors[i])
      if( length(grep(factors[i], grep("zero_", names(coef(mod_zeroinf)),value=T))) >0) factors_binom = c(factors_binom,factors[i])
    }
  }
  # Poisson model
  temp = data.frame(exp(suppressMessages(confint(mod_zeroinf)[grep("count_",names(coef(mod_zeroinf))),])))
  pois_rslt = data.frame(RateRatio = exp(coef(mod_zeroinf)[grep("count_",names(coef(mod_zeroinf)))]))
  pois_rslt$RateRatio = sprintf(paste0("%.",digits,"f"),pois_rslt$RateRatio)
  pois_rslt$CI = paste0("(",sprintf(paste0("%.",digits,"f"),temp[,1]),",",sprintf(paste0("%.",digits,"f"),temp[,2]),")")
  pois_rslt$p_val = format_pval(coef(summary(mod_zeroinf))$count[,"Pr(>|z|)"],equal="")
  pois_rslt = pois_rslt[-which(rownames(pois_rslt)=="count_(Intercept)"),]
  rownames(pois_rslt) = gsub("count_","",rownames(pois_rslt))
  pois_rslt = cbind("Var" = rownames(pois_rslt),pois_rslt);rownames(pois_rslt) = NULL
  if(!is.null(factors_pois)){
    factor_startRows = NULL
    for (i in 1:length(factors_pois)) factor_startRows = c(factor_startRows,min(grep(factors_pois[i],pois_rslt$Var)))
    for (i in 1:length(factors_pois)){
      name_here = factors_pois[i]
      first_row = min(grep(name_here,pois_rslt$Var))
      pois_rslt$Var = gsub(name_here,"&nbsp;&nbsp;&nbsp;&nbsp;",pois_rslt$Var)
      pois_rslt = add_row(pois_rslt,Var= paste0(name_here," (ref ",mod_zeroinf$levels[[which(names(mod_zeroinf$levels) == name_here)]][1],")") ,.before=first_row)
    }
  }
  pois_rslt
  # Binomial model
  binom_confint = data.frame(exp(suppressMessages(confint(mod_zeroinf)[grep("zero_",names(coef(mod_zeroinf))),])))
  binom_rslt = data.frame(OddsRatio = exp(coef(mod_zeroinf)[grep("zero_",names(coef(mod_zeroinf)))]))
  binom_rslt$OddsRatio = sprintf(paste0("%.",digits,"f"),binom_rslt$OddsRatio)
  binom_rslt$CI = paste0("(",sprintf(paste0("%.",digits,"f"),binom_confint[,1]),",",sprintf(paste0("%.",digits,"f"),binom_confint[,2]),")")
  binom_rslt$p_val = format_pval(coef(summary(mod_zeroinf))$zero[,"Pr(>|z|)"],equal="")
  binom_rslt = binom_rslt[-which(rownames(binom_rslt)=="zero_(Intercept)"),]
  rownames(binom_rslt) = gsub("zero_","",rownames(binom_rslt))
  binom_rslt = cbind("Var" = rownames(binom_rslt),binom_rslt);rownames(binom_rslt) = NULL
  if(!is.null(factors_binom)){
    factor_startRows = NULL
    for (i in 1:length(factors_binom)) factor_startRows = c(factor_startRows,min(grep(factors_binom[i],binom_rslt$Var)))
    for (i in 1:length(factors_binom)){
      name_here = factors_binom[i]
      first_row = min(grep(name_here,binom_rslt$Var))
      binom_rslt$Var = gsub(name_here,"&nbsp;&nbsp;&nbsp;&nbsp;",binom_rslt$Var)
      binom_rslt = add_row(binom_rslt,Var= paste0(name_here," (ref ",mod_zeroinf$levels[[which(names(mod_zeroinf$levels) == name_here)]][1],")") ,.before=first_row)
    }
  }
  binom_rslt
  # Both
  colnames(binom_rslt) = colnames(pois_rslt) = c("Variable","Est","95% CI","P-value")
  binom_rslt[,1] = as.character(binom_rslt[,1])
  pois_rslt[,1] = as.character(pois_rslt[,1])
  rslt = rbind(c("Binomial Model","Odds Ratio",NA,NA),binom_rslt,c(NA,NA,NA,NA),c("\n Poisson Model","Rate Ratio",NA,NA),pois_rslt)
  rslt
}
