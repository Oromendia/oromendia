#' Make a table with univariable and multivariable logistic results 
#'
#' Given a dataset, make two column output with univariable results for each predictor, and second column with single multivariable model
#' @param outcome Vector of outcome
#' @param covars_uni Matrix with columns to be used in univariable models
#' @param covars_multi Matrix with columns to be used in multivariable models. If NULL, univariable predictors with p < 0.05 will be used.
#' @param p_multiV ANOVA p-value threshold used for inclusion in multiariable model. Ignored if covars_multi passed in. 
#' @param family Model family for GLM: "binomial", "gaussian", "poisson".
#' @param name Name of model to be used for caption
#' @param d Decimal places used for HR and confidence interval.
#' @param d_pVal Decimal places used for p value.
#' @param CIs Logical to indicate whether confidence intervals should be printed.
#' @param z0 Critical value used for confidence interval.
#' @param file File name for LaTeX output to be sent to.
#' @param widthCharCol Width of first column with predictor names and levels.
#' @param markdown Boolean to indicate that output be compatible with markdown rather than LaTeX.
#' @export
#' @examples
#' dontrun{
#' univAndMultiGLM()
#' }
#' 
univAndMultiGLM <- function(outcome,covars_uni,covars_multi=NULL,p_multiV = 0.05,family="binomial",file="",d=2,d_pVal=3,CIs=TRUE,z0=1.96,name="GLM",widthCharCol = 6,markdown=FALSE){
  
  require(tibble)
  
  # Error if number of rows differs in input (missing values allowed)
  if(length(outcome) != nrow(covars_uni)) stop("Outcome and covars_uni must have same number of rows")
  if(!is.null(covars_multi)) {if (length(outcome) != nrow(covars_multi)) stop("Outcome and covars_multi must have same number of rows")}
  
  # Error if covars_multi does not have at least 2 variables 
  if(!is.null(covars_multi)) {if(ncol(covars_multi) == 1) stop("Must pass in at least 2 variables to covars_multi")}
  
  # Remove rows that are missing the outcome
  covars_uni = as.data.frame(covars_uni[!is.na(outcome),])
  if(!is.null(covars_multi)) covars_multi = as.data.frame(covars_multi[!is.na(outcome),])
  outcome = outcome[!is.na(outcome)]
  
  # Remove if all the same level or all unique levels 
  removeCols = c()
  for (i in 1:ncol(covars_uni)) {
    if (length(table(factor(covars_uni[,i]),useNA="no")) < 2 | (length(table(factor(covars_uni[,i]),useNA="no"))==sum(!is.na(covars_uni[,i]))) & !is.numeric(covars_uni[,i])) removeCols = append(removeCols,i)}
  if(length(removeCols >0)) covars_uni = covars_uni[,-removeCols]
  if (!markdown) cat(paste("Removed",length(removeCols)," columns from covars_uni \n "))
  
  # Row names with indenting for categorical variables.
  mkFctrLbl_here = function(colname,dat){ 
    if(is.numeric(dat[,colname])) return(colname); # numeric even if only two values

    # Copy from mkFctrLbl with level added 
    var = factor(dat[,colname])
    labs = c(paste0( colname," (vs ",levels(var)[1],")\\\\ \n \\hspace{10pt} ", levels(var)[2]),
             paste(colname,"\t",levels(var)[-c(1,2)]))
    if(length(levels(var))<=2)  labs = paste0(colname," (vs ",levels(var)[1],")\\\\ \n \\hspace{10pt}",levels(var)[2])
    return(labs)
    labs
  }
  covariates_uni = unlist(mapply(mkFctrLbl_here,colnames(covars_uni),MoreArgs = list(dat=covars_uni)))
  mods_uni = NULL; for (i in 1:ncol(covars_uni)) {
    # print(colnames(covars_uni)[i])
    datM = data.frame(covars_uni[,i])
    colnames(datM) = colnames(covars_uni)[i]
    mods_uni[[i]] = glm(outcome~.,data=datM,family=family)
  }
  # Multivariable model 
  ####
  # If  covars_multi is not passed in, add those that have ANOVA p < p_multiV in univariate 
  if(is.null(covars_multi)) {
    # True if global test is <  p_multiV 
    addToMultiv = function(mod) {
      if (is.nan(summary(mod)$dispersion)) return(FALSE) # deal w saturated mods
      sum(anova(mod,test="Chisq")$"Pr(>Chi)" < p_multiV, na.rm=T) > 0
    }
    vars_univSignif = colnames(covars_uni)[ unlist(lapply(mods_uni, addToMultiv)) ]                        
    covars_multi = subset(covars_uni,select = vars_univSignif)
  }
  removeCols = c()
  for (i in 1:ncol(covars_multi)) {if (length(table(factor(covars_multi[,i]),useNA="no")) < 2) removeCols = append(removeCols,i)}
  if(length(removeCols >0))  covars_multi = covars_multi[,-removeCols]
  if (!markdown) cat("\n Ran univariables, starting multivarible \n")
  # Run model 
  mod_adj = glm(outcome~.,data=data.frame(covars_multi),family=family)
  covariates_multi = unlist(mapply(mkFctrLbl_here,colnames(covars_multi),MoreArgs = list(dat=model.frame(mod_adj))))
  # Use data used in model only 
  if (!markdown) cat("\n Ran multivariable, assembling table")
  cleanp = function(p){
    f_p = paste0("%.",d_pVal,"f")
    p[p<(1/10^d_pVal)] = gsub("0.",".",paste0("<",1/10^d_pVal,"*"),fixed=T)
    options(warn = -1) # suppress warnings 
    p[as.numeric(p)<=.05 & !is.na(as.numeric(p))] =  paste0(sprintf(f_p,as.numeric(p[as.numeric(p)<=.05 & !is.na(as.numeric(p))])),"*") 
    options(warn = 0) # restore warnings
    if(length(grep("*",p,fixed=T)) >0) p[-grep("*",p,fixed=T)] = sprintf(f_p,as.numeric(p[-grep("*",p,fixed=T)]))
    if(length(grep("*",p,fixed=T)) == 0) p = sprintf(f_p,as.numeric(p))
    return(p)
  }
  # Make column for univariates
  f = paste0("%.",d,"f")
  if(family == "binomial" | family == "poisson"){
    coefs = unlist(lapply(mods_uni, function(x) sprintf(f,exp(coef(x))[-1],d)))
    coefs[mapply(nchar,coefs) > 7] = ">1000"
    lowci = unlist(lapply(mods_uni, function(x) sprintf(f,exp(coef(x)[-1]-z0*summary(x)$coef[,2][-1]),d)))
    hihci = unlist(lapply(mods_uni, function(x) sprintf(f,exp(coef(x)[-1]+z0*summary(x)$coef[,2][-1]),d)))
    varNames =  unlist(lapply(mods_uni, function(x) names(coef(x))[-1]))
  }
  if(family == "gaussian"){
    coefs = unlist(lapply(mods_uni, function(x) sprintf(f,coef(x)[-1],d)))
    lowci = unlist(lapply(mods_uni, function(x) sprintf(f,coef(x)[-1]-z0*summary(x)$coef[,2][-1],d)))
    hihci = unlist(lapply(mods_uni, function(x) sprintf(f,coef(x)[-1]+z0*summary(x)$coef[,2][-1],d)))
  }
  
  
  pval  = unlist(lapply(mods_uni, function(x) cleanp(summary(x)$coef[,4][-1])))
  if(CIs) Univariate = paste(coefs,paste0("(",lowci,",",hihci,")"))
  if(!CIs) Univariate = coefs
  n_Mod = unlist(lapply(mods_uni, function(x) c(nrow(model.frame(x)),rep(NA,nrow(summary(x)$coef)-2))))
  # Combine values 
  univ = cbind(Univariate,pval,n_Mod)
  # rownames(univ) = gsub("\\\\","slash",covariates_uni)
  rownames(univ) = covariates_uni
  
  # Make column for multivariable
  if(family == "binomial" | family == "poisson"){
    coefs = sprintf(f,exp(summary(mod_adj)$coef[,1])[-1],d)
    coefs[mapply(nchar,coefs) > 7] = ">1000"
    hihci = sprintf(f,exp(summary(mod_adj)$coef[,1][-1]+z0*summary(mod_adj)$coef[,2][-1]),d)
    lowci = sprintf(f,exp(summary(mod_adj)$coef[,1][-1]-z0*summary(mod_adj)$coef[,2][-1]),d)
  }
  if(family == "gaussian"){
    coefs = sprintf(f,summary(mod_adj)$coef[,1][-1],d)
    lowci = sprintf(f,summary(mod_adj)$coef[,1][-1]-z0*summary(mod_adj)$coef[,2][-1],d)
    hihci = sprintf(f,summary(mod_adj)$coef[,1][-1]+z0*summary(mod_adj)$coef[,2][-1],d)
  }
  
  pval  = mapply(cleanp,summary(mod_adj)$coef[,4][-1])
  if(CIs) Multivariable = paste(coefs,paste0("(",lowci,",",hihci,")"))
  if(!CIs) Multivariable = coefs
  multi = cbind(Multivariable,pval)
  # Add all variables, even if not converged
  multi = merge(cbind(1:length(coef(mod_adj)[-1]),coef(mod_adj)[-1]),multi,all=T,by="row.names") 
  multi = multi[order(multi$V1),c("Multivariable","pval")] # Same order and remove extra cols 
  multi[,1] = as.character(multi[,1]) # Show non converged as Nan instead of NA 
  multi[,1][is.na(multi[,1])] = "NaN"
  rownames(multi) = covariates_multi
  
  # Compile table
  table = as.matrix(merge(cbind(univ,seq(1:nrow(univ))),multi,by="row.names",all.x=T))
  table = table[order(as.numeric(table[,"V4"])),which(!(colnames(table) %in% c("V4")))]
  table = cbind(table[,1:4],NA,table[,5:6]) # add column of NA for spacing between models
  colnames(table) = c("Characteristic","Univariable","pval","N","-","Multivariable","pval")
  
  
  # Find which rows start a factor and add spacing a new line as appropriate
  factor_startRows = grep("\\\\ \n",table[,1]) 
  table = as_tibble(table)
  for (i in rev(factor_startRows)) {
    table = add_row(table,Characteristic=gsub("\\\\.*$", "", table$Characteristic[i] ),.before=i)
    table$Characteristic[i+1] = gsub(".*\\\\hspace\\{10pt\\}","\t",table$Characteristic[i+1])
  }
  # remove Hspace from all rows
  table$Characteristic = gsub(".*\\hspace\\{10pt\\}","\t",    table$Characteristic)
  
  
  return(table)
}

