#' Print results of Kruskal-Wallis Post Hoc Test 
#'
#' This function exports results of KW post hoc test in a legible manner
#' @param var vector of counts
#' @param grp vector of groupings of interest
#' @param varNm label for caption
#' @keywords tableone
#' @export
#' @examples 
#' \dontrun{
#' mktab_KW_postHoc(mtcars)
#' }
mktab_KW_postHoc <- function(var,grp, varNm="Var",file="",notes="",size="scriptsize",d=3){
  # cat(paste(varNm, "Table \n"))
  varNm = gsub("_",".",varNm) # underscores to periods 
  rslt = format_pval(posthoc.kruskal.dunn.test(x=var, g=as.factor(grp), p.adjust="BH")$p.value,equal="",d=d)
  print(xtable(rslt,caption=paste0("P-values for pairwise Post-Hoc Kruskal-Wallis test (Dunn test) for ",varNm,". ",notes," Note: only valid if KW is significant."),digits=d),file=file,table.placement="H",include.rownames=T,include.colnames=T,size=size,comment=FALSE)
  return(rslt)
}
