#' Wrapper around tableone to print in RMD
#'
#' This function exports results of Chi-sq Post Hoc Test in a legible manner
#' @param var vector of counts
#' @param grp vector of groupings of interest
#' @param varNm label for caption
#' @param control type of correction
#' @keywords tableone
#' @export
#' @examples 
#' \dontrun{
#' mktab_ChiSq_postHoc(mtcars)
#' }
mktab_ChiSq_postHoc <- function(var,grp, varNm="Var",file="",notes="",size="scriptsize",d=3,control = "bonferroni",markdown=FALSE){
  temp = chisq.post.hoc(table(var, grp), popsInRows = FALSE, control = control, digits = d)
  grp = factor(grp)
  mat = matrix(NA, ncol=(length(levels(grp))-1),nrow=(length(levels(grp))-1))
  colnames(mat)=levels(grp)[-length(levels(grp))]; rownames(mat)=levels(grp)[-1]
  # Add to nice matrix similar to KW post hoc 
  start = 1
  for (row_here in 1:nrow(mat)){
    end = start+nrow(mat)-row_here
    # print(start);   print(end)
    mat[row_here:nrow(mat),row_here] = temp$adj.p[start:end]
    start = end+1
  }
  varNm = gsub("_",".",varNm) # underscores to periods 
  if(markdown == FALSE) print(xtable(mat,caption=paste0("Post-Hoc Chi Squared Test for ",varNm,". ",notes," Note: only valid if global Chi Squared test is significant."),digits=d),file=file,table.placement="H",include.rownames=T,include.colnames=T,size=size)
  return(mat)
}
