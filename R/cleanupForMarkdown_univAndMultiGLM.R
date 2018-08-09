#' Extract output from univAndMultiGLM
#'
#' This is a wrapper around univAndMultiGLM to print correctly in Rmd files
#' @param mod_matrix Output from univAndMultiGLM
#' @export
#' @examples
#' \dontrun{
#' cleanupForMarkdown_univAndMultiGLM()
#'}
#'
#'
cleanupForMarkdown_univAndMultiGLM <- function(mod_matrix){
  require(tibble)
  colnames(mod_matrix)[which(colnames(mod_matrix) == "")] = "-"
  # Find which rows start a factor and add spacing as appropriate
  factor_startRows = grep("\\\\ \n",mod_matrix[,1]) 
  mod_matrix = as_tibble(mod_matrix)
  for (i in rev(factor_startRows)) {
    mod_matrix = add_row(mod_matrix,Characteristic=gsub("\\\\.*$", "", mod_matrix$Characteristic[i] ),.before=i)
    mod_matrix$Characteristic[i+1] = gsub(".*\\\\hspace\\{10pt\\}","\t",    mod_matrix$Characteristic[i+1])
  }
  # remove Hspace from all rows
  mod_matrix$Characteristic = gsub(".*\\hspace\\{10pt\\}","\t",    mod_matrix$Characteristic)

  return(mod_matrix)
}
