#' Wrapper around tableone to print in RMD
#'
#' This function allows you format the tidy output from broom in a more readable way
#' @param data_here dataset to summarize all columns
#' @param strata string to group by, NULL default
#' @param caption able title
#' @keywords tableone
#' @export
#' @examples
COCreateTableOne <- function(data_here,...,caption="Table",
                             # options for print.tableone, with some defaults changed
                             catDigits = 1, contDigits = 2, pDigits = 3, missing = FALSE, explain = TRUE,
                             test = TRUE, smd = FALSE, format = c("fp", "f", "p", "pf")[1],
                             showAllLevels = FALSE, cramVars = NULL, dropEqual = FALSE,
                             exact = NULL, nonnormal = NULL, minMax = FALSE,
                             # Kable styling options
                             full_width=F){

	require(tableone)
  # Create the table
  CreateTableOne(data=data_here,...) %>%
    print(noSpaces=T,printToggle=F, simulate.p.value=TRUE,
          catDigits = catDigits, contDigits = contDigits, pDigits = pDigits,
          missing = missing, explain = explain, test = test, smd = smd, format = format,
          showAllLevels = showAllLevels, cramVars = cramVars, dropEqual = dropEqual,
          exact = exact, nonnormal = nonnormal, minMax = minMax) -> temp2
    # Extract row names before special characters are removed
  temp2 %>%
    as.data.frame() %>%
    mutate(Var = rownames(temp2)) ->
    temp

  # Extract which rows should be indented
  whichRowsCat = grep("mean|%|^n$",temp$Var,invert = T)

  # Print
  temp %>%
    mutate(Var = gsub("(mean (sd))","",Var,fixed=T)) %>%
    select(-one_of("test")) %>%
    select(Var,everything()) %>%
    kable(caption=caption, padding=10) %>%
    kable_styling(c("striped","condensed","hover"),font_size=11,full_width = full_width) %>%
    row_spec(1, background = "white") %>%
    add_indent(whichRowsCat)
}

# dat %>%
#   select(one_of("sex","ethnicity","LMCA_LumDiam_avg","LMCA_VesDiam_avg",
#                 "RCA_LumDiam_avg","RCA_VesDiam_avg",
#                 "LCx_LumDiam_avg","LCx_VesDiam_avg","pLAD_LumDiam_avg","pLAD_VesDiam_avg")) %>%
#   COCreateTableOne(pDigits=2,caption = "my title")
#
# COCreateTableOne(data_here,strata="sex",caption = "my title")
