% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplotRegression.R
\name{ggplotRegression}
\alias{ggplotRegression}
\title{Make a crude scatterplot with regression line printed on it}
\usage{
ggplotRegression(fit, d = 3, d_pval = 3)
}
\arguments{
\item{fit}{Linear model with a single continuous predictor}

\item{d}{Decimal places used for HR and confidence interval}

\item{d_pval}{Decimal places used for p value.}
}
\description{
This is a wrapper around broom:::tidy that adds formatted confidence intervals and p values
}
\examples{
\dontrun{
ggplotRegression()
}
}
