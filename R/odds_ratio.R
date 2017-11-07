#' A simple function for calculating odds ratios from 2x2 tables
#'
#' This function allows for odds ratio calculations.
#' @param a The exposed + disease + count
#' @param b The exposed + disease - count
#' @param c The exposed - disease + count
#' @param d The exposed - disease - count
#' @param ci Return a confidence interval instead of point estimate
#' @param alpha The confidence level of the interval
#' @keywords odds-ratio
#' @export
#' @examples
#' odds_ratio(30,40,10,50)
#' odds_ratio(30,40,10,50,ci = TRUE)

odds_ratio <- function(a,b,c,d,ci = FALSE,alpha = 0.05)
{

  if(!all(c(a,b,c,d)>0))
  {
    stop("Must have positive cell counts")
  }

  or = (a*d)/(b*c)

  if(!ci)
  {
    return(or)
  }
  else
  {
    se_lnor = sqrt(1/a+1/b+1/c+1/d)
    z_crit = qnorm(1-alpha/2)
    return(exp(pm(log(or),z_crit*se_lnor)))
  }
}
