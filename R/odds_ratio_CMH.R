#' A function for calculating Cochran–Mantel–Haenszel odds ratios for across 2 strata
#'
#' This function allows for odds ratio calculations across two strata.
#' @param a A vector of the exposed + disease + count for each stratum
#' @param b A vector of the exposed + disease - count for each stratum
#' @param c A vector of the exposed - disease + count for each stratum
#' @param d A vector of the exposed - disease - count for each stratum
#' @param ci Return a confidence interval instead of point estimate
#' @param alpha The confidence level of the interval
#' @keywords odds-ratio
#' @export
#' @examples
#' odds_ratio(c(30,40),c(20,40),c(50,10),c(10,50))

odds_ratio_CMH <- function(a,b,c,d, ci = FALSE, alpha = 0.05)
{

  if(!all(c(a,b,c,d)>0))
  {
    stop("Must have positive cell counts")
  }

  num = (a[1]*d[1])/sum(a[1],b[1],c[1],d[1]) + (a[2]*d[2])/sum(a[2],b[2],c[2],d[2])

  denom = (b[1]*c[1])/sum(a[1],b[1],c[1],d[1]) + (b[2]*c[2])/sum(a[2],b[2],c[2],d[2])

  if(!ci)
  {
    return(num/denom)
  }

  else
  {
    arr <- array(c(a[1],b[1],c[1],d[1],
                   a[2],b[2],c[2],d[2]),
                 dim = c(2,2,2))
    return(mantelhaen.test(arr,conf.level = alpha)$conf.int)
  }
}
