#' An odds ratio table for pre-formatted tables
#'
#' This function allows for odds ratio calculations.
#' @param table A pre-formatted table with rows being exposure and columns being disease
#' @param ci Return a confidence interval instead of point estimate
#' @param alpha The confidence level of the interval
#' @keywords odds-ratio
#' @export
#' @examples
#' odds_ratio(30,40,10,50)
#' odds_ratio(30,40,10,50,ci = TRUE)

odds_ratio <- function(table,ci = FALSE,alpha = 0.05)
{
  return(odds_ratio(a = table[1,1],
                    b = table[1,2],
                    c = table[2,1],
                    d = table[2,2],ci,alpha))
}
