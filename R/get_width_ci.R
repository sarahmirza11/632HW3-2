
#' Title
#'
#' @param dat
#' @param est
#' @param iso_code
#' @param coverage
#'
#' @return
#' @export
#'
#' @examples
#'

get_width_ci <- function(est, iso_code, coverage = 95) {
  est <- est %>%
    filter(iso == iso_code)
  if (coverage == 80)
     {
      output <- est %>%
        mutate(width = U80 - L80)
      return(output %>%
        select(Year, width))
    } else if (coverage == 95)
      output <- est %>%
    mutate(width = U95 - L95)
  return(output %>%
    select(Year, width))
}

