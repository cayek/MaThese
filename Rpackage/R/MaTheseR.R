MaTheseR.params <- new.env()

#' Return MaThese params 
#'
#' @export
get_MaTheseRparams<- function() {
  MaTheseR.params
}


#' R package with functions a use for ma thesis
#'
#'
#' @docType package
#'
#' @name MaTheseR
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import MatrixFactorizationR
#' @import ExpRiment
#' @importFrom magrittr "%>%"
#' @importFrom foreach foreach %:% %do% %dopar%
#' @import tibble
#' @import RcppEigen
NULL

#########
## plots

MaTheseR.params$gtheme <- theme_bw(base_size = 12, base_family = "serif") +
  theme(strip.background = element_rect(fill = NA))
MaTheseR.params$textheightcm <- 19.4028
MaTheseR.params$textwidthcm <- 12.1708
