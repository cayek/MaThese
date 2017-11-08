MaTheseR.params <- new.env()
MaTheseR.params$Article2.env <- new.env()

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
#' @import lfmm
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
## in pt
MaTheseR.params$textheightpt <- 650.43
MaTheseR.params$textwidthpt <- 433.62
## in cm
MaTheseR.params$textheightcm <- 0.0352778 *  MaTheseR.params$textheightpt
MaTheseR.params$textwidthcm <- 0.0352778 * MaTheseR.params$textwidthpt
## in inch
MaTheseR.params$textheightinch <- 0.0138889 *  MaTheseR.params$textheightpt
MaTheseR.params$textwidthinch <- 0.0138889 * MaTheseR.params$textwidthpt



## color
MaTheseR.params$color.values <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
names(MaTheseR.params$color.values) <- c("lm", "sva-two-step", "oracle", "lassoLFMM",
                                         "PCAlm", "ridgeLFMM", "sva-irw", "cate")
MaTheseR.params$method.ordered <- c("lm", "PCAlm", "sva-two-step", "sva-irw",
                                    "lassoLFMM", "cate", "ridgeLFMM", "oracle")

MaTheseR.params$fig.dir <- "~/Projects/Thesis/MaThese/OUTPUT/Rplots/"

#################################################################################
## tess3
Article2.env <- MaTheseR.params$Article2.env
Article2.env$color <- list(TESS3 = "orange",
              APLS = "blue",
              AQP = "darkgrey",
              snmf = "chartreuse4",
              before.admixure = "azure4")
Article2.env$linetype <- list(TESS3 = "twodash",
                              APLS = "solid",
                              AQP = "dashed",
                              snmf = "twodash",
                              before.admixure = "dashed")

## ggplot param
Article2.env$cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


Article2.env$scale.linetype <- scale_linetype_manual(values = c("APLS" = Article2.env$linetype$APLS,
                                                                "TESS3" = Article2.env$linetype$TESS3,
                                                                "AQP" = Article2.env$linetype$AQP,
                                                                "sNMF" = Article2.env$linetype$snmf,
                                                                "before-admixure" = Article2.env$linetype$before.admixure))
Article2.env$scale.color <-  scale_color_manual(values = c("APLS" = Article2.env$color$APLS,
                                                           "TESS3" = Article2.env$color$TESS3,
                                                           "AQP" = Article2.env$color$AQP,
                                                           "sNMF" = Article2.env$color$snmf,
                                                           "before-admixure" = Article2.env$color$before.admixure))

