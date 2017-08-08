make_output_file <- function(filename, ext, path.dir) {
  if (is.null(filename)) {
    output.file <- tempfile(tmpdir = path.dir, fileext = paste0(".", ext))
  } else {
    output.file <- paste0(path.dir, "/", filename)
  }

  if (tools::file_ext(output.file) != ext) {
    stop(paste0("File ext must be ", ext))
  }

  output.file
}

##' @export
save_plot_png <- function(pl, filename = NULL, width = 600, height = 400) {

  path.dir = "~/Projects/Thesis/MaThese/OUTPUT/Rplots/"
  output.file <- make_output_file(filename, "png", path.dir)

  png(output.file, width, height)
  grid::grid.draw(pl)
  dev.off()

  filename <- base::basename(output.file)

  cat(paste0("[[./OUTPUT/Rplots/",filename,"]]\n"))
}

##' @export
save_plot_svg <- function(pl, filename = NULL, width = 7, height = 7) {

  path.dir = "~/Projects/Thesis/MaThese/OUTPUT/Rplots/"
  output.file <- make_output_file(filename, "svg", path.dir)

  svg(output.file, width = width, height = height)
  grid::grid.draw(pl)
  dev.off()

  filename <- base::basename(output.file)

  cat(paste0("[[./OUTPUT/Rplots/",filename,"]]\n"))
}

##' @export
save_expr <- function(expr, filename = NULL) {

  path.dir = "~/Projects/Thesis/MaThese/OUTPUT/Expr/"
  output.file <- make_output_file(filename, "rds", path.dir)

  saveRDS(expr, output.file)

  filename <- base::basename(output.file)

  cat(paste0("Expr save in ./OUTPUT/Expr/", filename, "\n"))

}

#' Test if a package is installed.
#'
#'
#' @param pkg Package name to test.
TestRequiredPkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste(pkg,"needed for this function to work. Please install it."),
         call. = FALSE)
  }
}

##' @export
save_plot_MaTheseR <- function(pl, filename = NULL, height = 29.7, width = height / sqrt(2),
                               units = "cm", res = 300) {

  path.dir = "~/Projects/Thesis/MaThese/OUTPUT/Rplots/"
  output.file <- make_output_file(filename, "png", path.dir)

  png(output.file, width = width, height = height, units = units, res = res)
  grid::grid.draw(pl)
  dev.off()

  filename <- base::basename(output.file)

  cat(paste0("[[./OUTPUT/Rplots/",filename,"]]\n"))
}

##' @export
drawable <- function(pl.func) {
  res <- list(pl.func = pl.func)
  class(res) <- "drawable"
  res
}

##' @export
grid.draw.drawable <- function(d) {
  d$pl.func()
}

##' @export
save_as_bin <- function(m, file) {
  con <- file(file, 'wb')
  on.exit(close(con))
  writeBin(c(m), con) 
  flush(con)
}

##' @export
save_dat <- function(dat, dirname, name, ...) {
  params <- list(...)
  md5 <- digest::digest(dat, algo = "md5")
  dat.file <- name
  for(n in names(params)) {
    dat.file <- paste0(dat.file,"_", n, params[[n]])
  }
  dat.file <- paste0(dat.file,"_",md5, ".rds")
  dat.file <- paste0("./OUTPUT/Dat/",dirname,"/",dat.file)
  saveRDS(dat, dat.file)
  dat.file
}

##' @export
retrieve_res <- function(m, f) {
  md5 <- digest::digest(m, algo = "md5")
  f <- paste0(sub(".rds", "", f), "_", md5, ".rds")
  readRDS(f)
}

##' @export
save_res <- function(m, m.res, f) {
  md5 <- digest::digest(m, algo = "md5")
  f <- paste0(sub(".rds", "", f), "_", md5, ".rds")
  saveRDS(m.res, f)
}

##' @export
exist_res <- function(m, f) {
  md5 <- digest::digest(m, algo = "md5")
  f <- paste0(sub(".rds", "", f), "_", md5, ".rds")
  file.exists(f)
}

##' @export
article3_method_name <- function(method) {
  aux <- function(m) {
    if (m == "sva_irw") {
      "sva-irw"
    } else if (m == "sva_two-step") {
      "sva-two-step"
    } else {
      m
    }
  }
  sapply(method, aux)
}

##' @export
g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}
