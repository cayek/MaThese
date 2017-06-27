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
  output.file <- make_output_file(filename)

  png(output.file, width, height)
  print(pl)
  dev.off()

  filename <- base::basename(output.file)

  cat(paste0("[[./OUTPUT/Rplots/",filename,"]]\n"))
}


##' @export
save_expr <- function(expr, filename = NULL) {

  path.dir = "~/Projects/Thesis/MaThese/OUTPUT/Expr/"
  output.file <- make_output_file(filename)

  saveRDS(expr, output.file)

  filename <- base::basename(output.file)

  cat(paste0("Expr save in ./OUTPUT/Expr/", filename, "\n"))

}

