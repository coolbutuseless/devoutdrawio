


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Graphics device for DrawIO output
#'
#' Graphics primitives will be rendered into DrawIO format.
#'
#' Uses \code{devout::rdevice()}.
#'
#' @param filename If given, write ascii to this file, otherwise write to console.
#' @param width,height dimensions of text output (in characters). Default: NULL
#'                     (auto-detect)
#' @param ... other parameters passed to the rdevice
#'
#' @import devout
#' @import minidrawio
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio <- function(filename = NULL, width = NULL, height = NULL, ...) {
  devout::rdevice(drawio_callback, filename = filename, width = width, height = height, ...)
}
