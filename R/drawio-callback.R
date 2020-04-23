

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# from '?par'
# The line type. Line types can either be specified as an integer
# (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash)
# or as one of the character strings
# "blank", "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash",
# where "blank" uses ‘invisible lines’ (i.e., does not draw them).
#
# Alternatively, a string of up to 8 characters (from c(1:9, "A":"F")) may be
# given, giving the length of line segments which are alternatively drawn and skipped.
# See section ‘Line Type Specification’.
# The five standard dash-dot line types (lty = 2:6) correspond to c("44", "13", "1343", "73", "2262").
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
base_linetypes      <- c( "1"    ,    "4 4",    "1 3", "1 3 4 3",      "7 3", "2 2 6 2")
base_linetype_names <- c( "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")


get_linetype <- function(lty) {
  if (lty %in% c(0, 'blank')) {
    return("1")
  }

  if (is.integer(lty) && lty <= 6) {
    return(base_linetypes[lty])
  }

  if (is.character(lty) && lty %in% base_linetype_names) {
    return(base_linetypes[lty == base_linetype_names])
  }

  # Otherwise, split the pattern.
  # and drawio doesn't want hex, so replace 'F' with '15' for example.
  pattern <- trimws(gsub("(\\w)", "\\1 ", lty))
  pattern <- gsub("a", "10", pattern, ignore.case = TRUE)
  pattern <- gsub("b", "11", pattern, ignore.case = TRUE)
  pattern <- gsub("c", "12", pattern, ignore.case = TRUE)
  pattern <- gsub("d", "13", pattern, ignore.case = TRUE)
  pattern <- gsub("e", "14", pattern, ignore.case = TRUE)
  pattern <- gsub("f", "15", pattern, ignore.case = TRUE)
  pattern
}


get_line_config <- function(lty, colour) {
  dashPattern <- NULL

  if (lty %in% c(1, 'solid')) {
    dashed <- 0
  } else {
    dashed      <- 1
    dashPattern <- get_linetype(lty)
  }

  if (lty %in% c('blank')) {
    colour <- 'none'
  }


  list(
    colour = colour,
    dashed = dashed,
    dashPattern = dashPattern
  )
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a 4 element RGBA numeric vector into a colour for drawio
#'
#' If alpha is zero then return none, otherwise return the hex colour without
#' the alpha component. Alpha is handled separately in drawio elements
#'
#' @param vec 4 element numeric vector containing RGBA values in range [0,255]
#' @importFrom grDevices rgb
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
v2hex <- function(vec) {
  if (vec[4] == 0) {
    return('none')
  }
  rgb(vec[1], vec[2], vec[3], maxColorValue = 255)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# When the device is opened
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_open <- function(args, state) {

  # The first two elements of a root node are always 2 standard cells.
  cell0 <- drawio_elem('mxCell', id = 0)
  cell1 <- drawio_elem('mxCell', id = 1, parent = 0)

  # Create a root node in the 'rdata'. This is what all elements will
  # be attached to.
  state$rdata$root <- minidrawio::drawio_elem('root', cell0, cell1)

  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# When the device is closed
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_close <- function(args, state) {

  # Create an empty document.
  # Insert a diagram in the document
  # Insert a graphmodel in the diagram
  # Insert the root node in the graphmodel
  doc  <- minidrawio::DrawIODocument$new()

  doc$
    diagram(id = 'devout')$
    mxGraphModel(
      dx         = 0,
      dy         = 0,
      pageWidth  = state$dd$right,
      pageHeight = state$dd$bottom
    )$
    append(state$rdata$root)


  doc$save(state$rdata$filename)

  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_line <- function(args, state) {

  # message('line')


  state$rdata$root$line(
    x1            = args$x1,
    y1            = args$y1,
    x2            = args$x2,
    y2            = args$y2,
    colour        = v2hex(state$gc$col),
    stroke_width  = state$gc$lwd,
    dashed        = as.integer(state$gc$lty > 0),
    dashPattern = get_linetype(state$gc$lty)
  )


  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_rect <- function(args, state) {

  xmin   <- min(c(args$x0, args$x1))
  ymin   <- min(c(args$y0, args$y1))
  width  <- abs(args$x1 - args$x0)
  height <- abs(args$y1 - args$y0)

  # Avoid some numerical issues where the xmin might be -6e-14
  # which messes with the canvas display in the app.
  xmin <- round(xmin, 3)
  ymin <- round(ymin, 3)


  line   <- get_line_config(state$gc$lty, colour = v2hex(state$gc$col))
  fill   <- v2hex(state$gc$fill)
  colour <- v2hex(state$gc$col)

  # Draw fill first
  if (state$gc$fill[4] != 0) {
    state$rdata$root$rect(
      x      = xmin,
      y      = ymin,
      width  = width,
      height = height,
      fill   = fill,
      colour = 'none',
      alpha  = state$gc$fill[4]/255
    )
  }

  # Then draw border as 'alpha' from fill shouldn't be applied to colour
  # Also, avoid drawing the border if it's the same colour as the fill, as
  # drawio seems to be var too keen to use the white border on the plot
  # background to do silly things with the canvas size
  if (state$gc$col[4] != 0 && fill != colour) {
    state$rdata$root$rect(
      x           = xmin,
      y           = ymin,
      width       = width,
      height      = height,
      fill        = 'none',
      colour      = line$colour,
      alpha       = state$gc$col[4]/255,
      size        = state$gc$lwd,
      dashed      = line$dashed,
      dashPattern = line$dashPattern
    )
  }

  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ToDo: stroke width
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_polyline <- function(args, state) {

  # message('polyline')
  line <- get_line_config(state$gc$lty, colour = v2hex(state$gc$col))

  state$rdata$root$polygon(
    xs            = args$x,
    ys            = args$y,
    colour        = line$colour,
    fill          = 'none',
    alpha         = state$gc$col[4]/255,
    stroke_width  = state$gc$lwd,
    dashed        = line$dashed,
    dashPattern   = line$dashPattern
  )

  state
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# No filled polygons (yet!)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_polygon <- function(args, state) {

  # message('polygon')

  line <- get_line_config(state$gc$lty, colour = v2hex(state$gc$col))

  state$rdata$root$polygon(
    xs          = args$x,
    ys          = args$y,
    colour      = line$colour,
    fill        = v2hex(state$gc$fill),
    alpha       = state$gc$fill[4]/255,
    dashed      = line$dashed,
    dashPattern = line$dashPattern
  )

  state
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Draw multiple paths
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_path <- function(args, state) {

  # message('path')

  extents <- c(0, cumsum(args$nper))
  line <- get_line_config(state$gc$lty, colour = v2hex(state$gc$col))

  fill  <- v2hex(state$gc$fill)
  alpha <- state$gc$fill[4]/255

  for (poly in seq_len(args$npoly)) {
    subargs   <- args
    lower     <- extents[poly     ] + 1L
    upper     <- extents[poly + 1L]
    x         <- args$x[lower:upper]
    y         <- args$y[lower:upper]
    state$rdata$root$polygon(
      xs          = x,
      ys          = y,
      colour      = line$colour,
      fill        = fill,
      alpha       = alpha,
      dashed      = line$dashed,
      dashPattern = line$dashPattern
    )

  }


  state
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate and return the string width in the current state
#
# device_StrWidth should return the width of the given
# string in DEVICE units.
#
# graphics parameters that should be honoured (if possible):
#   font, cex, ps
#
# @param str string
#
# @return Optionally return 'width' the display width of the string in device units (numeric).
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_strWidth <- function(args, state) {
  fontsize    <- state$gc$cex * state$gc$ps
  metrics     <- gdtools::str_metrics(args$str, fontname = "sans", fontsize = fontsize, bold = FALSE, italic = FALSE, fontfile = "")
  state$width <- metrics[['width']]

  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine the font metrics given the current drawing state
#'
#' @param args arguments to this particular method
#' @param state full state of current drawing environment. This includes the
#' graphics context (\code{state$gc)}) and any R state needed for the device
#' (\code{state$rdata}).
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_metricInfo <- function(args, state) {
  cint <- abs(args$c)
  str  <- intToUtf8(cint)

  fontsize <- state$gc$cex * state$gc$ps
  metrics  <- gdtools::str_metrics(str, fontname = "sans", fontsize = fontsize, bold = FALSE, italic = FALSE, fontfile = "")

  state$ascent  <- metrics[['ascent' ]]
  state$descent <- metrics[['descent']]
  state$width   <- metrics[['width'  ]]

  state
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_circle <- function(args, state) {

  line <- get_line_config(state$gc$lty, colour = v2hex(state$gc$col))

  state$rdata$root$circle(
    x           = args$x,
    y           = args$y,
    r           = args$r,
    fill        = v2hex(state$gc$fill),
    colour      = line$colour,
    alpha       = state$gc$fill[4]/255,
    dashed      = line$dashed,
    dashPattern = line$dashPattern
  )

  state
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Text
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_text <- function(args, state) {

  gc       <- state$gc
  fontsize <- gc$cex * gc$ps

  xoffset <- 0
  yoffset <- -fontsize * gc$lineheight

  if (args$rot == 90) {
    xoffset <- yoffset
  }

  state$rdata$root$text(
    x              = args$x + xoffset,
    y              = args$y + yoffset,
    angle          = args$rot,
    align          = 'left',
    vertical_align = 'middle',
    label_position = 'center',
    vertical_label_position = 'middle',
    text           = args$str,
    fill     = 'none',
    fontsize = fontsize
  )

  state
}






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' drawio callback for the rdevice
#'
#' @param device_call name of device function call
#' @param args arguments to device function call
#' @param state list of rdata, dd and gc. Some or all of which may be NULL
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drawio_callback <- function(device_call, args, state) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Anything we're not handling, just return() straight away
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # print(state$name)
  state <- switch(
    device_call,
    "open"         = drawio_open      (args, state),
    "close"        = drawio_close     (args, state),
    "line"         = drawio_line      (args, state),
    "polyline"     = drawio_polyline  (args, state),
    "circle"       = drawio_circle    (args, state),
    "rect"         = drawio_rect      (args, state),
    "text"         = drawio_text      (args, state),
    'strWidth'     = drawio_strWidth  (args, state),
    "textUTF8"     = drawio_text      (args, state),
    'strWidthUTF8' = drawio_strWidth  (args, state),
    'polygon'      = drawio_polygon   (args, state),
    'metricInfo'   = drawio_metricInfo(args, state),
    'path'         = drawio_path      (args, state),
    'circle'       = drawio_circle    (args, state),
    {
      # if (!device_call %in% c('size', 'mode')) {
      #   print(device_call);
      # }
      state
    }
  )

  state
}


if (FALSE) {

  library(devout)
  library(ggplot2)
  library(minidrawio)
  library(devoutdrawio)


  drawio("working/mtcars1.xml")
  p <- ggplot(mtcars) +
    # geom_point(aes(mpg, wt, size = cyl)) +
    geom_line(aes(mpg, wt, linetype = as.factor(cyl))) +
    theme_bw(20) +
    labs(title = "hello mike", subtitle = "subtitle")
  print(p)
  dev.off()


  drawio("working/mtcars3.xml")
  p <- ggplot(mtcars) +
    geom_density(aes(mpg, fill = as.factor(cyl)), colour = NA) +
    theme_bw()
  print(p)
  dev.off()



  drawio("working/mtcars2.xml")
  p <- ggplot(mtcars) +
    geom_histogram(aes(mpg, fill = as.factor(cyl)))
  print(p)
  dev.off()


}























