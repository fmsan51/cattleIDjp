# make_dialog
#' Makes dialog to choose a input file and a output path and starts scraping
#'
#' @param encoding Encoding of file names
#' @import tcltk
#' @import tcltk2
make_dialog <- function(encoding = getOption("encoding")) {

  win <- tcltk::tktoplevel()
  tcltk::tktitle(win) <- msg_make_dialog$title

  win$env$frm_input <- tcltk2::tk2labelframe(win, text = msg_make_dialog$input)
  win$env$frm_output <- tcltk2::tk2labelframe(win,
                                              text = msg_make_dialog$output)
  win$env$frm_button <- tcltk2::tk2frame(win, relief = "flat", height = 40)
  tcltk::tkpack(win$env$frm_input, fill = "x", padx = 10, pady = c(10, 0))
  tcltk::tkpack(win$env$frm_output, fill = "x", padx = 10, pady = c(10, 0),
                ipadx = 10, ipady = 10)
  tcltk::tkpack(win$env$frm_button, pady = c(20, 10))

  win$env$frm_from_file <- tcltk2::tk2frame(win$env$frm_input, relief = "flat",
                                            borderwidth = 0)
  win$env$frm_from_cb <- tcltk2::tk2frame(win$env$frm_input, relief = "flat",
                                          borderwidth = 0)
  win$env$frm_col <- tcltk2::tk2frame(win$env$frm_input, relief = "flat",
                                      borderwidth = 0)
  win$env$frm_row <- tcltk2::tk2frame(win$env$frm_input, relief = "flat",
                                      borderwidth = 0)
  tcltk::tkpack(win$env$frm_from_file, fill = "both", padx = 10, pady = 0)
  tcltk::tkpack(win$env$frm_col, fill = "x", padx = 100, pady = 3)
  tcltk::tkpack(win$env$frm_row, fill = "x", padx = 100, pady = 3)
  tcltk::tkpack(win$env$frm_from_cb, fill = "both", padx = 10, pady = 10)


  # Input from file
  use_clipboard <- tcltk::tclVar(0)
  win$env$rb_file <- tcltk2::tk2radiobutton(win$env$frm_from_file,
                                            text = msg_make_dialog$from_csv)
  tcltk::tkconfigure(win$env$rb_file, variable = use_clipboard, value = 0)
  input <- tcltk::tclVar()
  win$env$input <- tcltk2::tk2entry(win$env$frm_from_file, textvariable = input,
                                    width = 80)
  win$env$input_btn <- tcltk2::tk2button(win$env$frm_from_file,
                                         text = msg_make_dialog$choose_file)

  choose_input <- function() {
    filename <- tcltk::tclvalue(tcltk::tkgetOpenFile(
      filetypes = "{ {All Files} * } { {CSV Files} {.csv} }"
    ))
    Encoding(filename) <- encoding
    filename <- enc2utf8(filename)
    tcltk::tclvalue(input) <- filename
  }
  tcltk::tkconfigure(win$env$input_btn, command = choose_input)

  tcltk::tkpack(win$env$rb_file, side = "left")
  tcltk::tkpack(win$env$input, side = "left", padx = c(15, 0))
  tcltk::tkpack(win$env$input_btn, side = "left", padx = c(10, 0))

  # Cols to load
  col <- tcltk::tclVar()
  win$env$col <- tcltk2::tk2entry(win$env$frm_col, width = 6,
                                  textvariable = col)
  win$env$col_lbl <- tcltk::tklabel(win$env$frm_col,
                                    text = msg_make_dialog$col_label)
  win$env$col_lbl_at <- tcltk::tklabel(win$env$frm_col,
                                       text = msg_make_dialog$col_at)

  tcltk::tkpack(win$env$col_lbl, side = "left", padx = 10)
  tcltk::tkpack(win$env$col, win$env$col_lbl_at, side = "left", padx = 3)

  # Rows to load
  row_s <- tcltk::tclVar()
  row_e <- tcltk::tclVar()
  win$env$row_s <- tcltk2::tk2entry(win$env$frm_row, width = 6,
                                    textvariable = row_s)
  win$env$row_e <- tcltk2::tk2entry(win$env$frm_row, width = 6,
                                    textvariable = row_e)
  win$env$row_lbl <- tcltk::tklabel(win$env$frm_row,
                                    text = msg_make_dialog$row_label)
  win$env$row_lbl_from <- tcltk::tklabel(win$env$frm_row,
                                         text = msg_make_dialog$row_from)
  win$env$row_lbl_to <- tcltk::tklabel(win$env$frm_row,
                                       text = msg_make_dialog$row_to)

  tcltk::tkpack(win$env$row_lbl, side = "left", padx = 10)
  tcltk::tkpack(win$env$row_s, win$env$row_lbl_from,
                win$env$row_e, win$env$row_lbl_to,
                side = "left", padx = 3)

  # Input from clipboard
  win$env$rb_cb <- tcltk2::tk2radiobutton(win$env$frm_from_cb,
                                text = msg_make_dialog$from_cb)
  tcltk::tkconfigure(win$env$rb_cb, variable = use_clipboard, value = 1)
  tcltk::tkpack(win$env$rb_cb, side = "left")


  # Make file choose button unclickable,
  # and col number and row numbers unsettable
  # when "Input from clipboard" option is checked
  ck_cb <- function() {
    if (tcltk::tclvalue(use_clipboard) == 1) {
      new_state <- "disabled"
    } else {
      new_state <- "normal"
    }
    tcltk::tkconfigure(win$env$input, state = new_state)
    tcltk::tkconfigure(win$env$input_btn, state = new_state)
    tcltk::tkconfigure(win$env$col, state = new_state)
    tcltk::tkconfigure(win$env$row_s, state = new_state)
    tcltk::tkconfigure(win$env$row_e, state = new_state)
  }
  tcltk::tkconfigure(win$env$rb_file, command = ck_cb)
  tcltk::tkconfigure(win$env$rb_cb, command = ck_cb)


  # Output
  win$env$frm_output_file <- tcltk2::tk2frame(win$env$frm_output,
                                              relief = "flat",
                                              borderwidth = 0)
  win$env$frm_append <- tcltk2::tk2frame(win$env$frm_output, relief = "flat",
                                 borderwidth = 0)
  tcltk::tkpack(win$env$frm_output_file, fill = "both", padx = 10, pady = 0)
  tcltk::tkpack(win$env$frm_append, fill = "x", padx = 10, pady = 3)

  output <- tcltk::tclVar(paste0(getwd(), "/cattle_info.csv"))
  win$env$output <- tcltk2::tk2entry(win$env$frm_output_file,
                                     textvariable = output,
                                     width = 80)
  win$env$output_btn <- tcltk2::tk2button(win$env$frm_output_file,
                                          text = msg_make_dialog$choose_file)
  tcltk::tkpack(win$env$output, win$env$output_btn, side = "left", padx = 10)

  save_output <- function(output) {
    filename <- tcltk::tclvalue(tcltk::tkgetSaveFile(
      initialfile = "cattle_info.csv",
      filetypes = "{ {All Files} * } { {CSV Files} {.csv} }"
    ))
    if (filename != "") {
      Encoding(filename) <- encoding
      filename <- enc2utf8(filename)
      return(filename)
    } else {
      return(tcltk::tclvalue(output))
    }
  }
  tcltk::tkconfigure(win$env$output_btn,
                     command = function() {
                       tcltk::tclvalue(output) <- save_output(output)
                       })

  # append
  append <- tcltk::tclVar(1)
  win$env$append <-
    tcltk2::tk2checkbutton(win$env$frm_append,
                           text = msg_make_dialog$append,
                           variable = append)
  tcltk::tkpack(win$env$append, side = "left", padx = 10)


  # OK button / Cancel button
  is_ok_tcl <- tcltk::tclVar()
  win$env$btn_ok <-
    tcltk2::tk2button(win$env$frm_button, text = "OK", width = -10,
                      command = function() {tcltk::tclvalue(is_ok_tcl) <- 1})
  win$env$btn_cancel <-
    tcltk2::tk2button(win$env$frm_button, text = "Cancel", width = -10,
                      command = function() {tcltk::tclvalue(is_ok_tcl) <- 0})
  tcltk::tkpack(win$env$btn_ok, win$env$btn_cancel, side = "left", padx = 10)

  tcltk::tkbind(win, "<Destroy>", function() {tcltk::tclvalue(is_ok_tcl) <- 0})

  # Bring the window to the top
  tcltk::tcl("wm", "attributes", win, topmost = T)
  tcltk::tcl("wm", "attributes", win, topmost = F)

  # Wait until OK or Cancel button is clicked
  tcltk::tkwm.resizable(win, 0, 0)
  # tcltk::tkraise(win)
  # tcltk::tcl("wm", "deiconify", win)
  tcltk::tkwait.variable(is_ok_tcl)
  is_ok <- tcltk::tclvalue(is_ok_tcl)
  tcltk::tkdestroy(win)


  if (is_ok == "0") {
    opt <- options(show.error.messages = F)
    on.exit(options(opt))
  }

  variables <- c("input", "use_clipboard", "output", "append",
                 "col", "row_s", "row_e")
  env <- environment()
  for (var in variables) {
    assign(var, tcltk::tclvalue(get(var)), env)
  }
  Encoding(input) <- encoding
  input <- enc2utf8(input)
  Encoding(output) <- encoding
  output <- enc2utf8(output)
  col <- as.numeric(col)
  row_s <- as.numeric(row_s)
  row_e <- as.numeric(row_e)

  use_clipboard <- (use_clipboard == "1")
  append <- (append == "1")
  col <- ifelse(col == 0, 1, col)
  if (use_clipboard) {
    skip <- 0
    nrows <- -1
  } else {
    if (row_s != "" & row_e != "" & row_s > row_e) {
      stop(msg_make_dialog$row_err)
    }
    skip <- ifelse(row_s != "", row_s - 1, 0)
    nrows <- ifelse(row_e != "", row_e - row_s + 1, -1)
  }

  ids <- load_ids(input = input, use_clipboard = use_clipboard, col = col,
                  skip = skip, nrows = nrows)
  return(list(ids = ids, output = output, append = append))
}


# dialog_finished
#' Dialog to tell you scparing is finised
#'
#' @import tcltk
dialog_finished <- function() {
  # tcltk::tkmessagebox cannot be used here, because it doesn't become the topmost
  finished <- tcltk::tktoplevel()
  tcltk::tktitle(finished) <- msg_dialog_finised$title
  finmsg <- tcltk::tklabel(finished, text = msg_dialog_finised$message,
                           width = 40)
  finbtn <- tcltk::tkbutton(finished, text = "OK", width = -10,
                            command = function() {tcltk::tkdestroy(finished)})
  tcltk::tkbind(finished, "<Return>", function() {tcltk::tkdestroy(finished)})
  tcltk::tkpack(finmsg, finbtn, pady = 5)

  tcltk::tcl("wm", "attributes", finished, topmost = T)
  # tcltk::tcl("wm", "deiconify", finished)
  tcltk::tkfocus("-force", finbtn)
  # tcltk::tkraise(finished)
  invisible()
}

# ProgressBar
#' Progress bar
#'
#' @param gui_pb Show progress bar in a window (T) or in console (F)
#' @param ... Be passed to \code{tclck::tkProgressBar} or
#'   \code{utils::txtProgressBar}
#'
#' @importFrom utils txtProgressBar
#' @importFrom tcltk tkProgressBar
ProgressBar <- function(gui_pb, ...) {
  if (gui_pb) {
    tkProgressBar(...)
  } else {
    txtProgressBar(style = 3, ...)
  }
}

# setProgressbar
#' Set progress bar
#'
#' @param gui_pb Show progress bar in a window (T) or in console (F)
#' @param ... Be passed to \code{tcltk::setTkProgressBar} or
#'   \code{utils::setTxtProgressBar}
#'
#' @importFrom utils setTxtProgressBar
#' @importFrom tcltk setTkProgressBar
setProgressBar <- function(gui_pb, ...) {
  if (gui_pb) {
    setTkProgressBar(...)
  } else {
    setTxtProgressBar(...)
  }
}


