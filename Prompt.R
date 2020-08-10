library(tcltk2)

win1 <- tktoplevel()
win1$env$cb <- tk2checkbutton(win1, text = "It doesn't seem you have the data installed! Check the box and press ok to download.")
cbValue <- tclVar("0")
tkconfigure(win1$env$cb, variable = cbValue)
tkgrid(win1$env$cb, padx = 60, pady = 15)

onOK <- function() {
  cbVal <- as.character(tclvalue(cbValue))
  tkdestroy(win1)
  switch(cbVal,
    "1" = tkmessageBox(message = "Close this box to install."),
    "0" = tkmessageBox(
      message = "Very Well.",
      icon = "warning")
  )  
}
win1$env$butOK <- tk2button(win1, text = "OK", width = -6, command = onOK)
tkgrid(win1$env$butOK, padx = 10, pady = c(0, 15))

tkfocus(win1)