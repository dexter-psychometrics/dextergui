
# With some  changes borrowed from: 
# https://github.com/daattali/advanced-shiny/tree/master/busy-indicator


# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI = function(button) {
  id=button[['attribs']][['id']]
  if(is.null(id))
  {
    for(i in 1:length(button))
    {
      if(button[[i]][['name']] == 'button') id = button[[i]][['attribs']][['id']]
    }
  }
  if(is.null(id)){
    warning('not a valid button for busyindicator')
    return();
  }
  tags$div(
    `data-for-btn` = id,
    button,
    tags$span(
      class = "btn-loading-container",
      hidden(
        tags$i(class="fa fa-spinner fa-spin btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      tags$div(class = "btn-err",
          tags$div(icon("exclamation-circle"),
              tags$b("Error: "),
              tags$span(class = "btn-err-msg")
          )
      )
    ),
    # hardcoded for the moment
    style="display:inline-block;vertical-align:top;margin-top:25px;white-space:nowrap;"
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer = function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl = sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl = sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl = sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  #tryCatchLog
  tryCatch({
    value = expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc = function(err, buttonId) {
  errEl = sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg = sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage = gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}