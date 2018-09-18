



pvplotUI = function(){
  tagList(
    fluidRow(
      column(width = 12,
             imgSelect("pvp_plotbar"))
    ),
    fluidRow(
      br(),
      column(width = 8,
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             plotOutput(outputId = "pvp_plot")
      ),
      column(width = 4,
             hidden(selectInput(inputId = "pvp_group", label = "Grouping Variable", choices=c('none'), width='100%')),
             #choices = c(testvartypes$name[which(testvartypes$type == "nominal")]),
             #selected = firstnominal$name)),
             hidden(multiToggleButton(id = 'pvp_stackfacet',
                                      choices = c(stacked= 'Stacked', facetted='Facetted', joy='Joy'), selected = 'stacked')),
             hidden(selectInput(inputId = "pvp_xvar", label = "x-variable",
                                choices = c(), width='100%')),
             hidden(checkboxInput(inputId = "pvp_fill", label = "Fill", value = TRUE)),
             hidden(checkboxInput(inputId = "pvp_grid", label = "Grid", value = TRUE)),
             hidden(tags$input(id = "pvp_color", type = 'color', value = '#4DAF4A', style = 'width:5em;', class = 'shiny-color-picker')),
             hidden(checkboxInput(inputId = "pvp_linetype", label = "Varying line types", value = FALSE)),
             hidden(sliderInput(inputId = "pvp_bins", label = "Number of bins",
                                min = 5, max = 60, value = 30, step = 1, round = TRUE, ticks = FALSE, width='100%')),
             hidden(sliderInput(inputId = "pvp_trans", label = "Transparency",
                                min = 0.2, max = 1, value = 0.5, step = 0.05, ticks = FALSE, width='100%')),
             hidden(checkboxInput(inputId = "pvp_err", label = "Error Bars")),
             hidden(checkboxInput(inputId = "pvp_dodge", label = "Dodge")),
             hidden(checkboxInput(inputId = "pvp_marg", label = "Marginal plots")),
             hidden(checkboxInput(inputId = "pvp_fitlines", label = "Fitline(s)")),
             hidden(textInput(inputId = "pvp_main",
                              label = "Title", width='100%')), 
             fluidRow(
               column(width = 6,
                      hidden(textInput(inputId = "pvp_xlab",
                                       label = "Label x-axis"))),
               column(width = 6,
                      hidden(textInput(inputId = "pvp_ylab",
                                       label = "Label y-axis")))
             ),
             
             conditionalPanel(
               condition = "$('#pvp_plotbar img').length > 0", 
               tags$h4('Save plot'),
               tagAppendAttributes(numericInput('pvp_download_width', 'Width (cm)', value = 14, min = 2, max = 50, 
                                                width='6em'),style='display:inline-block;'),
               tagAppendAttributes(numericInput('pvp_download_height', 'Height (cm)', value=8, min = 2, max = 50, 
                                                width='6em'),style='display:inline-block;'),
               downloadButton('pvp_download', 'Download'))
      )
    ))
}
