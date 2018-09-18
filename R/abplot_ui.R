



abplotUI = function(){
  tagList(
    fluidRow(
      column(width = 12,
             imgSelect("abp_plotbar"))
    ),
    fluidRow(
      br(),
      column(width = 8,
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             plotOutput(outputId = "abp_plot")
      ),
      column(width = 4,
             hidden(selectInput(inputId = "abp_group", label = "Grouping Variable", choices=c('none'), width='100%')),
             #choices = c(testvartypes$name[which(testvartypes$type == "nominal")]),
                                #selected = firstnominal$name)),
             hidden(multiToggleButton(id = 'abp_stackfacet',
                               choices = c(stacked= 'Stacked', facetted='Facetted', joy='Joy'), selected = 'stacked')),
             hidden(selectInput(inputId = "abp_xvar", label = "x-variable",
                                choices = c(), width='100%')),
             hidden(checkboxInput(inputId = "abp_fill", label = "Fill", value = TRUE)),
             hidden(checkboxInput(inputId = "abp_grid", label = "Grid", value = TRUE)),
             hidden(tags$input(id = "abp_color", type = 'color', value = '#4DAF4A', style = 'width:5em;', class = 'shiny-color-picker')),
             hidden(checkboxInput(inputId = "abp_linetype", label = "Varying line types", value = FALSE)),
             hidden(sliderInput(inputId = "abp_bins", label = "Number of bins",
                                min = 5, max = 60, value = 30, step = 1, round = TRUE, ticks = FALSE, width='100%')),
             hidden(sliderInput(inputId = "abp_trans", label = "Transparency",
                                min = 0.2, max = 1, value = 0.5, step = 0.05, ticks = FALSE, width='100%')),
             hidden(checkboxInput(inputId = "abp_err", label = "Error Bars")),
             hidden(checkboxInput(inputId = "abp_dodge", label = "Dodge")),
             hidden(checkboxInput(inputId = "abp_marg", label = "Marginal plots")),
             hidden(checkboxInput(inputId = "abp_fitlines", label = "Fitline(s)")),
             hidden(textInput(inputId = "abp_main",
                              label = "Title", width='100%')), 
             fluidRow(
               column(width = 6,
                      hidden(textInput(inputId = "abp_xlab",
                                       label = "Label x-axis"))),
               column(width = 6,
                      hidden(textInput(inputId = "abp_ylab",
                                       label = "Label y-axis")))
             ),
             
             conditionalPanel(  # Hoe toon ik dit panel alleen wanneer values$person_abl niet leeg is?
               condition = "$('#abp_plotbar img').length > 0", 
               tags$h4('Save plot'),
               tagAppendAttributes(numericInput('abp_download_width', 'Width (cm)', value = 14, min = 2, max = 50, 
                                                width='6em'),style='display:inline-block;'),
               tagAppendAttributes(numericInput('abp_download_height', 'Height (cm)', value=8, min = 2, max = 50, 
                                                width='6em'),style='display:inline-block;'),
               downloadButton('abp_download', 'Download'))
             )
             ))
}
  