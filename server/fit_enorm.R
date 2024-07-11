
# graph title is side effect of this

output$design_plot = renderForceNetwork(
{
  req(db)
  values$ctt_items #so shiny knows to reinit this on new project
  if(trimws(input$enorm_predicate == ''))
  {
    design = design_info(db)
  } else
  {
    design = try(eval(parse(text=paste0("design_info(db, ",
                                        "predicate={",input$enorm_predicate,"})"))),
                 silent=TRUE)
    
    if(inherits(design,'try-error'))
    {
      err_message = gsub('\n',' ', as.character(design))
      if(grepl('no such column', err_message, fixed=TRUE))
      {
        output$enorm_design_connected = renderUI({gsub('^.+no such column','unknown variable',err_message, perl=TRUE)})
      } else if(grepl('no data', err_message, fixed=TRUE))
      {
        output$enorm_design_connected = renderUI({'no data selected'})
      } else 
      {
        output$enorm_design_connected = renderUI({'invalid predicate'})
      }
      return(NULL)
    }
  }

  output$enorm_design_connected = renderUI({paste0(n_distinct(design$design$booklet_id),
                                                   ' booklet(s), design is ',
                                                   ifelse(design$connected, 'connected', 'NOT connected'))})
  wm = design$adj_matrix$weighted_by_items
  if(ncol(wm) >= 80)
  {
    # too large to plot items and booklets
    # to do: some message
    
    colnames(wm) = paste0(colnames(wm),'\u200C')

    tri = upper.tri(wm)
    links = tibble(source = rep(1:ncol(wm)-1L,nrow(wm))[tri], target = rep(1:ncol(wm)-1L,each=nrow(wm))[tri], 
                   value = as.vector(wm[tri])) |> 
      filter(value>0)
    nodes = tibble(name=colnames(wm), group = 1:ncol(wm))
    
  } else
  {
    nodes = bind_rows(arrange(design$testlets, .data$item_id), 
                      tibble(item_id=paste0(colnames(wm),'\u200C'), 
                             testlet = max(design$testlets$testlet)+1L))
    colnames(nodes) = c('name','group')
    n_itm = n_distinct(design$design$item_id)
    links = design$design |>
      arrange(.data$booklet_id,.data$item_id) |>
      mutate(source = dense_rank(.data$item_id) - 1L, target = dense_rank(.data$booklet_id) -1L + n_itm) |>
      select(.data$source, .data$target)
    
    
  }

  forceNetwork(Links = links, Nodes = nodes, fontSize=11, zoom=TRUE,
               Source = 'source', Target = 'target', opacity=0.7,
               NodeID = 'name', Group = 'group')
  
})


output$fit_enorm_result = renderUI(
{
  req(values$parms)
  x = values$parms

  tagList(
    tags$hr(),
    tags$p(tags$i('Calibration:')),
    tags$table(
      tags$tbody(
        tags$tr(tags$th('method: '), tags$td(x$inputs$method)),
        if.else(x$inputs$method == 'CML', 
          tags$tr(tags$th('iterations: '), tags$td(x$est$n_iter)),
          tags$tr(tags$th('Gibbs samples: '), tags$td(nrow(x$est$beta)))),
        tags$tr(tags$th('items:'), tags$td(nrow(x$inputs$ssI))),
        tags$tr(tags$th('responses: '), tags$td(sum(x$inputs$ssIS$sufI))))))
})

go_fit_enorm = function()
{
  if(trimws(input$enorm_predicate != ''))
  {
    values$parms = eval(parse(text=paste0("fit_enorm(db, predicate={",input$enorm_predicate,"},method='",
                                          input$enorm_method,"')")))
    
    # small fix, dexter does not correctly deparse this
    values$parms$xpr = input$enorm_predicate
      
  } else
  {
    values$parms = fit_enorm(db, method=input$enorm_method)
  }
  show(selector='#enorm_tabs + div.tab-content > div.tab-pane[data-value="enorm_items"] > *')
  show(selector='#enorm_tabs + div.tab-content > div.tab-pane[data-value="new_test"] > *')
  #show(selector='#enorm_tabs + div')
  isolate({
    # if not on the relevant tab, postpone updating the slider, it can take a long time with many items
      values$update_enorm_plots = (input$enorm_tabs == 'enorm_items')
  })
}

observeEvent(input$enorm_tabs,{
  req(values$parms)
  if(input$enorm_tabs == 'enorm_items' && !values$update_enorm_plots)
    values$update_enorm_plots = TRUE
})



observeEvent(input$go_fit_enorm,{
  withBusyIndicatorServer("go_fit_enorm",{
    go_fit_enorm()
  })
})




observe({
  if(is.null(values$parms) || values$parms$inputs$method=='Bayes' || n_distinct(values$parms$inputs$ssIS$item_score) <=2)
  {
    hide('coef_format')
  } else
  {
    show('coef_format')
  }
})


enorm_coef_table = reactive({
  req(values$parms, input$coef_format)
  
  cf = coef(values$parms) 
  if(input$coef_format == "norm" || values$parms$inputs$method == 'Bayes')
  {
    cf
  } else
  {
    cf |>
      gather('var','val', 3:4 ) |>
      unite('temp', .data$var, .data$item_score) |>
      spread(.data$temp, .data$val)
  }
})



output$enorm_coef = renderDataTable(
{
  req(enorm_coef_table())

  cf = enorm_coef_table() |> 
    mutate_if(is.numeric, round, digits=3)
  
  selected=1
  isolate({
    if(!is.null(values$enorm_item_selected)){
      selected = min(which(cf$item_id==values$enorm_item_selected))
    }
  })
  
  if(input$coef_format == "denorm" && values$parms$inputs$method == 'CML')
  {
    cdef_target = as.list(1:(ncol(cf)-1))
    sketch = tags$table(
      class='compact',
      tags$thead(
        tags$tr(
          tags$th(''), 
          tags$th('beta', colspan=(ncol(cf)-1)/2), 
          tags$th('se', colspan=(ncol(cf)-1)/2)),
        tags$tr(do.call(tagList, 
                        lapply(c('item_id',
                                 gsub('[^\\d]','',colnames(cf)[2:ncol(cf)], perl=TRUE)), 
                               tags$th)))))
  } else 
  {
    cdef_target = if.else(values$parms$inputs$method == 'CML', 
                          list(2,3),
                          as.list(2:(ncol(cf)-1)))
    
    sketch = tags$table(tableHeader(colnames(cf)))
  }

  datatable(cf, rownames = FALSE, class='compact',
            selection = 'single',
            container = sketch, extensions = 'Buttons',
            options = list(dom='<"dropdown" B>lrtip',
                           buttons =  dt_buttons('enorm_coef'),
                           pageLength = 20, scrollX = TRUE,
                           columnDefs = list(list(className = "dec-3", targets = cdef_target)),
                           initComplete = JS("dt_btn_dropdown"),
                           fnDrawCallback = JS('dt_numcol')))
})




output$enorm_coef_xl_download = downloadHandler(
  filename = function(){paste0(gsub('\\.\\w+$','',basename(values$project_name), perl=TRUE),'_enorm_coef.xlsx')},
  content = function(file) {
    write_xlsx(enorm_coef_table(), file)
  }
)
output$enorm_coef_csv_download = downloadHandler(
  filename = function(){paste0(gsub('\\.\\w+$','',basename(values$project_name), perl=TRUE),'_enorm_coef.csv')},
  content = function(file) {
    write.csv2(enorm_coef_table(), file, row.names = FALSE, fileEncoding = "utf8")
  }
)

observe({
  req(values$parms, input$enorm_slider_nbins, values$update_enorm_plots)
  
  isolate({selected = enorm_coef_table()[input$enorm_coef_rows_selected,]$item_id})
  if(length(selected)==0)
    selected=NULL

  updateSlider(session, 'enorm_slider',selected=selected, 
               choices=
               lapply(sort(unique(coef(values$parms)$item_id)), function(item)
               {
                 outfile = tempfile(fileext = '.png')
                 png(outfile, width = 200, height = 140)
                 par(mar=rep(0,4))
                 plot(values$parms,item_id=item,nbins=input$enorm_slider_nbins,main='',bty='n',axes=FALSE)
                 dev.off()
                 list(src = outfile, contentType = 'image/png', choice_id = item)
               })
  )
})

output$enorm_slider_plot = renderPlot({
  req(values$parms, input$enorm_slider_nbins, input$enorm_slider_select)
  plot(values$parms, item_id=input$enorm_slider_select, nbins=input$enorm_slider_nbins)
})

observeEvent(input$enorm_coef_rows_selected,{

  updateSlider(session, 'enorm_slider',
               selected = enorm_coef_table()[input$enorm_coef_rows_selected,]$item_id)
})

output$enorm_slider_download = downloadHandler(
  filename = function(){
    paste0('enorm_',input$enorm_slider_select,'.png')
  },
  content = function(file){
    req(values$parms, input$enorm_slider_nbins, input$enorm_slider_select)
    png(filename=file, type='cairo-png', width=960,height=640)
    plot(values$parms, item_id=input$enorm_slider_select, nbins=input$enorm_slider_nbins)
    dev.off()
    
  },
  contentType = "image/png"
)
