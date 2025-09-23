
# Booklets (Interaction model) ----------------------------------------------    

output$inter_booklets = renderDataTable({
  req(values$ctt_booklets)

  cdef = list(list(targets = ncol(values$ctt_booklets)-1, 
                   render = JS("function(data, type, full){ return '<span class=\"sparkbox\">' + data + '</span>' }")),
              list(className = "numeric", targets = int_col_indx(values$ctt_booklets)-1L),
              list(className = "dec-2", targets = double_col_indx(values$ctt_booklets)-1L))
  
  
  
  drawcallback = init_sparks(.box = list(chartRangeMin = 0, chartRangeMax = max(values$ctt_booklets$max_booklet_score)),
                             add_js='dt_numcol(settings);')
  
  selected = 1
  isolate({
    if(!is.null(values$inter_booklet))
    {
      selected = min(which(values$ctt_booklets$booklet_id == values$inter_booklet))
    }
  })
  
  dat = values$ctt_booklets |>
    mutate(across(where(is.double),~round(.x,digits=2))) |>
    rename_with(tbl_names)
  
  datatable(dat, 
            rownames = FALSE, selection = list(mode = 'single', selected = selected), 
            class='compact', extensions = 'Buttons',
            options = list(columnDefs = cdef, fnDrawCallback = drawcallback,
                           buttons = dt_buttons('inter_booklets', title = '_ctt_booklets',
                                                list(exportOptions = list(columns=':not(:last-child)'))),
                           searching = FALSE, pageLength = 15, scrollX = TRUE, autoWidth=FALSE, dom='<"dropdown" B>lrtip',
                           initComplete = JS("dt_btn_dropdown")))
  
})

#write minus the spark column
output$inter_booklets_xl_download = downloadHandler(
    filename = function(){paste0(gsub('\\.\\w+$','',basename(values$project_name), perl=TRUE),'_ctt_booklets.xlsx')},
    content = function(file) {
      write_xlsx(select(values$ctt_booklets, -"test_score"), file)
    }
)
output$inter_booklets_csv_download = downloadHandler(
  filename = function(){paste0(gsub('\\.\\w+$','',basename(values$project_name), perl=TRUE),'_ctt_booklets.csv')},
  content = function(file) {
    write.csv2(select(values$ctt_booklets, -"test_score"), file, row.names = FALSE, fileEncoding = "utf8")
  }
)

observe({
  req(values$ctt_booklets,input$inter_booklets_rows_selected)
    values$inter_booklet = as.character(values$ctt_booklets$booklet_id[input$inter_booklets_rows_selected])
    values$inter_plot_items = dbGetQuery(db, 
             'SELECT item_id FROM dxBooklet_design WHERE booklet_id=:booklet ORDER BY item_position;',
             tibble(booklet=values$inter_booklet)
             )$item_id
    
}, priority=2)


output$inter_current_booklet = renderUI(tags$b(paste('Booklet:', values$inter_booklet)))

observe({
  req(values$inter_booklet, values$inter_plot_items)
  stats = filter(values$ctt_booklets, .data$booklet_id==values$inter_booklet)

  if(stats$n_persons <= stats$n_items)
  {
    updateSlider(session, 'interslider', 
                 error='Cannot compute the interaction model because the number of responses is smaller than the number of items')
    return(NULL);
  }
  
  f = try(interaction_models$get(values$inter_booklet), silent=TRUE)
  if(inherits(f,"try-error"))
  {
    print(f)
    updateSlider(session, 'interslider', 
                 error='Cannot compute the interaction model for this booklet')
    return(NULL);
  }

  selected = NULL
  isolate({
    if(!is.null(input$interslider_select) && input$interslider_select %in% values$inter_plot_items)
      selected = input$interslider_select
  })
  updateSlider(session, 'interslider', selected=selected,
    choices =
      lapply(values$inter_plot_items, function(item)
      {
          outfile = tempfile(fileext = '.png')
          png(outfile, width = 200, height = 140)
          par(mar=rep(0,4))
          plot(f, items = item, show.observed = input$inter_show_observed, curtains = input$inter_curtains, 
               summate = input$inter_summate, main=NULL,xlab=NA,ylab=NA,sub=NULL,xaxt='n',yaxt='n', ann=FALSE)
          dev.off()
          list(src = outfile, contentType = 'image/png', choice_id = item)
       })
  )
}, priority = 1)

output$interslider_plot = renderPlot({
  req(values$inter_booklet, values$inter_plot_items, input$interslider_select, 
      input$interslider_select %in% values$inter_plot_items) # not evaluate before new booklet has finished evaluating
  
  f = interaction_models$get(values$inter_booklet)
  plot(f, items = input$interslider_select, show.observed = input$inter_show_observed, 
       curtains = input$inter_curtains, summate = input$inter_summate,main='$item_id')
})

output$interslider_download = downloadHandler(
  filename = function(){
    paste0(values$inter_booklet, '_im_', input$interslider_select, '.png')
  },
  content = function(file){
    req(values$inter_booklet, values$inter_plot_items, input$interslider_select, 
        input$interslider_select %in% values$inter_plot_items) # not evaluate before new booklet has finished evaluating

    f = interaction_models$get(values$inter_booklet)
    png(filename=file, type='cairo-png', width=960,height=640)
    plot(f, items = input$interslider_select, show.observed = input$inter_show_observed, 
         curtains = input$inter_curtains, summate = input$inter_summate,main='$item_id')
    dev.off()

  },
  contentType = "image/png"
)





# items & distractor plots ---------------------------------------------------------

output$ctt_items = renderDataTable(
{
  req(values$ctt_items)
  data = ctt_items_table(values$ctt_items, input$ctt_items_averaged) |>
    mutate(across(where(is.double),~round(.x,digits=2)))
  selected = 1
  search_ = ""

  isolate({

    if(!is.null(values$selected_ctt_item))
        selected = min(which(data[['item_id']] == values$selected_ctt_item[['item_id']]))

    if(values$ctt_items_settings$keep_search && !is.null(input$ctt_items_search))
      search_ = input$ctt_items_search

  })  

  datatable(rename_with(data,tbl_names), 
    rownames = FALSE, selection = list(mode = 'single', selected = selected), class='compact',
    extensions = 'Buttons',
    options = list(dom='<"dropdown" B>lfrtip',
                   
                   buttons = dt_buttons('ctt_items', title='ctt_items'),
                   search = list(search = search_, smart=FALSE),
                   pageLength = 20, scrollX = TRUE,
                   columnDefs = list(list(className = "numeric", targets = int_col_indx(data)-1L),
                                     list(className = "dec-2", targets = double_col_indx(data)-1L)),
                   fnDrawCallback = JS('dt_numcol'),
                   initComplete = JS(paste0(
                                      'function(dtsettings){
                                        dt_btn_dropdown(dtsettings);
                                        dt_show_row(dtsettings,',selected-1,');
                                      }'))))
})

output$ctt_items_xl_download = downloadHandler(
  filename = function(){paste0(gsub('\\.\\w+$','',basename(values$project_name), perl=TRUE),'_ctt_items.xlsx')},
  content = function(file) {
    write_xlsx(ctt_items_table(values$ctt_items, input$ctt_items_averaged), file)
  }
)
output$ctt_items_csv_download = downloadHandler(
  filename = function(){paste0(gsub('\\.\\w+$','',basename(values$project_name), perl=TRUE),'_ctt_items.csv')},
  content = function(file) {
    write.csv2(ctt_items_table(values$ctt_items, input$ctt_items_averaged), file, 
               row.names = FALSE, fileEncoding = "utf8")
  }
)


observeEvent(input$ctt_itemprop,{
  req(input$ctt_itemprop, length(input$ctt_itemprop)>1)
  add_item_properties(db, as_tibble(input$ctt_itemprop))
  values$item_properties = get_items(db)
})

observeEvent(values$item_properties,{
  req(values$item_properties)
  if.else(ncol(values$item_properties)>1,show,hide)('ctt_itemprop-container') 
})

observeEvent(values$item_properties,{
  req(values$item_properties, ncol(values$item_properties) > 1)
  fields = lapply(values$item_properties, function(col){
    list(type = if.else(is.numeric(col),'number', 'text'))
  })
  names(fields) = names(values$item_properties)
  fields$item_id$type = 'hidden'
  value = if.else(is.null(values$selected_ctt_item), NULL, 
                  filter(values$item_properties,.data$item_id==values$selected_ctt_item$item_id))
  updateListInput(session,'ctt_itemprop',fields=fields,value=value)
}, priority=1)

observeEvent(values$selected_ctt_item,{
  req(values$selected_ctt_item)
  updateListInput(session,'ctt_itemprop',value=filter(values$item_properties,.data$item_id==values$selected_ctt_item$item_id))
})


# infer from the ctt items table which item was selected
observeEvent(input$ctt_items_rows_selected,{

  if(is.null(input$ctt_items_rows_selected))
  { 
    values$selected_ctt_item = NULL
  } else
  {
    values$selected_ctt_item = ctt_items_table(values$ctt_items, input$ctt_items_averaged)[input$ctt_items_rows_selected,]
  }
}) 



output$ctt_selected_item = renderUI({if(!is.null(values$selected_ctt_item)) values$selected_ctt_item$item_id})




output$ctt_plot = renderPlot({req(db, values$selected_ctt_item);distr_plot()})

distr_plot = function(update_legend=TRUE)
{
  
  ctt_item = values$selected_ctt_item
  item_id = pull(ctt_item, 'item_id')
  
  if('booklet_id' %in% names(ctt_item))
  {
    booklet = pull(ctt_item, booklet_id)
    lgnd = distractor_plot(db, predicate={booklet_id==booklet}, item_id = item_id,main='pos. $item_position in $booklet_id',sub=NULL,legend=FALSE)
  } else
  {
    isolate({
      booklets = values$ctt_items |> 
        filter(.data$item_id==!!item_id & .data$n_persons>1) |>
        pull(.data$booklet_id)
     })
    
   ly = matrix_layout(length(booklets)) 
   
   if(ncol(ly)<=3)
    {
     main = 'item $item_position in $booklet_id'
     axes=TRUE
   } else
   {
     main = '$booklet_id'
     axes=FALSE
     par(mar=c(1,1,1,1))
   }
   layout(ly)
   lgnd = distractor_plot(db, item_id = item_id, predicate={booklet_id %in% booklets},main=main,sub=NULL,legend=FALSE,axes=axes, col=qcolors)
  }
  if(update_legend)
    values$distr_legend = lgnd
}


output$ctt_plot_download = downloadHandler(
  filename = function()
  {
    paste0('distr_',values$selected_ctt_item$item_id,'.png')
  },
  content = function(file)
  {
    png(filename=file, type='cairo-png', width=960,height=640)
    distr_plot(FALSE)
    dev.off()
    
  },
  contentType = "image/png"
)


output$item_viewer = renderUI({
  req(values$selected_ctt_item)


  item = dbGetQuery(db, 'SELECT * FROM dxItems WHERE item_id=:item_id;', list(item_id=values$selected_ctt_item$item_id)) |>
    select_if(function(x) !is.na(x))
  
  if('item_html' %in% colnames(item))
  {
    tags$iframe(srcdoc = item$item_html)
  } else if('item_href' %in% colnames(item))
  {
    tags$iframe(src = item$item_href)   
  } else if('item_screenshot' %in% colnames(item))
  {
    tags$img(src = paste0("data:image/png;base64,", item$item_screenshot))
  }
})


# moet fit_inter summate =F labels hebben?


output$item_rules = renderDataTable({
  req(db, values$distr_legend)

  ctt_item = values$selected_ctt_item
  
  df = dbGetQuery(db, 
          'SELECT item_id, response, item_score FROM dxScoring_rules 
                    WHERE item_id=?;', values$selected_ctt_item$item_id) |>
        inner_join(values$distr_legend, by='response') |>
        select("item_id",legend="color", "response", "n", "item_score")
      
  sketch = tags$table(
        class = "compact readable",
        tableHeader(c('item_id','','response','n','score','')),
        tags$tfoot(tags$tr(tags$td(),
                           tags$td(),
                           tags$td('sum: ', style='text-align: right;'), 
                           tags$td(tags$div(sum(df$n), style="background-color:lightgrey;width:100%;height:100%;text-align:center;")),
                           tags$td(sprintf('avg: %.2f',ctt_item$mean_score), style='text-align: right;'),
                           tags$td()),
                   style="font-style:italic;"))
      
  df$n = paste(df$n, sum(df$n),sep=',')
  df$old_item_score = df$item_score
      
  runjs("$('#go_save_ctt_item_rules').removeClass('btn-primary');")
      
  datatable(df, container = sketch, selection = 'none',  rownames = FALSE, class = "compact readable",
                options = list(autoWidth = FALSE,
                               paging = FALSE,
                               scrollY = '300px',
                               scrollCollapse = TRUE,
                               dom = 't',
                               #initComplete = JS("function(settings){dtshrink(settings)}"),
                               fnDrawCallback = init_sparks(),
                               columnDefs = list(list(targets = 5, 
                                                      render = JS("function(data,type,row){
                                                                  return(row[4] == row[5] ? '' : '<span class=\"label label-info\">!</span>')}")),
                                                 list(targets = 3,
                                                      render = JS("function(data, type, full){ return '<span class=\"sparkcount\">' + data + '</span>' }")),
                                                 list(targets = 1,
                                                      render = JS("function(data, type, full){ return '<span class=\"sparklegend\">' + data + '</span>' }"),
                                                      orderable = FALSE),
                                                 list(targets = 0,
                                                      visible = FALSE))))

}, server = FALSE)

observeEvent(input$item_rules_data, {
  runjs("$('#go_save_ctt_item_rules').addClass('btn-primary');")
})

observeEvent(input$go_save_ctt_item_rules, {
  req(input$item_rules_data)

  new_rules = as_tibble(lapply(input$item_rules_data, unlist)) |>
      select("item_id", "response", item_score = 'score', old_val = 'V6')
  
  withBusyIndicatorServer("go_save_ctt_item_rules",
  {
    
    if(any(new_rules$item_score %% 1 != 0))
    {
      stop('only integer scores allowed')
    } else if(min(new_rules$item_score) < 0 )
    {
      stop('negative scores not allowed')
    } else if(min(new_rules$item_score) > 0 )
    {
        stop('Item should have at least one option scored 0')
    } else if(max(new_rules$item_score) < 1)
    {
      stop('Item should have at least one option with a score > 0')
    } else
    {
      if(nrow(filter(new_rules, .data$item_score != .data$old_val)) >  0)
      {
        touch_rules(db, new_rules)
        values$ctt_items_settings$preselected = isolate(input$ctt_items_rows_selected)
        values$ctt_booklets_settings$preselected = isolate(input$inter_booklets_rows_selected)
        init_project()
      }
      runjs("$('#go_save_ctt_item_rules').removeClass('btn-primary');")
    }
  })
})
