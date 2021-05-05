
# ability -----------------------------------------------------------------

observe(
{
  input$ability_method
  input$ability_prior
  if(input$ability_method != 'EAP')
  {
    runjs('hide_inputs("#ability_prior,#ability_npv,#ability_mu,#ability_sigma")')
  } else
  {
    runjs('show_inputs("#ability_prior")')
    if(input$ability_prior == 'normal')
    {
      runjs('show_inputs("#ability_npv,#ability_mu,#ability_sigma")')
    } else
    {
      runjs('hide_inputs("#ability_npv,#ability_mu,#ability_sigma")')
    }
  }
})

observe(
  {
    input$ability_tables_method
    input$ability_tables_prior
    
    runjs('hide_inputs("#ability_tables_use_draw")')
    
    if(input$ability_tables_method != 'EAP')
    {
      runjs('hide_inputs("#ability_tables_prior,#ability_tables_npv,#ability_tables_mu,#ability_tables_sigma")')
    } else
    {
      runjs('show_inputs("#ability_tables_prior")')
      if(input$ability_tables_prior == 'normal')
      {
        runjs('show_inputs("#ability_tables_npv,#ability_tables_mu,#ability_tables_sigma")')
        if(!is.null(values$parms) && values$parms$inputs$method == 'Bayes')
          runjs('show_inputs("#ability_tables_use_draw")')
      } else
      {
        runjs('hide_inputs("#ability_tables_npv,#ability_tables_mu,#ability_tables_sigma")')
      }
    }
  })




# to do: check inputs correct
observeEvent(input$go_ability, {
  withBusyIndicatorServer("go_ability",{

    if(is.null(values$parms)) 
      go_fit_enorm()

    if(!(is.null(input$ability_predicate) || trimws(input$ability_predicate) == ''))
    {
      abl = eval(parse(text=paste0("ability(db, parms=values$parms, predicate={",input$ability_predicate,"},method='",input$ability_method,
                                   "',prior='",input$ability_prior,"',mu=",input$ability_mu,",sigma=",input$ability_sigma,")")))
    } else
    {
      abl = ability(db, parms = values$parms, method = input$ability_method, prior = input$ability_prior, 
                    npv = input$ability_npv, mu = input$ability_mu, sigma = input$ability_sigma )
    }
    
    
    values$person_abl = inner_join(abl, get_persons(db), by='person_id')
    
 
    show(selector='#enorm_tabs + div.tab-content > div.tab-pane[data-value="ability"] > *')

  })
})

output$person_abilities = renderDataTable(
{
  if(!is.null(values$person_abl))
  {
    datatable( mutate_if(values$person_abl, is.double, round, digits = 3), 
               rownames = FALSE, selection = 'none', 
               class='compact', extensions = 'Buttons',
               options = list(buttons = dt_buttons('person_abilities'),
                              pageLength = 15, autoWidth=FALSE, dom='<"dropdown" B>lrtip',
                              initComplete = JS("dt_btn_dropdown")))
  }  
     
})


output$person_abilities_xl_download = downloadHandler(
  filename = function(){paste0(gsub('\\.\\w+$','',basename(db@dbname), perl=TRUE),'_abl_person.xlsx')},
  content = function(file) {
    write_xlsx(values$person_abl, file)
  }
)
output$person_abilities_csv_download = downloadHandler(
  filename = function(){paste0(gsub('\\.\\w+$','',basename(db@dbname), perl=TRUE),'_abl_person.csv')},
  content = function(file) {
    write.csv2(values$person_abl, file, row.names = FALSE, fileEncoding = "utf8")
  }
)



observeEvent(input$go_ability_tables, {
  withBusyIndicatorServer("go_ability_tables",{
    if(is.null(values$parms)) 
      go_fit_enorm()
    
    values$abl_tables = ability_tables(parms = values$parms, method = input$ability_tables_method,
                                       npv = input$ability_tables_npv, sigma = input$ability_tables_sigma,
                                       prior = input$ability_tables_prior)
      
    bkl = unique(pull(values$abl_tables, .data$booklet_id))
    
    if(is.null(isolate(input$abl_tables_plot_booklet))){
      selected = bkl
    } else
    {
      selected = intersect(bkl, isolate(input$abl_tables_plot_booklet))
    }
    
    updateSelectizeInput(session, 'abl_tables_plot_booklet', 
                         choices = bkl, selected = selected)
    
    show(selector='#enorm_tabs + div.tab-content > div.tab-pane[data-value="ability_tables"] > *')
     
  })
})

output$abl_tables = renderDataTable(
{
    if(!is.null(values$abl_tables))
    {
         mutate(values$abl_tables, theta = round(.data$theta,3), se = round(.data$se,3))
    }
},rownames = FALSE, selection = 'none', class='compact',extensions = 'Buttons',
   options = list(dom='<"dropdown" B>lfrtip',
                   buttons= dt_buttons('abl_tables'),
                   pageLength = 20, scrollX = TRUE,
                   columnDefs = list(list(className = "dec-3", targets = list(2,3))),
                   fnDrawCallback = JS('dt_numcol'),
                   initComplete = JS('dt_btn_dropdown')))

output$abl_tables_xl_download = downloadHandler(
  filename = function(){paste0(gsub('\\.\\w+$','',basename(db@dbname), perl=TRUE),'_abl.xlsx')},
  content = function(file) {
    write_xlsx(values$person_abl, file)
  }
)
output$abl_tables_csv_download = downloadHandler(
  filename = function(){paste0(gsub('\\.\\w+$','',basename(db@dbname), perl=TRUE),'_abl.csv')},
  content = function(file) {
    write.csv2(values$person_abl, file, row.names = FALSE, fileEncoding = "utf8")
  }
)


# selectize input needs to be debounced since changes happen too quickly
abl_tables_plot_booklet = reactive({input$abl_tables_plot_booklet}) %>% debounce(300)


abl_tables_plot = reactive({
  req(values$abl_tables, abl_tables_plot_booklet())

  booklets = abl_tables_plot_booklet()
  
  abl = filter(values$abl_tables, is.finite(.data$theta)) %>%
    inner_join(tibble(booklet_id = booklets), by='booklet_id') 
  
  xmin = floor(min(abl$theta))
  xmax = ceiling(max(abl$theta))
  
  # approximately
  ymax = ceiling(1/(min(abl$se, na.rm=T)**2))

  colr = qcolors(length(booklets))
  names(colr) = booklets
  
  # to do: search all code for bklist, it does not exist anymore
  
  # scoretab = values$parms$inputs$bkList[abl_tables_plot_booklet()] %>%
  #   lapply(function(bk){ tibble(booklet_score=0:(length(bk$scoretab)-1), n=bk$scoretab)} ) %>%
  #   bind_rows(.id = 'booklet_id')

  abl = abl %>%
    inner_join(values$parms$inputs$scoretab, by=c('booklet_id','booklet_score'))
  
  #make a histogram, 31 bins, from weighted data, range xmin,xmax
  offs = (xmax-xmin)/62
  mids = seq(xmin+offs,xmax-offs,length.out=30)
  hist_counts = abl %>%
    group_by(.data$theta) %>%
    mutate(x = which.min(abs(mids - .data$theta[1]))) %>%
    ungroup() %>%
    group_by(.data$x) %>%
    summarize(y = sum(.data$N)) %>%
    ungroup() %>%
    right_join(tibble(x=1:31), by='x') %>%
    mutate(y = coalesce(.data$y,0L)) %>%
    arrange(.data$x)
  
  par(mar = c(5,4,3,4))

  barplot(hist_counts$y, axes=FALSE,space=0, ylim=c(0,max(hist_counts$y)*2))
  axis(side=4 ) 
  
  par(new=TRUE)
  plot(type='n',x=c(xmin,xmax),y=c(0,ymax),xlab=expression(theta), ylab='Information',bty='l')
  
  for(bkl in booklets)
  {
    plot(information(values$parms, booklet_id = bkl), 
         from = xmin, to = xmax, add=TRUE,col = colr[bkl])
  }

  mtext("n persons", side=4, line=2.5)

})

output$abl_tables_plot_ti = renderPlot({abl_tables_plot()})

output$abl_tables_plot_ti_download = downloadHandler(
  filename = 'test_information.png',
  content = function(file){
    png(filename=file, type='cairo-png', width=960,height=640)
    abl_tables_plot()
    dev.off()
  },
  contentType = "image/png"
)




output$abl_tables_plot_ti_hinf = renderUI({
  
  req(values$abl_tables, input$abl_tables_plot_ti_hov)
  abl = values$abl_tables
  bkl = abl_tables_plot_booklet()
  colr = qcolors(length(bkl))
  names(colr) = bkl

  hover = input$abl_tables_plot_ti_hov

  theta = hover$x
  bky = sapply(bkl, function(bk){information(values$parms, booklet_id = bk)(theta)})
  
  # needs to be somewhat close or we don't plot
  req(min(abs(bky-hover$y)) < hover$domain$top/12 )
  
  booklet_id = bkl[which.min(abs(bky-hover$y))]


  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct = (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct = (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px = hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px = hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

  if(left_pct > .5)
  {
    trnsp = "transform:translateY(-100%);"
    left_px = left_px + 5
  } else
  {
    left_px = left_px - 5
    trnsp = "transform:translate(-100%,-100%);"
  }

  
  # create style property for tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure tooltip will be on top
  style = paste0("position:absolute; z-index:100; background-color: rgba(255, 255, 255, 0.85); ",
                  "left:", left_px, "px; top:", (top_px-5), "px;padding:5px;",
                  trnsp,
                  "border: 1px solid ", colr[booklet_id], "; border-radius:2px;")

  tags$div(booklet_id, style=style)


})
