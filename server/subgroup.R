
#for profile plot
observe({
  req(values$ctt_booklets, input$main_navbar == 'DIF_pane')
  updateSelectInput(session, 'prof_booklet', choices=values$ctt_booklets$booklet_id)
})


observe({
  req(input$prof_booklet, values$person_properties)

  items = get_items(db) |>
    inner_join(get_design(db), by='item_id') |>
    filter(booklet_id == input$prof_booklet) |>
    select(-"item_id")
  
  iprop = tibble(name=colnames(items), n = sapply(items, n_distinct)) |>
    filter(between(.data$n, 2, nrow(items)/2))
  
  updateSelectInput(session, 'prof_item', choices = iprop$name)
  updateSelectInput(session, 'DIF_item', choices = c('item_id', iprop$name))
  
  if(ncol(values$person_properties)>1)
  {
    persons = values$person_properties |>
      semi_join(dbGetQuery(db,
                           'SELECT person_id FROM dxadministrations WHERE booklet_id=:booklet;', 
                           list(booklet=input$prof_booklet)),
                by='person_id') |>
      select(-"person_id")
    
    pprop = tibble(name=colnames(persons), n = sapply(persons, n_distinct)) |>
      filter(between(.data$n, 2, 3))
    
    updateSelectInput(session, 'prof_person', choices = pprop$name)
    
  }
})

prof_item_prop_vals = reactive({
  req(input$prof_booklet, input$prof_item)
  
  get_items(db) |>
    inner_join(get_design(db), by='item_id') |>
    filter(booklet_id == input$prof_booklet) |>
    pull(.data[[input$prof_item]]) |>
    unique()
})

observe({
  req(input$prof_booklet, input$prof_item, prof_item_prop_vals())
  prop = prof_item_prop_vals()
  
  updateSelectInput(session, 'prof_item_xvals', choices = prop, selected=prop[1:(round(length(prop)/2))])
})


# this needs a little more checking on 2 unique values
output$prof_plot = renderPlot({
  req(input$prof_booklet,input$prof_item, input$prof_person)
  
  nvals = length(prof_item_prop_vals())
  
  req(between(length(input$prof_item_xvals),1,nvals-1))
  
  stm = "get_responses(db, 
                columns=c('person_id','item_id','item_score',input$prof_item, input$prof_person),
                predicate=booklet_id == input$prof_booklet)"
  
  dat = eval(parse(text=stm))

  if(nvals != 2)
  {
    prop = tibble(val = prof_item_prop_vals(),
                  g = .data$val %in% input$prof_item_xvals) |>
      group_by(.data$g) |>
      mutate(p = paste(.data$val, collapse=','))

    dat = inner_join(prop, dat, by=c('val'=input$prof_item))
    
    colnames(dat)[colnames(dat) == 'p'] = input$prof_item
  }

  if(packageVersion("dexter") >= '1.1.5')
  {
    profile_plot(dat, item_property = input$prof_item, covariate = input$prof_person, 
                 main=input$prof_item, 
                 x=paste(input$prof_item_xvals, collapse=','), 
                 cex.legend=1.2,cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
  } else
  {
    profile_plot(dat, item_property = input$prof_item, covariate = input$prof_person, 
                 main=input$prof_item, 
                 x=paste(input$prof_item_xvals, collapse=','))
  }
})

output$prof_plot_download = downloadHandler(
  filename = function(){
    paste0('profile_',input$prof_booklet,input$prof_item,'.png')
  },
  content = function(file){
    req(input$prof_booklet,input$prof_item, input$prof_person)
    
    nvals = length(prof_item_prop_vals())
    
    req(between(length(input$prof_item_xvals),1,nvals-1))
    
    stm = "get_responses(db, 
                columns=c('person_id','item_id','item_score',input$prof_item, input$prof_person),
                predicate=booklet_id == input$prof_booklet)"
    
    dat = eval(parse(text=stm))
    
    if(nvals != 2)
    {
      prop = tibble(val = prof_item_prop_vals(),
                    g = .data$val %in% input$prof_item_xvals) |>
        group_by(.data$g) |>
        mutate(p = paste(.data$val, collapse=','))
      
      dat = inner_join(prop, dat, by=c('val'=input$prof_item))
      
      colnames(dat)[colnames(dat) == 'p'] = input$prof_item
    }
    
    png(filename=file, type='cairo-png', width=960,height=640)
    if(packageVersion("dexter") >= '1.1.5')
    {
      profile_plot(dat, item_property = input$prof_item, covariate = input$prof_person, 
                   main=input$prof_item, 
                   x=paste(input$prof_item_xvals, collapse=','), 
                   cex.legend=1.2,cex.axis=1.2,cex.lab=1.2,cex.main=1.2)
    } else
    {
      profile_plot(dat, item_property = input$prof_item, covariate = input$prof_person, 
                   main=input$prof_item, 
                   x=paste(input$prof_item_xvals, collapse=','))
    }
    dev.off()
    
  },
  contentType = "image/png"
)



# DIF
observe({
  req(values$person_properties, input$main_navbar == 'DIF_pane')
  
  if(ncol(values$person_properties)>1)
  {
    persons = select(values$person_properties, -"person_id")
    
    pprop = tibble(name=colnames(persons), n = sapply(persons, n_distinct)) |>
      filter(.data$n==2)
    
    updateSelectInput(session, 'DIF_person', choices = pprop$name, selected = pprop$name[1])
  }
})


DIF_object = reactive({
  req(db,input$DIF_person)
  DIF(db, person_property=input$DIF_person)
})

output$DIF_plot = renderPlot({
  req(input$DIF_item, DIF_object())
  items=NULL
  if(input$DIF_item != 'item_id')
  {
    items = get_items(db) |>
      arrange(.data[[input$DIF_item]]) |>
      pull(.data$item_id)
  }
  plot(DIF_object(), items=items, cex.axis=1)
})


output$DIF_text = renderPrint({
  DIF_object()
})

output$DIF_plot_download = downloadHandler(
  filename = function(){
    paste0('DIF',input$DIF_person,'.png')
  },
  content = function(file){
    req(DIF_object()) 
    
    items=NULL
    if(input$DIF_item != 'item_id')
    {
      items = get_items(db) |>
        arrange(.data[[input$DIF_item]]) |>
        pull(.data$item_id)
    }
    
    png(filename=file, type='cairo-png', width=960,height=640)
    plot(DIF_object(), items=items,cex.axis=1)
    dev.off()
    
  },
  contentType = "image/png"
)
