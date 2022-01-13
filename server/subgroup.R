# to do: style inputs
#style pluts (legend size, etc.)
#to do: case where no props
observe({
  req(input$prof_booklet, values$person_properties)

  items = get_items(db) %>%
    inner_join(get_design(db), by='item_id') %>%
    filter(booklet_id == input$prof_booklet) %>%
    select(-.data$item_id)
  
  iprop = tibble(name=colnames(items), n = sapply(items, n_distinct)) %>%
    filter(between(.data$n, 2, nrow(items)/2))
  
  updateSelectInput(session, 'prof_item', choices = iprop$name)
  
  if(ncol(values$person_properties)>1)
  {
    persons = values$person_properties %>%
      semi_join(dbGetQuery(db,
                           'SELECT person_id FROM dxadministrations WHERE booklet_id=:booklet;', 
                           list(booklet=input$prof_booklet)),
                by='person_id') %>%
      select(-.data$person_id)
    
    pprop = tibble(name=colnames(persons), n = sapply(persons, n_distinct)) %>%
      filter(between(.data$n, 2, 3))
    
    updateSelectInput(session, 'prof_person', choices = pprop$name)
    
  }
})

observe({
  req(input$prof_booklet, input$prof_item)
  
  prop = get_items(db) %>%
    inner_join(get_design(db), by='item_id') %>%
    filter(booklet_id == input$prof_booklet) %>%
    pull(.data[[input$prof_item]]) %>%
    unique()
  
  updateSelectInput(session, 'prof_item_xvals', choices = prop, selected=prop[1:(round(length(prop)/2))])
})


# this needs a little more checking on 2 unique values
output$prof_plot = renderPlot({
  req(input$prof_booklet,input$prof_item, input$prof_person)
  stm = "get_responses(db, 
                columns=c('person_id','item_id','item_score',input$prof_item, input$prof_person),
                predicate=booklet_id == input$prof_booklet)"
  
  dat = eval(parse(text=stm))
  if(length(input$prof_item_xvals)>1)
  {
    prop = get_items(db) %>%
      inner_join(get_design(db), by='item_id') %>%
      filter(booklet_id == input$prof_booklet) %>%
      distinct(.data[[input$prof_item]]) %>%
      mutate(x = .data[[input$prof_item]] %in% input$prof_item_xvals) %>%
      group_by(x) %>%
      mutate(p = paste(.data[[input$prof_item]], collapse=','))
    
    dat = inner_join(dat, prop, by=input$prof_item) %>%
      select(-.data[[input$prof_item]])
    
    colnames(dat)[colnames(dat) == 'p'] = input$prof_item
  }
  
  
  profile_plot(dat, item_property = input$prof_item, covariate = input$prof_person, 
               main=paste(input$prof_booklet, input$prof_item, sep='-'))
})


# DIF
observe({
  req(values$person_properties)
  
  if(ncol(values$person_properties)>1)
  {
    persons = select(values$person_properties, -.data$person_id)
    
    pprop = tibble(name=colnames(persons), n = sapply(persons, n_distinct)) %>%
      filter(.data$n==2)
    
    updateSelectInput(session, 'DIF_person', choices = pprop$name)
  }
})

output$DIF_plot = renderPlot({
  req(input$DIF_person)
  d = DIF(db, person_property=input$DIF_person)
  plot(d)
})