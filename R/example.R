

example_datasets_ui = function(...)
{
  card = function(pck,hlp, name)
  {
    tags$div(
      tags$div(
        tags$h4(hlp$title, class='card-title'),
        tags$i('from package ',pck),
        tags$p(hlp$description,
          class='card-text'),
        class='card-body'),
      class='card',`data-dataset`= name)
  }
  
  out = list()

  out$verbAggr = card('dexter',getHelpList('verbAggrData','dexter'), 'verbAggr')
  
  if (requireNamespace("psych", quietly = TRUE))
    out$blot = card('psych',getHelpList('blot','psych'), 'blot')

  if (requireNamespace("sirt", quietly = TRUE))
  {
    out$bs07a = card('sirt',getHelpList('data.bs07a','sirt'), 'bs07a')
    out$math = card('sirt',getHelpList('data.math','sirt'), 'math')
    out$timss = card('sirt',getHelpList('data.timss07.G8.RUS','sirt'), 'timss')
  }

  if (requireNamespace("MLCIRTwithin", quietly = TRUE))
  {
    out$sf12 = card("MLCIRTwithin",getHelpList('SF12','MLCIRTwithin'), 'SF12')
    out$rlms = card("MLCIRTwithin",getHelpList('RLMS','MLCIRTwithin'), 'RLMS')
  }
  tags$div(do.call(tagList, out), tags$hr(), ...)
}

example_db = function(name)
{
  db=NULL

  if(name=='blot')
  {
    ev = new_environment()
    data('blot', package='psych', envir=ev)
    rules = tibble(item_id = sort(rep(colnames(ev$blot),2)), 
                   response = rep_len(c(0,1),ncol(ev$blot)*2),
                   item_score = .data$response)
    db = start_new_project(rules,':memory:')
    ev$blot$person_id=1:nrow(ev$blot)
    add_booklet(db, ev$blot, 'blot')
  } else if(name=='verbAggr')
  {
    db=start_new_project(dexter::verbAggrRules, ':memory:', person_properties=list(gender='<NA>',anger=as.integer(NA)))
    add_booklet(db, dexter::verbAggrData, 'verbal aggression')
    add_item_properties(db, dexter::verbAggrProperties)
  } else if(name=='bs07a')
  {
    ev = new_environment()
    data('data.bs07a', package='sirt', envir=ev)
    ev$data.bs07a = select(ev$data.bs07a, -.data$idpatt)
    rules = tibble(item_id = sort(rep(colnames(ev$data.bs07a),2)), 
                   response = rep_len(c(0,1),ncol(ev$data.bs07a)*2),
                   item_score = .data$response)
    
    ev$data.bs07a$person_id = 1:nrow(ev$data.bs07a)
    db=start_new_project(rules, ':memory:')
    add_booklet(db, ev$data.bs07a, 'Gefechtsangst')
    
  } else if(name=='math')
  {
    ev = new_environment()
    data('data.math', package='sirt', envir=ev)
    colnames(ev$data.math$data)[1] = 'person_id'
    
    rules = tibble(item_id = sort(rep(colnames(ev$data.math$data)[3:ncol(ev$data.math$data)],2)), 
                   response = rep_len(c(0,1),ncol(ev$data.math$data)*2-4),
                   item_score = .data$response)
    

    db=start_new_project(rules, ':memory:', person_properties=list(female=as.integer(NA)))
    add_booklet(db, ev$data.math$data, 'Math')
    add_item_properties(db, rename(ev$data.math$item,item_id='item'))
  } else if(name=='timss')
  {
    ev = new_environment()
    data('data.timss07.G8.RUS', package='sirt', envir=ev)
    raw = as_tibble(ev$data.timss07.G8.RUS$raw) %>%
      mutate_all(as.character) %>%
      gather(key='item_id', value='response', -.data$idstud, na.rm=TRUE) %>%
      mutate(response = if_else(is.na(.data$response), '', .data$response))
    
    rules = as_tibble(ev$data.timss07.G8.RUS$scored) %>%
      mutate(idstud=as.character(.data$idstud)) %>%
      gather(key='item_id', value='item_score', -.data$idstud, na.rm=TRUE) %>%
      inner_join(raw,by=c('idstud','item_id')) %>%
      distinct(.data$item_id, .data$response, .data$item_score)
    
    db = start_new_project(rules, ':memory:')
    
    raw = raw %>%
      rename(person_id = 'idstud') %>%
      mutate(inum = dense_rank(.data$item_id)) %>%
      arrange(.data$person_id, .data$inum) %>%
      group_by(.data$person_id) %>%
      mutate(booklet_id=paste(.data$inum,collapse=' ')) %>%
      ungroup() %>%
      mutate(booklet_id=paste('booklet', dense_rank(.data$booklet_id))) %>%
      group_by(.data$booklet_id) %>%
      filter(n_distinct(.data$person_id)>10) %>%
      ungroup()

    add_response_data(db, raw)
    add_item_properties(db, rename(ev$data.timss07.G8.RUS$iteminfo, item_id='item'))
  } else if(name=='SF12')
  {
    ev = new_environment()
    data('SF12', package='MLCIRTwithin', envir=ev)
    rules = select(ev$SF12, -.data$age) %>%
      gather(key='item_id', value='response') %>%
      mutate(item_score = if_else(is.na(.data$response),0L,as.integer(.data$response))) %>%
      distinct()
    
    db = start_new_project(rules, ':memory:', person_properties=list(age = as.double(NA)))
    
    add_booklet(db, ev$SF12 %>% mutate(age=round(.data$age,1)), 'Sf12')
    
    add_item_properties(db,tibble(
      item_id=paste0('Y',1:12),
      item_content = c('general health','limits in moderate activities','limits in climbing several flights of stairs','accomplished less than he/she would like, as a result of his/her physical health',
                       'limited in the kind of work or daily activities, as a result of his/her physical health','accomplished less than he/she would like, as a result of his/her emotional health',
                       'did work less carefully than usual, as a result of his/her emotional health','how much did pain interfere with normal work',
                       'how much of the time have he/she felt calm and peaceful','how much of the time did he/she have a lot of energy',
                       'how much of the time have he/she felt downhearted and depressed','how much of the time physical health or emotional health interfered with social activities')
    ))
    
  } else if(name=='RLMS')
  {
    ev = new_environment()
    data('RLMS', package='MLCIRTwithin', envir=ev)
    rules = select(ev$RLMS, starts_with('Y')) %>%
      gather(key='item_id', value='response') %>%
      mutate(item_score = if_else(is.na(.data$response),0L,as.integer(.data$response))) %>%
      distinct()
    
    db = start_new_project(rules, ':memory:', 
                           person_properties=list(age = as.integer(NA), education = as.integer(NA),
                                                  marital=as.integer(NA), gender=as.integer(NA),
                                                  work=as.integer(NA)))
    
    add_booklet(db, ev$RLMS, 'Rlms')
    
  } 

  
  db
}

