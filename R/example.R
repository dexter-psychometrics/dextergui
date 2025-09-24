

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
  
  if(file.exists(system.file('extdata/p3.rds',package='dextergui')))
  {
    out$pirls = card('dextergui',list(title='Pirls 2021',
      description=tagList("Data for paper based administrations in 3 countries for the Progress in International Reading Literacy Study (PIRLS).",
        "Source: ", tags$a("www.iea.nl/studies/iea/pirls", href="https://www.iea.nl/studies/iea/pirls", target='blank_'))),
      'pirls3')
  }
  
  out$verbAggr = card('dexter', getHelpList('verbAggrData','dexter'), 'verbAggr')
  
  if (requireNamespace("psych", quietly = TRUE))
    try({out$blot = card('psych',getHelpList('blot','psych'), 'blot')}, silent=TRUE)

  if (requireNamespace("sirt", quietly = TRUE))
  {
    try({out$bs07a = card('sirt',getHelpList('data.bs07a','sirt'), 'bs07a')}, silent=TRUE)
    try({out$math = card('sirt',getHelpList('data.math','sirt'), 'math')}, silent=TRUE)
    try({out$timss = card('sirt',getHelpList('data.timss','sirt'), 'timss')}, silent=TRUE)
  }
  
  #if (requireNamespace("MLCIRTwithin", quietly = TRUE))
  #{
  #  try({out$sf12 = card("MLCIRTwithin",getHelpList('SF12','MLCIRTwithin'), 'SF12')}, silent=TRUE)
  #  try({out$rlms = card("MLCIRTwithin",getHelpList('RLMS','MLCIRTwithin'), 'RLMS')}, silent=TRUE)
  #}
 
  tags$div(do.call(tagList, out), tags$hr(), ...)
}

example_db = function(name)
{
  db=NULL

  if(name=='pirls3')
  {
    x = readRDS(system.file('extdata/p3.rds',package='dextergui'))
    
    x$persons = mutate(x$persons,person_id=paste(.data$idcntry,.data$idschool,.data$idstud,sep='_')) |>
      select(-'idstud') |>
      group_by(.data$idcntry) |>
      mutate(senwgt = 500*.data$totwgt/sum(.data$totwgt)) |>
      ungroup() |>
      mutate(idschool=paste(.data$idcntry,.data$idschool,sep='_'),
             original_booklet_id = gsub(' .+$','',.data$booklet_id, perl=TRUE))
    
    x$rsp = mutate(x$rsp,person_id=paste(.data$idcntry,.data$idschool,.data$idstud,sep='_'))
    x$rsp = inner_join(x$rsp,select(x$persons, 'person_id','booklet_id'),by='person_id')
    
    db = start_new_project(x$rules,':memory:')
    # item position might be nice
    add_response_data(db, x$rsp, design=distinct(x$rsp, .data$booklet_id, .data$item_id))
    add_item_properties(db,x$items)
    add_person_properties(db,select(x$persons,-'booklet_id') |> 
        mutate(idcntry = case_match(.data$idcntry,528~'NLD',957~'BFR',926~'ENG')))
    
  } else if(name=='blot')
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
    db = start_new_project(dexter::verbAggrRules, ':memory:', person_properties=list(gender='<NA>',anger=as.integer(NA)))
    add_booklet(db, dexter::verbAggrData, 'verbal aggression')
    add_item_properties(db, dexter::verbAggrProperties)
  } else if(name=='bs07a')
  {
    ev = new_environment()
    data('data.bs07a', package='sirt', envir=ev)
    ev$data.bs07a = select(ev$data.bs07a, -"idpatt")
    rules = tibble(item_id = sort(rep(colnames(ev$data.bs07a),2)), 
                   response = rep_len(c(0,1),ncol(ev$data.bs07a)*2),
                   item_score = .data$response)
    
    ev$data.bs07a$person_id = 1:nrow(ev$data.bs07a)
    db = start_new_project(rules, ':memory:')
    add_booklet(db, ev$data.bs07a, 'Gefechtsangst')
    
  } else if(name=='math')
  {
    ev = new_environment()
    data('data.math', package='sirt', envir=ev)
    colnames(ev$data.math$data)[1] = 'person_id'
    
    rules = tibble(item_id = sort(rep(colnames(ev$data.math$data)[3:ncol(ev$data.math$data)],2)), 
                   response = rep_len(c(0,1),ncol(ev$data.math$data)*2-4),
                   item_score = .data$response)
    

    db = start_new_project(rules, ':memory:', person_properties=list(female=as.integer(NA)))
    add_booklet(db, ev$data.math$data, 'Math')
    add_item_properties(db, rename(ev$data.math$item, item_id='item'))
  } else if(name=='timss')
  {
    ev = new_environment()
    data('data.timss', package='sirt', envir=ev)
    items = ev$data.timss$item |> 
      rename(item_id='item')
    
    db = start_new_project(
      rules=tibble(item_id=rep(items$item_id,2), 
        response=rep(0:1,each=nrow(items)),
        item_score=.data$response), 
      ':memory:',
      person_properties = list(gender='<unknown>', age=NA_real_))
    
    add_booklet(db,ev$data.timss$data |> mutate(gender=if_else(.data$girl==1,'girl','boy')) |> rename(person_id='idstud'),
      booklet_id='TIMSS')
    
    add_item_properties(db, items)
    
  } 


  
  db
}