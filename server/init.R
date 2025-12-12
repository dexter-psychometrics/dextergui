
init_project = function()
{
  show('project_load_icon')
  hide('prj_alter_rules')
  
  # reset all input and some semi-static output elements
  reset('doc-body')
  booklets = dbGetQuery(db, 'SELECT booklet_id FROM dxBooklets ORDER BY booklet_id;')$booklet_id
  updateSelectInput(session, 'add_booklet_name', choices = c('type or choose booklet_id' = '', booklets))
  runjs("Shiny.onInputChange('example_datasets',null);")
  
  covariates = setdiff(dbListFields(db, 'dxpersons'),'person_id')
  updateSelectInput(session,'prs_abl_plot_variable', choices = covariates)
  updateSelectInput(session,'prs_abl_plot_fill', choices = covariates)
  
  output$data_import_result = renderUI({})
  output$data_import_result_long = renderUI({})
  updateSlider(session, 'enorm_slider',list())

  # reset reactiveValues
  for(nm in names(default_reactive)){ values[[nm]] = default_reactive[[nm]] }
  
  rules = get_rules(db)
  persons = get_persons(db) |> mutate_if(is_integer_, as.integer)
  values$rules = rules
  items = as_tibble(get_items(db))
  values$item_properties = items[,!colnames(items) %in% c('item_screenshot','item_html','item_href'),drop=FALSE]
  values$person_properties = persons
  
  interaction_models$clear()

  if(length(booklets) > 0)
  {

    data = get_resp_data(db,summarised=FALSE,retain_person_id=FALSE)

    ## prepare interaction
    for(bkl in booklets)
    {
      env = new.env()
      env$bkl = eval(bkl)
      interaction_models$assign(bkl, {fit_inter(resp_data_bkl(data, bkl))}, env=env)
    }

    ## prepare CTT ##
    
    tia = tia_tables(data, type='raw',max_scores='theoretical',omit_item_novar = FALSE)
    
    # don't need to make distinct by person since nbr of items is equal within booklet
    sparks = data$x |>
      group_by(.data$booklet_id) |>
      summarise(test_score = sparkbox_vals(.data$booklet_score)) |>
      ungroup() |>
      mutate(booklet_id = as.character(.data$booklet_id))
    
    tia$booklets = tia$booklets |>
      inner_join(sparks, by='booklet_id')
    
    tia$items = inner_join(tia$items, mutate(data$design, across(where(is.factor), as.character)), by=c('booklet_id','item_id')) |>
      relocate('item_position', .after='booklet_id')
    
    if(all(grepl('^\\d+$',tia$booklets$booklet_id)))
    {
      tia$tbooklets = arrange(tia$booklets, as.integer(.data$booklet_id))
      tia$items = arrange(tia$items, .data$item_id, as.integer(.data$booklet_id))
    } else
    {
      tia$items = arrange(tia$items, .data$item_id, .data$booklet_id)
    }
    
    values$ctt_items = tia$items
    values$ctt_booklets = tia$booklets
  }
  
  set_js_vars(db, session)
  
  
  lapply(c('project_load_icon','oplm_inputs','example_datasets'), hide)
  
  show('proj_rules_frm')
  if.else(NROW(rules) > 0, show, hide)('proj_items_frm')
  if.else(NROW(persons) > 0, show, hide)('proj_persons_frm')
  
  if.else(NROW(rules) > 0, enable_panes, disable_panes)('data_pane')
  if.else(NROW(persons) > 0, enable_panes, disable_panes)(c('ctt_pane', 'inter_pane','enorm_pane','DIF_pane'))
  
  if(any(dbListFields(db,'dxItems') %in% c('item_screenshot','item_html','item_href')))
  {
    show('item-viewer-btn')
  } else
  {
    hide(selector = '#item-viewer-img, #item-viewer-btn')
  }
  
  if(length(setdiff(dbListFields(db,'dxItems'), c('item_screenshot','item_html','item_href'))) > 1)
  {
    show('ctt_itemprop-btn')
  } else
  {
    hide(selector = '#ctt_itemprop, #ctt_itemprop-btn')
  }
  
  # hide all the subtabs under enorm, they take away attention from the buttons that the user will need to press
  hide(selector="#enorm_tabs + div.tab-content > div.tab-pane > *:not(.well)")
  #hide(selector = '#enorm_tabs + div')        
  updateImgSelect(session, inputId = "abp_plotbar",choices=list())
}


## init for the first time if a db is specified, otherwise disable all other panes
if(is.null(db))
{
  lapply(c('project_load_icon','proj_items_frm','proj_persons_frm','proj_rules_frm'), hide)
  disable_panes(c( 'ctt_pane', 'inter_pane','data_pane', 'enorm_pane', 'DIF_pane'))
} else
{
  init_project()
}
output$project_pth = renderText({values$project_name})
hide('oplm_inputs')
hide('example_datasets')

plottypes = tibble(plot = c("hist", "box", "ecdf", "dens", "pointrange", "box", "line", "scat"), 
  type = c("nominal", "nominal", "nominal", "nominal", "nominal", "nominal", "ordinal", "continuous"),
  aim = c("dist", "dist", "dist", "dist", "comp", "comp", "rel", "rel"),
  message = c(rep("grouping", 7), "covariate"))
