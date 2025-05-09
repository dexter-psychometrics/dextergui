

observeEvent(values$person_properties, {
  req(values$person_properties)

  if(ncol(values$person_properties)>1)
  {
    choices = setdiff(colnames(values$person_properties),'person_id')
  } else
  {
    choices = c('choose_covariates'='none')
  }

  updateSelectInput(session, 'plausible_values_covariates', choices = choices)
})


observeEvent(input$go_plausible_values, {

  withBusyIndicatorServer("go_plausible_values",{
    
    if(is.null(values$parms)) 
      go_fit_enorm()

    covariates = input$plausible_values_covariates

    # predicates zijn een beetje tricky als text string

    if(!(is.null(input$plausible_values_predicate) || trimws(input$plausible_values_predicate) == ''))
    {
      pv = eval(parse(text=paste0("plausible_values(db, parms=values$parms, nPV = input$plausible_values_nPV,covariates=covariates,",
                                    "predicate={",input$plausible_values_predicate,"})")))
    } else
    {
      pv = plausible_values(db, parms=values$parms, nPV = input$plausible_values_nPV, covariates=covariates)
    }
    persons = values$person_properties[,!colnames(values$person_properties) %in% covariates]
    if(ncol(persons)>1){
      pv = inner_join(pv,persons,by='person_id')}
    values$plausible_values = pv
    
    show(selector='#enorm_tabs + div.tab-content > div.tab-pane[data-value="plausible_values"] > *')
  })
})

..include_macro_file("generate_ab_plot.R", "plausible_values")
