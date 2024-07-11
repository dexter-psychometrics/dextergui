
observe({
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

    covariates = none2null(input$plausible_values_covariates)
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

pvp_varinfo = reactive({
  req(values$plausible_values)

  vi = lapply(
    select(values$plausible_values, -.data$person_id, -grep("PV", names(values$plausible_values))), 
    function(col)
    {
       tibble(type = typeof(col), n = n_distinct(col), min_ = if.else(is.numeric(col), min(col), -9999))
    }) |> 
    bind_rows(.id='name')  |> 
    mutate(fun_indx = case_when(.data$n==1 ~ -2, .data$name=='booklet_score' ~ -1, .data$name=='booklet_id' ~ 0,TRUE ~ 1))

  list(
    all = vi,
    nominal = filter(vi, .data$n <= 40 & .data$name != 'booklet_score')  |> arrange(desc(.data$fun_indx), .data$n),
    ordinal = filter(vi,  .data$n > 1 & .data$type %in% c('integer','double')) |> arrange(desc(.data$fun_indx), .data$n),
    continuous = filter(vi, .data$n > 5 & .data$type %in% c('integer','double'))  |> arrange(desc(.data$fun_indx), desc(.data$n)),
    weights = filter(vi, .data$n > 1 & .data$type %in% c('integer','double') & .data$min_ >= 0 & .data$name != 'booklet_score')
  )
})


plottypes = tibble(plot = c("hist", "box", "ecdf", "dens", "bar", "box", "line", "scat"), 
                    type = c("nominal", "nominal", "nominal", "nominal", "nominal", "nominal", "ordinal", "continuous"),
                    aim = c("dist", "dist", "dist", "dist", "comp", "comp", "comp", "rel"),
                    message = c(rep("grouping", 7), "covariate"))

observeEvent(values$plausible_values, {
  var_info = pvp_varinfo()
  req(values$plausible_values, var_info)
  
  
  firstnominal = var_info$nominal |> slice(1)
  firstordinal = var_info$ordinal |> slice(1)
  firstcontinuous = var_info$continuous |> slice(1)
  
  
  if(nrow(firstordinal) == 0) plottypes = filter(plottypes, .data$type != "ordinal")
  if(nrow(firstcontinuous) == 0) plottypes = filter(plottypes, .data$type != "continuous")
  
  updateSelectInput(session, inputId = "pvp_xvar",
                    choices = filter(var_info$all, .data$type %in% c('integer','double'))$name,
                    selected = firstcontinuous$name)
  
  
  choices = lapply(unique(plottypes$plot), function(id)
  {
    xvar = NULL
    if(id=='line') xvar = firstordinal$name
    if(id=='scat') xvar = firstcontinuous$name
    
    outfile = tempfile(fileext = '.png')
    
    p = ability_plot(values$plausible_values, plot_type=id, group=firstnominal$name, xvar=xvar, 
                     fill=id!='dens', thumbnail=TRUE)
    
    ggsave(outfile, p, width = 1, height = 1)
    
    list(src = outfile,
         contentType = 'image/png',
         choice_id = id,
         group = ifelse(id %in% c("hist", "box", "ecdf", "dens"), 'distr', 
                        ifelse(id %in% c("bar", "line"), 'comp', 'rel')))
  })
  
  
  group_options = list(distr = list(label = 'Distribution'),
                       comp = list(label = 'Comparison'),
                       rel = list(label = 'Relationships'))
  
  choices[[2]]$group = c('distr', 'comp')
  
  updateImgSelect(session, choices = choices, inputId = "pvp_plotbar", group_options = group_options, selected = "hist")
  
})




observe(
             {
               var_info = pvp_varinfo()
               if(is.null(var_info))
               {
                 hide(selector=paste0('#pvp_group,#pvp_main,#pvp_xlab,#pvp_ylab,#pvp_grid,#pvp_bins,#pvp_fill,',
                                      '#pvp_linetype,#pvp_fitlines,#pvp_xvar,#pvp_color,#pvp_stackfacet,#pvp_trans'))
                 
               } else if(!(is.null(input$pvp_plotbar$value)))
               {
                 nominal_var = var_info$nominal
                 ordinal_var = var_info$ordinal
                 continuous_var = var_info$continuous
                 weight_var = var_info$weights
                 
                 firstnominal =  nominal_var |> slice(1)
                 firstordinal =  ordinal_var |> slice(1)
                 firstcontinuous =  continuous_var |> slice(1)
                 
                 if(nrow(firstordinal) == 0) plottypes = filter(plottypes, .data$type != "ordinal")
                 if(nrow(firstcontinuous) == 0) plottypes = filter(plottypes, .data$type != "continuous")
                 
                 
                 currentgroup = input$pvp_group
                 if (currentgroup %in% pull(nominal_var, 'name')) {
                   barboxgroup = input$pvp_group
                 } else {barboxgroup = firstnominal$name}
                 
                 if (input$pvp_plotbar$value %in% c("hist", "dens", "ecdf", "line", "scat")) {
                   updateSelectInput(session, 
                                     inputId = "pvp_group", 
                                     choices = c("none", pull(nominal_var, 'name')),
                                     selected = currentgroup)
                 } else if (input$pvp_plotbar$value %in% c("box", "bar")) {
                   updateSelectInput(session,
                                     inputId = "pvp_group",
                                     choices = pull(nominal_var, 'name'),
                                     selected = barboxgroup)
                 }
                 
                 
                 if (input$pvp_plotbar$value %in% c("hist", "dens", "ecdf")){
                   updateSelectInput(session,
                                     inputId = "pvp_weight",
                                     choices = c("none", pull(weight_var, 'name')),
                                     selected = 'none')
                 }
                 
                 if (input$pvp_plotbar$value == "scat"){
                   updateSelectInput(session,
                                     inputId = "pvp_xvar",
                                     choices = pull(continuous_var, 'name'),
                                     selected = input$pvp_xvar)
                 } else if (input$pvp_plotbar$value == "line"){
                   updateSelectInput(session,
                                     inputId = "pvp_xvar",
                                     choices = pull(ordinal_var, 'name'),
                                     selected = input$pvp_xvar)
                 }
                 
                 show(id = "pvp_group")
                 show(id = "pvp_main")
                 show(id = "pvp_xlab")
                 show(id = "pvp_ylab")
                 show(id = "pvp_grid")
                 
                 if (input$pvp_plotbar$value %in% c("hist", "dens", "ecdf") & nrow(weight_var) > 0){show(id = "pvp_weight")} else {hide(id = "pvp_weight")}
                 if (input$pvp_plotbar$value == "hist") {show(id = "pvp_bins")} else {hide(id = "pvp_bins")}
                 if (input$pvp_plotbar$value %in% c("box", "dens")) {show(id = "pvp_fill")} else hide(id = "pvp_fill")
                 # if (input$pvp_plotbar$value == "bar") {show(id = "pvp_dodge")} else {hide(id = "pvp_dodge")}
                 # if (input$pvp_plotbar$value %in% c("bar", "line")){show(id = "pvp_err")} else {hide(id = "pvp_err")}
                 # if (input$pvp_plotbar$value == "scat") {show(id = "pvp_marg")} else {hide(id = "pvp_marg")}
                 if (input$pvp_plotbar$value == "line") {show(id = "pvp_linetype")} else {hide(id = "pvp_linetype")}
                 if (input$pvp_plotbar$value == "scat") {show(id = "pvp_fitlines")} else {hide(id = "pvp_fitlines")}
                 if (input$pvp_plotbar$value %in% c("line", "scat")) {show(id = "pvp_xvar")} else {hide(id = "pvp_xvar")}
      
                 
                 
                 if (input$pvp_group %in% pull(nominal_var, 'name') &&
                     input$pvp_plotbar$value %in% c("hist", "dens")) {
                   show(id = "pvp_stackfacet")
                 } else {hide(id = "pvp_stackfacet")}
                 
                 
                 
                 if (input$pvp_plotbar$value %in% c("hist", "ecdf", "dens", "line", "scat") & input$pvp_group == "none") {show(id = "pvp_color")} 
                 else {hide(id = "pvp_color")}
                 
                 
                 
                 if (input$pvp_fill == TRUE && input$pvp_plotbar$value %in% c("hist", "box", "dens", "bar")) {
                   show(id = "pvp_trans")
                 } else { hide(id = "pvp_trans") }
                 
               }
             })

observe(
  {
    var_info = pvp_varinfo()
    # haalt gekozen group var weg uit x var indien nodig
    req(var_info, input$pvp_plotbar$value)
    
    if(input$pvp_plotbar$value %in% c('scat','line'))
    {

      ordinal_var = var_info$ordinal
      continuous_var = var_info$continuous
      
      if(input$pvp_plotbar$value == 'line')
      {
        selected = if.else(input$pvp_group == isolate(input$pvp_xvar), NULL, isolate(input$pvp_xvar))
        updateSelectInput(session,
                          inputId = "pvp_xvar",
                          choices = setdiff(pull(ordinal_var, 'name'), input$pvp_group),
                          selected = selected)
        
      } else if(input$pvp_plotbar$value == 'scat')
      {
        selected = if.else(input$pvp_group == isolate(input$pvp_xvar), NULL, isolate(input$pvp_xvar))
        updateSelectInput(session,
                          inputId = "pvp_xvar",
                          choices = setdiff(pull(continuous_var, 'name'),input$pvp_group),
                          selected = selected)
      }
      
    }
  }, priority=1)



pvplot = reactive({
  req(input$pvp_plotbar$value, values$plausible_values, 
      !((input$pvp_xvar == '' || input$pvp_xvar == input$pvp_group) && input$pvp_plotbar$value %in% c('scat','line')))

  if(input$pvp_plotbar$value=='bar') updateCheckboxInput(session, "pvp_fill", value = TRUE)
  if(input$pvp_plotbar$value=='line' && input$pvp_group != 'none') show(id = "pvp_linetype")
  if(input$pvp_plotbar$value=='line' && input$pvp_group == 'none') hide(id = "pvp_linetype")

  
  ability_plot(values$plausible_values, input$pvp_plotbar$value, color=input$pvp_color, alpha=input$pvp_trans, 
         bins=input$pvp_bins, group=input$pvp_group, stackfacet = input$pvp_stackfacet, 
         xvar=input$pvp_xvar, fitlines=input$pvp_fitlines, linetype=input$pvp_linetype, 
         title=input$pvp_main, xlab=input$pvp_xlab,ylab=input$pvp_ylab,
         grid=input$pvp_grid,fill=input$pvp_fill, weights=input$pvp_weight)
  
})

output$pvp_plot = renderPlot({pvplot()})

output$pvp_download = downloadHandler(
  filename = function(){paste0(values$project_name,'_plausiblevalues.png')},
  content = function(file) {
    
    png()
    plt = pvplot() +  theme(axis.text = element_text(size = 8),
                            axis.title = element_text(size = 8),
                            legend.text = element_text(size = 8),
                            legend.title = element_text(size = 8),
                            legend.key.size = unit(0.4,"cm"))
    
    ggsave(file, plot = plt, device = "png", units = 'cm', 
           width = input$pvp_download_width, height = input$pvp_download_height,
           dpi = 600)
  },
  contentType = "image/png"
)



