
# expansion macro's
#| .prefix.: c(ability='abp_', plausible_values='pvp_')

#| .dat.:  c(ability='values$person_abl', plausible_values='values$plausible_values')

#| .caller.: c(ability='input$go_ability', plausible_values='input$go_plausible_values')





.prefix.varinfo = reactive({
  req(.dat.)
  
  vi = lapply(
    select(.dat., -any_of(c('se','person_id','theta')), -starts_with("PV")), 
    function(col)
    {
      tibble(type = typeof(col), n = n_distinct(col), min_ = if.else(is.numeric(col), min(col), -9999))
    }) |> 
    bind_rows(.id='name')  |> 
    mutate(fun_indx = case_when(.data$n==1 ~ -2, .data$name=='booklet_score' ~ -1, .data$name=='booklet_id' ~ 0,TRUE ~ 1))
  
  list(
    all = vi,
    
    nominal = filter(vi, .data$n > 1 & .data$name != 'booklet_score')  |> arrange(desc(.data$fun_indx), .data$n),
    ordinal = filter(vi,  .data$n > 1 & .data$type %in% c('integer','double')) |> arrange(desc(.data$fun_indx), .data$n),
    continuous = filter(vi, .data$n > 5 & .data$type %in% c('integer','double'))  |> arrange(desc(.data$fun_indx), desc(.data$n)),
    weights = filter(vi, .data$n > 1 & .data$type %in% c('integer','double') & .data$min_ > 0 & .data$name != 'booklet_score')
  )
})




observeEvent(.dat., {
  var_info = .prefix.varinfo()
  req(.dat., var_info)
  
  dat_id = sprintf(".prefix.%i", isolate(.caller.))

  firstnominal = var_info$nominal |> slice(1)
  firstordinal = var_info$ordinal |> slice(1)
  firstcontinuous = var_info$continuous |> slice(1)

  
  if(nrow(firstordinal) == 0) plottypes = filter(plottypes, .data$type != "ordinal")
  if(nrow(firstcontinuous) == 0) plottypes = filter(plottypes, .data$type != "continuous")
  
  updateSelectInput(session, inputId = ".prefix.xvar",
    choices = filter(var_info$all, .data$type %in% c('integer','double'))$name,
    selected = firstcontinuous$name)
  

  updateSelectInput(session,
      inputId = ".prefix.weights",
      choices = pull(var_info$weights, 'name'))

  updateSelectInput(session,
    inputId = ".prefix.cluster",
    choices = pull(var_info$nominal, 'name'))
  
  updateSelectInput(session,
    inputId = ".prefix.stratum",
    choices = pull(var_info$nominal, 'name'))
  
  # update plotbar
  
  choices = lapply(unique(plottypes$plot), function(id)
  {
    xvar = NULL
    if(id=='line') xvar = firstordinal$name
    if(id=='scat') xvar = firstcontinuous$name
    
    outfile = tempfile(fileext = '.png')
    
    p = ability_plot(.dat., plot_type=id, group=firstnominal$name, xvar=xvar, 
      fill=(id!='dens'), thumbnail=TRUE,dat_id=dat_id,cache=cache)
    
    ggsave(outfile, p, width = 1, height = 1)
    
    list(src = outfile,
      contentType = 'image/png',
      choice_id = id,
      group = ifelse(id %in% c("hist", "box", "ecdf", "dens"), 'distr', 
        ifelse(id %in% c("pointrange", "line"), 'comp', 'rel')))
  })
  
  
  group_options = list(distr = list(label = 'Distribution'),
    comp = list(label = 'Comparison'),
    rel = list(label = 'Relationships'))
  
  choices[[2]]$group = c('distr', 'comp')
  
  updateImgSelect(session, choices = choices, inputId = ".prefix.plotbar", group_options = group_options, selected = "hist")
  
})



# colors, linetypes, stack
observeEvent(c(input$.prefix.plotbar$value,input$.prefix.group, input$.prefix.outputformat), {
  var_info = .prefix.varinfo()
  req(var_info)
  
  has_group = isTruthy(input$.prefix.group)
  is_plot = !(input$.prefix.plotbar$value == 'pointrange' && input$.prefix.outputformat == 'table')
  has_color = input$.prefix.plotbar$value != 'pointrange' && is_plot
  
  if(has_group)
  {
    N = var_info$nominal$n[var_info$nominal$name == input$.prefix.group] 

    updateSelectizeInput(session,
      inputId = ".prefix.palette",
      choices = palette_choices(N))
  } 
  toggle(".prefix.stackfacet", condition = has_group && input$.prefix.plotbar$value %in% c("hist", "dens"))
  toggle(id = ".prefix.color", condition = has_color && !has_group)
  toggle(id = ".prefix.palette", condition = has_color && has_group)
  toggle(id = ".prefix.linetype", condition = input$.prefix.plotbar$value=='line' && has_group)
})




observeEvent(input$.prefix.cluster,{
  var_info = .prefix.varinfo()
  req(var_info)
  
  updateSelectInput(session,
    inputId = ".prefix.stratum",
    choices = setdiff(pull(var_info$nominal, 'name'), input$.prefix.cluster),
    selected = setdiff(input$.prefix.stratum, input$.prefix.cluster))
})

observeEvent(input$.prefix.stratum,{
  var_info = .prefix.varinfo()
  req(var_info)
  
  updateSelectInput(session,
    inputId = ".prefix.cluster",
    choices = setdiff(pull(var_info$nominal, 'name'), input$.prefix.stratum),
    selected = setdiff(input$.prefix.cluster, input$.prefix.stratum))
  
})

observeEvent(c(input$.prefix.plotbar$value, input$.prefix.outputformat), {
  is_plot = !(input$.prefix.plotbar$value == 'pointrange' && input$.prefix.outputformat == 'table')
  toggle(selector= "#.prefix.labels,#.prefix.download-container, #.prefix.grid, #.prefix.plot", condition=is_plot)
  toggle(selector= "#.prefix.table", condition=!is_plot)
})

observeEvent(c(.prefix.varinfo(), input$.prefix.plotbar$value),
  {
    var_info = .prefix.varinfo()

    if(is.null(var_info))
    {
      # when new db is opened
      hide(selector=paste0('#.prefix.labels,#.prefix.download-container,#.prefix.group,#.prefix.grid,#.prefix.bins,#.prefix.fill,',
        '#.prefix.linetype,#.prefix.fitlines,#.prefix.xvar,#.prefix.color,#.prefix.stackfacet,#.prefix.trans,#.prefix.cluster,#.prefix.stratum,',
        '#prefix.weights,#prefix.ci'))
      
    } else if(!is.null(input$.prefix.plotbar$value)) 
    {
      nominal_var = var_info$nominal
      ordinal_var = var_info$ordinal
      continuous_var = var_info$continuous
      
      
      firstnominal =  nominal_var |> slice(1)
      firstordinal =  ordinal_var |> slice(1)
      firstcontinuous =  continuous_var |> slice(1)

      currentgroup = input$.prefix.group
      if (currentgroup %in% pull(nominal_var, 'name')) {
        barboxgroup = currentgroup
      } else {barboxgroup = firstnominal$name}
      
      if (input$.prefix.plotbar$value %in% c("hist", "dens", "ecdf", "line", "scat")) {
        updateSelectInput(session, 
          inputId = ".prefix.group", 
          choices = pull(nominal_var, 'name'),
          selected = currentgroup)
      } else if (input$.prefix.plotbar$value %in% c("box", "pointrange")) {
        updateSelectInput(session,
          inputId = ".prefix.group",
          choices = pull(nominal_var, 'name'),
          selected = barboxgroup)
      }
      
      
      if (input$.prefix.plotbar$value == "scat"){
        updateSelectInput(session,
          inputId = ".prefix.xvar",
          choices = pull(continuous_var, 'name'),
          selected = isolate(input$.prefix.xvar))
      } else if (input$.prefix.plotbar$value == "line"){
        updateSelectInput(session,
          inputId = ".prefix.xvar",
          choices = pull(ordinal_var, 'name'),
          selected = isolate(input$.prefix.xvar))
      }
      
      show('.prefix.group')
      toggle(id = ".prefix.weights", condition = input$.prefix.plotbar$value %in% c("hist", "dens", "ecdf","box","pointrange"))
      toggle(id = ".prefix.bins", condition = input$.prefix.plotbar$value == "hist")
      toggle(id = ".prefix.fill", condition = input$.prefix.plotbar$value %in% c("box", "dens"))
      toggle(id = ".prefix.linetype", condition = input$.prefix.plotbar$value == "line")
      toggle(id = ".prefix.fitlines", condition = input$.prefix.plotbar$value == "scat")
      toggle(id = ".prefix.xvar", condition = input$.prefix.plotbar$value %in% c("line", "scat"))
      toggle(selector="#.prefix.cluster,#.prefix.stratum,#.prefix.ci", condition = input$.prefix.plotbar$value == "pointrange")

      toggle(id = ".prefix.trans", condition = input$.prefix.fill == TRUE && input$.prefix.plotbar$value %in% c("hist", "box", "dens"))
      
      toggle(".prefix.outputformat", condition=input$.prefix.plotbar$value == 'pointrange')
    }
  })


observe(
  {
    var_info = .prefix.varinfo()
    # haalt gekozen group var weg uit x var indien nodig
    req(var_info, input$.prefix.plotbar$value)
    
    if(input$.prefix.plotbar$value %in% c('scat','line'))
    {
      
      ordinal_var = var_info$ordinal
      continuous_var = var_info$continuous
      
      if(input$.prefix.plotbar$value == 'line')
      {
        selected = if.else(input$.prefix.group == isolate(input$.prefix.xvar), NULL, isolate(input$.prefix.xvar))
        updateSelectInput(session,
          inputId = ".prefix.xvar",
          choices = setdiff(pull(ordinal_var, 'name'), input$.prefix.group),
          selected = selected)
        
      } else if(input$.prefix.plotbar$value == 'scat')
      {
        selected = if.else(input$.prefix.group == isolate(input$.prefix.xvar), NULL, isolate(input$.prefix.xvar))
        updateSelectInput(session,
          inputId = ".prefix.xvar",
          choices = setdiff(pull(continuous_var, 'name'),input$.prefix.group),
          selected = selected)
      }
      
    }
  }, priority=1)



.prefix.plot = reactive({
  req(input$.prefix.plotbar$value, .dat.)
  
  plot_possible = !((input$.prefix.xvar == '' || input$.prefix.xvar == input$.prefix.group) && input$.prefix.plotbar$value %in% c('scat','line'))
  plot_desired = !(input$.prefix.plotbar$value == 'pointrange' && input$.prefix.outputformat == 'table')
  req(plot_possible, plot_desired)  
  
  dat_id = sprintf(".prefix.%i", isolate(.caller.))

  ability_plot(.dat., input$.prefix.plotbar$value, color=input$.prefix.color, alpha=input$.prefix.trans, 
        bins=input$.prefix.bins, group=input$.prefix.group, stackfacet = input$.prefix.stackfacet, 
        xvar=input$.prefix.xvar, fitlines=input$.prefix.fitlines, linetype=input$.prefix.linetype, 
        title=input$.prefix.main, xlab=input$.prefix.xlab,ylab=input$.prefix.ylab,
        grid=input$.prefix.grid,fill=input$.prefix.fill, weights=input$.prefix.weights,
        cluster=input$.prefix.cluster, stratum = input$.prefix.stratum, ci=input$.prefix.ci,
      palette = input$.prefix.palette, dat_id=dat_id, cache=cache
      )
  
})

output$.prefix.table = renderTable({
  req(.dat.,input$.prefix.plotbar$value == 'pointrange', input$.prefix.outputformat == 'table')
  
  dat_id = sprintf(".prefix.%i", isolate(.caller.))
  ci = input$.prefix.ci
  
  pv_mean(dat, group=input$.prefix.group, cluster=input$.prefix.cluster, stratum=input$.prefix.stratum, 
    weights=input$.prefix.weights, dat_id=dat_id, cache=cache) |>
    mutate(ci_min = .data$estimate + .data$se*qnorm((1-ci)/2),
      ci_max = .data$estimate + .data$se*qnorm(1-(1-ci)/2))
})


output$.prefix.plot = renderPlot({.prefix.plot()})

output$.prefix.download = downloadHandler(
  filename = function(){paste0(values$project_name,'_plausiblevalues.png')},
  content = function(file) {
    
    png()
    plt = .prefix.plot() +  theme(axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.key.size = unit(0.4,"cm"))
    
    ggsave(file, plot = plt, device = "png", units = 'cm', 
      width = input$.prefix.download_width, height = input$.prefix.download_height,
      dpi = 600)
  },
  contentType = "image/png"
)




