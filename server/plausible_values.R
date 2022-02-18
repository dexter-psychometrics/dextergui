
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
    }) %>% 
    bind_rows(.id='name')  %>% 
    mutate(fun_indx = case_when(.data$n==1 ~ -2, .data$name=='booklet_score' ~ -1, .data$name=='booklet_id' ~ 0,TRUE ~ 1))

  list(
    all = vi,
    nominal = filter(vi, .data$n <= 40 & .data$name != 'booklet_score')  %>% arrange(desc(.data$fun_indx), .data$n),
    ordinal = filter(vi,  .data$n > 1 & .data$type %in% c('integer','double')) %>% arrange(desc(.data$fun_indx), .data$n),
    continuous = filter(vi, .data$n > 5 & .data$type %in% c('integer','double'))  %>% arrange(desc(.data$fun_indx), desc(.data$n)),
    weights = filter(vi, .data$n > 1 & .data$type %in% c('integer','double') & .data$min_ >= 0 & .data$name != 'booklet_score')
  )
})


plottypes <- tibble(plot = c("hist", "box", "ecdf", "dens", "bar", "box", "line", "scat"), 
                    type = c("nominal", "nominal", "nominal", "nominal", "nominal", "nominal", "ordinal", "continuous"),
                    aim = c("dist", "dist", "dist", "dist", "comp", "comp", "comp", "rel"),
                    message = c(rep("grouping", 7), "covariate"))

observeEvent(values$plausible_values,
             {
               var_info = pvp_varinfo()
               req(values$plausible_values, var_info)
               
           
               firstnominal <- var_info$nominal %>% slice(1)
               firstordinal <- var_info$ordinal %>% slice(1)
               firstcontinuous <- var_info$continuous %>% slice(1)
               
               
               if(nrow(firstordinal) == 0) plottypes <- filter(plottypes, .data$type != "ordinal")
               if(nrow(firstcontinuous) == 0) plottypes <- filter(plottypes, .data$type != "continuous")
               
               updateSelectInput(session, inputId = "pvp_xvar",
                                 choices = filter(var_info$all, .data$type %in% c('integer','double'))$name,
                                 selected = firstcontinuous$name)
               
               
               choices <- lapply(unique(plottypes$plot), function(id)
               {
                 outfile <- tempfile(fileext = '.png')
                 if (id == "hist"){
                   p <- ggplot(values$plausible_values, aes_string("PV1", group = firstnominal$name, fill = firstnominal$name)) +
                     geom_histogram(alpha = 0.5,na.rm=TRUE, bins=30) + 
                     theme(legend.position = "none") + 
                     theme_nothing()} 
                 else if (id == "box") {
                   p <- ggplot(values$plausible_values, aes_string(x = firstnominal$name, y = "PV1", colour = firstnominal$name)) +
                     geom_boxplot(na.rm=TRUE) +
                     theme(legend.position = "none") + 
                     theme_nothing()} 
                 else if (id == "ecdf") {
                   p <- ggplot(values$plausible_values, aes_string("PV1", colour = firstnominal$name)) +
                     stat_ecdf(na.rm=TRUE) + 
                     theme_nothing()} 
                 else if (id == "dens") {
                   p <- ggplot(values$plausible_values, aes_string("PV1")) +
                     geom_density(aes_string(group = firstnominal$name, colour = firstnominal$name),na.rm=TRUE) + 
                     theme_nothing()} 
                 else if (id == "bar") {
                   p <- ggplot(values$plausible_values, aes_string(firstnominal$name, "PV1", fill = firstnominal$name)) +
                     stat_summary(geom='bar', fun = "mean",na.rm=TRUE) +
                     theme(legend.position = "none") + 
                     theme_nothing()} 
                 else if (id == "line") {
                   p <- ggplot(values$plausible_values, aes_string(firstordinal$name, "PV1", fill = firstnominal$name, colour = firstnominal$name)) +
                     stat_summary(geom='line', fun = "mean", na.rm=TRUE) + 
                     theme_nothing()} 
                 else if (id == "scat") {
                   p <- ggplot(values$plausible_values, aes_string(firstcontinuous$name, "PV1", colour = firstnominal$name)) + 
                     geom_point(na.rm=TRUE) + 
                     theme_nothing()}
                 
                 ggsave(outfile, p, width = 1, height = 1)
                 
                 list(src = outfile,
                      contentType = 'image/png',
                      choice_id = id,
                      group = ifelse(id %in% c("hist", "box", "ecdf", "dens"), 'distr', 
                                     ifelse(id %in% c("bar", "line"), 'comp', 'rel')))
               })
               
               
               group_options <- list(distr = list(label = 'Distribution'),
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
                 nominal_var <- var_info$nominal
                 ordinal_var <- var_info$ordinal
                 continuous_var <- var_info$continuous
                 weight_var = var_info$weights
                 
                 firstnominal <-  nominal_var %>% slice(1)
                 firstordinal <-  ordinal_var %>% slice(1)
                 firstcontinuous <-  continuous_var %>% slice(1)
                 
                 if(nrow(firstordinal) == 0) plottypes <- filter(plottypes, .data$type != "ordinal")
                 if(nrow(firstcontinuous) == 0) plottypes <- filter(plottypes, .data$type != "continuous")
                 
                 
                 currentgroup <- input$pvp_group
                 if (currentgroup %in% pull(nominal_var, 'name')) {
                   barboxgroup <- input$pvp_group
                 } else {barboxgroup <- firstnominal$name}
                 
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

  if(input$pvp_weight == 'none'){pvpweights = NULL} 
  else{pvpweights = input$pvp_weight}
  
  switch(input$pvp_plotbar$value,
         
         # HISTOGRAM
         # https://stackoverflow.com/questions/30355938/histogram-with-weights-in-r
         # https://stackoverflow.com/questions/20342494/density-of-each-group-of-weighted-geom-density-sum-to-one
         # https://stackoverflow.com/questions/17368223/ggplot2-multi-group-histogram-with-in-group-proportions-rather-than-frequency
         
         hist = {
           
           if (input$pvp_group == "none"){
             p <- ggplot(values$plausible_values, aes_string("PV1", weights = pvpweights)) + 
               geom_histogram(fill = input$pvp_color, alpha = input$pvp_trans, bins = input$pvp_bins,na.rm=TRUE)
           } else if (input$pvp_group != "none" && input$pvp_stackfacet != "joy") {
             p <- ggplot(values$plausible_values %>% mutate(!!input$pvp_group := as.factor(.data[[input$pvp_group]])), 
                         aes_string("PV1", fill = input$pvp_group, weights = pvpweights)) + 
               geom_histogram(alpha = input$pvp_trans, bins = input$pvp_bins,na.rm=TRUE)
             
             if (input$pvp_stackfacet == 'facetted') {
               p <- p + 
                 facet_grid(reformulate(input$pvp_group, "."))
             }
             
           } else if (input$pvp_group != "none" && input$pvp_stackfacet == "joy") {
             p <- ggplot(values$plausible_values %>% mutate(!!input$pvp_group := as.factor(.data[[input$pvp_group]])), 
                         aes_string(x = "PV1", 
                                    y = input$pvp_group, 
                                    group = input$pvp_group, 
                                    fill = input$pvp_group, weights = pvpweights)) +
               geom_density_ridges2(stat = "binline", bins = input$pvp_bins,
                                    show.legend = FALSE, alpha = input$pvp_trans,
                                    na.rm=TRUE)
           }
           
           p <- p + 
             theme(legend.position = "none") +
             theme_minimal()
           
         },
         
         # BOX PLOT
         box = {
           if(input$pvp_group == 'none')
           {
             p = ggplot(values$plausible_values, aes_string(y = "PV1")) +
               geom_boxplot(alpha = input$pvp_trans, show.legend = FALSE, na.rm=TRUE) +
               theme_minimal()
           } else
           {
             p <- ggplot(values$plausible_values %>% mutate(!!input$pvp_group := as.factor(.data[[input$pvp_group]])), 
                         aes_string(x = input$pvp_group, y = "PV1", 
                                    colour = input$pvp_group)) +
               geom_boxplot(alpha = input$pvp_trans, show.legend = FALSE, na.rm=TRUE) +
               theme_minimal()
             
             if (input$pvp_fill == TRUE){
               p <- p + aes_string(fill = input$pvp_group)
             }
           }
         },
         
         # ECDF
         ecdf = {
          
           # https://stackoverflow.com/questions/32487457/r-ggplot-weighted-cdf

           if (input$pvp_group != "none" && input$pvp_weight != "none"){

             data_weighted <- values$plausible_values[order(values$plausible_values$PV1),]
             data_weighted <- data_weighted[which(!(is.na(data_weighted[,input$pvp_weight]))),]
             data_weighted$cum.pct <- cumsum(data_weighted[,input$pvp_weight]) / sum(data_weighted[,input$pvp_weight])

             p <- ggplot(data_weighted %>% mutate(!!input$pvp_group := as.factor(.data[[input$pvp_group]])),
                         aes_string("PV1", "cum.pct", color = input$pvp_group)) +
               geom_line()
           }

           if (input$pvp_group == "none" && input$pvp_weight != "none"){

             data_weighted <- values$plausible_values[order(values$plausible_values$PV1),]
             data_weighted <- data_weighted[which(!(is.na(data_weighted[,input$pvp_weight]))),]
             data_weighted$cum.pct <- cumsum(data_weighted[,input$pvp_weight]) / sum(data_weighted[,input$pvp_weight])

             p <- ggplot(data_weighted, aes_string("PV1", "cum.pct")) +
               geom_line(color = input$pvp_color)
           }

           if (input$pvp_group != "none" && input$pvp_weight == 'none'){
             p <- ggplot(values$plausible_values %>% mutate(!!input$pvp_group := as.factor(.data[[input$pvp_group]])), 
                         aes_string("PV1", color = input$pvp_group)) +
               stat_ecdf(na.rm=TRUE)
           } else if (input$pvp_group == "none" && input$pvp_weight == 'none'){
             p <- ggplot(values$plausible_values, aes_string("PV1")) +
               stat_ecdf(color = input$pvp_color, na.rm=TRUE)
           }
           
           p <- p + 
             theme_minimal()
           
         },
         
         # DENSITY PLOT
         dens = {
           
           if(input$pvp_group == "none")
           {
             p <- ggplot(values$plausible_values, aes_string("PV1", weights = pvpweights))
           } else
           {
             p <- ggplot(values$plausible_values %>% mutate(!!input$pvp_group := as.factor(.data[[input$pvp_group]])),
                         aes_string("PV1", weights = pvpweights))
           }
           
           
           if (input$pvp_group != "none" && input$pvp_stackfacet != "joy") {
             p <- p + geom_density(aes_string(group = input$pvp_group, 
                                              colour = input$pvp_group, weights = pvpweights),
                                   alpha = input$pvp_trans,na.rm=TRUE)
             
             if (input$pvp_stackfacet == 'facetted') {
               p <- p + 
                 facet_grid(reformulate(input$pvp_group, "."))
             }
             
             if (input$pvp_fill == TRUE) {
               p <- p + aes_string(fill = input$pvp_group, weights = pvpweights)
             }
             
           } else if (input$pvp_group != "none" && input$pvp_stackfacet == "joy") {
             
             p <- ggplot(filter(values$plausible_values, is.finite(.data$PV1)), aes_string(x = "PV1", 
                                                                                       y = input$pvp_group, 
                                                                                       group = input$pvp_group, 
                                                                                       weights = pvpweights)) +
               geom_density_ridges2(show.legend = FALSE, alpha = input$pvp_trans,na.rm=TRUE)
             
             if (input$pvp_fill == TRUE) {
               p <- p + aes_string(fill = input$pvp_group, weights = pvpweights)
             }
             
           } else if (input$pvp_group == "none" && input$pvp_fill == TRUE) {
             p <- p + geom_density(color = input$pvp_color, 
                                   fill = input$pvp_color,
                                   alpha = input$pvp_trans,na.rm=TRUE)
           } else if (input$pvp_group == "none" && input$pvp_fill == FALSE) {
             p <- p + geom_density(color = input$pvp_color,
                                   alpha = input$pvp_trans,na.rm=TRUE)
           }
           
           p <- p + theme_minimal()
           
         },
         
         # error bars
         # dodge
         
         # BAR CHART
         bar = {
           
           updateCheckboxInput(session, "pvp_fill", value = TRUE)
           
           p <- ggplot(values$plausible_values, aes_string(input$pvp_group, "PV1")) +
             stat_summary(geom='bar', fun = "mean", 
                      show.legend = FALSE,
                      alpha = input$pvp_trans,
                      na.rm=TRUE) +
             aes_string(fill = input$pvp_group) +
             theme_minimal()
           
         },
         
         # LINE CHART
         line = {
           
           if (input$pvp_group == "none"){
             hide(id = "pvp_linetype")
           } else {show(id = "pvp_linetype")}
           
           p <- ggplot(if.else(input$pvp_group == 'none',
                               values$plausible_values,
                               values$plausible_values %>% mutate(!!input$pvp_group := as.factor(.data[[input$pvp_group]]))),
                       aes_string(input$pvp_xvar, "PV1")) +
             theme_minimal()
           
           if (input$pvp_group == "none"){
             p <- p + stat_summary(geom='line', fun = "mean", colour = input$pvp_color, na.rm=TRUE)
           } else if (input$pvp_group != "none"){
             p <- p + stat_summary(geom='line', fun = "mean", na.rm=TRUE) +
               aes_string(fill = input$pvp_group, colour = input$pvp_group)
             
             if (input$pvp_linetype == TRUE) {
               p <- p + aes_string(linetype = input$pvp_group)
             }
           }
           
           
         },
         
         # marg       marginal plots
         
         # SCATTERPLOT
         scat = {
           
           p <- ggplot(if.else(input$pvp_group == 'none',
                               values$plausible_values,
                               values$plausible_values %>% mutate(!!input$pvp_group := as.factor(.data[[input$pvp_group]]))),
                       aes_string(input$pvp_xvar, "PV1")) + 
             theme_minimal()
           
           if (input$pvp_group == "none"){
             p <- p + 
               geom_point(color = input$pvp_color,na.rm=TRUE)
             
             # if (input$pvp_marg == TRUE){
             #   ggExtra::ggMarginal(p, type = "density", margins = "both", size = 4, marginCol = "red")
             # }
             
           } else if (input$pvp_group != "none"){
             p <- p + 
               geom_point(na.rm=TRUE) +
               aes_string(colour = input$pvp_group)
           }
           
           if (input$pvp_fitlines == TRUE){
             p <- p + geom_smooth() # method = lm ?
           }
           
         }
  )
  
  if (input$pvp_xlab != "") {p <- p + xlab(input$pvp_xlab)}
  if (input$pvp_ylab != "") {p <- p + ylab(input$pvp_ylab)}
  # titel experimentje
  if (input$pvp_main != "") {p <- p + ggtitle(rstr_eval(input$pvp_main,values$plausible_values)) +
    theme(plot.title = element_text(size = 20,
                                    hjust = 0.5))}
  if (input$pvp_grid == FALSE){
    p <- p + theme(panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank())
  }
  
  p
  
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




