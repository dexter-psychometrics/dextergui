




plottypes <- tibble(plot = c("hist", "box", "ecdf", "dens", "bar", "box", "line", "scat"), 
                        type = c("nominal", "nominal", "nominal", "nominal", "nominal", "nominal", "ordinal", "continuous"),
                        aim = c("dist", "dist", "dist", "dist", "comp", "comp", "comp", "rel"),
                        message = c(rep("grouping", 7), "covariate"))


abl_varinfo = reactive({
  req(values$person_abl)
  
  vi = lapply(
    select(values$person_abl, -.data$person_id, -.data$theta), 
    function(col)
    {
      tibble(type = typeof(col), n = n_distinct(col))
    }) %>% 
    bind_rows(.id = 'name')  %>% 
    mutate(fun_indx = case_when(.data$n==1 ~ -2, .data$name=='booklet_score' ~ -1, .data$name=='booklet_id' ~ 0,TRUE ~ 1))
  
  list(
    all = vi,
    nominal = filter(vi, .data$n <= 40 & .data$name != 'booklet_score')  %>% arrange(desc(.data$fun_indx), .data$n),
    ordinal = filter(vi,  .data$n > 1 & .data$type %in% c('integer','double')) %>% arrange(desc(.data$fun_indx), .data$n),
    continuous = filter(vi, .data$n > 5 & .data$type %in% c('integer','double'))  %>% arrange(desc(.data$fun_indx), desc(.data$n))
  )
})



observeEvent(values$person_abl,
{
    var_info = abl_varinfo()
    req(values$person_abl, var_info)

    firstnominal <- var_info$nominal %>% slice(1)
    firstordinal <- var_info$ordinal %>% slice(1)
    firstcontinuous <- var_info$continuous %>% slice(1)
  #print(firstnominal)
    
    if(nrow(firstordinal) == 0) plottypes <- filter(plottypes, .data$type != "ordinal")
    if(nrow(firstcontinuous) == 0) plottypes <- filter(plottypes, .data$type != "continuous")
    
    updateSelectInput(session, inputId = "abp_xvar",
                      choices = filter(var_info$all, .data$type %in% c('integer','double'))$name,
                      selected = firstcontinuous$name)
   

      choices <- lapply(unique(plottypes$plot), function(id)
      {
        outfile <- tempfile(fileext = '.png')

        if (id == "hist"){
          p <- ggplot(values$person_abl, aes_string("theta", group = firstnominal$name, fill = firstnominal$name)) +
            geom_histogram(alpha = 0.5,na.rm=TRUE, bins=30) + 
            theme(legend.position = "none") + 
            theme_nothing()} 
        else if (id == "box") {
          p <- ggplot(values$person_abl, aes_string(x = firstnominal$name, y = "theta", colour = firstnominal$name)) +
            geom_boxplot(na.rm=TRUE) +
            theme(legend.position = "none") + 
            theme_nothing()} 
        else if (id == "ecdf") {
          p <- ggplot(values$person_abl, aes_string("theta", colour = firstnominal$name)) +
            stat_ecdf(na.rm=TRUE) + 
            theme_nothing()} 
        else if (id == "dens") {
          p <- ggplot(values$person_abl, aes_string("theta")) +
            geom_density(aes_string(group = firstnominal$name, colour = firstnominal$name),na.rm=TRUE) + 
            theme_nothing()} 
        else if (id == "bar") {
          p <- ggplot(values$person_abl, aes_string(firstnominal$name, "theta", fill = firstnominal$name)) +
            geom_bar(stat = "summary", fun.y = "mean",na.rm=TRUE) +
            theme(legend.position = "none") + 
            theme_nothing()} 
        else if (id == "line") {
          p <- ggplot(values$person_abl, aes_string(firstordinal$name, "theta", fill = firstnominal$name, colour = firstnominal$name)) +
            geom_line(stat = "summary", fun.y = "mean", na.rm=TRUE) + 
            theme_nothing()} 
        else if (id == "scat") {
          p <- ggplot(values$person_abl, aes_string(firstcontinuous$name, "theta", colour = firstnominal$name)) + 
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
  
  updateImgSelect(session, choices = choices, inputId = "abp_plotbar", group_options = group_options, selected = "hist")
  
})




observe(
{
  var_info = abl_varinfo()
  
  if(is.null(var_info))
  {
    hide(selector=paste0('#abp_group,#abp_main,#abp_xlab,#abp_ylab,#abp_grid,#abp_bins,#abp_fill,',
                         '#abp_linetype,#abp_fitlines,#abp_xvar,#abp_color,#abp_stackfacet,#abp_trans'))

  } else if(!(is.null(input$abp_plotbar$value)))
  {
    
    
    # sorteren?
    nominal_var <- var_info$nominal
    ordinal_var <- var_info$ordinal
    continuous_var <- var_info$continuous

    
    firstnominal <-  nominal_var %>% slice(1)
    firstordinal <-  ordinal_var %>% slice(1)
    firstcontinuous <-  continuous_var %>% slice(1)
    
    if(nrow(firstordinal) == 0) plottypes <- filter(plottypes, .data$type != "ordinal")
    if(nrow(firstcontinuous) == 0) plottypes <- filter(plottypes, .data$type != "continuous")
    
    
    currentgroup <- input$abp_group
    if (currentgroup %in% pull(nominal_var, 'name')) {
      barboxgroup <- input$abp_group
    } else {barboxgroup <- firstnominal$name}
    
    if (input$abp_plotbar$value %in% c("hist", "dens", "ecdf", "line", "scat")) {
      updateSelectInput(session, 
                        inputId = "abp_group", 
                        choices = c("none", pull(nominal_var, 'name')),
                        selected = currentgroup)
    } else if (input$abp_plotbar$value %in% c("box", "bar")) {
      updateSelectInput(session,
                        inputId = "abp_group",
                        choices = pull(nominal_var, 'name'),
                        selected = barboxgroup)
    }
    
    if (input$abp_plotbar$value == "scat"){
      updateSelectInput(session,
                        inputId = "abp_xvar",
                        choices = pull(continuous_var, 'name'),
                        selected = input$abp_xvar)
    } else if (input$abp_plotbar$value == "line"){
      updateSelectInput(session,
                        inputId = "abp_xvar",
                        choices = pull(ordinal_var, 'name'),
                        selected = input$abp_xvar)
    }
    
    show(id = "abp_group")
    show(id = "abp_main")
    show(id = "abp_xlab")
    show(id = "abp_ylab")
    show(id = "abp_grid")
    
    if (input$abp_plotbar$value == "hist") {show(id = "abp_bins")} else {hide(id = "abp_bins")}
    if (input$abp_plotbar$value %in% c("box", "dens")) {show(id = "abp_fill")} else hide(id = "abp_fill")
    # if (input$abp_plotbar$value == "bar") {show(id = "abp_dodge")} else {hide(id = "abp_dodge")}
    # if (input$abp_plotbar$value %in% c("bar", "line")){show(id = "abp_err")} else {hide(id = "abp_err")}
    # if (input$abp_plotbar$value == "scat") {show(id = "abp_marg")} else {hide(id = "abp_marg")}
    if (input$abp_plotbar$value == "line") {show(id = "abp_linetype")} else {hide(id = "abp_linetype")}
    if (input$abp_plotbar$value == "scat") {show(id = "abp_fitlines")} else {hide(id = "abp_fitlines")}
    if (input$abp_plotbar$value %in% c("line", "scat")) {show(id = "abp_xvar")} else {hide(id = "abp_xvar")}
    

    if (input$abp_group %in% pull(nominal_var, 'name') &&
          input$abp_plotbar$value %in% c("hist", "dens")) {
      show(id = "abp_stackfacet")
    } else {hide(id = "abp_stackfacet")}

    

    if (input$abp_plotbar$value %in% c("hist", "ecdf", "dens", "line", "scat") & input$abp_group == "none") {show(id = "abp_color")} 
    else {hide(id = "abp_color")}

    

    if (input$abp_fill == TRUE && input$abp_plotbar$value %in% c("hist", "box", "dens", "bar")) {
      show(id = "abp_trans")
    } else { hide(id = "abp_trans") }

  }
})

observe(
  {
    var_info = abl_varinfo()
    # haalt gekozen group var weg uit x var indien nodig
    req(var_info, input$abp_plotbar$value)

    if(input$abp_plotbar$value %in% c('scat','line'))
    {

      ordinal_var = var_info$ordinal
      continuous_var = var_info$continuous

      if(input$abp_plotbar$value == 'line')
      {
        selected = if.else(input$abp_group == isolate(input$abp_xvar), NULL, isolate(input$abp_xvar))
        updateSelectInput(session,
                          inputId = "abp_xvar",
                          choices = setdiff(pull(ordinal_var, 'name'), input$abp_group),
                          selected = selected)

      } else if(input$abp_plotbar$value == 'scat')
      {
        selected = if.else(input$abp_group == isolate(input$abp_xvar), NULL, isolate(input$abp_xvar))
        updateSelectInput(session,
                          inputId = "abp_xvar",
                          choices = setdiff(pull(continuous_var, 'name'),input$abp_group),
                          selected = selected)
      }

    }
  }, priority=1)



abplot = reactive({
  req(input$abp_plotbar$value, values$person_abl, 
      !((input$abp_xvar == '' || input$abp_xvar == input$abp_group) && input$abp_plotbar$value %in% c('scat','line')))


    switch(input$abp_plotbar$value,
        
           # HISTOGRAM
           hist = {
             
             if (input$abp_group == "none"){
               p <- ggplot(values$person_abl, aes_string("theta")) + 
                 geom_histogram(fill = input$abp_color, alpha = input$abp_trans, bins = input$abp_bins,na.rm=TRUE)
             } else if (input$abp_group != "none" && input$abp_stackfacet != "joy") {
               p <- ggplot(values$person_abl %>% mutate(!!input$abp_group := as.factor(.data[[input$abp_group]])), 
                           aes_string("theta", fill = input$abp_group)) + 
                 geom_histogram(alpha = input$abp_trans, bins = input$abp_bins,na.rm=TRUE)
               
               if (input$abp_stackfacet == 'facetted') {
                 p <- p + 
                   facet_grid(reformulate(input$abp_group, "."))
               }
               
             } else if (input$abp_group != "none" && input$abp_stackfacet == "joy") {
               p <- ggplot(values$person_abl %>% mutate(!!input$abp_group := as.factor(.data[[input$abp_group]])), 
                           aes_string(x = "theta", 
                                      y = input$abp_group, 
                                      group = input$abp_group, 
                                      fill = input$abp_group)) +
                 geom_density_ridges2(stat = "binline", bins = input$abp_bins,
                                      show.legend = FALSE, alpha = input$abp_trans,
                                      na.rm=TRUE)
             }
             
             p <- p + 
               theme(legend.position = "none") +
               theme_minimal()
             
           },
           
           # BOX PLOT
           box = {
             
             p <- ggplot(values$person_abl %>% mutate(!!input$abp_group := as.factor(.data[[input$abp_group]])), 
                         aes_string(x = input$abp_group, y = "theta", 
                                              colour = input$abp_group)) +
               geom_boxplot(alpha = input$abp_trans, show.legend = FALSE, na.rm=TRUE) +
               theme_minimal()
             
             if (input$abp_fill == TRUE){
               p <- p + aes_string(fill = input$abp_group)
             }
             
           },
           
           # ECDF
           ecdf = {
             
             if (input$abp_group != "none"){
               p <- ggplot(values$person_abl %>% mutate(!!input$abp_group := as.factor(.data[[input$abp_group]])), 
                           aes_string("theta", color = input$abp_group)) +
                 stat_ecdf(na.rm=TRUE)
             } else if (input$abp_group == "none"){
               p <- ggplot(values$person_abl, aes_string("theta")) +
                 stat_ecdf(color = input$abp_color,na.rm=TRUE)
             }
             
             p <- p + 
               theme_minimal()
             
           },
           
           # DENSITY PLOT
           dens = {
             
             if(input$abp_group == "none")
             {
                p <- ggplot(values$person_abl, aes_string("theta"))
             } else
             {
               p <- ggplot(values$person_abl %>% mutate(!!input$abp_group := as.factor(.data[[input$abp_group]])),
                           aes_string("theta"))
             }
             
             
             if (input$abp_group != "none" && input$abp_stackfacet != "joy") {
               p <- p + geom_density(aes_string(group = input$abp_group, 
                                                colour = input$abp_group),
                                     alpha = input$abp_trans,na.rm=TRUE)
               
               if (input$abp_stackfacet == 'facetted') {
                 p <- p + 
                   facet_grid(reformulate(input$abp_group, "."))
               }
               
               if (input$abp_fill == TRUE) {
                 p <- p + aes_string(fill = input$abp_group)
               }
               
             } else if (input$abp_group != "none" && input$abp_stackfacet == "joy") {

               p <- ggplot(filter(values$person_abl, is.finite(.data$theta)), aes_string(x = "theta", 
                                                y = input$abp_group, 
                                                group = input$abp_group)) +
                 geom_density_ridges2(show.legend = FALSE, alpha = input$abp_trans,na.rm=TRUE)
               
               if (input$abp_fill == TRUE) {
                 p <- p + aes_string(fill = input$abp_group)
               }
               
             } else if (input$abp_group == "none" && input$abp_fill == TRUE) {
               p <- p + geom_density(color = input$abp_color, 
                                     fill = input$abp_color,
                                     alpha = input$abp_trans,na.rm=TRUE)
             } else if (input$abp_group == "none" && input$abp_fill == FALSE) {
               p <- p + geom_density(color = input$abp_color,
                                     alpha = input$abp_trans,na.rm=TRUE)
             }
             
             p <- p + theme_minimal()
             
           },
           
           # error bars
           # dodge
           
           # BAR CHART
           bar = {
             
             updateCheckboxInput(session, "abp_fill", value = TRUE)
             
             p <- ggplot(values$person_abl, aes_string(input$abp_group, "theta")) +
               geom_bar(stat = "summary", fun.y = "mean", 
                        show.legend = FALSE,
                        alpha = input$abp_trans,
                        na.rm=TRUE) +
               aes_string(fill = input$abp_group) +
               theme_minimal()
             
           },
           
           # LINE CHART
           line = {
             
             if (input$abp_group == "none"){
               hide(id = "abp_linetype")
             } else {show(id = "abp_linetype")}
             
             p <- ggplot(if.else(input$abp_group == 'none',
                                 values$person_abl,
                                 values$person_abl %>% mutate(!!input$abp_group := as.factor(.data[[input$abp_group]]))),
                                 aes_string(input$abp_xvar, "theta")) +
               theme_minimal()
             
             if (input$abp_group == "none"){
               p <- p + geom_line(stat = "summary", fun.y = "mean", colour = input$abp_color, na.rm=TRUE)
             } else if (input$abp_group != "none"){
               p <- p + geom_line(stat = "summary", fun.y = "mean", na.rm=TRUE) +
                 aes_string(fill = input$abp_group, colour = input$abp_group)
               
               if (input$abp_linetype == TRUE) {
                 p <- p + aes_string(linetype = input$abp_group)
               }
             }
             
             
           },
           
           # marg       marginal plots
           
           # SCATTERPLOT
           scat = {
             
             p <- ggplot(if.else(input$abp_group == 'none',
                                 values$person_abl,
                                 values$person_abl %>% mutate(!!input$abp_group := as.factor(.data[[input$abp_group]]))),
                         aes_string(input$abp_xvar, "theta")) + 
               theme_minimal()
             
             if (input$abp_group == "none"){
               p <- p + 
                 geom_point(color = input$abp_color,na.rm=TRUE)
               
               # if (input$abp_marg == TRUE){
               #   ggExtra::ggMarginal(p, type = "density", margins = "both", size = 4, marginCol = "red")
               # }
               
             } else if (input$abp_group != "none"){
               p <- p + 
                 geom_point(na.rm=TRUE) +
                 aes_string(colour = input$abp_group)
             }
             
             if (input$abp_fitlines == TRUE){
               p <- p + geom_smooth() # method = lm ?
             }
             
           }
    )
    
    if (input$abp_xlab != "") {p <- p + xlab(input$abp_xlab)}
    if (input$abp_ylab != "") {p <- p + ylab(input$abp_ylab)}
  # titel experimentje
    if (input$abp_main != "") {p <- p + ggtitle(rstr_eval(input$abp_main,values$person_abl)) +
      theme(plot.title = element_text(size = 20,
                                      hjust = 0.5))}
    if (input$abp_grid == FALSE){
      p <- p + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank())
    }

    p
    
})

output$abp_plot = renderPlot({abplot()})

output$abp_download = downloadHandler(
  filename = function(){paste0(values$project_name,'_ability.png')},
  content = function(file) {
    
    png()
    plt = abplot() +  theme(axis.text = element_text(size = 8),
                            axis.title = element_text(size = 8),
                            legend.text = element_text(size = 8),
                            legend.title = element_text(size = 8),
                            legend.key.size = unit(0.4,"cm"))

    ggsave(file, plot = plt, device = "png", units = 'cm', 
           width = input$abp_download_width, height = input$abp_download_height,
           dpi = 600)
  },
  contentType = "image/png"
)



