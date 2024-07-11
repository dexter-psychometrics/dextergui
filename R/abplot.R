

# Eva's plotting functions, updated


ability_plot = function(dat, plot_type=c("hist", "box", "ecdf", "dens", "bar", "line", "scat"), 
                   color='#4DAF4A', alpha=0.5, bins=30,group='none', xvar=NULL, stackfacet = c('stacked','joy','facetted'), 
                  fitlines=NULL, linetype=FALSE, title='',xlab='',ylab='',
                  grid=TRUE,fill=TRUE, weights=NULL, thumbnail=FALSE)
{
  stackfacet=match.arg(stackfacet)
  plot_type = match.arg(plot_type)
  
  plot_theme = theme_minimal(base_size=ifelse(thumbnail,11,14))
  
  weights = coalesce(weights,'none')
  
  aes_weights = function(){
    if(weights == 'none') NULL
    else aes(weights=.data[[weights]])
  }
  
  outvar = ifelse(any(grepl('^PV\\d+$',colnames(dat))),'PV1','theta')
  if(outvar=='theta') dat = filter(dat, is.finite(.data$theta))
  
  if(group != 'none') dat[[group]] = as.factor(dat[[group]])

  p = switch(plot_type,
    hist={
      if (group == "none")
      {
        p = ggplot(dat, aes(x=.data[[outvar]])) + 
            aes_weights() +
            geom_histogram(fill = color, alpha = alpha, bins = bins,na.rm=TRUE) +
            plot_theme
             
      } else 
      {
        p = ggplot(dat, aes(x=.data[[outvar]], fill = .data[[group]])) + 
          aes_weights() + 
          plot_theme
        
        if(stackfacet == 'joy')
        {
          p = p + aes(y = .data[[group]], group = .data[[group]]) +
            geom_density_ridges2(stat = "binline", bins = bins,
                                 show.legend = FALSE, alpha = alpha,
                                 na.rm=TRUE)
        } else
        {
          p = p + geom_histogram(alpha = alpha, bins = bins,na.rm=TRUE) 
          if (stackfacet == 'facetted')
          {
              p = p + 
                facet_grid(reformulate(group, ".")) + 
                theme(legend.position='none')
          }

        }
      }
      p
      
    }, 
    box = {
      if(group == 'none')
      {
        p = ggplot(dat, aes(y = .data[[outvar]])) +
          geom_boxplot(alpha = alpha, show.legend = FALSE, na.rm=TRUE) +
          plot_theme
      } else
      {
        p = ggplot(dat, aes(x = .data[[group]], y = .data[[outvar]], colour = .data[[group]])) +
          geom_boxplot(alpha = alpha, show.legend = FALSE, na.rm=TRUE) +
          plot_theme
        
        if (fill == TRUE) p = p + aes(fill = .data[[group]])
        p
      }
    },
    
    bar = {
      
      
      mean_pv = mean(dat[[outvar]])
      
      ggplot(dat , aes(x=.data[[group]], y=.data[[outvar]], fill = .data[[group]])) +
        stat_summary(geom='bar', fun = "mean", 
                     show.legend = FALSE,
                     alpha = alpha,
                     na.rm=TRUE) +
        scale_y_continuous(transform=scales::trans_new("shift",transform = function(x) {x-mean_pv},inverse = function(x) {x+mean_pv})) +
        geom_hline(yintercept=mean_pv, linetype='dashed') +
        plot_theme
      
    },
    line = {
      
      if (group == "none"){
        
        p = ggplot(dat, aes(x=.data[[xvar]], y=.data[[outvar]])) +
          stat_summary(geom='line', fun = "mean", colour = color, na.rm=TRUE) +
          plot_theme
        
      } else {
        

        p = ggplot(dat, aes(x=.data[[xvar]], y=.data[[outvar]], fill = .data[[group]], colour = .data[[group]])) +
          stat_summary(geom='line', fun = "mean", na.rm=TRUE) +
          plot_theme
        
        if (linetype == TRUE) p = p + aes(linetype = .data[[group]])
      }
      p
    },
    
    scat = {
      
      if (group == "none"){
        
        p = ggplot(dat, aes(x=.data[[xvar]], y=.data[[outvar]])) +
          geom_point(color = color,na.rm=TRUE)

      } else {
        
        p = ggplot(dat, aes(x=.data[[xvar]], y=.data[[outvar]], fill = .data[[group]], colour = .data[[group]])) +
          geom_point(na.rm=TRUE)
        
        if(linetype == TRUE) p = p + aes(linetype = .data[[group]])
      }
      
      if ('fitlines' %in% fitlines)  p = p + geom_smooth(se='se' %in% fitlines) 
      p + plot_theme
    },
    dens = {
      p = ggplot(dat, aes(x=.data[[outvar]])) +
        aes_weights()
      
      if(fill & group != "none") p = p + aes(fill=.data[[group]])
      
      if (group != "none" && stackfacet != "joy") 
      {
        p = p + geom_density(aes(group = .data[[group]], colour = .data[[group]]),
                             alpha = alpha,na.rm=TRUE)
        
        if (stackfacet == 'facetted')  p = p + facet_grid(reformulate(group, "."))

      } else if (group != "none" && stackfacet == "joy") 
      {
        
        p = p + aes(y = .data[[group]], group = .data[[group]]) + 
          geom_density_ridges2(show.legend = FALSE, alpha = alpha,na.rm=TRUE)

        
      } else 
      {
        p = p + geom_density(color = input$pvp_color, 
                             fill = ifelse(fill,color,'transparent'),
                             alpha = alpha,na.rm=TRUE)
      }

      p + plot_theme
      
    },
    ecdf = {
      
      # https://stackoverflow.com/questions/32487457/r-ggplot-weighted-cdf
      
      if (weights != "none")
      {
        dat = dat |>
          filter(!is.na(.data[[weights]])) |>
          arrange(.data[[outvar]]) |>
          mutate(cum.pct = cumsum(.data[[weights]])/sum(.data[[weights]]))
        
        if(group=='none')
        {
          p = ggplot(dat, aes(x=.data[[outvar]], y=.data$cum.pct)) +
            geom_line(color = color)
        } else
        {
          p = ggplot(dat,aes(x=.data[[outvar]], y=.data$cum.pct, color = .data[[group]])) +
            geom_line()
        }
      } else
      {
        if(group=='none')
        {
          p = ggplot(dat, aes(x=.data[[outvar]])) +
            stat_ecdf(color = color, na.rm=TRUE)
        } else
        {
          p = ggplot(dat, aes(x=.data[[outvar]], color = .data[[group]])) +
            stat_ecdf(na.rm=TRUE)
        }
      }
      p + plot_theme
    },
  )
  
  if(thumbnail){ return(p + theme_nothing())} 

  if(xlab != '') p = p + labs(x=xlab)
  if(ylab != '') p = p + labs(y=ylab)

  if(!grid) p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  if(title!='')
  {
    p = p + 
      ggtitle(rstr_eval(title,dat)) +
      theme(plot.title = element_text(size = 20,
                                    hjust = 0.5))
  }
  
  p
}