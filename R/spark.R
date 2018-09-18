

sparkbox_vals = function(x){
  b = boxplot(x, plot=FALSE)
  
  outl = b$out[b$out < b$stats[3,1]]
  outh = b$out[b$out > b$stats[3,1]]
  outl = ifelse(length(outl) > 0, min(outl), 'null')
  outh = ifelse(length(outh) > 0, max(outh), 'null')
  
  paste0(c(outl ,b$stats[,1], outh), collapse=',')
}

sparkhist_vals = function(x, nbar = 10, .min = min(x,na.rm=TRUE), .max = max(x,na.rm=TRUE), as.what = c('html','data'))
{
  as.what=match.arg(as.what)

  h = hist(x, breaks = seq(.min, .max, l = nbar + 1), plot = FALSE)
  if(as.what=='html')
  {
    paste0(h$counts, collapse = ',')
  } else
  {
    list(values=paste0('[',paste0(h$counts,collapse=','),']'),
         labels=paste0('[',paste0(round(h$mids,1),collapse=','),']'),
         labels_extended = paste0('[',
                              paste0('"',round(h$breaks[1:(length(h$counts)-1)],1),':', 
                                  round(lead(h$breaks)[1:(length(h$counts)-1)],1),'"',
                                  collapse=','),']'))
  }
}


init_sparks = function(.box = list(), .hist = list(), add_js='')
{
  .box = modifyList(
    list(type = 'box', raw = TRUE, lineColor = 'black', whiskerColor = 'black',
         outlierFillColor = 'black', outlierLineColor = 'black', medianColor = 'black',
         boxFillColor = 'orange', boxLineColor = 'black',
         chartRangeMin = 0, chartRangeMax = 100,
         tooltipFormatFieldlist= c('lq', 'med', 'uq'), 
         tooltipFormatFieldlistKey= 'field'),
    .box)
  
  .hist = modifyList(
    list(type = 'bar', barColor = '#bfb5b6', barSpacing = 0, zeroAxis = FALSE, barWidth = 5),
    .hist)
  
  
  JS(paste0("function (settings, json) {",
            "var api = new $.fn.dataTable.Api(settings);",
            "var c = $(api.table().container());",
            "c.find('.sparkcount:not(:has(canvas))').sparkcount();",
            "c.find('.sparklegend:not(:has(canvas))').sparklegend();",
            "c.find('.sparkbox:not(:has(canvas))').sparkline('html',", 
            toJSON(.box, auto_unbox=TRUE),
            ");",
            "c.find('.sparkhist:not(:has(canvas))').sparkline('html',",
            toJSON(.hist, auto_unbox=TRUE),
            ");",add_js,"}"))
}
           

# footer plots

footplot_html = function(col)
{

  if(is.masked.integer(col)) col = as.integer(col)
  
  if(inherits(col,'character') || (is.integer(col) && n_distinct(col)<=6))
  {
    # horizontal hist
    vt = tibble(v = trimws(col)) %>%
      group_by(.data$v) %>%
      summarise(n=n()) %>%
      ungroup() %>%
      arrange(desc(n))
    
    if(sum(vt$n[1:5],na.rm=TRUE) >= length(col)/2)
    {
      if(nrow(vt) > 6)
      {
        vt = slice(vt,1:5) %>% add_row(v='other', n=sum(vt$n[6:nrow(vt)]))
      }
      mx = sum(vt$n)
      
      return(tags$td(do.call(tags$div,
                                 apply(vt,1,function(r){
                                   tags$div(r[1], title=paste(round(100*as.integer(r[2])/mx),'%'),
                                            tags$div(style=paste0('width:',100*as.integer(r[2])/mx,'%;')))}))))  

    } 
  } else if(is.integer(col))
  {
    sprk = sparkhist_vals(col,as.what='data')
    return(tags$td(tags$div(tags$div(class='sparkhist', `data-min`=min(col),`data-max`=max(col),
                                          `data-values`=sprk$values,
                                          `data-labels`=sprk$labels,
                                          `data-labels_ext`=sprk$labels_extended)),style='vertical-align:bottom;'))

  } else if(is.numeric(col))
  {
    ds = density(col,na.rm=TRUE,n=54)
    return(tags$td(tags$div(tags$div(class='sparkdensity', `data-min`=ds$x[1],`data-max`=ds$x[54],
                                          `data-values`=paste0('[',paste0(ds$y,collapse=','),']'))),
                        style='vertical-align:bottom;'))
    
  } 
  
  NULL 
}
