

sparkbox_vals = function(x){
  b = boxplot(x, plot=FALSE)
  
  outl = b$out[b$out < b$stats[3,1]]
  outh = b$out[b$out > b$stats[3,1]]
  outl = ifelse(length(outl) > 0, min(outl), 'null')
  outh = ifelse(length(outh) > 0, max(outh), 'null')
  
  paste0(c(outl ,b$stats[,1], outh), collapse=',')
}

sparkhist_vals = function(x, nbar = 10, .min = min(x), .max = max(x))
{
  h = hist(x, breaks = seq(.min, .max, l = nbar + 1), plot = FALSE)
  paste0(h$counts, collapse = ',')
}


init_sparks = function(.box = list(), .hist = list(), add_js='')
{
  .box = modifyList(
    list(type = 'box', raw = TRUE, lineColor = 'black', whiskerColor = 'black',
         outlierFillColor = 'black', outlierLineColor = 'black', medianColor = 'black',
         boxFillColor = 'orange', boxLineColor = 'black',
         chartRangeMin = 0, chartRangeMax = 100),
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
           
