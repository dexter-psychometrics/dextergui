


get_palettes = function(category=c('qual','div','seq','all'), max_colors = 100)
{
  ctg = match.arg(category)
  if(category == 'all')
  {
    pal = brewer.pal.info %>% mutate_if(is.factor, as.character) %>% add_column(name = rownames(brewer.pal.info))
  } else
  {
    pal = brewer.pal.info %>% mutate_if(is.factor, as.character) %>% add_column(name = rownames(brewer.pal.info))  %>% 
      filter(.data$category == ctg)
  }

  pal = split(pal, pal$name)
  
  lapply(pal, function(p){
    paste(p$name,paste(brewer.pal(min(p$maxcolors, max_colors), p$name), collapse=' '), sep=';')
  } )
}