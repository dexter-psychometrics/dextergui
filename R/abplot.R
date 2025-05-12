

list2aes = function(l)
{
  l = lapply(l, \(x) expr(.data[[x]]))
  do.call(aes, l)
}

pv_mean = function(dat, group=NULL, cluster=NULL, stratum=NULL, weights=NULL, dat_id=NULL, cache=NULL)
{
  if(!is.null(cache) && !is.null(dat_id))
  {
    prev = cache(list('pv_mean',group, cluster, stratum, weights, dat_id))
    if(!is.null(prev))
      return(prev)
  }
  
  dat = ungroup(dat)
  probs=NULL
  v2formula = function(v, default=NULL)
  {
    if(!isTruthy(v)) return(default)
    as.formula(sprintf('~%s',paste(v,collapse='+')))
  }
  
  if(isTruthy(weights))
  {
    if(all(dat[[weights]]<1))
    {
      probs = weights
      weights=NULL
    }
    
  }
  dsg = svydesign(ids=v2formula(cluster, default=as.formula('~0')), strata=v2formula(stratum), nest=TRUE, data=dat,
    probs=v2formula(probs), weights=v2formula(weights))
  
  pvnames = colnames(dat)[startsWith(colnames(dat),'PV')]
  if(length(pvnames)==0)
    pvnames='theta'
  
  if(!isTruthy(group))
  {
    out = lapply(pvnames, function(pvname){
      x = svymean(v2formula(pvname),dsg)
      tibble(estimate=as.double(x),se=sqrt(attr(x,'var')))
    }) |>
      bind_rows()
    
  } else
  {
    out = lapply(pvnames, function(pvname){
      x = svyby(v2formula(pvname), v2formula(group), dsg, svymean) 
      colnames(x)[colnames(x)==pvname] = 'estimate' 
      x
    }) |>
      bind_rows() |>
      group_by(.data[[group]])
  }
  
  if(length(pvnames)>1)
  {
    out = out |>
      summarise(b = var(.data$estimate), k=n(), estimate = mean(.data$estimate),
        w = mean(.data$se^2)) |>
      mutate(se = sqrt(.data$w + .data$b*(1+.data$k)/.data$k))
  }
  
  if(!is.null(cache) && !is.null(dat_id))
  {
    cache(list('pv_mean',group, cluster, stratum, weights, dat_id), out)
  }

  out
}


weighted_ecdf = function(dat,weights=NULL, group=NULL, npoints=400, dat_id=NULL, cache=NULL)
{
  if(!is.null(cache) && !is.null(dat_id))
  {
    prev = cache(list('ecdf',group, weights, npoints, dat_id))
    if(!is.null(prev))
      return(prev)
  }
  
  rng = range(select(dat,any_of(c('PV1','theta'))))
  grid = seq(rng[1],rng[2],length.out=npoints)
  w_ecdf = function(x, w=1)
  {
    tibble(x=x,w=w) |>
      mutate(brk = cut(x, c(-Inf,grid,Inf), labels=FALSE)) |>
      group_by(.data$brk) |>
      summarise(p = sum(.data$w)) |>
      ungroup() |>
      right_join(tibble(brk=1:npoints), by='brk') |>
      mutate(p = coalesce(.data$p,0)) |>
      arrange(.data$brk) |>
      mutate(p = cumsum(.data$p/sum(.data$p))) |>
      pull('p')
  }
  
  if(isTruthy(group))
    dat = group_by(dat, pick(all_of(group)))  
  
  npv = max(1L,sum(startsWith(colnames(dat),'PV')))
  
  out = dat |>
    do({
      m = reframe(., across(c(starts_with('PV'),starts_with('theta')),
        ~w_ecdf(.x, if(!isTruthy(weights)){1}else{.data[[weights]]}))) |>
        select(starts_with('PV'), starts_with('theta')) |>
        as.matrix()
      
      if(ncol(m)==1)
      {
        tibble(theta=grid,p=drop(m), imp_var=0, se2=.data$p*(1-.data$p)/nrow(.))
      } else
      {
        
        tibble(theta=grid,p=rowMeans(m), imp_var = pmax(0,smooth.spline(grid,apply(m,1,var),df=20)$y),
          se2=mean(apply(m,1,\(x)x*(1-x)))/nrow(.))
      }
    }) |>
    ungroup() |>
    mutate(se = sqrt(.data$se2 + .data$imp_var*(1+npv)/npv))
  
  if(!is.null(cache) && !is.null(dat_id))
  {
    cache(list('ecdf',group, weights, npoints, dat_id), out)
  }
  out
}


weighted_density = function(dat,weights=NULL, group=NULL, npoints=400, dat_id=NULL, cache=NULL)
{
  if(!is.null(cache) && !is.null(dat_id))
  {
    prev = cache(list('density',group, weights, npoints, dat_id))
    if(!is.null(prev))
      return(prev)
  }
  
  rng = range(select(dat,any_of(c('PV1','theta'))))
  w_dens = function(u, w=NULL)
  {
    # R(K) for a normal
    Rk = 1 / (2 * sqrt(pi))
    
    # Compute the kde (NR bandwidth)
    kde = density(x = u, from = rng[1], to = rng[2], n = npoints, bw = "nrd")
    
    # Selected bandwidth
    h = kde$bw
    
    if(is.null(w))
    {
      N = length(u)
    } else
    {
      N = sum(w)
      w = w/N
      kde = density(x = u, from = rng[1], to = rng[2], n = npoints, bw = h, weights=w)
    }
    
    tibble(theta=kde$x, d = kde$y, se2 = kde$y * Rk / (N * h))
  }
  
  if(isTruthy(group))
    dat = group_by(dat,pick(all_of(group)))  
  
  npv = max(1L,sum(startsWith(colnames(dat),'PV')))
  
  out = dat |>
    do({
      w = if(!isTruthy(weights)){NULL}else{.[[weights]]}
      
      dns = select(.,starts_with('PV'),starts_with('theta')) |>
        lapply(w_dens, w=w) |>
        bind_rows()
      
      if(npv==1)
      {
        mutate(dns, imp_var=0)
      } else
      {
        dns |>
          group_by(.data$theta) |>
          summarise(imp_var = var(.data$d), d=mean(.data$d),se2=mean(.data$se2))
      }
    }) |>
    ungroup() |>
    mutate(se = sqrt(.data$se2+.data$imp_var*(1+npv)/npv))
  
  if(!is.null(cache) && !is.null(dat_id))
  {
    cache(list('density',group, weights, npoints, dat_id), out)
  }
  out
}



dat2long = function(dat,group=NULL,weights=NULL)
{
  npv = max(1L,sum(startsWith(colnames(dat),'PV')))
  res = tibble(theta = as.double(as.matrix(select(dat, any_of('theta'), starts_with('PV')))))
  
  if(isTruthy(weights))
    res[[weights]] = rep(dat[[weights]],npv)
  
  if(isTruthy(group))
  {
    for(g in group)
      res[[g]] = rep(dat[[g]], npv)
  }
  res
}


weighted_hist = function(dat, group=NULL, weights=NULL, nbins=30,  dat_id=NULL, cache=NULL)
{
  if(!is.null(cache) && !is.null(dat_id))
  {
    prev = cache(list('hist',group, nbins, weights, dat_id))
    if(!is.null(prev))
      return(prev)
  }
  
  w_hist = function(x, w=1, nbins=30, rng=range(x))
  {
    binwidth = diff(rng)/(nbins-1)
    brk = seq(rng[1] - binwidth/2, rng[2] + binwidth/2, binwidth)
    if(!isTruthy(w)) w=1
    
    tibble(x=x, w=w, bin = cut(x, brk, labels=FALSE)) |>
      group_by(.data$bin) |>
      summarise(N = sum(.data$w)) |>
      right_join(tibble(mid = seq(rng[1], rng[2], binwidth), bin=1:nbins), by='bin') |>
      mutate(left=.data$mid - binwidth/2, right = .data$mid + binwidth/2, N = coalesce(.data$N,0L))
    
  }
  
  plt = dat2long(dat, group=group, weights = weights)
  
  npv = max(1L,sum(startsWith(colnames(dat),'PV')))
  rng = range(plt$theta)
  
  if(isTruthy(group)) plt = group_by(plt,pick(all_of(group)))
  
  plt = plt |>
    do({
      if(isTruthy(weights))
        w=.[[weights]]
      else 
        w=1
      
      h = w_hist(.$theta, w=w, rng=rng, nbins=nbins)
      
      res = pivot_longer(select(h,'left','right','N','bin'), all_of(c('left','right')), values_to='theta', names_to='side') |>
        arrange(.data$bin, .data$side)
      if(res$N[1]>0)
        res = bind_rows(mutate(res[1,],N=0L), res)
      if(last(res$N)>0)
        res = bind_rows(res,mutate(res[nrow(res),],N=0L))
      res
    }) |>
    ungroup() |>
    mutate(N=.data$N/npv)
  
  
  if(!is.null(cache) && !is.null(dat_id))
  {
    cache(list('hist',group, nbins, weights, dat_id), plt)
  }
  plt
}


weighted_box = function(dat, group=NULL, weights=NULL,dat_id=NULL, cache=NULL)
{
  if(!is.null(cache) && !is.null(dat_id))
  {
    prev = cache(list('boxplot',group, weights, dat_id))
    if(!is.null(prev))
      return(prev)
  }
  
  out = if(!isTruthy(weights))
  {
    dat = dat2long(dat, group=group, weights = weights)
    
    if(isTruthy(group))
      dat = group_by(dat,pick(all_of(group)))
    
    dat |>
      reframe(
        qnt = boxplot.stats(.data$theta,do.out=FALSE,do.conf=FALSE)$stats,
        p = c('mn','q1','q2','q3','mx')) |>
      pivot_wider(names_from='p', values_from='qnt')
  } else
  {
    dat = weighted_ecdf(dat,weights=weights, group=group, dat_id=dat_id, cache=cache)
    
    if(isTruthy(group))
      dat = group_by(dat,pick(all_of(group)))
    
    stat_box = function(theta,p)
    {
      bp = sapply(c(0,0.25,0.5,0.75,1), function(prob){
        i = min(which(p>=prob))
        if(i==1) return(theta[i])
        theta[i-1] + (p[i] - p[i-1]) * (theta[i]-theta[i-1])
      })
      box_length = bp[4] - bp[2]
      bp[1] = max(bp[1], bp[2] - 1.5 * box_length)
      bp[5] = min(bp[5], bp[4] + 1.5 * box_length)
      bp
    }
    
    dat |>
      reframe(
        qnt = stat_box(.data$theta,.data$p),
        p = c('mn','q1','q2','q3','mx')) |>
      pivot_wider(names_from='p', values_from='qnt')
  }

  if(!is.null(cache) && !is.null(dat_id))
  {
    cache(list('boxplot',group, weights, dat_id), out)
  }
  out
}




ability_plot = function(dat, plot_type=c("hist", "box", "ecdf", "dens", "pointrange", "line", "scat"), 
                   color='#4DAF4A', alpha=0.5, bins=30,group=NULL,cluster=NULL, stratum=NULL, ci=0.95, xvar=NULL, stackfacet = c('overlay','joy','facetted'), 
                  fitlines=NULL, linetype=FALSE, title='',xlab='',ylab='',
                  grid=TRUE,fill=TRUE, weights=NULL, thumbnail=FALSE, palette='ggplot', dat_id=NULL, cache=NULL)
{

  # debugging
  # x= list(dat=dat, plot_type=plot_type,  color=color, alpha=alpha, bins=bins,group=group,cluster=cluster, stratum=stratum, ci=ci, xvar=xvar, stackfacet =stackfacet, 
  #   fitlines=fitlines, linetype=linetype, title=title,xlab=xlab,ylab=ylab,
  #   grid=grid,fill=fill, weights=weights, thumbnail=thumbnail)
  # 
  # assign("debug", x, envir = .GlobalEnv)
  # end debugging
  
  stackfacet = match.arg(stackfacet)
  plot_type = match.arg(plot_type)

  plot_theme = theme_minimal(base_size=ifelse(thumbnail,11,14))
  has_group = isTruthy(group)
  
  
  aes_weights = if(!isTruthy(weights)){ NULL } else { aes(weight=.data[[weights]]) }

  aes_group = function(...){
    if(!has_group) return(NULL)
    nms = c(...)
    l = as.list(rep(group, length(nms)))
    names(l)=nms
    list2aes(l)
  }
  
  outvar = ifelse(any(grepl('^PV\\d+$',colnames(dat))),'PV1','theta')
  if(outvar=='theta') dat = filter(dat, is.finite(.data$theta))
  
  if(isTruthy(weights))
    dat = filter(dat, is.finite(.data[[weights]]))
  
  if(has_group) dat[[group]] = as.factor(dat[[group]])
  
  if(xlab=='' && outvar != 'theta' && !plot_type %in% c('line','scat')) xlab = 'Plausible value' 

  p = switch(plot_type,
    hist={
      plt = weighted_hist(dat, group=group, weights=weights, nbins=bins,  dat_id=dat_id, cache=cache)
      
      p = ggplot(plt, aes(x = .data$theta,y=.data$N)) +
        aes_group('fill','group') +
        plot_theme
      
      if (!has_group)
      {
        p = p + geom_polygon(fill = color, alpha = alpha)
      } else
      {
        if (stackfacet == 'joy')
        {
          incr = max(plt$N)/3
          
          plt = plt |> 
            mutate(num = dense_rank(.data[[group]]), N = .data$N+(.data$num-1)*incr)
          
          lbl = distinct(plt, .data[[group]], .data$num) |>
            mutate(N = (.data$num-1)*incr)
          
          
          p = ggplot(plt,aes(x=.data$theta,y=.data$N,group=.data[[group]],fill=.data[[group]])) +
            geom_polygon(alpha=alpha,color='black') +
            geom_text(data=lbl,aes(label=.data[[group]]), x=min(plt$theta), vjust=0,hjust=1.5,size=4.5,color='black') +
            coord_cartesian(clip = 'off') +
            plot_theme + 
            theme(axis.text.y=element_blank(), panel.grid.major.y=element_blank(), 
              panel.grid.minor.y=element_blank(), axis.title.y=element_blank(),
              plot.margin=unit(c(5.5,5.5,5.5,20),'pt'))
          
        } else
        {
          p = p + geom_polygon( alpha = alpha)
          if (stackfacet == 'facetted')
          {
            p = p + 
              facet_wrap(vars(.data[[group]]),nrow=1) + 
              theme(legend.position='none')
          }
        }
      }
      if(has_group && stackfacet != "overlay")
      {
        p=p+theme(legend.position='none')
      }
      p
    }, 
    box = {
      plt = weighted_box(dat, group=group, weights=weights,dat_id=dat_id, cache=cache)

      p = ggplot(plt, aes(x='')) +
        aes_group('x','color', if(fill){'fill'}else{NULL}) 

      if(has_group || !fill)
      {
        p = p + geom_boxplot(aes(ymin = .data$mn, lower = .data$q1, middle = .data$q2, upper = .data$q3, ymax = .data$mx),
          alpha = alpha, show.legend = FALSE,stat = "identity") 
      } else
      {
        p = p + geom_boxplot(aes(ymin = .data$mn, lower = .data$q1, middle = .data$q2, upper = .data$q3, ymax = .data$mx),
          fill=color,alpha = alpha, show.legend = FALSE,stat = "identity") 
      }
      p +  plot_theme
    },
    
    pointrange = {

      
      approx_sd = if('PV1' %in% colnames(dat)){ sd(dat$PV1) }else{ sd(dat$theta) }
      approx_mean = if('PV1' %in% colnames(dat)){ mean(dat$PV1) }else{ mean(dat$theta) } 
      
      dat = pv_mean(dat, group=group, cluster=cluster, stratum=stratum, weights=weights, dat_id=dat_id,cache=cache) |>
        mutate(ci_min = .data$estimate + .data$se*qnorm((1-ci)/2),
               ci_max = .data$estimate + .data$se*qnorm(1-(1-ci)/2))
      

      
      lim = approx_mean + c(-1,1) * approx_sd
      lim = c(min(lim[1], dat$ci_min), max(lim[2], dat$ci_max))

      if(xlab=='') xlab = 'ability (mean)'
      
      ggplot(dat,aes(x=.data$estimate, y=if(has_group){reorder(.data[[group]], .data$estimate)}else{""})) +
        geom_pointrange(aes(xmin=.data$ci_min, xmax=.data$ci_max,x=.data$estimate)) +
        geom_text(aes(label=sprintf('%.2f',.data$estimate)), nudge_y=if.else(thumbnail,-0.3,-0.15)) +
        xlim(lim) +
        labs(y='') +
        plot_theme
    },
    line = {
      # doet het nog met 1pv
      if (!has_group){
        
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
      
      if (!has_group){
        
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
      plt = weighted_density(dat,weights=weights,group=group,npoints=400,dat_id=dat_id, cache=cache)
      
      #fill gebruitk deze neit meer, moet weg in gui
      
      if(has_group && stackfacet == "joy")
      {
        plt = plt |> 
          mutate(num = dense_rank(.data[[group]]), d = .data$d+(.data$num-1)*.2)
        
        lbl = distinct(plt, .data[[group]], .data$num) |>
          mutate(d=(.data$num-1)*.2)
        
        p = ggplot(plt,aes(x=.data$theta,y=.data$d)) +
          geom_text(data=lbl,aes(label=.data[[group]]), x=min(plt$theta), vjust=0,hjust=1.5,size=4.5,color='black') +
          geom_hline(data=lbl,aes(yintercept=.data$d),color='darkgray') +
          coord_cartesian(clip = 'off')
      } else
      {
        p = ggplot(plt,aes(x=.data$theta,y=.data$d))
      }

      p = p + aes_group('group','color','fill') 

      
      if(!thumbnail && ci>0)
      {
        z = abs(qnorm((1-ci)/2))
        if(has_group)
          p = p + geom_ribbon(aes(ymin=pmax(0,.data$d-z*.data$se), ymax=pmin(1,.data$d+z*.data$se)),alpha=0.3,color='transparent') 
        else
          p = p + geom_ribbon(aes(ymin=pmax(0,.data$d-z*.data$se), ymax=pmin(1,.data$d+z*.data$se)),alpha=0.3,fill=color,color='transparent') 
      }
      
      if(has_group) p = p + geom_line()
      else p = p + geom_line(color=color)
      
      if(has_group && stackfacet == "facetted")
      {
        p = p + facet_wrap(vars(.data[[group]]),nrow=1)
      }
      
      if(has_group && stackfacet == "joy")
      {
        p=p + 
          plot_theme + 
          theme(axis.text.y=element_blank(), panel.grid.major.y=element_blank(), 
                panel.grid.minor.y=element_blank(), axis.title.y=element_blank(),
                plot.margin=unit(c(5.5,5.5,5.5,20),'pt'))
      } else
      {
        p=p + plot_theme
      }
      
      if(has_group && stackfacet != "overlay")
      {
        p=p+theme(legend.position='none')
      }
      p
    },
    ecdf = {
      
      plt = weighted_ecdf(dat,weights=weights, group=group, npoints=400, dat_id=dat_id, cache=cache)

      p = ggplot(plt,aes(x=.data$theta,y=.data$p)) +
        aes_group('group','fill','color')
      
      if(!thumbnail && ci>0)
      {
        z = abs(qnorm((1-ci)/2))
        if(has_group)
          p = p + geom_ribbon(aes(ymin = pmax(0,.data$p-z*.data$se), ymax=pmin(1,.data$p+z*.data$se)),alpha=0.3, color='transparent')
        else
          p = p + geom_ribbon(aes(ymin = pmax(0,.data$p-z*.data$se), ymax=pmin(1,.data$p+z*.data$se)),alpha=0.3, 
            color='transparent', fill=color)
      }
      if(has_group)
        p = p + geom_line()
      else
        p = p + geom_line(color=color)
      p  + plot_theme
      
    },
  )

  if(thumbnail){ return(p + theme_nothing())} 
  
  if(palette != 'ggplot' && has_group && plot_type != 'pointrange')
  {
    p = p + scale_colour_brewer(palette=palette, aesthetics=c('colour','fill'))
  }

  if(xlab != '') p = p + labs(x=xlab)
  if(ylab != '') p = p + labs(y=ylab)

  if(!grid) p = p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  if(title!='')
  {
    p = p + 
      ggtitle(rstr_eval(title,dat)) +
      theme(plot.title = element_text(size = 20))
  }
  
  p
}