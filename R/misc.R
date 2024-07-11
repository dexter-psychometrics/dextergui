utils::globalVariables(c("."))

# very permissive variant of ifelse
if.else = function(a,b,c)
{
  if(isTruthy(a)) return(b)
  c
}

none2null = function(x){
  if(length(x)==1 && tolower(x)=='none') return(NULL)
  x[tolower(x)!='none']
} 

is_integer_ = function(x) is.integer(x) || (is.numeric(x) && all(x %% 1 == 0))

dropNulls = function(x) x[!vapply(x, is.null, FUN.VALUE = logical(1))]

# colors derived from http://colorbrewer2.org
qcolors = function(n)
{
   pal = c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#999999",
            "#BC80BD", "#CCEBC5","#FFED6F","#FB8072","#80B1D3","#FDB462","#8DD3C7","#FFFFB3","#BEBADA",
            "#B3DE69","#A50F15","#08306B","#00441B","#54278F","#fc4E2A","#525252","#66C2A4")

  rep_len(pal,n)
}

resp_data_bkl = function(rsp, bkl)
{
  rsp$x = filter(rsp$x, .data$booklet_id == bkl)
  rsp$design = filter(rsp$design, .data$booklet_id == bkl)
  rsp
}

resp_data_split_bkl = function(rsp)
{
  ds = split(rsp$design, rsp$design$booklet_id)
  res = lapply(split(rsp$x, rsp$x$booklet_id), 
    function(x){ 
      res = list(x=x, design=ds[[x$booklet_id[1]]], summarised = rsp$summarised)
      class(res) = class(rsp)
      res
    })
  names(res) = lapply(res, function(x) x$design$booklet_id[1])
  res
}


disable_panes = function(panes)
{
  runjs(paste0('$("', paste0("a[data-value='",panes,"']", collapse=','), 
               "\").closest('li').addClass('disabled')"))
}

enable_panes = function(panes)
{
  runjs(paste0('$("', paste0("a[data-value='",panes,"']", collapse=','), 
               "\").closest('li').removeClass('disabled')"))
}



set_js_vars = function(db, session)
{
  vr = get_variables(db) 
  vr = vr[,!colnames(vr) %in% c('item_screenshot','item_html')]
    
  session$sendCustomMessage(type = 'set_js_vars', 
                            message=list(data = list(variables = vr)))
}


delayed_list = setRefClass('delayed_list',
  fields = list(data_ = 'list'),
  methods = list(
    clear = function() data_ <<- list(),
    assign = function(name, expr, env = NULL)
    {
      if(is.null(env)) env = caller_env()
      # can do partial_eval, maybe better
      cll = list(env = env, qtexpr = eval(substitute(quote(expr))))
      class(cll) = append(class(cll), 'delay')

      data_[[name]] <<- cll
    },
    get = function(name)
    {
      if(inherits(data_[[name]], 'delay')) 
        data_[[name]] <<- eval(data_[[name]]$qtexpr, envir = data_[[name]]$env)
      return(data_[[name]])
    }
))

# returns c(nrow,ncol) based on npic to minimise whitespace in faceted plot display
# based on the assumption of slightly more available width than height
matrix_layout = function(npic){
  rw = round(sqrt(npic))
  cl = ceiling(npic/rw)
  matrix(1:(rw*cl),rw,cl,byrow=TRUE)
}



# is incidence matrix connected
im_is_connected = function(im)
{
  d = crossprod(im, im)
  diag(d) = 0

  visited = rep(FALSE, ncol(d))
  rownames(d) = c(1:nrow(d))
  colnames(d) = c(1:nrow(d))
  dfs = function(start) {
    start = as.integer(start)
    if (visited[start])
      return(0)
    visited[start] <<- TRUE
    vapply(rownames(d)[d[, start] > 0], dfs, 0)
    0
  }
  dfs(1)
  return(all(visited))
}

combined_var = function(means,vars,n)
{
  if(length(vars)<=1L)
    return(vars)
  q = (n-1)*vars + n*means^2
  (sum(q) - sum(n)* weighted.mean(means,n)^2)/(sum(n)-1)
}

ctt_items_table = function(items, averaged)
{
  if(averaged)
  {
    items = items |> 
      group_by(.data$item_id) |> 
      summarise(n_booklets = n(), 
                w_mean_score = weighted.mean(.data$mean_score, w = .data$n_persons, na.rm = TRUE), 
                sd_score = sqrt(combined_var(.data$mean_score, .data$sd_score^2, .data$n_persons)),
                max_score = max(.data$max_score), 
                pvalue = weighted.mean(.data$pvalue, w = .data$n_persons, na.rm = TRUE), 
                rit = weighted.mean(.data$rit, w = .data$n_persons, na.rm = TRUE), 
                rir = weighted.mean(.data$rir, w = .data$n_persons, na.rm = TRUE), 
                n_persons = sum(.data$n_persons)) |>
      ungroup() |>
      rename(mean_score = .data$w_mean_score)
  }
  
  items
}


dxvar_suggestion = function(db, var, .starts_with = '', .max = 10)
{
  for(tbl in c('dxItems','dxScoring_rules','dxBooklets', 'dxBooklet_design','dxPersons','dxResponses'))
  {
    if(var %in% dbListFields(db,tbl))
    {
      return(
        dbGetQuery(db,paste0('SELECT DISTINCT ', var, ' FROM ', tbl, 
                             ' WHERE CAST(', var, " AS TEXT) LIKE '", gsub("'", "", .starts_with, fixed = TRUE), "%'",
                             ' ORDER BY ', var, ' LIMIT ', .max, ';'))[,1])
    }
  }
  return('')
}

#experimental interpolation of r statements, rmarkdown like
rstr_eval = function(txt, dataset){

  m = gregexpr('`r [^`]+`',txt,perl=TRUE)
  if(length(m[[1]])>0)
  {
    env = list2env(dataset)
    m = regmatches(txt,m)
    for(s in unlist(m))
    {
      res = try(paste0(eval(parse(text=substr(s,4,nchar(s)-1)),envir=env),collapse=' '),silent=TRUE)
      if(inherits(res,'try-error'))
      {
        if(grepl('parse(text',res,fixed=TRUE))
        {
          res = '<...>'
        } else
        {
          res = trimws(gsub('^[^:]+:','',res,perl=TRUE))
        }
      }
      txt = sub(s,res,txt,fixed=TRUE)
    }
  }
  txt
}


# ggplot empty theme, no margins
theme_nothing = function() 
{
  theme(line = element_blank(), rect = element_blank(), 
        text = element_blank(), axis.ticks.length = unit(0, "cm"), 
        legend.position = "none", panel.spacing = unit(0, "lines"), 
        plot.margin = unit(c(0, 0, 0, 0), "lines"), complete = TRUE)
}

# solve invalid names for aes string
qaes_string = function(...){
  args = sapply(list(...), function(x) {paste0('`',x,'`')}, simplify=FALSE, USE.NAMES=TRUE )
  do.call(aes_string, args)
}



# guess parameters for read.csv
# based on heuristics
guess_csv_format = function(txt, delim = c('|',';',',','\t'))
{
  out = list(stringsAsFactors = FALSE, dec='.', quote = "\"'")
  # first guess quote
  # assumes doubling quote character to escape
  dbl_q = gregexpr('"', txt, fixed = TRUE) |>
    sapply(function(ps) if.else(length(ps) %% 2 != 0, -1, min(ps)))
  
  sngl_q = gregexpr("'", txt, fixed = TRUE) |>
    sapply(function(ps) if.else(length(ps) %% 2 != 0, -1, min(ps)))
  
  
  res = case_when(
    dbl_q == -1 & sngl_q == -1 ~ 'none',
    dbl_q < sngl_q | sngl_q == -1 ~ '"',
    TRUE ~ "'")
  
  # remove quoted strings as they can contain potential delimiters
  # note: unquoted strings will remain
  if(!all(res == 'none'))
  {
    res = table(res[res != 'none'])
    out$quote = names(res)[which.max(as.vector(res))]
    txt = gsub(paste0(out$quote,'[^',out$quote,']*', out$quote),'',txt,perl=TRUE)
  }

  delim = sapply(delim, function(dl) nchar(txt) - nchar( gsub(dl, "", txt,fixed=TRUE)), simplify=FALSE) |>
    sapply(unique, simplify=FALSE)
  
  delim = delim[sapply(delim, length) == 1 & sapply(delim, min) > 0]

  if(length(delim) == 1)
  {
    out$sep = names(delim)
  } else if(length(delim) == 0)
  {
    # single column so separator does not matter
    out$sep = ';'
  } else if(length(delim) == 2 && ',' %in% names(delim))
  {
    # , is probably decimal sign
    out$sep = names(delim)[names(delim) != ',']
  } else 
  {
    stop('cannot guess delimiter')
  }
  if(out$sep != ',' && any(grepl(',',txt,fixed=TRUE)) && !any(grepl('.',txt,fixed=TRUE)))
  {
    out$dec = ','
  }
  out
}

read_spreadsheet = function(fn)
{
  stopifnot(length(fn) == 1)
  
  if(grepl('\\.xlsx?$', fn, perl=TRUE, ignore.case=TRUE))
  {
    read_excel(fn, sheet = 1, col_names = TRUE)
  } else if(grepl('\\.ods$', fn, perl=TRUE, ignore.case=TRUE))
  {
    read_ods(fn, sheet = 1, col_names = TRUE)
  } else
  {
    con = file(fn, "r")
    smpl = readLines(con, n= 1000, encoding='utf8')
    close(con)
    do.call(read.csv, modifyList(guess_csv_format(smpl),list(file = fn)))
  }
}



readSCR = function (file) 
{
  z = file(file, "rb")
  n = readBin(z, integer(), size = 2, 3)
  nit = n[3]
  itemLabels = sapply(1:nit, function(x) {
    sl = readBin(z, integer(), size = 1, 1)
    rawToChar(readBin(z, raw(), n = 8)[1:sl])
  })
  globCal = readBin(z, integer(), size = 1, nit)
  discrim = readBin(z, integer(), size = 1, nit)
  maxScore = readBin(z, integer(), size = 1, nit)
  parFixed = readBin(z, integer(), size = 1, nit)
  four = readBin(z, integer(), size = 1, 4)
  sl = readBin(z, integer(), size = 1, 1)
  jobname = rawToChar(readBin(z, raw(), n = 12)[1:sl])
  five = readBin(z, integer(), size = 1, 5)
  sl = readBin(z, integer(), size = 1, 1)
  title = rawToChar(readBin(z, raw(), n = 79)[1:sl])
  for (i in 1:20) {
    sl = readBin(z, integer(), size = 1, 1)
    if (sl > 0) 
      someComment = rawToChar(readBin(z, raw(), n = sl))
  }
  sl = readBin(z, integer(), size = 1, 1)
  dataDir = rawToChar(readBin(z, raw(), n = 60)[1:sl])
  sl = readBin(z, integer(), size = 1, 1)
  dataFile = rawToChar(readBin(z, raw(), n = 12)[1:sl])
  expanded = readBin(z, integer(), size = 1, 1)
  expanded = 1 - expanded
  sl = readBin(z, integer(), size = 1, 1)
  fmt = rawToChar(readBin(z, raw(), n = sl))

  close(z)
  
  fmt = as.integer(unlist(regmatches(fmt, gregexpr('\\d+',fmt,perl=TRUE))))
  
  list(nit = nit, booklet_position = c(fmt[1], fmt[1] + fmt[2] - 1L),
       responses_start = fmt[3], response_length = fmt[5],
       expanded = expanded)
}
