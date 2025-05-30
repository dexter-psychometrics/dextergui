library(devtools)
#library(dextergui)
library(rmarkdown)
library(dplyr)
library(shiny)
library(stringr)


generate_server = function()
{
  ..include_macro_file = function(fn, spec)
  {
    lines = trimws(readLines(file.path('server',fn)))
    include_if = which(str_detect(lines,'^\\s*#\\|\\s*include_if'))
    
    if(length(include_if)>0)
    {
      end_if = which(str_detect(lines,'^\\s*#\\|\\s*end_if'))
      stopifnot(length(include_if) == length(end_if) && all(end_if>include_if))
      for(i in rev(seq_along(include_if)))
      {
        if(str_detect(lines[include_if[i]], fixed(sprintf('(%s)',spec))))
        {
          lines = lines[-c(include_if[i],end_if[i])]
        }
        else
        {
          lines = lines[-(include_if[i]:end_if[i])]
        }
      }
    }
    
    macro = str_replace(lines[startsWith(lines,'#|')],'^#\\|','') |>
      str_split(':')
    
    lines = lines[!startsWith(lines,'#|') & !lines=='']
    
    repl = tibble(value=trimws(sapply(macro,'[[',1)), replacement=sapply(macro,\(x) eval(parse(text=x[[2]]))[spec] ))
    
    for(i in 1:nrow(repl))
      lines = str_replace_all(lines,fixed(repl$value[i]), repl$replacement[i])
    
    paste0(lines, collapse='\n')
  }
  
  ..include_file = function(fn)
  {
    paste0(trimws(readLines(file.path('server',fn))), collapse='\n')
  }
  
  skeleton = paste0(trimws(readLines('server/skeleton.R')),collapse='\n')
  
  while(str_detect(skeleton,regex('^\\.\\.include', multiline=TRUE)))
  {
    skeleton = str_replace_all(skeleton, regex('^\\.\\.include[^\\)]+\\)', multiline=TRUE), \(x) eval(parse(text=x)))
  }
  

  # comments and whitespace unnecessary
  txt = paste0('\n## autogenerated ##\n\n',
               gsub("(?m)^\\s*#[^'].+$", '', skeleton, perl=TRUE) %>%
                 gsub('\n\\s+','\n',.,perl=TRUE) %>%
                  gsub('#[^"\'\n]+\n','\n',.,perl=TRUE) %>%
                    gsub('\n\\s*\\}','}',.,perl=TRUE) %>%
                      gsub('\n\\s*\\{','{',.,perl=TRUE))
  
  write(txt,file = 'R/serve.R')
  invisible(txt)
}


build_ = function()
{
  generate_server()
  devtools::build()
  devtools::reload()
}

load_ = function()
{
  generate_server()
  devtools::load_all()
}

#options(shiny.fullstacktrace = TRUE)
run_ = function(..., display.mode=c("normal","showcase"))
{
  # unfrotunately showcase does not work
  load_()
  do.call(dextergui, list(...)) |> runApp(launch.browser=TRUE, display.mode=match.arg(display.mode))
}

manual = function()
{
  rmarkdown::render(input='vignettes/dextergui.Rmd', run_pandoc=TRUE, output_file='dextergui.html')
  con = file("vignettes/dextergui.html", "r", blocking = FALSE)
  htm = paste0(readLines(con), collapse='\n')
  close(con)
  start = regexpr('<body>', htm, fixed=TRUE) + nchar('<body>')
  end = regexpr('</body>', htm, fixed=TRUE) - 1
  htm = substr(htm,start,end)
  htm = gsub('<style>.+</style>','',htm,perl=TRUE)
  cat(htm, file = 'inst/extdata/manual.html', append=FALSE)
  file.remove("vignettes/dextergui.html")
}




make_inno = function(bld = FALSE, app_dir='inno_app')
{
  if(bld)
    build_()
  
  if(!dir.exists(app_dir))
    dir.create(app_dir)
  
  if(!dir.exists(paste0(app_dir,'/bin')))
    dir.create(paste0(app_dir,'/bin'))
  

  ui = 'ui = dextergui:::get_ui()\n'
  
  server = paste("wd = Sys.getenv('HOME')",
                 'app = dextergui::dextergui(wd=wd)',
                 'server = app$serverFuncSource()',
                 sep='\n')

  
  write(server, file = paste0(app_dir,'/server.R'))
  write(ui, file = paste0(app_dir,'/ui.R'))
  
  # force rewrite
  if(dir.exists(paste0(app_dir,'/utils')))
    unlink(paste0(app_dir,'/utils'), recursive=TRUE)

  options(url.method='curl')
  RInno::create_app('dexter',
    app_dir      = app_dir,
    dir_out      = 'wizard',
    pkgs         = c('dextergui','BH'),
    user_browser = 'chrome',
    include_R    = TRUE,
    privilege = 'admin',
    default_dir  = 'pf')
  
  msg = paste(
    'Thanks for choosing dexter.\n\n Dexter is based on the R-packages dexter and dextergui',
    'All these are free and open source software. See: https://CRAN.R-project.org/package=dextergui for a list of contributors.')
  
  write(msg,file=paste0(app_dir,'/infobefore.txt'))
  write(msg,file=paste0(app_dir,'/infoafter.txt'))

  iss = readLines(paste0(app_dir,'/dexter.iss'))
  iss = iss[!grepl('library/BH',iss,perl=TRUE)]
  write(iss,file=paste0(app_dir,'/dexter.iss'))
  
  #file.copy('./server/BH_1.69.0-1.zip', paste0(app_dir,'/bin/BH_1.69.0-1.zip'),overwrite=TRUE)
 
  pman = readLines(paste0(app_dir,'/utils/package_manager.R'))
  pman[grepl('^applibpath', pman, perl=T)] = 
    'applibpath <- file.path(Sys.getenv("APPDATA"), "dexter", "library")'
  
  pman[grepl('^\\s*dir\\.create\\(applibpath', pman, perl=T)] = "dir.create(applibpath, recursive=TRUE)"
  
  write(paste0(pman,collapse='\n'), file=paste0(app_dir,'/utils/package_manager.R'))
  
  runjs = readLines(paste0(app_dir,'/utils/wsf/js/run.js'))

  s = min(which(grepl("sLogPath = 'log';", runjs, fixed=T)))
  e = s + min(which(grepl("^\\}", runjs[s:length(runjs)], perl=T))) - 1
  
  runjs[s:e] = ""
  runjs[s] = paste( 'var appDataPath = oShell.ExpandEnvironmentStrings("%APPDATA%") + "\\\\" + oConfig.appname;',
                    'var sLogPath = appDataPath + "\\\\log";',
                    'if (!oFSO.FolderExists(appDataPath)) {',
                    'oFSO.CreateFolder(appDataPath);',
                    '}',
                    'if (!oFSO.FolderExists(sLogPath)) {',
                    '	oFSO.CreateFolder(sLogPath);',
                    '}', 
                    sep='\n')

  write(paste0(runjs,collapse='\n'), file=paste0(app_dir,'/utils/wsf/js/run.js'))
  
  #RInno::compile_iss()
}

