
#' Start dextergui
#'
#' Opens a shiny application providing a graphical user interface to dexter. 
#' 
#' @param dbpath path to a dexter project database
#' or NULL, in which case you can select a project after starting dextergui
#' @param wd where dextergui first looks for and saves project files, defaults to current working directory. 
#' @param roots volumes or paths on your hard drive available for opening and saving project files. Set NULL  
#' to use all accessible volumes
#' 
#' @details
#' The best results are achieved when the gui is opened in a browser (Chrome, Brave, FireFox). Somewhat
#' less aesthetically pleasing results are achieved in Internet Explorer. The Edge browser is not supported at this time.
#' 
#' The RStudio browser does not currently support downloads of plots and tables. Starting the gui
#' in your default browser automatically can be achieved in several ways. One way, shown below, 
#' is to set the \code{shiny.launch.browser} option to \code{TRUE}.
#' 
#' @examples
#' \dontrun{
#' # best results are achieved in a browser
#' options(shiny.launch.browser = TRUE)
#' 
#' # start the program
#' dextergui()
#' 
#' }
#' 
#' 
#' 
dextergui = function(dbpath = NULL, wd = getwd(), roots = NULL)
{
 
  restricted = NULL
  # accessing restricted volumes breaks the app on windows
  if(Sys.info()["sysname"] == 'Windows')
  {
    try({
      restricted = tibble(name = trimws(system("wmic logicaldisk get Caption", intern = TRUE)),
                          size = trimws(system("wmic logicaldisk get Size", intern = TRUE))) |>
        filter(!grepl('^\\d+$',.data$size,perl=TRUE) & !(.data$name %in% c('Caption',''))) |>
        pull(.data$name)
      
      if(length(restricted)==0)
        restricted=NULL
    }, silent=TRUE)
  } 
  

  if(is.null(roots))
  {
    roots = function()
    {
      v = getVolumes()()
      if(!is.null(restricted))
      {
        v = v[!apply(sapply(restricted, startsWith, x=v), 1, any)]
      }
      v
    } 
    vol = roots()
  } else
  {
    if(!is.null(restricted))
    {
      roots = roots[!apply(sapply(restricted, startsWith, x=roots), 1, any)]
    }
    vol = roots
    if(is.null(names(roots)))
      stop('roots must be a named vector')
  }
  
  if(!is.character(wd) || length(wd)!=1)
    stop('wd must be a string')
  
  wd = trimws(wd)
  if(!endsWith(wd,':'))
    wd = normalizePath(wd)
  
  wd = unlist(strsplit(wd,':',fixed=TRUE))
  if(length(wd) == 2)
  {
    default_root = names(vol)[startsWith(tolower(vol), tolower(paste0(wd[1],':')))]
    if(length(default_root) != 1)
    {
      default_root = default_path = ""
    } else
    {
      default_path = wd[2]
    }
  } else
  {
    default_root = NULL
    default_path = ""
  }
  
  if(!is.null(dbpath) && !file.exists(dbpath))
    stop(paste0("file '", dbpath, "' not found"))

  # nicer plots, 100mb upload limit, no text prog bars
  backup_opts = options(shiny.usecairo = TRUE, shiny.maxRequestSize = 100*1024^2, dexter.progress=FALSE)
  on.exit({options(backup_opts)})


  server = function(input, output, session)
  {
    # some server globals
    db = NULL
    
    #apparently have to repeat this
    options(shiny.usecairo = TRUE, shiny.maxRequestSize = 100*1024^2, dexter.progress=FALSE)
    
    if(!is.null(dbpath)) 
      db = open_project(dbpath)
    
    cache = lru_cache(50)
    
    # defaults are always reset in init_project
    default_reactive = list(rules = NULL, new_rules = NULL, ctt_items=NULL, ctt_booklets=NULL,
                            inter_booklet = NULL, inter_plot_items = NULL, item_properties=NULL,
                            import_data=NULL, import_data_long=NULL, import_design_long=NULL, parms=NULL, person_abl = NULL, selected_ctt_item = NULL,
                            person_properties=NULL, new_person_properties = NULL, abl_tables=NULL,
                            abl_varinfo=NULL, oplm_preview=NULL, plausible_values=NULL,
                            ctt_items_settings = list(keep_search = FALSE), 
                            update_person_properties=TRUE, update_item_properties=TRUE, update_enorm_plots=FALSE,
                            distr_legend=NULL,
                            project_name='No project loaded')
    
    # these are kept between re-init, switch to databases etc.
    start_reactive = list()
    
    values = do.call(reactiveValues, modifyList(default_reactive, start_reactive, keep.null=TRUE))

    interaction_models = delayed_list$new()

    
    shinyFileChoose(input, 'open_proj_fn', filetypes=c('db','sqlite'),
                    roots = roots,
                    defaultPath = default_path,
                    defaultRoot = default_root)
    
    shinyFileSave(input, 'new_proj_fn', filetypes=c('db','sqlite'), 
                  roots = roots,
                  defaultPath = default_path,
                  defaultRoot = default_root)
    
    shinyFileSave(input, 'start_new_project_from_oplm_dbname', filetypes=c('db','sqlite'), 
                  roots = roots,
                  defaultPath = default_path,
                  defaultRoot = default_root)

    
# RE-INIT, run init_project() at the start and  whenever a (significant) db change occurs ------------------------
    
..include_file('init.R')

values$ctt_items_settings = list(keep_search = TRUE)
      
# autofill for predicate
observeEvent(input$varsuggest, 
{
  req(db)
  session$sendCustomMessage(type = 'predicate_suggestion', 
      message = list(variable = input$varsuggest$variable, 
                     start = input$varsuggest$start,
                     suggestions = dxvar_suggestion(db, input$varsuggest$variable, input$varsuggest$start)))
})
    
# clean up
session$onSessionEnded(function(x)
{ 
  if(!is.null(db)) close_project(db) 
  if (!interactive()) 
  {
      stopApp()
      q("no")
  }
})

observeEvent(input$quit_application,{
  req(input$quit_application)
  stopApp()
  if (!interactive()) 
    q("no")
})



# project page ------------------------------------------------------------

..include_file('projects.R')

# Respons data import -------------------------------------------------------

..include_file('respons_data.R')

# Classical test theory ---------------------------------------------------

..include_file('ctt.R')

# ENORM -------------------------------------------------------------------

..include_file('fit_enorm.R')
..include_file('ability.R')
..include_file('plausible_values.R')


# DIF etc. ----------------------------------------------------------------

..include_file('subgroup.R')

# for suggested packages
  observeEvent(input$do_install_package,{
    req(input$install_package_name)
    removeModal()
    install.packages(input$install_package_name)
    
  })

}
  
  shinyApp(get_ui(), server)
}
