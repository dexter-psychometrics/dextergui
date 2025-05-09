

# OPEN OR START PROJECT ---------------------------------------------------


observeEvent(input$open_proj_fn,
{
  open_proj_fn = parseFilePaths(roots, input$open_proj_fn)
  req(open_proj_fn$datapath)
  if(!is.null(db))
    close_project(db)
  db <<- open_project(as.character(open_proj_fn$datapath))
  values$ctt_items_settings$keep_search = FALSE
  init_project()
  values$project_name = gsub('\\.\\w+$','',basename(open_proj_fn$datapath), perl=TRUE)
  values$ctt_items_settings$keep_search = TRUE
})

observeEvent(input$new_proj_fn,
{
  new_proj_fn = parseSavePath(roots, input$new_proj_fn)
  req(new_proj_fn$datapath)
  if(!is.null(db))
    close_project(db)             
  # start a truly empty project in a way not advertised anywhere
  db <<- start_new_project(as.character(new_proj_fn$datapath),
                           rules=tibble(item_id=character(0),response=character(0),item_score=integer(0)))
  values$ctt_items_settings$keep_search = FALSE
  init_project()
  values$project_name = gsub('\\.\\w+$','',basename(new_proj_fn$datapath), perl=TRUE)
  values$ctt_items_settings$keep_search = TRUE
  updateTabsetPanel(session, 'proj_rules_tabs', selected = 'from_file')
  
})


observeEvent(input$start_new_project_from_oplm_scr_path,
{
  req(input$start_new_project_from_oplm_scr_path$datapath)
  
  # TO DO: something useful with the error
  tryCatch({scr = readSCR(input$start_new_project_from_oplm_scr_path$datapath)},
            error=function(e) stop('not a valid scr file'))
  
  if(is.na(input$start_new_project_from_oplm_responses_start))
    updateNumericInput(session, 'start_new_project_from_oplm_responses_start', value=scr$responses_start)
  
  if(is.null(input$start_new_project_from_oplm_booklet_position))
    updateRangeInput(session, 'start_new_project_from_oplm_booklet_position', value=scr$booklet_position)


})


observeEvent(input$start_new_project_from_oplm_dat_path,
{
  data_file = input$start_new_project_from_oplm_dat_path
  if(is.null(data_file))
  {
    values$oplm_preview = NULL
  } else
  {
    con = file(data_file$datapath, "r", blocking = FALSE) 
    pv = readLines(con, 10)
    close(con)
    values$oplm_preview = pv
  }
}) 



# oplm inlezen
output$oplm_dat = renderTable(
{
  
  req(values$oplm_preview)
  
  bkl = input$start_new_project_from_oplm_booklet_position
  prs = input$start_new_project_from_oplm_person_id
  rsp = input$start_new_project_from_oplm_responses_start
  
  pos = list()
  
  if(!is.na(rsp)) 
    pos$rsp = tibble(name = 'responses', begin=rsp, end=as.integer(NA))

  if(!is.null(bkl))
  {
    if(is.na(bkl[2]) || bkl[2] < bkl[1]) 
      bkl[2] = bkl[1]
    if(is.na(rsp) || !(rsp <= bkl[2] ))
      pos$bkl = tibble(name = 'booklet', begin=bkl[1], end=bkl[2])
  }
  
  if(!is.null(prs))
  {
    if(is.na(prs[2]) || prs[2] < prs[1]) 
      prs[2] = prs[1]
    if((is.null(rsp) || !(rsp <= prs[2])) && (is.null(bkl) || length(intersect(prs[1]:prs[2],bkl[1]:bkl[2])) == 0))
      pos$prs = tibble(name = 'person_id', begin=prs[1], end=prs[2])
  }
  
  if(length(pos) == 0)
  {
    out = data.frame(skip1 = values$oplm_preview)
  } else
  {
    pos = pos |> bind_rows() |> arrange(.data$begin)
    n=nrow(pos)
    l = 0
    
    for(i in 1:n)
    {
      if(l < (pos[i,]$begin - 1))
      {
        pos = add_row(pos, name=paste0('skip',i),begin = l+1, end = pos[i,]$begin -1)
      }
      l = pos[i,]$end
    }
    if(!is.na(l)) pos = add_row(pos, begin = max(pos$end)+1, name='skip.end')
    
    pos = arrange(pos, .data$begin) |> mutate_if(is.numeric, as.integer)
    
    out = list()
    
    for(i in 1:nrow(pos))
    {
      out[[pull(pos, .data$name)[i]]] = substring(values$oplm_preview, pull(pos, .data$begin)[i], coalesce(pull(pos, .data$end)[i], 10000L))
    }
    out = as.data.frame(out)
  }
  
  colnames(out) = gsub('^skip.+$','',colnames(out))
  out
}, 
  bordered=FALSE, spacing='xs', caption='.dat file preview, top 10 rows',caption.placement='top')

observeEvent(input$start_new_project_from_oplm_dbname,
{
  dbpath = parseSavePath(roots, input$start_new_project_from_oplm_dbname)
  req(dbpath)
  updateTextInput(session, 'start_new_project_from_oplm_dbname_display',
                           value = dbpath$name)
})

observeEvent(input$go_start_new_project_from_oplm,
{
  new_proj_fn = parseSavePath(roots, input$start_new_project_from_oplm_dbname)
  data_file = input$start_new_project_from_oplm_dat_path
  scr_file = input$start_new_project_from_oplm_scr_path

  withBusyIndicatorServer("go_start_new_project_from_oplm",{
    
    if(nrow(new_proj_fn) == 0)  stop('dbname is required')
    if(is.null(data_file))      stop('No .dat file selected')
    if(is.null(scr_file))       stop('No .scr file selected')
    
    if(is.null(input$start_new_project_from_oplm_booklet_position)) 
      stop('booklet_position is required')
    
    if(input$start_new_project_from_oplm_booklet_position[2] >= input$start_new_project_from_oplm_responses_start)
      stop('responses overlap with booklet_id')

    if(!is.null(db))
      close_project(db)
    
    db <<- start_new_project_from_oplm(
      dbname = as.character(new_proj_fn$datapath),
      dat_path = as.character(data_file$datapath),
      scr_path = as.character(scr_file$datapath),
      booklet_position = input$start_new_project_from_oplm_booklet_position,
      responses_start = input$start_new_project_from_oplm_responses_start,
      person_id = input$start_new_project_from_oplm_person_id,
      use_discrim = input$start_new_project_from_oplm_use_discrim,
      response_length = input$start_new_project_from_oplm_response_length)
    
    values$ctt_items_settings$keep_search = FALSE
    init_project()
    values$project_name = gsub('\\.\\w+$','',basename(new_proj_fn$datapath), perl=TRUE)
    values$ctt_items_settings$keep_search = TRUE
  })

})


observeEvent(input$example_datasets,{
  req(input$example_datasets)
  show('project_load_icon')
  if(!is.null(db))
    close_project(db)
  db <<- example_db(input$example_datasets)
  values$ctt_items_settings$keep_search = FALSE
  init_project()
  values$project_name = paste0(input$example_datasets,'_example')
  values$ctt_items_settings$keep_search = TRUE
})




# VIEW / ADD / AMEND RULES ------------------------------------------------

observeEvent(input$rules_file,{
  
  input_file = input$rules_file
  rules = read_spreadsheet(input_file$datapath)
  colnames(rules) = tolower(colnames(rules))
  if(length(setdiff(c('item_id','item_score','response'),colnames(rules))) == 0)
  {
    values$new_rules = rules |>
      mutate(item_score = as.integer(.data$item_score), item_id = as.character(.data$item_id), 
             response = gsub('\\.0+$','',as.character(.data$response), perl=TRUE)) |>
      select(.data$item_id, .data$response, .data$item_score)
    output$rules_upload_error = renderText({''})
  } else if(length(setdiff(c('item_id','noptions','key'),colnames(rules))) == 0)
  {
    values$new_rules = keys_to_rules(rules |> mutate(nOptions = as.integer(.data$noptions)))
    output$rules_upload_error = renderText({''})
  } else
  {
    output$output$rules_upload_error = renderText(
      {
        paste0('The input file has to contain columns (item_id, item_score, response) ',
               'or (item_id, nOptions, key)')
      })
    values$new_rules = NULL
  }

})

output$new_rules_preview = renderTable({
  req(values$new_rules)

  tibble(column = c('item_id','response','item_score'), 
           values = paste0(sapply(values$new_rules[1:10, c('item_id','response','item_score')], paste, collapse = ', '),', ...'))
}, caption = 'file preview')


observeEvent(input$go_import_new_rules,{
  withBusyIndicatorServer("go_import_new_rules",
  {
    if(is.null(values$new_rules))
      stop('No file selected')
    touch_rules(db, values$new_rules)
    reset('rules_file')
    init_project()
    updateTabsetPanel(session, 'proj_rules_tabs', selected = 'view')
  })

})


output$rules = renderDataTable(
{
  req(values$rules)
  values$rules |> 
     mutate(old_item_score = .data$item_score) 
}, 
 selection = 'none', rownames = FALSE, colnames = c('item_id','response','item_score',''), 
 class='compact readable', escape=FALSE, server=FALSE,
 options = list(pageLength = 20, autoWidth = FALSE,
                columnDefs = list(list(targets = 3, 
                                       render = JS("function(data,type,row){return(row[2] == row[3] ? '' : '<span class=\"label label-info\">!</span>')}"))))
)
outputOptions(output, "rules", suspendWhenHidden=FALSE)

observeEvent(input$rules_data, {
  show('prj_alter_rules')
})

observeEvent(input$prj_alter_rules, {
  new_rules = as_tibble(lapply(input$rules_data, unlist))
  colnames(new_rules)[ncol(new_rules)] = 'old_val'
  new_rules = filter(new_rules, .data$item_score != .data$old_val)
  if(nrow(new_rules) > 0)
  {
    withBusyIndicatorServer('prj_alter_rules',
    {
      # dexter error message if wrong not extremely intelligble yet without output
      touch_rules(db, new_rules)
      init_project()
      hide('prj_alter_rules')
    })
  } else
  {
    hide('prj_alter_rules')
  }
})



# ITEM PROPERTIES ---------------------------------------------------------

output$item_properties = renderDataTable(
  {

    req(values$item_properties)
    # skip once if necessary for updatable DT
    # this has a side effect (toggle) !
    isolate({
      update = values$update_item_properties
      values$update_item_properties = TRUE})
    
    req(update, cancelOutput = TRUE)

    
    if(ncol(values$item_properties)>1)
    {
      sketch=tags$table(
        tableHeader(colnames(values$item_properties)),
        tags$tbody(),
        dt_foot_summary(values$item_properties))
    } else
    {
      sketch=tags$table(
        tableHeader(colnames(values$item_properties)),
        tags$tbody())
    }

    datatable(values$item_properties, container=sketch,
              selection = 'none', rownames = FALSE,  
              class='compact readable', 
              options = list(pageLength = 20, autoWidth = FALSE,
                             scrollX = TRUE, 
                             fixedColumns = list(leftColumns = 1),
                             orderCellsTop = TRUE,
                             initComplete = JS("function(s){draw_dt_footer(s);dt_add_column_btn(s)}")),
                             extensions = 'FixedColumns')
    
  })   

# react to user update without redrawing the table
ip_proxy = dataTableProxy('item_properties')

observeEvent(input$item_properties_user_update,
{

  indx = input$item_properties_user_update$col_index
  upd = tibble(item_id = input$item_properties_user_update$row[[1]])
  upd[[colnames(values$item_properties)[indx]]] = input$item_properties_user_update$row[[indx]]
  add_item_properties(db, upd)
  values$item_properties = get_items(db)
  values$update_item_properties = FALSE
  session$sendCustomMessage(type = 'update_footplot', 
                            message=list(jqstring = paste0("#item_properties tfoot.dt-footer-plots td:nth-child(",indx,")"),
                                         html = toString(footplot_html(values$item_properties[[indx]]))))
  
  replaceData(ip_proxy, values$item_properties, rownames = FALSE, resetPaging = FALSE)
})
                                      

# read from file
observeEvent(input$itemprop_file,{
  input_file = input$itemprop_file
  values$new_item_properties = read_spreadsheet(input_file$datapath)
  
})

output$new_itemprop_preview = renderTable({
  if(!is.null(values$new_item_properties))
  {
    tibble(column = colnames(values$new_item_properties), 
           values = paste0(sapply(slice(values$new_item_properties, 1:10), paste, collapse = ', '),', ...'))
  }
})

observeEvent(input$go_import_new_itemprop,
{
  req(values$new_item_properties)
  withBusyIndicatorServer("go_import_new_itemprop",
  {
    if(!('item_id' %in% tolower(colnames(values$new_item_properties))))
      stop('missing item_id column')

    add_item_properties(db, values$new_item_properties )
    values$new_item_properties = NULL
    reset('itemprop_file')
    set_js_vars(db, session)
    items = get_items(db)
    values$item_properties = items[,!colnames(items) %in% c('item_screenshot','item_html','item_href')]
    if(ncol(values$item_properties) > 1)
    {
      show('ctt_itemprop-btn')
    } else
    {
      hide(selector = '#ctt_itemprop, #ctt_itemprop-btn')
    }
  })
})


observeEvent(input$item_properties_add_column,{
  req(input$item_properties_add_column)
  val = input$item_properties_add_column

  tc = switch(val$prop_type,integer = as.integer, double = as.double, as.character)

  dflt = list(tc(val$prop_dflt))
  names(dflt) = val$prop_name

  add_item_properties(db, default_values=dflt)
  items = get_items(db)

  values$item_properties = items[,!colnames(items) %in% c('item_screenshot','item_html','item_href')]
  set_js_vars(db, session)
  show('ctt_itemprop-btn')
})


# to do: screen columns should not come along in get_variables
output$new_itemcontents_preview = renderUI({
  req(db,input$itemcontents_file$datapath)
  
  nms = lapply(unzip(input$itemcontents_file$datapath, list=TRUE)$Name, basename)
  nms = nms[grepl('\\.(png)|(html?)$',nms,perl=TRUE)]
  nms = gsub('\\.(png)|(html?)$','',nms,perl=TRUE)
  
  tags$div(paste('Itemcontents found for', length(nms), 'items.'),
           tags$p(paste(nms[1:50],collapse=', '), if.else(length(nms)>50,', ...','')))

})

observeEvent(input$go_import_item_contents,
{
  req(db,input$itemcontents_file$datapath)
  withBusyIndicatorServer("go_import_item_contents",
  {
    td = tempdir()
    file_nms = unzip(input$itemcontents_file$datapath, junkpaths = TRUE, exdir = td)
    flds = dbListFields(db,'dxItems')
    
    png = file_nms[grepl('\\.png$', file_nms, perl=TRUE)]
    
    add_item_properties(db,
      tibble(item_id = gsub('\\.png$','',basename(png), perl=TRUE),
             item_screenshot = sapply(png, function(fn){
             base64Encode(readBin(fn, "raw", file.info(fn)[1, "size"]), "txt")}, simplify=TRUE)))
    
    
    htm = file_nms[grepl('\\.html?$', file_nms, perl=TRUE)]
    
    add_item_properties(db,
      tibble(item_id = gsub('\\.png$','',basename(htm), perl=TRUE),
             item_html = sapply(htm, function(fn){
             readChar(fn, file.info(fn)[1, "size"])}, simplify=TRUE)))
    

    unlink(td)
    reset('itemcontents_file')
    show('item-viewer-btn')
  })
  
})



# PERSON PROPERTIES -------------------------------------------------------

output$person_properties = renderDataTable(
{
  req(values$person_properties)
  # skip once if necessary
  isolate({
    update = values$update_person_properties 
    values$update_person_properties = TRUE})
  
  req(update, cancelOutput = TRUE)
  
  if(ncol(values$person_properties)>1)
  {
   sketch=tags$table(
     tableHeader(colnames(values$person_properties)),
     tags$tbody(),
     dt_foot_summary(values$person_properties))
  } else
  {
    sketch =tags$table(
      tableHeader(colnames(values$person_properties)),
      tags$tbody())
  }

  options = list(pageLength = 20, autoWidth = FALSE,
    scrollX = TRUE,
    fixedColumns = list(leftColumns = 1),
    orderCellsTop = TRUE,
    initComplete = JS("draw_dt_footer"))
  
  dbls = sapply(values$person_properties, is_double_)

  if(any(dbls))
  {
    options$columnDefs = list(list(targets = unname(which(dbls))-1L, render=JS("function(data,type,row){return(dt_render_dec(data,2));}")))
  }

  datatable(values$person_properties, 
            container=sketch,
            selection = 'none', rownames = FALSE,  
            class='compact readable', 
            options=options,
            extensions = 'FixedColumns')

})   




# react to user update without redrawing the table (which will loose position and scuh otherwise)
pp_proxy = dataTableProxy('person_properties')

observeEvent(input$person_properties_user_update,
{
  indx = input$person_properties_user_update$col_index
  upd = tibble(person_id = input$person_properties_user_update$row[[1]])
  upd[[colnames(values$person_properties)[indx]]] = input$person_properties_user_update$row[[indx]]
  add_person_properties(db, upd)
  values$person_properties = get_persons(db)
  # update the footer plot
  session$sendCustomMessage(type = 'update_footplot', 
        message=list(jqstring = paste0("#person_properties tfoot.dt-footer-plots td:nth-child(",indx,")"),
                     html = toString(footplot_html(values$person_properties[[indx]]))))

  # prevent datatable update
  values$update_person_properties = FALSE
  replaceData(pp_proxy, values$person_properties, rownames = FALSE, resetPaging = FALSE)
})


observeEvent(input$person_property_file,
{
  values$new_person_properties = read_spreadsheet(input$person_property_file$datapath)
})


output$new_personprop_preview = renderTable({
  req(values$new_person_properties)

    tibble(column = colnames(values$new_person_properties), 
           values = paste0(sapply(slice(values$new_person_properties, 1:10), paste, collapse = ', '),', ...'))
})

observeEvent(input$go_import_new_personprop,
{
  withBusyIndicatorServer("go_import_new_personprop",
  {
    if(is.null(values$new_person_properties))
       stop('No file selected')
      
    if(!('person_id' %in% tolower(colnames(values$new_person_properties))))
      stop('missing person_id column')
      
    add_person_properties(db, values$new_person_properties)
    values$new_person_properties = NULL
    reset('person_property_file')
    session$sendCustomMessage(type = 'set_js_vars', 
                              message=list(data = list(variables = get_variables(db))))
  })
})


