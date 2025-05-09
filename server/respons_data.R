

observeEvent(input$data_file,{
  data_file = input$data_file
  values$import_data = if.else(is.null(data_file), NULL, read_spreadsheet(data_file$datapath))
})


output$show_data_unknown_rsp = renderUI({
  req(values$import_data)
  
  items = intersect(dbGetQuery(db,'SELECT item_id FROM dxItems;')$item_id, 
                    colnames(values$import_data))
  
  if(length(items) == 0)
    return(tags$b("None of the column names in your data correspond to known item_id's"))
  
  unknown_rsp = values$import_data[,items] |>
    gather(key='item_id', value='response') |>
    mutate(response = if_else(is.na(.data$response), 'NA', as.character(.data$response))) |>
    distinct() |>
    anti_join(get_rules(db), by=c('item_id','response')) |>
    arrange(.data$item_id, .data$response)
  
  if(nrow(unknown_rsp) == 0)
  {
    NULL
  } else
  {
    unknown_rsp$score = 0L
    tagList(tags$p('The following responses are unknown and will be scored as 0:'),
            df2html(unknown_rsp, class="min-table",
                    style="max-height:20em; overflow-y:auto;display:inline-block;"))
  }
  
})


output$data_preview = renderTable({
  req(values$import_data)

  reserved_names = c('person_id','item_id','item_position',
                     'response','item_score','booklet_id')
  
  preview = tibble(column = trimws(colnames(values$import_data)), 
                   type = 'ignored',
                   change = '',
                   values = paste0(substring(
                     sapply(slice(values$import_data,1:10),paste, collapse=', '),
                     1,100),', ...')) 
  
  preview$type[tolower(preview$column) %in% dbListFields(db, 'dxpersons')] = 'person property'
  preview$type[preview$column %in% get_items(db)$item_id] = 'item'
  preview$type[tolower(preview$column) == 'person_id'] = 'person identifier'
  
  
  btn = paste0('<button type="button" onclick="',"
        me = $(this); 
        Shiny.onInputChange('add_covariate',me.closest('tr').find('td:first-child').text());
        me.closest('tr').find('td:nth-child(2)').text('person property');
        me.remove();",
      '">add as person property</button>')

  preview$change[preview$type == 'ignored' & !(preview$column %in% reserved_names)] = btn

  preview = mutate(preview, column = htmlEscape(.data$column), values = htmlEscape(.data$values))
  colnames(preview) = c('column','import as','','values')
  
  preview
}, sanitize.text.function = identity, caption='Response data preview')


observeEvent(input$add_covariate, {
  if(!is.null(values$import_data))
  {
    var = trimws(input$add_covariate)
    col = pull(values$import_data, var)
    dflt = list()
    if(typeof(col) == 'integer' || (typeof(col) == 'character' && all(grepl('^\\d+(\\.0)?$', col, perl = TRUE)))) 
    {
      dflt[var] = as.integer(NA)
    } else if(is.numeric(col) || (typeof(col) == 'character' && all(grepl('^\\d+(\\.\\d+)?$', col, perl = TRUE)))) 
    {
      dflt[var] = as.double(NA)
    } else
    {
      dflt[var] = ""
    }

    add_person_properties(db, default_values = dflt) 
    session$sendCustomMessage(type = 'set_js_vars', 
                              message=list(data = list(variables = get_variables(db))))
  }
})


observeEvent(input$go_import_data, {
  withBusyIndicatorServer("go_import_data",
  {

    booklet_id = trimws(input$add_booklet_name)
    
    if(is.null(values$import_data))
      stop('no response data to import')
    
    if(booklet_id == '')
      stop('please provide a booklet_id')
    
    #print(values$import_data)
    #assign("idt", values$import_data, envir = .GlobalEnv)
    
    result = add_booklet(db, values$import_data, booklet_id = booklet_id, auto_add_unknown_rules=TRUE)
    n = nrow(values$import_data)
    
    
    msg = list(
      hr(),
      tags$p(tags$i('Most recently imported:')),
      tags$p(
        tags$b('File: '),
        tags$span(basename(input$data_file$name))),
      tags$p(
        tags$b('Booklet: '),
        tags$span(booklet_id)),
      tags$p(
        tags$b('Respondents: '),
        tags$span(n)),
      tags$p(
        tags$b('Items: '),
        tags$span(paste(result$items, collapse=', '))))
    
    if('person_properties' %in% names(result) && length(result$person_properties > 0 ) )
      msg = append(msg, 
                   list(tags$p(tags$b('Person properties: '),
                               tags$span(paste(result$person_properties, collapse=', ')))))
    
    if('columns_ignored' %in% names(result) && length(result$columns_ignored > 0 ))
      msg = append(msg, 
                   list(tags$p(tags$b('Columns ignored: '),
                               tags$span(paste(result$columns_ignored, collapse=', ')))))

    values$import_data = NULL
    reset('data_file')
    init_project()
    
    output$data_import_result = renderUI(tagList(msg))
    
  })
})


# long format import data -------------------------------------------------



observeEvent(input$data_file_long,{
  data_file = input$data_file_long
  values$import_data_long = if.else(is.null(data_file), NULL, read_spreadsheet(data_file$datapath)) |>
    rename_all(tolower)
})

observeEvent(input$design_file_long,{
  design_file = input$design_file_long
  values$import_design_long = if.else(is.null(design_file), NULL, read_spreadsheet(design_file$datapath)) |>
    rename_all(tolower)
})


output$data_preview_long = renderTable({
  req(values$import_data_long)
  values$import_data_long |>
    slice(1:20) |>
    mutate_if(function(x){is.numeric(x) && all(x %% 1 == 0)}, as.integer)
}, caption='Response data preview (rows 1-20)')

output$design_preview_long = renderTable({
  req(values$import_design_long)
  values$import_design_long |>
    slice(1:20) |> 
    mutate_if(function(x){is.numeric(x) && all(x %% 1 == 0)}, as.integer)
}, caption='Design preview (rows 1-20)')



output$show_data_unknown_rsp_long = renderUI({
  req(values$import_data_long)
  
  missing_col = setdiff(c('item_id', 'person_id', 'response','booklet_id'), colnames(values$import_data_long))
  
  if(length(missing_col) > 0)
  {
    tagList(tags$p(tags$b('Your data file should contain column(s):')), 
            do.call(tags$ul,lapply(missing_col, tags$li)))
  } else
  {
    unknown_items = setdiff(values$import_data_long$item_id,
                            dbGetQuery(db,'SELECT item_id FROM dxItems;')$item_id)
    if(length(unknown_items) > 0)
    {
      tagList(tags$p('The following items are unknown in your project, 
                     you will have to import scoring rules first (see the project page)'),
        df2html(tibble(item_id=unknown_items), class="min-table",
                style="max-height:20em; overflow-y:auto;display:inline-block;"))
    } else
    {
      unknown_rsp = values$import_data_long|>
        mutate(response = if_else(is.na(.data$response), 'NA', as.character(.data$response))) |>
        distinct(.data$item_id, .data$response) |>
        anti_join(get_rules(db), by=c('item_id','response')) |>
        arrange(.data$item_id, .data$response)
      
      if(nrow(unknown_rsp) == 0)
      {
        NULL
      } else
      {
        unknown_rsp$score = 0L
        tagList(tags$p('The following responses are unknown and will be scored as 0:'),
                df2html(unknown_rsp, class="min-table",
                        style="max-height:20em; overflow-y:auto;display:inline-block;"))
      }
    }
  }
})


observeEvent(input$go_import_data_long, {
  withBusyIndicatorServer("go_import_data_long",
  {
    if(is.null(values$import_data_long))
      stop('no response data to import')
    
    design = values$import_design_long
    
    add_response_data(db, values$import_data_long, design=design, auto_add_unknown_rules=TRUE)
    
    
    msg = tagList(
      hr(),
      tags$p(tags$i('Most recently imported:')),
      tags$p(
        tags$b('File: '),
        tags$span(basename(input$data_file_long$name))),
      tags$p(
        tags$b('Booklets: '),
        tags$span(n_distinct(values$import_data_long$booklet_id))),
      tags$p(
        tags$b('Items: '),
        tags$span(n_distinct(values$import_data_long$item_id))),
      tags$p(
        tags$b('Persons: '),
        tags$span(n_distinct(values$import_data_long$person_id))),
      tags$p(
        tags$b('Responses: '),
        tags$span(nrow(values$import_data_long))))
   
    
    
    #cleanup
    values$import_data_long = NULL
    values$import_design_long = NULL
    reset('data_file_long')
    reset('design_file_long')
    init_project()
    output$data_import_result_long = renderUI(msg)
  })
})
