### extensions to shiny tags ###

dt_attach_dependencies = function(dt)
{
  dep = datatable(tibble(a=0))$dependencies
  attachDependencies(dt, dep, TRUE)
}

dt_readable = function(dt_output){
  dt_output[[1]]$attribs$class = paste(dt_output[[1]]$attribs$class, 'readable')
  dt_output
}

df2html = function(df, ...)
{
  do.call(tags$table,
    modifyList(
      list(tags$thead(do.call(tags$tr, lapply(colnames(df), tags$th))),
                 tags$tbody(apply(df,1,function(x) do.call(tags$tr,lapply(as.vector(x), tags$td))))),
      list(...)))
  
}

# add a downloadbutton to a plotOutput element
plot_add_download_btn = function(plt)
{
  id = paste0(tagGetAttribute(plt, 'id'), '_download')
  btn = tags$a(class = 'plot-save-btn shiny-download-link',
               download="", id=id, target="_blank", href="",
               tags$span(class="glyphicon glyphicon-floppy-disk"))
  
  tags$div(plt, btn, style='position:relative')
}



# editable also implies readable
# columns currenlty a vector of column numbers
# maybe change to a list of desired input element for each column in the future to offer more control

# columns can be an integer vector, or a string like '1:', '2, 5:7' or 'all'
# everything is 1-based index like in R
dt_editable = function(dt_output, columns = 'all')
{
  dt = dt_readable(dt_output)
  dt[[1]]$attribs$class = paste(dt[[1]]$attribs$class, 'editable')
  # columns -1 for javascript indexing
  
  dt[[1]]$attribs$`data-editable-columns` = ifelse(is.numeric(columns), 
                                                   paste0(paste0(columns, collapse=',')),
                                                   columns)
  dt
}

sfButton_add_icon = function(btn, icon_class)
{
  btn[[2]][['children']] = append(list(tags$i(class=icon_class)),btn[[2]][['children']])
  btn
}

savename_btn = function(id, label,width='116px',...)
{
  save_btn = shinySaveButton(id = id, label='Browse...',title='Save as...',...)
  save_btn[[2]]$attribs$style='display:inline-block;border-bottom-right-radius:0;border-top-right-radius:0;float:left;'

  tags$div(
      tags$label(label),
      tags$div(
        save_btn,
        tags$input(type='text',class="form-control", id = paste0(id,'_display'), value="", readonly = 'readonly',
                   style=paste0('float:left;display:inline-block;border-bottom-left-radius:0;border-top-left-radius:0;width:', width),
        class="input-group"),
      class='form-group'),
      style='display:inline-block;vertical-align:top;margin-bottom:5px;')
}


# if choices is a list, names will be taken as keys and values can be plain text or tags
# if choices is an unnamed vector, the elements will be both display and value
# if choices is a named vector, names are the values
multiToggleButton = function(id, choices, selected='', btn_width='7em', style=NULL)
{
  if(is.null(names(choices)))
    names(choices) = choices
  
  if(selected != '' && !selected %in% names(choices))
    stop(paste0("'", selected,"' not in choices"))
  
  if(nchar(btn_width)>0)
    btn_width = paste('width:',btn_width)
    
  args = mapply(function(val,txt){
    tags$button(txt, type='button', 
                class=paste("btn",if_else(val==selected,'btn-primary','btn-default')),
                `data-value`= val,
                style=paste("white-space: nowrap;",btn_width))
    
    }, names(choices), choices, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  
  args$id = id
  if(!is.null(style)) args$style = style
  args$class = "btn-group toggle-button-group multi-toggle-input"
  
  do.call(tags$div, args)
  
}


# slight updates to some standard input tags
#
out_in_style = function(inline, width)
{
  div_style = ifelse(inline,'display:inline-block;vertical-align:top;margin-bottom:5px;','')
  inpt_style = ''
  if(!is.null(width))
  {
    if(grepl('%', width, fixed=TRUE)){
      div_style = paste0(';',div_style, 'width:',width)
    } else
    {
      inpt_style = paste('width:',width)
    }
  }
  list(outer=div_style, inner=inpt_style)
}


etextInput = function(inputId, label, value = "", ..., inline = FALSE, class='',width=NULL)
{
  style = out_in_style(inline, width)
  tags$div(style = style$outer,
           tags$label(label, `for` = inputId),
           tags$br(),
           tags$input(id = inputId, type = "text", value = value, class=paste(class,"form-control"), style = style$inner, ...))
}

etextAreaInput = function(inputId, label, value = "", ..., inline = FALSE, class='',width=NULL,resize=NULL)
{
  style = out_in_style(inline, width)
  if(!is.null(resize))
    style$inner = paste0('resize:',resize,';',style$inner)
  tags$div(style = style$outer,
           class=class,
           tags$label(label, `for` = inputId),
           tags$br(),
           tags$textarea(id = inputId, value = value, class="form-control", style = style$inner, ...))
}


enumericInput = function(inputId, label, value=NULL, inline = FALSE, class='', width=NULL, ...)
{
  style = out_in_style(inline, width)
  tags$div(style = style$outer,
           tags$label(label, `for` = inputId),
           tags$input(id = inputId, type = "number", value = value, 
                      class=paste(class,"form-control"), style = style$inner, ...))
}

eselectInput = function(inputId, label, choices, ..., inline = FALSE, width=NULL)
{
  inpt = selectInput(inputId, label, choices, width=width, ...)
  if(inline){
    inpt$attribs$class = paste(inpt$attribs$class, 'shiny-input-container-inline')
  }
  inpt
}

css_divide = function(css, divide_by)
{
  sub('^\\d+',as.numeric(sub('[^\\d].+$','',css,perl=TRUE))/divide_by,css, perl=TRUE)
}



rangeInput = function(inputId, label, value=NULL, min = NA, max = NA, step = 1, width, inline=FALSE)
{

  style = out_in_style( inline, width)
  style$inner = paste0('display:inline-block;width:',width,';')
  
  input1 = tags$input(id = paste0(inputId,'__r__0'), type="number",class="form-control", 
                      style=paste0('margin-right:0px;float:left;',
                                   'border-right:0px;border-bottom-right-radius:0px;',
                                   'border-top-right-radius:0px;',style$inner))
  input2 = tags$input(id = paste0(inputId,'__r__1'), type="number",class="form-control", 
                      style=paste0('margin-left:0px;float:left;',
                                   'border-bottom-left-radius:0px;',
                                   'border-top-left-radius:0px;',style$inner))
  
  if (!is.na(min)) 
  {
    input1$attribs$min = min
    input2$attribs$min = min
  }
  if (!is.na(max)) 
  {
    input1$attribs$max = max
    input2$attribs$max = max
  }
  if (!is.na(step))
  {
    input1$attribs$step = step
    input2$attribs$step = step
  }
  if (!is.null(value))
  {
    input1$attribs$value = value[1]
    input2$attribs$value = value[2]
  }
  
  tags$div(
    tags$label(label, `for` = paste0(inputId,'__r__0')),
    tags$br(),
      tags$div(
        input1,          
        input2,
        style = "display:inline-block;whitespace:nowrap;"
      ),
  id = inputId,
  class = 'form-group e-range-input shiny-input-container-inline',
  style = style$outer
  )
}

updateRangeInput = function(session, inputId,  value = NULL,
                             min = NULL, max = NULL, step = NULL)
{
  paste0(inputId,'__r__0')
  updateNumericInput(session, paste0(inputId,'__r__0'),  
                     value = value[1],min = min, max = max, step = step)
  
  updateNumericInput(session, paste0(inputId,'__r__1'),  
                     value = value[2],min = min, max = max, step = step)
}



# generate a set of input tags and an execute button for an R function
#
generate_inputs = function(fun, id = deparse(substitute(fun)),
                           omit = c('db','dataSrc'), inline = FALSE, class='', width = '100px',
                           tooltip = TRUE, with_busy = TRUE,
                           input_type = NULL, label = id,
                           go_button=actionButton)
{
  args = formals(fun)
  tgs = list()
  hlp = getHelpList(deparse(substitute(fun)), package='dexter')

  contents = paste(deparse(fun), collapse = ' ')

  input_is_vector = function(argname)
  {
    ! grepl(paste0('\\W',argname,'\\s*(\\=|(\\<-))\\s*match\\.arg\\(',argname), contents, perl = TRUE)
  }

  arg_names = setdiff(names(args), omit)
  
  dflt_types = lapply(setdiff(names(args), omit), function(argname)
    {
      if(grepl('path', argname, fixed=TRUE)) return('file_input')
      if(inherits(args[[argname]],'name')) return('text');
      arg = eval(args[[argname]])
      if(length(arg) > 1)
      {
        if(input_is_vector(argname)) return('text');
        return('select');
      }
      if(is.numeric(arg)) return('numeric');
      if(is.logical(arg)) return('boolean');
      return('text');
    })
  
  names(dflt_types) = arg_names
  
  input_type = modifyList(dflt_types, as.list(input_type))

  for(argname in arg_names)
  {
    arg = args[[argname]]
    if(is.function(input_type[[argname]]))
    {
      tgs[[argname]] = input_type[[argname]](id = paste(id, argname,sep='_'), label = argname)
    } else if(argname == 'predicate')
    {
      
      tgs[[argname]] = etextAreaInput(paste(id, argname,sep='_'), 
                                      label = 'data selection', 
                                      rows=1, resize='none', width='200px',
                                      class='predicate-with-help', inline=TRUE)
    } else
    {
      tgs[[argname]] = switch(input_type[[argname]],
          text = etextInput(paste(id, argname,sep='_'), label = argname, 
                            inline=inline, class=class),                   
          `numeric` = enumericInput(paste(id, argname, sep='_'), label = argname, 
                                   value = if.else(missing(arg),NULL,arg), inline = inline, class=class,width=width),
          select = eselectInput(paste(id, argname,sep='_'), label = argname, 
                                choices = eval(arg), inline=inline,width=width),
          boolean = eselectInput(paste(id, argname,sep='_'), label = argname,
                                 choices=c(TRUE,FALSE), selected = arg, inline = inline, width = width),
          `range` = rangeInput(paste(id, argname,sep='_'), label = argname, 
                                value = if.else(missing(arg), NULL, arg), width = css_divide(width,1.5), inline=inline, min=1),
          file_input = tagAppendAttributes(
                          fileInput(paste(id, argname,sep='_'), label = argname, width = '200px'),
                          style="display:inline-block;margin-bottom:0px;vertical-align:top;"),
          file_save = shinySaveButton(paste(id, argname, sep='_'), label = argname, 'Save file as...', 
                                      filetype = list(db='db', sqlite = 'sqlite')),
          
          file_savename = savename_btn(paste(id, argname,sep='_'), label = argname,filetype = list(db='db', sqlite = 'sqlite')))
    }    
    if(tooltip && argname %in% names(hlp$arguments))
    {
      if(input_type[[argname]] == 'file_input')
      {
        tgs[[argname]]$attribs$id = paste0(paste(id, argname,sep='_'),'-fg-container')
        tt_id = tgs[[argname]]$attribs$id
      } else
      {
        tt_id = paste(id, argname,sep='_')
      }
      tgs[[paste0(tt_id,'tip',sep='_')]] = bsTooltip(
          tt_id,
          hlp$arguments[[argname]] %>%
            gsub("'", "\\'", ., fixed=TRUE) %>%
            gsub('\n',' ', .) %>%
            gsub('NULL', 'empty', ., fixed=TRUE),
          options=list(delay=300, html=TRUE))
    }
  }


  if(with_busy){
    tgs[[paste0('go_', id)]] = withBusyIndicatorUI(go_button(paste0('go_', id), label, class = 'btn btn-primary'))
  } else
  {  
    tgs[[paste0('go_', id)]] = go_button(paste0('go_', id), label, class = paste('btn btn-primary',ifelse(inline,'inline','')))
  }
  
  if(tooltip) tgs[[paste0(id,'descr')]] = bsTooltip(paste0('go_', id), gsub('\n',' ', hlp$description), options=list(delay=300))
  do.call(tagList,tgs)
}


# custom in/output element
#
# Shows plots as a slideshow
#
# @param id string uniquely identifying the element
#
# plot_slider exposes the following in/output properties
#
# input$[id]_select works similar to selectInput, contains the image chosen by the user
# output$[id]_plot works similar to plotOutput, set this to update the main plot
#
# in addition use updateSlider to update the choices
#
plotSlider = function(id, width='100%', height='700px')
{
  tags$div(id = id,
           tags$div(
             tags$div(class='plot_left img_nav', tags$span(class='glyphicon glyphicon-chevron-left',style='top:50%;left:40%')),
             plotOutput(paste0(id,'_plot'),width='100%', height='100%'),
             tags$div(class='plot_right img_nav', tags$span(class='glyphicon glyphicon-chevron-right',style='top:50%;left:60%;'))),
           tags$div(class='thumbs',id=paste0(id,'_slider'),
                    tags$div(class='slide_left', tags$span(class='glyphicon glyphicon-chevron-left',style='top:50%;')),
                    tags$div(class='slidearea',
                             tags$div(class='slider')),
                    tags$div(class='slide_right', tags$span(class='glyphicon glyphicon-chevron-right',style='top:50%;'))),
           tags$div(class='alert alert-danger',style='display:none;position:absolute;top:5px;left:5px;'),
           class = 'plot_slider uninitialized', style = paste0('width:',width,';height:',height,';position:relative;'),
           tags$a(class = 'plot-save-btn shiny-download-link',
                  download="", id=paste0(id,'_download'), target="_blank", href="",
                  tags$span(class="glyphicon glyphicon-floppy-disk")))
}


selectSlider = function(id, width='100%', height = '100px')
{
  tags$div(id = id,
           tags$div(class='thumbs',id=paste0(id,'_slider'),
                    tags$div(class='slide_left', tags$span(class='glyphicon glyphicon-chevron-left',style='top:50%;')),
                    tags$div(class='slidearea',
                             tags$div(class='slider')),
                    tags$div(class='slide_right', tags$span(class='glyphicon glyphicon-chevron-right',style='top:50%;'))),
           class = 'select_slider uninitialized', style = paste0('width:',width,';height:',height,';'))
}



# function to update the choices bar of de plot_slider
#
# works similar to updateSelectInput, but with a different format of choices
#
# @param choices list containing the choices, each choice should be a list similar to the output of imageOutput
# with the element src referring to an existing temporary file (it will be deleted after reading) containing a png image
# and an extra element choice_id with a string uniquely identifying the choice. This string is returned by input$[inputId]_select
#
updateSlider = function(session, inputId, choices=NULL,selected=NULL, error=NULL){
  msg = list(id=inputId)
  if(!is.null(error))
  {
    msg$error=error
  } else
  {
    if(!is.null(choices))
    {
      msg$data = lapply(choices, function(imageinfo)  {
    
        txt = base64Encode(readBin(imageinfo$src, "raw", file.info(imageinfo$src)[1, "size"]), "txt")
        unlink(imageinfo$src)
        list(src = paste0("data:image/png;base64,",txt), image_id = imageinfo$choice_id)
      })
    }
    if(!is.null(selected))
      msg$selected = selected
  }
  session$sendCustomMessage(type='updateSlider', message=msg)
}



imgSelect = function(inputId, choices = NULL, selected = NULL, group_options = list(),
          width = '100%', width.choice = '60px', height.choice = width.choice)
{
  if(!is.null(selected)) stop('selected bij begin nog niet geimplementeerd')
  if(!is.null(selected)) stop('choices bij begin nog niet geimplementeerd')
  
  tags$div(
    tags$div(class = 'img-select-scrollbody'),
    `data-width-choice` = width.choice,
    `data-height-choice` = height.choice,
    `data-group_options` = toJSON(group_options, auto_unbox=TRUE),
    class = 'img-select',
    id = inputId,
    style = paste0('width:',width ))
}

updateImgSelect = function(session, inputId, choices=NULL, selected=NULL, group_select = NULL, group_options = NULL){
  
  data = list(selected = selected, group_select = group_select, group_options = group_options)

  if(!is.null(choices))
  {
    data$choices = lapply(dropNulls(choices), function(imageinfo)
    {
      if(is.null(imageinfo$group)) imageinfo$group = '__all__'
         
      txt = base64Encode(readBin(imageinfo$src, "raw", file.info(imageinfo$src)[1, "size"]), "txt")
      unlink(imageinfo$src)
      list(src = paste0("data:image/png;base64,",txt), choice_id = imageinfo$choice_id, group = imageinfo$group)
    })
  }

  session$sendInputMessage(inputId, data)
}

listInput = function(inputId, label=NULL, class=NULL, ...) 
{
  label = if.else(is.null(label),'',tags$p(tags$b(label)))
  tags$div(label,
           class=paste(c(class,'inputList'), collapse=' '), 
           id=inputId, ...)
}

updateListInput = function(session, inputId, fields=NULL, value=NULL)
{
  session$sendInputMessage(inputId, toJSON(dropNulls(list(fieldset=fields, value=as.list(value))),auto_unbox=TRUE))
}



download_buttons = function(dt_id)
{
  tags$div(downloadButton(paste0(dt_id,'_xl_download'), ''),
           downloadButton(paste0(dt_id,'_csv_download'), ''),
           class='full_download_buttons')
}

dt_buttons = function(dt_id, title = '', btn_options = NULL )
{
  if(is.null(btn_options))
    btn_options = list()
  
  fn = function(ext) JS(paste0('function(){return($("#project_pth").text()+"',title,'" + "',ext,'");}'))

  list(
    modifyList(list(extend='copy', text='', className='fa fa-clipboard'), btn_options),
    modifyList(list(extend='csv', text='csv', title = fn('.csv')), btn_options),
    modifyList(list(extend='excel', text='', title = fn('.xlsx'), className='fa fa-file-excel-o'), btn_options),
    modifyList(list(extend='pdf', text='', title = fn('.pdf'), className='fa fa-file-pdf-o'), btn_options),
    modifyList(list(extend='print', text='', className='fa fa-print'), btn_options),
    list(extend='', text='', className='fa fa-file-excel-o full-download', 
         action = unbox(JS(paste0("function(){ $('#",dt_id,"_xl_download').get(0).click()}")))),
    list(extend='', text='csv', className='full-download', 
         action = unbox(JS(paste0("function(){ $('#",dt_id,"_csv_download').get(0).click()}"))))
  )
}

dt_foot_summary = function(df)
{
  out=list(tags$td(tags$i('Summary:'),style='vertical-align:top;'))
  any_plots = FALSE
  
  for(cn in colnames(df)[2:ncol(df)])
  {
    fp = footplot_html(df[[cn]])
    if(is.null(fp))
    {
      out[[cn]] = tags$td()
    } else
    {
      any_plots = TRUE
      out[[cn]] = fp
    }
    
  }

  names(out) = NULL
  if(any_plots)
  {
    tags$tfoot(do.call(tags$tr,out),class='dt-footer-plots')
  } else
  {
    tags$tfoot(do.call(tags$tr,lapply(1:ncol(df),function(i) tags$tr())))
  }
  
}



