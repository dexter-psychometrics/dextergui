
header = function()
{
  tags$head(
    tags$title("dextergui"),
    tags$script(src = "shinydexter/jquery.sparkline.min.js"),
    tags$script(src = "shinydexter/jquery-ui.min.js"),
    tags$link(href = "shinydexter/shinydexter.css", type='text/css', rel='stylesheet'),
    tags$link(href = "shinydexter/jquery-ui.min.css", type='text/css', rel='stylesheet'),
    tags$script(src = "shinydexter/jquery.mousewheel.min.js"),
    tags$script(src = "shinydexter/patches.js"),
    tags$script(src = "shinydexter/pr_helper.js"),
    tags$script(src = "shinydexter/dt_extensions.js"),
    tags$script(src = "shinydexter/img-select.js"),
    tags$script(src = "shinydexter/shinydexter.js")
  )
}


get_ui = function()
{
	tagList(header(),		
	useShinyjs(),
	tags$div(tags$div(class='loader'),id='project_load_icon',style='display:none;'),
  navbarPage(title=NULL, id='main_navbar', position = 'fixed-top',
		tabPanel('Project',
		  tags$div(
		    sfButton_add_icon(
		      shinySaveButton('new_proj_fn', ' Start new project', 'Save dexter project as...', filetype=list(db='db', sqlite = 'sqlite')),
		      'fa fa-file-o'),
		    sfButton_add_icon(
		      shinyFilesButton('open_proj_fn', label=' Open project', title='Select a dexter project file', multiple = FALSE),
		      'fa fa-folder-open-o'),
		    tags$button(tags$i(class="fa fa-upload"), tags$span(' Import oplm project'),
		                type='button', id='oplm_btn',  class='btn btn-default'),
		    tags$button(tags$i(class="fa fa-lightbulb-o"), tags$span(' example datasets'),
		                type='button', id='example_dts_btn',  class='btn btn-default'),
		    tags$div(' | Project: ',textOutput('project_pth', inline=TRUE), 
		             style='display:inline-block;font-weight:bold;vertical-align:bottom;margin-bottom:5px'),
		    class='project-buttons'),
		  tags$div(
		    generate_inputs(start_new_project_from_oplm, inline=TRUE, width='100px', omit=c('format','missing_character'),
		                    input_type = list(booklet_position='range', person_id='range', responses_start='numeric', 
		                                      dbname='file_savename'),
		                    label='Start project'),
		    tableOutput('oplm_dat'),
		    tags$hr(),
		    id='oplm_inputs'),
		  example_datasets_ui(id='example_datasets'),
		  fluidRow(
		    column(4,
		      tags$div(
		        withBusyIndicatorUI(hidden(actionButton('prj_alter_rules','Save changes', class="btn btn-primary"))),
		        style="float:right;"),
		      tags$h3('Scoring rules',style='margin-bottom:15px;'),
		      tabsetPanel(type = 'tabs',
		        tabPanel('View/alter rules', value = 'view',
    		      dt_editable(dataTableOutput("rules"),columns=3)),
		        tabPanel('Add rules from a file', value = 'from_file',
		          tags$br(),
		          tags$div(
  		          tagAppendAttributes(
  		            fileInput('rules_file', 'Select scoring rules file', width = '300px'),
  		            style = 'display:inline-block;margin-bottom:0;margin-right:1em;'),
  		          withBusyIndicatorUI(actionButton('go_import_new_rules', 'import', class='btn btn-primary')),
  		          tags$a(tags$i(class="fa fa-question-circle"),class="btn btn-lg", 
  		                 style='vertical-align:top; margin-top:1em;',
		                 `data-toggle`='collapse',`data-target`='#help-add-rules-file'),
		          style="white-space:nowrap;"),
		          tableOutput("new_rules_preview"),
		          tags$div(
		            tags$p('The scoring rules file can have one of these two formats:'),
		            tags$div(
        		      tags$b('1) Scoring rules per response'),
        		      tags$p('A csv or excel file with columns item_id, response and item_score with a separate row for each item-response combination',
        		             style="padding:5px;"),
    		          df2html(data.frame(item_id = c('S1DoCurse', 'S1DoScold'), response = c(0,0,1,1,2,2), item_score = c(0,0,1,1,2,2)) %>%
        		                arrange(.data$item_id, .data$response),
        		              class="min-table", style="margin-bottom:16px;"),
      		        tags$b('2) Keys'),
          		    tags$p('Only for multiple choice items, a csv or excel file with columns item_id, nOptions and key. ',
          		           'Keys can be either alphabetical or numeric', style="padding:5px;"),
    		          df2html(data.frame(item_id = c('mcItem_1', 'mcItem_2','mcItem_3'), nOptions = c(3,4,3), key=c('C','A','A')),
      		                class="min-table"),
        		      style="margin:7px;"),
    		        class='alert alert-light collapse', id='help-add-rules-file')),
		        id='proj_rules_tabs'),
		      id='proj_rules_frm'
		      ),
		    column(4,
		      tags$h3('Items',style='margin-bottom:15px;'),
		      tabsetPanel(type = 'tabs',
		        tabPanel('View/alter item properties',
		          dt_editable(dataTableOutput("item_properties"),columns='2:')),
		        tabPanel('Add item properties from a file',
		          tags$br(),
		          tags$b('Item properties'),
		          tags$p('Upload a csv, osd or excel file with a column named ', tags$i('item_id'), ' and other columns specifying the item properties.'),
		          tags$p('The first row should contain column names.'),
		          tagAppendAttributes(
		            fileInput('itemprop_file', 'Select spreadsheet file', width = '300px'),
		            style = 'display:inline-block;margin-bottom:0;margin-right:1em;'),
		          withBusyIndicatorUI(actionButton('go_import_new_itemprop', 'import')),
		          tags$div(textOutput('rules_upload_error', inline=TRUE), class='error'),
		          tableOutput("new_itemprop_preview"),
		          tags$b('Item contents'),
		          tags$p('upload the content of items themselves so they can be displayed on the ',
                      ' Classical Analysis tab'),
		          tags$p('This should be a .zip file including a .png image or a .html file ',
		                 'for each item and the name of each file should correspond to an item id.'),
		          tagAppendAttributes(
		            fileInput('itemcontents_file', 'Select zip file', width = '300px'),
		            style = 'display:inline-block;margin-bottom:0;margin-right:1em;'),
		          withBusyIndicatorUI(actionButton('go_import_item_contents', 'import')),
		          tags$div(textOutput('itemcontents_upload_error', inline=TRUE), class='error'),
		          uiOutput("new_itemcontents_preview")
		          )),
		      id='proj_items_frm'),
		    column(4,
		      tags$h3('Persons',style='margin-bottom:15px;'),
		      tabsetPanel(type = 'tabs',
		        tabPanel('View/alter person properties',
		          dt_editable(dataTableOutput("person_properties"),columns='2:')),  
		        tabPanel('Add person properties from a file',
		          tags$br(),
		          tags$p('Upload a csv or excel file with a column named ', tags$i('person_id'), ' and other columns specifying the person properties.'),
		          tags$p('The first row should contain column names.'),
		          tagAppendAttributes(
		            fileInput('person_property_file', 'Import person properties'),
		            style = 'display:inline-block;margin-bottom:0;margin-right:1em;'),
		          withBusyIndicatorUI(actionButton('go_import_new_personprop', 'import')),
		          dataTableOutput("new_personprop_preview"))),
		      id='proj_persons_frm'),
		    style='margin-left:0;')),
		tabPanel('Data import',
		  tags$h3('Import respons data'),
		  tags$hr(),
		  sidebarLayout(
			sidebarPanel(
			  selectizeInput('add_booklet_name','Booklet id',
						  choices=c('type or choose booklet_id' = ''),
						  options=list(create=TRUE, createOnBlur=TRUE)),
			  fileInput('data_file', 'Data file'),
			  uiOutput('show_data_unknown_rsp'),
			  withBusyIndicatorUI(actionButton('go_import_data','Import',class='btn btn-primary')),
			  htmlOutput('data_import_result'),
			  width=3
			),
			mainPanel(tableOutput('data_preview'))
		  ),
		  value = 'data_pane'
		),
		tabPanel('Classical analysis',
		  tabsetPanel(type = 'tabs', id = 'ctt_panels',
				tabPanel('booklets',
				  fluidRow(
				    column(6,
				      tags$h4('Classical statistics for booklets',style='margin-bottom:1em;'),
				      dataTableOutput('inter_booklets'),
				      download_buttons('inter_booklets'),
				      tags$p(tags$i('Click on one of the rows to display the item total regressions.')),
				      style='padding:3em;padding-top:1em;'),
				    column(6,
				      tags$h4('Item total regressions',style='margin-bottom:1em;display:inline-block;'),
				      tags$div(
				        tags$div(
				          checkboxInput('inter_summate','summate', value=TRUE), 
				          checkboxInput('inter_show_observed','show observed', value=TRUE),
				          style='display:inline-block;width:20ex;'),
				        enumericInput('inter_curtains', 'curtains', value='10', min='0', max='100', width = '6em', inline=TRUE),
				        style='display:inline-block;float:right;'),
				      uiOutput('inter_current_booklet'),
				      tags$div(
				        plotSlider('interslider'),
				        style='border:1px solid #ddd;border-radius:5px;padding:5px;clear:both;'),
				      style='padding:3em;padding-top:1em;'))),
				tabPanel('items',
				  fluidRow(
				    column(6,
    				  checkboxInput('ctt_items_averaged','averaged over booklets', value=TRUE),       
    				  dataTableOutput('ctt_items'),
    				  download_buttons('ctt_items'),
    				  style='padding:3em;padding-top:1em;'),
				    column(6,
				      tags$h3(uiOutput('ctt_selected_item')),
				      tags$div(
				        tags$a(
				          tags$span('Show item',class='collapsed-vis'),
				          tags$span('Hide item',class='collapsed-invis'), class="btn collapsed", 
				               `data-toggle`='collapse',`data-target`='#item-viewer-img'),
				        tags$div(uiOutput('item_viewer'),
				                 class="alert alert-light collapse",
				                 id='item-viewer-img'),
				        id='item-viewer-container'),
				      tags$div(
				        tags$a(
				          tags$span('Show item properties',class='collapsed-vis'),
				          tags$span('Hide item properties',class='collapsed-invis'), class="btn collapsed", 
				          `data-toggle`='collapse',`data-target`='#ctt_itemprop'),
				        listInput('ctt_itemprop', 
				                 class="alert alert-light collapse"),
				        id='ctt_itemprop-container'),
				      tagAppendAttributes(plot_add_download_btn(plotOutput('ctt_plot', width ='90%')), 
				                          style='; max-width: 600px; margin-left:1em;'),
				      tags$div(
				        withBusyIndicatorUI(actionButton('go_save_ctt_item_rules','Save changes')),
  				      tags$a(tags$i(class="fa fa-question-circle"),class="btn btn-lg", style='margin-top:1em;',
  				                 `data-toggle`='collapse',`data-target`='#help-ctt-change-rule'),
  				      tags$div(HTML('You can alter the score for an option by clicking a cell in the column <b>score</b>',
      				                    ' and pressing the <i>Save changes</i> button.'),
      				               class="alert alert-light collapse",
  				                   id='help-ctt-change-rule', style='margin-top:1em;'),  
			          dt_editable(dataTableOutput("item_rules"),columns=4),style='display:inline-block;'),
				        style='padding:3em;padding-top:1em;',id='col-ctt-item')))),     
		  value = 'ctt_pane'),
		tabPanel('IRT analysis',
		  sidebarLayout(
			sidebarPanel(
			  tags$h3('Fit enorm'),
			  tagAppendAttributes(textAreaInput('enorm_predicate',
			                                    label='data selection (optional)',
			                                    rows=1,
			                                    resize='none'),
			                      class="predicate-with-help"),
			  uiOutput('enorm_design_connected'),
			  forceNetworkOutput('design_plot', height=450),
			  eselectInput('enorm_method',label='Method',choices = eval(formals(fit_enorm)$method), width = '30%', inline=TRUE),
			  #enumericInput('enorm_nIterations', label = 'nIterations',value=eval(formals(fit_enorm)$nIterations), width = '30%', inline=TRUE),
			  withBusyIndicatorUI(actionButton('go_fit_enorm','fit_enorm',class='btn btn-primary')),
			  htmlOutput('fit_enorm_result'),
			  width=3
			),
			mainPanel(
			  tabsetPanel(type = 'tabs',id='enorm_tabs',
				tabPanel('Abilities', value='ability',
				  # standard_errors weggelaten, op termijn zouden die in abplot meegenomen kunnen worden       
				  wellPanel(generate_inputs(ability, omit=c('dataSrc','parms','person_level','asOPLM','standard_errors','use_draw'), 
				                            inline=TRUE,width='120px'),
				            style='border-top:none;'),
				  tabsetPanel(type='tabs',
					tabPanel('plots', abplotUI()
					         ),
					tabPanel('data',
					         tags$br(),
					         dataTableOutput('person_abilities'),
					         download_buttons('person_abilities')))),
				tabPanel('Plausible values', value='plausible_values',
				  wellPanel(generate_inputs(plausible_values, omit=c('dataSrc','parms','use_draw','asOPLM','covariates'), 
				                            inline=TRUE,width='150px'),
				            style='border-top:none;'),
				  pvplotUI()),
				tabPanel('Score-ability tables', value='ability_tables',
				  wellPanel(generate_inputs(ability_tables, omit=c('parms','design','standard_errors','asOPLM'), 
				                            input_type=list(use_draw='numeric'),inline=TRUE, width='120px'),
				            style='border-top:none;'),
				  fluidRow(
				    column(6, 
				      tags$h3('Score transformation table'),
				      tags$hr(),
				      tags$div(dataTableOutput('abl_tables'), download_buttons('abl_tables') ),style='max-width:600px;'),
				    column(6, 
				      tags$h3('Test information functions'),
				      tags$hr(),
				      tags$div( # this div to give the hovering tooltip a reference point
				      plot_add_download_btn(
				        plotOutput('abl_tables_plot_ti', height='400px', 
				                      hover= hoverOpts("abl_tables_plot_ti_hov", delay = 200, delayType = "debounce"))),
				      uiOutput("abl_tables_plot_ti_hinf"), style='position:relative;'),
             selectizeInput('abl_tables_plot_booklet', 'Choose booklets to plot', c(), multiple = TRUE),
				      style="padding-left:4em;"))),
				tabPanel('Items', value='enorm_items',
				  fluidRow(
				    column(6,
				      tags$h3('Item fit'),
				      tags$hr(),
				      enumericInput("enorm_slider_nbins","number of ability groups",min=2,step=1,value=5,inline=TRUE,width='5em'),
				      plotSlider('enorm_slider',height='600px',width='100%')),
				    column(6,
				      tags$h3('Parameters'),
				      tags$hr(),
				      hidden(multiToggleButton('coef_format', selected='norm',btn_width='3em',style='float:right;margin-top:-12px;',
				                        choices=list(norm = tags$span(class="fa fa-list"),
				                                     denorm = tags$i(class="fa fa-columns")))),
    				   tags$p(
    				     dataTableOutput('enorm_coef'),
    				     download_buttons('enorm_coef')),
				      style="padding-left:4em;")))),
			  width=9)),
		  value = 'enorm_pane'
		),
		tabPanel('Help', 
		         tags$div(
		           tags$div(
		            includeHTML(system.file("extdata", "manual.html", package = "dextergui", mustWork = TRUE)),
		            class='help-page'),
		           class='help-page-outer'))))
}


