// kan nog %in% bij, quotes zijn alleen lastig
var shinydexter = {
  operators: [
    ['equal','{0} == {1}'], 
    ['not equal','{0} != {1}'],
    ['smaller or equal','{0} <= {1}'],
    ['smaller than','{0} < {1}'],
    ['larger or equal','{0} >= {1}'],
    ['larger than','{0} > {1}'],
    ['regexp matches','grepl({1}, {0}, perl=TRUE)'],
    ['regexp no match','!grepl({1}, {0}, perl=TRUE)']
  ],
  lgop: [
    ['and',' & '],
    ['or',' | ']
  ],
  gpop: [
    ['',''],
    ['all',' & '],
    ['any',' | '] 
  ],
  ph_var_autocomplete: {}
};

var sdtooltips = {}

// not working on selectinput
//position a little off for fileinput
showtooltip = function(e)
{

	var container = $(e.target).closest('.shiny-input-container, .btn.shiny-bound-input');
	var id = container.is('.shiny-bound-input, .multistringinput') ? container.attr('id') : container.find('.shiny-bound-input').attr('id');

	if(id in sdtooltips)
	{
		setTimeout(function(){
			if(container.is(':hover') && $('#'+id+'_tooltip').length==0) 
			{
				$('.sdtooltip').remove();	
				var offset = container.offset();
				var top = offset.top + container.outerHeight() + 9;
				var left = Math.max(10, offset.left + container.outerWidth()/2 - 100);
				
				if(container.find('.progress').length > 0)
				{
					top = container.find('.progress').offset().top + 12;
				}
				
				var tt = $('<div>')
					.addClass('sdtooltip')
					.attr({'id': id+'_tooltip'})
					.html(sdtooltips[id])
					.css({'position':'absolute','left': left+'px', 'top': top+'px','width':Math.max(container.outerWidth(),220) +'px'});
				
				$('body').append(tt);
				
			}
		},300);
	}
}

removetooltip = function(e)
{
	var container = $(e.target).closest('.shiny-input-container, .btn.shiny-bound-input');
	var id = container.is('.shiny-bound-input, .multistringinput') ? container.attr('id') : container.find('.shiny-bound-input').attr('id');
	$('#'+id+'_tooltip').remove();
}



get_input_groups = function(selector)
{
  var $obj = $();
  $(selector).each(function()
  {
    var me = $(this);
    var container = me.closest('.form-group');
	if(container.length === 0)
	{
		var lbl = $('[for=' + this.id + ']');
		if(lbl.length > 0)
		{
			container = common_ancestor(lbl, me);
		}
	}
	$obj = $obj.add((container.length === 0 ? me : container));
  });
  return $obj;
};

hide_inputs = function(selector) {get_input_groups(selector).hide()}

show_inputs = function(selector) {get_input_groups(selector).show()}


// custom table of input elements
var inputlistBinding = new Shiny.InputBinding();
$.extend(inputlistBinding, {
  find: function(scope) {
    return $(scope).find("div.inputList");
  },
  getValue: function(el) {
	return $(el).serializeObject();
  },
  setValue: function(el, value) {
    $.each(value,function(name, val){
		$(el).find('input[name="' + name + '"]').val(val);
	});
  },
  subscribe: function(el, callback) {
    $(el).on("change.inputList", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off("div.inputList");
  },
  receiveMessage: function(el, message) {
	var me = $(el);
	//console.log(message)
	if(message.hasOwnProperty('fieldset'))
	{
		me.find('table').remove();	
		me.append('<table><tbody>' +
							$.map(message.fieldset, function(val,nm)
							{
								var tr = val.type == 'hidden' ? '<tr style="display:none">' : '<tr>'
								return tr + '<td>'+nm+':</td><td><input name="'+nm+'" type="'+val.type+'"/></td></tr>';
							}).join('') +
						'</tbody></table>');
	}
	if(message.hasOwnProperty('value'))
	{
		$.each(message.value, function(nm, val)
		{
			me.find('input[name="'+nm+'"]').val(val);
		});
	}
  }
});

Shiny.inputBindings.register(inputlistBinding);


// custom multi toggle button
var mtoggleBinding = new Shiny.InputBinding();
$.extend(mtoggleBinding, {
  find: function(scope) {
    return $(scope).find("div.multi-toggle-input");
  },
  getValue: function(el) {
	return $(el).find('button.btn-primary').data('value');
  },
  setValue: function(el, value) {
    $(el).find('button').removeClass('btn-primary');
	$(el).find('button[data-val="'+value+'"]').addClass('btn-primary');
  },
  subscribe: function(el, callback) {
    $(el).on("change", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off("div.multi-toggle-input");
  }
});

Shiny.inputBindings.register(mtoggleBinding);

// custom range as two numeric inputs
var erangeBinding = new Shiny.InputBinding();
$.extend(erangeBinding, {
  find: function(scope) {
    return $(scope).find("div.e-range-input");
  },
  getValue: function(el) {
    return [parseInt($(el).find('input').first().val()), parseInt($(el).find('input').eq(1).val())];
  },
  setValue: function(el, value) {
    $(el).find('input').first().val(value[0]);
    $(el).find('input').eq(1).val(value[1]);
  },
  subscribe: function(el, callback) {
    $(el).on("change.e-range-input", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off("div.e-range-input");
  }
});

Shiny.inputBindings.register(erangeBinding);

// custom multitextpinut



$(function()
{
  // actually disable disabled elements
  $('body').on('click', '.disabled', function(e) {
      e.preventDefault();
      return false;
  });
  
  // id for body to use in shinyjs
  $('body').attr('id','doc-body');
  
  //insert a quit button
  var navlist = $('ul.navbar-nav');
  if(navlist.length==1) //otherwise don't know what to do, so no quit button
  {
	var rmenu = $('<ul class="nav navbar-nav navbar-right">');
	
	$('<li data-fs="false"><a id="full_screen"><span class="glyphicon glyphicon-fullscreen"></span> Fullscreen</a></li>')
		.click(function(){
			if($(this).data('fs'))
			{
				$(this).data('fs', false);
				if (document.exitFullscreen) {
					document.exitFullscreen();
				} else if (document.mozCancelFullScreen) { /* Firefox */
					document.mozCancelFullScreen();
				} else if (document.webkitExitFullscreen) { /* Chrome, Safari and Opera */
					document.webkitExitFullscreen();
				} else if (document.msExitFullscreen) { /* IE/Edge */
					document.msExitFullscreen();
				}
			} else
			{
				$(this).data('fs', true);
				var elem = document.documentElement;
				if (elem.requestFullscreen) {
					elem.requestFullscreen();
				} else if (elem.mozRequestFullScreen) { /* Firefox */
					elem.mozRequestFullScreen();
				} else if (elem.webkitRequestFullscreen) { /* Chrome, Safari & Opera */
					elem.webkitRequestFullscreen();
				} else if (elem.msRequestFullscreen) { /* IE/Edge */
					elem.msRequestFullscreen();
				}
			}
		})
		.appendTo(rmenu);
  
	
    $('<li><a id="app_quit"><span class="glyphicon glyphicon-log-out"></span> Quit</a></li>')
		.click(function(){Shiny.onInputChange('quit_application', true);setTimeout(function(){window.open('','_self').close()},300)})
		.appendTo(rmenu);
	
	rmenu.insertAfter(navlist);
  }
  /*
  if(navlist.length==1) //otherwise don't know what to do, so no quit button
  {
    $('<ul class="nav navbar-nav navbar-right"><li><a id="app_quit"><span class="glyphicon glyphicon-log-out"></span> Quit</a></li></ul>')
		.click(function(){Shiny.onInputChange('quit_application', true);setTimeout(function(){window.open('','_self').close()},300)})
		.insertAfter(navlist);
  }
*/
  $('#oplm_btn').click(function()
  {
	$('#example_datasets').hide();
    var i = $('#oplm_inputs');
    if ( i.is( ":hidden" ) ) {
      i.slideDown();
    } else {
      i.slideUp();
    }
  });

  $('#example_dts_btn').click(function()
  {
	$('#oplm_inputs').hide();
    var i = $('#example_datasets');
    if ( i.is( ":hidden" ) ) {
      i.slideDown();
    } else {
      i.slideUp();
    }
  });

  $(document).on('change','.e-range-input input:first-child', function(e)
  {
    //$(this).next().attr('min',$(this).val());
    if(parseFloat($(this).next().val()) < parseFloat($(this).val()))
    {
      $(this).next().val($(this).val());
      $(this).trigger('change');
    }    
  });
  $(document).on('change','.e-range-input input:nth-child(2)', function(e)
  {
    //$(this).prev().attr('max',$(this).val());
    if(parseFloat($(this).prev().val()) > parseFloat($(this).val()))
    {
      $(this).prev().val($(this).val());
      $(this).trigger('change');
    }    
  });


  $(document).on('click','.toggle-button-group button', function(e)
  {
    $(this).closest('.toggle-button-group').find('button')
        .addClass('btn-default').removeClass('btn-primary');
    
    $(this)
      .removeClass('btn-default').addClass('btn-primary');
	$(this).trigger('change');
  });
  
  $(document).click(function(){$('.tooltip').remove()});
  
  $('body').on('mouseenter','.selectize-dropdown', function(e)
  {
    $('.tooltip').remove();
    return(false);
  });
  
  // multistringinput behavior
  $('body').on('click','.multistringinput button.add-field', function(e){
	var new_input = $(this).parent().find('input[type=text]').first().clone();
	new_input.val("");
	new_input.insertBefore($(this));	
  });
  
  $('body').on('keydown','.multistringinput input[type=text]', function(e){
	var i = $(this);
	if( (e.keyCode == 8 || e.keyCode == 46) && i.val() == "" && i.parent().find('input[type=text]').length > 1)
	{
		event.preventDefault();
		i.remove();
	}
  });
  
  $('body').on('change','.multistringinput input[type=text]', function(e){
	var inputs = $(this).parent().find('input[type=text]');
	var id = $(this).closest('div.shiny-input-container').attr('id');
	
	Shiny.onInputChange(id, inputs.map(function(i,e){return $(e).val()}).get().filter((word) => word.length > 0));
  });  
  // end multistringinput

  $('body').on('change','.shiny-color-picker', function(e)
  {
    Shiny.onInputChange(this.id, $(this).val());
  });
  
  //trigger change on custom elements 
   $(document).on('shiny:sessioninitialized', function(event) {
    $('.shiny-color-picker').trigger('change');
	$('.multistringinput input:first-child').trigger('change');
  });
  
  $('#example_datasets div[data-dataset]').click(function(e){
	Shiny.onInputChange('example_datasets',$(this).data('dataset'));
  });
  
  
  $(document).on("click", ".img-select-scrollbody img", function(e) {
   if($(this).hasClass('disabled') || $(this).hasClass('active')) return false; 
   // don't process click on active since we don't unselect
   // multiple is not supported at this point
   if($(this).closest('div.img-select').data('multiple'))
   {
     $(this).toggleClass('active'); 
   }
   else
   {
      if($(this).hasClass('active'))
      {
        $(this).removeClass('active');
      }
      else
      {
        $(this).closest('.img-select-scrollbody').find('img').removeClass('active');
        $(this).toggleClass('active');
      }
   }
   $(this).closest('.img-select-scrollbody').find('div.img-select-grouping').each(function(){
     if($(this).find('img.active').length === 0) {$(this).removeClass('active')}
     else {$(this).addClass('active')}
   });
   
   $(this).trigger('img-select-update');
  });
  

   // live update of footer plots
  Shiny.addCustomMessageHandler("update_footplot",
    function(message){
		update_footplot(message.jqstring, message.html);
    }
  );


  // make some information from the dexter db available for javascript
  Shiny.addCustomMessageHandler("set_js_vars",
    function(message){
      $.each(message.data, function(i,e){
        shinydexter[i] = e;
      });
      //shinydexter.ph_var_autocomplete = {};
    }
  );

  Shiny.addCustomMessageHandler("predicate_suggestion",
    function(message){
      var sg = message.suggestions;
      if(!Array.isArray(sg)){
        sg = [sg];
      }
      shinydexter.ph_var_autocomplete[message.variable][message.start](sg);
    });



  // init the predicate helpers
  $('.predicate-with-help input, .predicate-with-help textarea')
	.autogrow({maxHeight:150,reset_on_leave:true})
	.predicate_helper();

  // init the dt editable
  $('div.datatables.editable').dt_editable();


  // all else is concerned with the implementation of the plotslider
  
  Shiny.addCustomMessageHandler("updateSlider",
    function(message) {
      var me = $('#' + message.id);	  
	  var slider = me.find('div.slider');
      
	  if(message.hasOwnProperty('error'))
	  {
		slider.empty();
		me.find('.alert').css('display','initial').text(message.error);		
	  } else
	  {
		me.find('.alert').css('display','none').text('');
	  }
	  
	  if(message.hasOwnProperty('data'))
	  {		  
		  slider.empty();
		  slider.css('left','0');
		  $.each(message.data, function(i,e)
			{
			  var img = $('<img>').attr('src', e.src).attr('image_id', e.image_id);
				slider.append(img);
			});
		  //me.removeClass('uninitialized');
		  indx=0;
		  if(!message.hasOwnProperty('selected'))
		  {
			me.data('index',0);
			me.find('.slider img').eq(0).click();		  
		  }
		  
	  }
	  if(message.hasOwnProperty('selected'))
	  {
		slider.find('img').each(function(i,e)
		{
			if($(this).attr('image_id') == message.selected)
			{				
				me.data('index',i);
				me.find('.slider img').eq(i).click();	
				return(false);
			}
		});
	  }
	  slider.find('img').length>0 ? me.removeClass('uninitialized') : me.addClass('uninitialized');
	  
	  
    }
  );


  $('div.plot_slider .slider, div.select_slider .slider')
    .draggable({axis:'x', start:function(){$(this).data('dragged',true)} })
    .on('mousedown', function(){$(this).data('dragged',false)});


  $('.slider').on('click','img', function(e){

    var me = $(this);
    var slider = me.closest('div.slider');
    var i = slider.find('img').index(me);
    var w = me.outerWidth(true);
    var plot_slider = me.closest('div.plot_slider, div.select_slider');
    if(slider.data('dragged')) {return(false)}

    Shiny.onInputChange(plot_slider.attr('id') + '_select', me.attr('image_id'));

    plot_slider.data('index', i);

    slider.find('img').removeClass('plot_active');
    var offset = Math.min((slider.parent().width()/2) - i * w - 0.5 * w,0);
    slider.animate({left:offset + 'px'}, 400);
    me.addClass('plot_active');
  });


   $('div.plot_slider').on('click','.plot_right', function(e){
    var me = $(this);
    var plot_slider = me.closest('div.plot_slider');
    var i = plot_slider.data('index');
    var thumbs = plot_slider.find('.slider img');
    plot_slider.find('div.slider').data('dragged',false);
    if(i<thumbs.length)
    {
      thumbs.eq(i+1).trigger('click');
    }
   });


   $('div.plot_slider').on('click','.plot_left', function(e){

    var me = $(this);
    var plot_slider = me.closest('div.plot_slider');
    var i = plot_slider.data('index');
    var thumbs = plot_slider.find('.slider img');

    plot_slider.find('div.slider').data('dragged',false);
    if(i>0)
    {
      thumbs.eq(i-1).trigger('click');
    }
   });


  $('div.plot_slider, div.select_slider').on('click','.slide_right', function(e){
    var me = $(this);
    var slider = me.closest('div.plot_slider, div.select_slider').find('.slider');

    var offset = parseInt(slider.css('left')) - slider.width()/slider.find('img').length;
    var min = slider.parent().width() - slider.width();
    slider.animate({left: Math.max(offset,min) + 'px'},400);
  });


  $('div.plot_slider, div.select_slider').on('click','.slide_left', function(e){
    var me = $(this);
    var slider = me.closest('div.plot_slider, div.select_slider').find('.slider');

    var offset = parseInt(slider.css('left')) + slider.width()/slider.find('img').length;
    slider.animate({left: Math.min(offset,0) + 'px'},400);
  });

  
  $('div.plot_slider .slider, div.select_slider .slider').on('mousewheel', function(event) {
	var me = $(this);
	if(me.data('wheelbusy') === true) return(false);
	me.data('wheelbusy', true);
    var slider = me.closest('div.plot_slider, div.select_slider').find('.slider');
	var min = slider.parent().width() - slider.width();
	var move = 0;
	
	if(event.deltaX == -1) move=-1.1 * event.deltaFactor
	else if(event.deltaX == 1) move=1.1*event.deltaFactor
	else if(event.deltaY == -1) move=-1.1*event.deltaFactor
	else if(event.deltaY == 1) move=1.1*event.deltaFactor;
	
	var offset = Math.min(Math.max(parseInt(slider.css('left')) + move,min),0);
	

	slider.animate({left: offset + 'px'},50,'linear', function(){me.data('wheelbusy', false);} );	
	return(false);
  });


	$('body').on('mouseenter','.shiny-input-container, .btn.shiny-bound-input' , showtooltip);
	$('body').on('mouseleave mouseup','.shiny-input-container, .btn.shiny-bound-input', removetooltip);



});
