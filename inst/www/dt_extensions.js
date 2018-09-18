
  
// make it possible to read the search filter in a datatable
// shiny only makes this possible by forcing you to save and reload the entire state
// hence this workaround
// for completeness included setValue(not tested)
var dtSearchBinding = new Shiny.InputBinding();

$.extend(dtSearchBinding, {
  find: function(scope) {
    return $(scope).find(".dataTables_filter input");
  },
  getId: function(el){
    return $(el).closest('div.datatables.shiny-bound-output').attr('id') + '_search';
  },
  getValue: function(el) {
    return $(el).val();
  },
  setValue: function(el, value) {
    $(el).closest('table.dataTable').DataTable({retrieve:true}).search(value).draw();
  },
  subscribe: function(el, callback) {
    $(el).on("change", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off("change");
  }
});

Shiny.inputBindings.register(dtSearchBinding);


// make it possible to read from datatables
// useful for datatables that have been made editable in some way
var dtread_binding = new Shiny.InputBinding();

$.extend(dtread_binding, {
  find: function(scope) {
    return $(scope).find("div.datatables.html-widget.readable");
  },
  getId: function(el){
    return el.id + '_data';
  },
  getValue: function(el) {
    if($(el).find('.dataTable').length === 0){ return(null)}
    else 
    { 
      // unfortunately the shiny datables version is rather old and the column interface does not work
      // so columns have no retrievable names
      // use header instead but this is a bit awkward if the fixedcolumns extension is also used
	  // hack is to omit classnames containing 'clone'
	  var dt = $(el).find('.dataTable').filter(function(i,el){return(el.className.match(/clone/i) === null)}).DataTable({retrieve:true});
	  
      var colnames = $(dt.columns().header()).map(function(i,e)
        {
          var n = $.trim($(this).text());
          if(n === '') n = 'V' + (i+1); 
          return n;
        }
        ).get();
      if(colnames.nthIndexOf(colnames[0],2) > -1)
      {
        colnames = colnames.slice(0,colnames.nthIndexOf(colnames[0],2));
      }
      var res = {};
	  console.log(colnames)
      $.each(colnames , function(i,n)
      {
        res[n] = dt.column(i, {page: 'all'}).data().toArray();
      });
      return res;
    }
  },
  setValue: function(el, value) {
    //do nothing
  },
  subscribe: function(el, callback) {
    $(el).on("datatable_change", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off("datatable_change");
  }
});

Shiny.inputBindings.register(dtread_binding);

// also send updates directly
jQuery(function()
{
  $(document).on("datatable_change", function(e, row, col_index)
  {
    var id = $(e.target).closest('div.datatables.shiny-bound-output').attr('id') + '_user_update';
    Shiny.onInputChange(id, {row: row, col_index: col_index + 1});
  });
});




/* *********** jquery extension datatable editable ************** */

(function ( $ ) {
  $.fn.dt_editable = function()
  {
    return this.each(function()
    {
      var me = $(this), 
        selector;
  
      if(me.data('editableColumns') === 'all')
      {
        selector = 'table.dataTable tbody td';
      } else 
      {
        selector = $.map( (me.data('editableColumns') + '').split(','), function(e,i) 
          { 
            if(/^\d+$/.test(e.trim()))
            {
              return 'table.dataTable tbody td:nth-child(' + e + ')';
            } 
            else if(/^\:\d+$/.test(e.trim()))
            {
              return 'table.dataTable tbody td:nth-child(-n+' + e.replace(':','') + ')';
            }
            else if(/^\d+\:$/.test(e.trim()))
            {
              return 'table.dataTable tbody td:nth-child(n+' + e.replace(':','') + ')';
            } 
            else if(/^\d+\:\d+$/.test(e.trim()))
            {
              return $.map(range(parseInt(e), parseInt(e.split(':')[1])+1), function(e,i)
                            {
                              return 'table.dataTable tbody td:nth-child(' + e + ')';
                            }
                          ).join(',');
            }
          }).join(',');
      }

      me.on('click', selector, function(e)
      { 

        var td = $(this);
        var dt = td.closest('table.dataTable').DataTable({retrieve:true});
        var id = td.closest('div.datatables.html-widget.html-widget-output').attr('id');
        
        if(td.find('input').length > 0){
          return(false);
        }
        
        var cell = dt.cell(td);
        var old_data = cell.data();
        var editor = $('<input class="dt-editor-field">');
        if(typeof(old_data) === 'number') 
		{
			// for some reason cannot get this to work in firefox
			editor.attr('type','number');				
			editor.width(td.width() + 20);
			editor.css('right','-20px');
			td.css('max-width',td.width() + 'px');
        }
		else
		{
			editor.width(td.width()-2)
		}
		editor.val(old_data);
        editor.css('position','relative');        
        editor.blur(function(e)
        {
          var val = typeof(old_data) === 'number' ? parseFloat($(this).val()) : $(this).val();
          if(val != old_data )
          {
            var tb = $(this).closest('table.dataTable');
            // calling cell.data will nullify the cell reference for some reason 
            // so we need to acquire a pointer to the row before we do that
            var row = cell.row($(this).closest('tr'));
            //console.log(cell)
            cell.data(val);
            if(dt.page.info().serverSide)
            {
              //row.draw('page');
              //dependent on server to save the state
              //otherwise change will be removed on paging and other redraws
            }
            else 
            {
              row.invalidate().draw('page');
            }
            
            // important to trigger on datatable and not on cell  because the td elements get replaced frequently 
            // and can be detached at the moment of sending a trigger
            tb.trigger('datatable_change', [row.data(), cell.index().column]);
          }
          else
          {
            td.text($(this).val());
          }
		  td.css('max-width','')
          $(this).remove();
        });
    
        td.empty().append(editor);
		
		if(isFirefox)
		{	
			// ff bug with focus firing blur
			setTimeout(function(){editor.focus()},100);
		} 
		else
		{
			editor.focus();
		}	
		
        });
    });
  };
}( jQuery ));



/* *********** sparkcount for datatable *********** */

(function ( $ ) {
  // to do: this can be done much easier: https://rstudio.github.io/DT/010-style.html
  $.fn.sparkcount = function(options)
  {
    var opts = $.extend( {}, $.fn.sparkcount.defaults, options );
    
    return this.each(function()
    {
      var me = $(this);
      var vals = me.text().split(',');
      var cnt = parseInt(vals[0]);
      var N = parseInt(vals[1]);
      me.text('');
      var canv = $('<canvas width="' + opts.width + 'px" height="' + opts.height + 'px">').appendTo(me).get(0);
      var ctx = canv.getContext('2d');
      
      // ctx.clearRect(0, 0, opts.width , opts.height);
      ctx.fillStyle = opts.fill;
      // ctx.fillRect(0,2, opts.width * cnt/N ,opts.height-4);
      ctx.fillRect(0,0, opts.width * cnt/N ,opts.height);
      
      me.attr('title', 'count: ' + cnt);
      
    });
  };
  
  $.fn.sparkcount.defaults = {width: 50, height:16, fill: 'lightgrey'};
}( jQuery ));    

/* *********** sparklegend for datatable *********** */

(function ( $ ) {
  $.fn.sparklegend = function(options)
  {
    var opts = $.extend( {}, $.fn.sparklegend.defaults, options );
    
    return this.each(function()
    {
      var me = $(this);
      var color = me.text();
      me.text('');
      
      var canv = $('<canvas width="' + opts.width + 'px" height="' + opts.height + 'px">').appendTo(me).get(0);
      var ctx = canv.getContext('2d');
      var cy = Math.round(opts.height * 0.5);
      var cx = Math.round(opts.width * 0.5);
      
      ctx.clearRect(0, 0, opts.width , opts.height);
      ctx.fillStyle = color;
      ctx.strokeStyle = color;
      ctx.moveTo(0,cy);
      ctx.lineTo(opts.width, cy);
      ctx.stroke();

      ctx.beginPath();
      ctx.arc(cx,cy,3,0,2*Math.PI);
      ctx.fill();
      return this;
    });
  };
  
  $.fn.sparklegend.defaults = {width: 50, height:16, shape: 'dotline'};
}( jQuery ));  

// heuristic for minimizing datatable width based on header and contents of the first page
dtshrink = function(dtsettings)
{
  var w = 0;
  var api = new $.fn.dataTable.Api(dtsettings);
  var container = $(api.table().container());
  var outp = container.closest('div.shiny-bound-output');
  var fake = container.clone(false).css({'visibility':'hidden', 'width':'50px'}).appendTo(document.body);
  fake.find('div.dataTables_scrollHeadInner, div.dataTables_scrollFootInner').css('width','');
  fake.find('table.dataTable').each(function()
  {
    w = Math.max(w, $(this).css({'white-space':'nowrap','width':''}).outerWidth(true));
  });
  
  fake.remove();
  outp.css('max-width',(w + 25)+'px'); 
  api.draw();
};


dt_numcol = function(dtsettings){
  var api = new $.fn.dataTable.Api(dtsettings);
  var container = $(api.table().container());
  
  container.find('td.numeric').each(function()
  {
    var t = $(this).text().split('.');

    if(t[0].indexOf('\u2008') >=0){ return }

    if(t[0].length > 3)
    {
      t[0] = t[0].trim().split('').reverse().join('').replace(/(\d{3})/g,"$1\u2006").split('').reverse().join('');
      $(this).text(t.join('.').trim());
    }
  });
  
  container.find('td.dec-1').each(function()
  {
    var t = $(this).text().trim();
    
    if(! /\.\d$/.test(t))
    {
      $(this).text(t + '\u2008\u2007');
    } else
    {
      $(this).text(t);
    }
  });
  
  container.find('td.dec-2').each(function()
  {
    var t = $(this).text().trim();

    var n = t.indexOf('.');
    if(n <0){
      $(this).text(t + '\u2008\u2007\u2007');
    } else 
    {
      $(this).text(t + '\u2007'.repeat(n + 3 - t.length));
    }
  });
  
  container.find('td.dec-3').each(function()
  {
    var t = $(this).text().trim();
    var n = t.indexOf('.');
    if(n <0){
      $(this).text(t + '\u2008\u2007\u2007\u2007');
    } else 
    {
      $(this).text(t + '\u2007'.repeat(n + 4 - t.length ));
    }
  });  
  

};


dt_btn_dropdown = function(dtsettings){
  var api = new $.fn.dataTable.Api(dtsettings);
  var btn = $('<a href="#"><span class="glyphicon glyphicon-floppy-disk"/></a>')
    .click(function(e){
      $(this).next().toggleClass('hidden');
    });

  // hide shiny download buttons
  $(api.table().container())
    .closest('.datatables.shiny-bound-output')
    .next('.full_download_buttons')
    .hide();

  var dt_buttons = $(api.table().container())
    .find('div.dropdown')
    .prepend(btn)
    .find('div.dt-buttons')
    .addClass('hidden');
  
  $('<span class="scope">this page</span>').insertBefore(dt_buttons.find('a:first-child'));  
  $('<span class="scope">all data</span>').insertBefore(dt_buttons.find('a.full-download').first());  
};


    
dt_show_row = function(dtsettings, rownum)
{
  var api = new $.fn.dataTable.Api(dtsettings);
  var page_info = api.table().page.info();
  // if not already on right page
  if(rownum < page_info.start || rownum > page_info.end ) 
  {
      var page_to_display = Math.floor( rownum / api.table().page.len() );

      api.table().page( page_to_display );
      //horrible but this is the only thing that works
      setTimeout(function(){api.table().draw('page')},50); 
      
  }
}

dt_add_column_btn = function(dtsettings){
	var api = new $.fn.dataTable.Api(dtsettings);	
	var flt = $(api.table().container()).find('div.dataTables_filter');

	if(flt.find('div.dt-add-col').length==0)
	{
		flt.append('<div class="dt-add-col"><a><span class="glyphicon glyphicon-plus"></span></a></div>');
		flt.find('.dt-add-col a').click(dt_toggle_addcol_dialog);
	}
}

dt_toggle_addcol_dialog = function(e)
{
	if($(this).parent().find('div.dt-add-col-dialog').length>0)
	{
		$(this).parent().find('div.dt-add-col-dialog').remove();
		$(this).removeClass('dlg-open');
	}
	else
	{
		var dlg = $('<div class="card dt-add-col-dialog">' +
					'<div class="card-header">Add column</div>' +
					'<div class="card-body"><form><p><table><tbody>' +
					'<tr><td>name</td><td><input class="form-control" name="prop_name"/><div class="invalid-feedback" style="margin-left:1em;">Not a valid column name</div></td></tr>'+
					'<tr><td>type</td><td><select class="form-control" name="prop_type"><option value="character">text</option><option value="integer">integer number</option><option value="double">decimal number</option></select></td></tr>'+
					'<tr><td>default value</td><td><input class="form-control" name="prop_dflt"/></td></tr>'+
					'</tbody></table></p></form>'+
					'<p><a href="#" class="btn btn-primary ok-btn">Ok</a><a href="#" class="btn btn-default can-btn">Cancel</a></p></div></div>')
			.appendTo($(this).parent());
		
		$(this).addClass('dlg-open');
		
		dlg.find('select').change(function(e){
			var me = $(this);
			var dfl = dlg.find('input[name="prop_dflt"]');
			if(me.val() == 'character')
			{
				dfl.attr('type','text').val('');
			} else
			{
				dfl.attr('type','number').val(0);
			}			
		});
		
		dlg.find('.can-btn').click(function(){dlg.closest('div.dt-add-col').find('.dlg-open').removeClass('dlg-open'); dlg.remove();});
		dlg.find('.ok-btn').click(function(){
			var nm_inp = $(dlg).find('input[name="prop_name"]');
			if(nm_inp.val().trim().match(/^\D/) !== null)
			{
				var dt_id = dlg.closest('div.datatables.shiny-bound-output').attr('id');
				Shiny.onInputChange(dt_id+'_add_column', dlg.serializeObject());
			}
			else
			{
				nm_inp.addClass('invalid');
				return false;
			}		
		});
		
	}


}


draw_dt_footer = function(dtsettings){
	var plot_height=100;
	var api = new $.fn.dataTable.Api(dtsettings);	
	
// to do: maybe try https://github.com/flot/flot, easier axis

	plot_footer = function(dtsettings)
	{
		var api = new $.fn.dataTable.Api(dtsettings);	
		var cont = $(api.table().container());
		if(cont.find('tfoot.dt-footer-plots').length>0)
		{
			if(cont.find('tfoot.dt-footer-plots tr:first-child').height()<100)
				cont.find('tfoot.dt-footer-plots tr:first-child').height(100);	
	
			// bugfix for footer misalignment with scroll
			/*
			if(cont.find('.dataTables_scrollBody').length > 0)
			{
				var widths = $.map(cont.find('.dataTables_scrollBody tr[role="row"]:first-child td'), function(e,i) {return($(e).outerWidth())});
				console.log(cont.find('.dataTables_scrollBody tr[role="row"]:first-child td'))
				
				cont.find('.dataTables_scrollFootInner table').css('table-layout','fixed');		
				cont.find('.dataTables_scrollFootInner colgroup').remove();
				cont.find('.dataTables_scrollFootInner table').append('<colgroup>'+$.map(widths,function(e,i){return '<col style="width:'+e+'px">'}).join('')+'</colgroup>');
				cont.find('.dataTables_scrollFootInner tfoot.dt-footer-plots tr:first-child td').each(function(i,e){ $(this).width(widths[i])})
			}
			*/
		}
		cont.find('tfoot.dt-footer-plots td').each(function(i,e){update_footplot(e)});

	}	
	plot_footer(dtsettings)
		
	$(api.table().container()).on( 'column-sizing.dt', function ( e, settings ) {
		var api = new $.fn.dataTable.Api(settings);			
		plot_footer(settings);
		setTimeout(function(){api.table().draw('page')},50); 
	});	
}



update_footplot = function(td, html)
{
	td = $(td);

	
	if(typeof html !== "undefined")
	{
		var tmp = $(html);
		td.empty().append(tmp.children());		
		td.attr('style',tmp.attr('style'))
	}
	
	td.find('div.sparkhist').each(function()
	{
		var me = $(this);
			
		me.find('canvas').remove();
		
		var width = me.closest('td').innerWidth();

		var values = me.data('values');
		var nbars = values.length;
		var barw = Math.floor(width/nbars);
		width = barw * nbars;
		
		
		var height = Math.min(width,100);
		var axis = false
		
		if(height>50)
		{
			height = height-15;
			axis=true;		
		}

		me.sparkline(values, 
			{type: 'bar', barColor: '#bfb5b6', barSpacing: 0, zeroAxis: false, barWidth: barw, height:height+'px',width:width+'px'});
			
			
		if(axis)
		{
			var mn =  parseFloat(me.data('min'))
			var mx =  parseFloat(me.data('max'))
			
			var ticks = 4;
			
			var canv = $('<canvas width="' + width + 'px" height="15px">').appendTo(me).get(0);

			var ctx = canv.getContext('2d');
			ctx.lineWidth=1;
			ctx.strokeStyle="#404040";
			ctx.fillStyle="#404040";
			ctx.clearRect(0, 0, width , 15);
			ctx.moveTo(0,2.5);
			ctx.lineTo(width, 2.5);
			ctx.stroke();
			ctx.textBaseline="top"; 
			ctx.font="8px Arial"
				
			var tickx;
			for(var i=0;i<=ticks-1;i++)
			{
				if(i==0){ ctx.textAlign="start"}
				else if(i==ticks-1){ ctx.textAlign="end"}
				else ctx.textAlign="center";
				tickx = Math.max(Math.min(Math.round(width*i/(ticks-1))+.5,width-.5),1.5)
				ctx.moveTo(tickx,2.5);
				ctx.lineTo(tickx, 4.5);
				ctx.stroke();
				ctx.fillText(Math.round(mn+i*(mx-mn)/(ticks-1)),tickx,4.5);
			}
				
				
		}
		me.find('canvas').css('display','block');
	});

	
	td.find('div.sparkdensity').each(function()
	{
		var me = $(this);
		me.find('canvas').remove();
			
		var width = me.parent().width();
		var values = me.data('values');
					
		var height = Math.min(width,100);
		var axis = false

		if(height>50)
		{
			height = height-16;
			axis=true;		
		}

		me.sparkline(values, 
			{type: 'line', spotColor: '', minSpotColor: '',maxSpotColor: '', height:height, width:width,
				numberFormatter:function(x){return(x.toFixed(3))}
			});

		if(axis)
		{
			var mn =  parseFloat(me.data('min'))
			var mx =  parseFloat(me.data('max'))
				
			var ticks = 4;
			
			var canv = $('<canvas width="' + width + 'px" height="14px">').appendTo(me).get(0);
			var ctx = canv.getContext('2d');
			ctx.lineWidth=1;
			ctx.strokeStyle="#404040";
			ctx.fillStyle="#404040";
			ctx.clearRect(0, 0, width , 15);
			ctx.moveTo(0,2.5);
			ctx.lineTo(width, 2.5);
			ctx.stroke();
			ctx.textBaseline="top"; 
			ctx.font="8px Arial"
				
			var tickx;
				
			if(mx-mn>ticks)
			{
				var numformat = Math.round
			}
			else
			{
				var numformat = function(x){return x.toFixed(1)}
			}
			for(var i=0;i<=ticks-1;i++)
			{
				if(i==0){ ctx.textAlign="start"}
				else if(i==ticks-1){ ctx.textAlign="end"}
				else ctx.textAlign="center";
				tickx = Math.max(Math.min(Math.round(width*i/(ticks-1))+.5,width-.5),1.5);
				ctx.moveTo(tickx,2.5);
				ctx.lineTo(tickx, 4.5);
				ctx.stroke();
				ctx.fillText(numformat(mn+i*(mx-mn)/(ticks-1)),tickx,4.5);
			}
		}
	});
	
	td.find('canvas').css('display','block');	

}


