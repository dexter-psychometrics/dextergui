

// to quote (or not) strings depending on datatype of dexter variable
quote_val = function(s, variable)
{
  var tp = shinydexter.variables.type[shinydexter.variables.name.indexOf(variable)];
  
  if(tp != 'character') return(s);
  
  var n = s.length - 1;
  
  if(s[0] == s[n] == '"' || s[0] == s[n] == "'") return(s);
  if(s.indexOf('"') >= 0) return("'" + s + "'");
  return('"' + s + '"');
};


// .predicate_helper() initializes a predicate textfield
(function ( $ ) {
    var walk_groups = function(h)
    {
        var group = false;
        var odd = false;
        h.find('tr').each(function(i,e)
        {

          var me = $(this);
          if(me.hasClass('group-start')) { group = true; odd = ! odd}
          else if(me.hasClass('group-end')) { group = false}
          else if(group) {me.addClass('group-member')}
          else {me.removeClass('group-member'); odd = ! odd}
          

          if(odd) {me.addClass('odd')} else {me.removeClass('odd')}
        });
        
    };
    

    var phelper = function() 
    {
	
	  var help = '<a class="btn btn-lg" data-toggle="collapse" data-target="#help-pr-helper" href="#" style="padding:0px 5px;"><i class="fa fa-question-circle"></i></a>' +
		'<div class="collapse" id="help-pr-helper" style="text-align:left;padding:10px;font-size:13px;">You can use the fields below to make a subset of your data for analysis. ' +
		'Each line is a logical statement that omits or includes data, e.g. <b><i>booklet_id equal pretest</i></b> will select just the data from the booklet called pretest. ' +
		'You can combine multiple lines by selecting <b><i>and/or</i></b> or <b><i>all/any</i></b> from the <i>group</i> inputs at the start of each line. Alternatively, if you\'re more familiar with R, you can directly type an R statement.</div>';
	
      var variable = $('<select name="variable"/>');
      $.each(shinydexter.variables.name, function(i,e)
      {
        variable.append($('<option>').val(e).text(e));
      });
      
      var operator = $('<select name="operator"/>');
      $.each(shinydexter.operators, function(i, e)
      {
        operator.append($('<option>').val(e[1]).text(e[0]));
      });
      
      var lgoperator = $('<select name="lgoperator"/>');
      $.each(shinydexter.lgop, function(i, e)
      {
        lgoperator.append($('<option>').val(e[1]).text(e[0]));
      });
      
      var gpoperator = $('<select name="gpoperator" class="sl_group_start"/>');
      $.each(shinydexter.gpop, function(i, e)
      {
        gpoperator.append($('<option>').val(e[1]).text(e[0]));
      });

 
      var row = $('<tr>')
        .append($('<td>').append(lgoperator))
        .append($('<td>').append(gpoperator))
        .append($('<td>').append(variable))
        .append($('<td>').append(operator))
        .append('<td><input name="value"/><div class="invalid-feedback">Please provide a value</div></td>')
        .append('<td>' + 
                '<button class="btn_remove_row btn btn-default"><span class="glyphicon glyphicon-remove"/></button>' + 
                '<button class="btn_add_line btn btn-info">add line below</button>' +
                '<button class="btn_end_group btn btn-default sbs-toggle-button">end group</button>' +
                '</td>');
      

      
      var ok_btn = $('<button type="button" class="btn btn-primary">OK</button>')
        .click(function(e){
          var pred = [];
          var hlp = $(this).closest('div.pred_helper');
          var grouping = '';
          
          hlp.find('table.pred_lines tbody tr').each(function()
          {
            var vals = $(this).find('select, input').filter(function(){return($(this).css('visibility') !== 'hidden')})
              .serializeObject();

            if(vals.lgoperator !== undefined || pred.length === 0)
            {
              if(grouping !== '') pred.push(')');
              pred.push(vals.lgoperator);
              grouping = '';
              if(vals.gpoperator !== undefined &&  vals.gpoperator !== '' ){

                grouping = vals.gpoperator;
                pred.push('(');
              } 
            } 
            if(vals.gpoperator === undefined || vals.gpoperator === '') pred.push(grouping);
            pred.push(vals.operator.format(vals.variable, quote_val(vals.value, vals.variable )));
            if(vals.value === '')
            {
              $(this).find('input[name="value"]').addClass('invalid');
            }
          });
          if(grouping !== '') pred.push(')');

          if(hlp.find('.invalid').length === 0)
          {
            $('#' + hlp.data('for'))
              .val(pred.join(''))
              .trigger('change');
            hlp.hide();
          }
        });
      
      var cancel_btn = $('<button type="button" class="btn btn-default">Cancel</button>')
        .click(function(e){
          var hlp = $(this).closest('div.pred_helper');
          var bck = hlp.data('backup');
          var prnt = hlp.parent();
          hlp.remove();
          if(bck !== undefined)
          {
            bck.appendTo(prnt).hide();
          } 
        });
      
      var btns = $('<div class="modal-footer"/>')
        .append(cancel_btn)
        .append(ok_btn);
        
        
      var card = $('<div class="pred_helper card">' + 
                    '<div class="card-header">Data selection' + help + '</div>' + 
                    '<div class="card-body"><table class="pred_lines"/></div></div>')
        .append(btns);
      
      

      card.find('table')
        .append('<thead><tr><th style="min-width:51px;"/><th>group</th><th>variable</th><th>comparison</th><th>value</th><th/></tr></thead><tbody/>')
        .find('tbody')
        .append(row.clone(true))
        .on('click','.btn_remove_row', function(e)
        {
          var tb = $(this).closest('tbody');  
          $(this).closest('tr').remove();
          tb.find('tr:first-child td:first-child select').remove();
          walk_groups(tb);
        })
        .on('click','.btn_end_group', function(e)
        {
          $(this).toggleClass('btn-default').toggleClass('btn-primary')
            .closest('tr')
            .toggleClass('group-end');
          walk_groups($(this).closest('tbody'));
        })
        .on('click','.btn_add_line', function(e)
        {
          var r = row.clone(true);
          $(this).closest('tr').after(r);
          walk_groups($(this).closest('tbody'));
          r.find('select[name="variable"]').trigger('change');
          r.find('input[name="value"]')  
            .autocomplete({
              minLength: 1,
              source: function( request, response ) {
                var vrb = r.find('select[name="variable"]').val();
                shinydexter.ph_var_autocomplete[vrb] = $.extend(shinydexter.ph_var_autocomplete[vrb],{});
                shinydexter.ph_var_autocomplete[vrb][request.term] = response;
                Shiny.onInputChange('varsuggest', {variable:vrb, start: request.term});
              }});
          
        })
        .on('change','.sl_group_start', function(e)
        {
          if($(this).val() === ''){$(this).closest('tr').removeClass('group-start')}
          else {$(this).closest('tr').addClass('group-start')}
          walk_groups($(this).closest('tbody'));
        })
        .on('change','input[name="value"]', function(){ 
          if($(this).val().length > 0) $(this).removeClass('invalid');
        })
        .on('change','select[name="variable"]', function(){
          var me = $(this);
          var vartype = shinydexter.variables.type[shinydexter.variables.name.indexOf(me.val())];
          if(vartype === 'integer' || vartype === 'double'){
            me.closest('tr').find('input[name="value"]').attr('type','number');
          } else
          {
            me.closest('tr').find('input[name="value"]').attr('type','text');
          }
        });
      

      return(card);
  } ;
  
  
  $.fn.predicate_helper = function(options)
  {
     return this.each(function()
     {
       var inp = $(this).uniqueId().css('display','inline');
	   
       var btn = $('<span class="glyphicon glyphicon-edit pr-helper-button"/>')
            .click(function(e){
                if($('#' + inp.attr('id') + '-helper').length === 0)
                {
                  var hlp = phelper()
                    .attr('id', inp.attr('id') + '-helper')
                    .data('for', inp.attr('id'))
					.appendTo($(this).parent());
                 
                  
                  hlp.find('input[name="value"]')  
                    .autocomplete({
                      minLength: 0,
                      source: function( request, response ) {
                        var vrb = hlp.find('tbody tr:first-child select[name="variable"]').val();
                        shinydexter.ph_var_autocomplete[vrb] = $.extend(shinydexter.ph_var_autocomplete[vrb],{});
                        shinydexter.ph_var_autocomplete[vrb][request.term] = response;
                        Shiny.onInputChange('varsuggest', {variable:vrb, start: request.term});
                      }});
                  
                  hlp.find('select[name="variable"]').trigger('change');
                } else 
                {
                  var hlp = $('#' + inp.attr('id') + '-helper');
                  hlp.removeData('backup'); // avoid russian doll
                  hlp
                    .data('backup', hlp.clone(true))
                    .show();
                }
              });
      
       inp
        .wrap('<div>')
        .parent()
        .css({'white-space':'nowrap', position:'relative'})
        .append(btn);
        
       inp.change(function(){$('#' + this.id + '-helper:hidden').remove()});
  
       inp.css('width','calc(' + (inp.css('width') || '100%') + ' - 30px)');
     });
  };
}( jQuery ));
  










