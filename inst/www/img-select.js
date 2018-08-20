
var ims_binding = new Shiny.InputBinding();

$.extend(ims_binding, {
  find: function(scope) {
    return $(scope).find("div.img-select");
  },
  getId: function(el){
    return el.id;
  },
  getValue: function(el) {
    var s = $(el).find('.img-select-scrollbody img.active');
    return {value: s.data('value'), group: s.data('group')};
  },
  setValue: function(el, value) {
    $(el).find('img[value="' + value + '"').trigger('click');
  },
  subscribe: function(el, callback) {
    $(el).on("img-select-update", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off("img-select-update");
  },
  receiveMessage: function(el, message) {
    var me = $(el);
    var b = me.find('div.img-select-scrollbody');
    
    if(message.hasOwnProperty('group_options')) me.data('group_options', $.extend(me.data('group_options'), message.group_options));
    
    if(message.hasOwnProperty('choices'))
    {
      var container, grps, l = message.choices.length;
      b.empty();

      for(var i=0; i < l; i++)
      {
        if(Array.isArray(message.choices[i].group))
        {
          grps = message.choices[i].group;
          message.choices[i].group = grps.shift();
          grps.forEach(function(e)
          {
            message.choices.push(Object.assign({}, message.choices[i], {group:e}));
          });
        }
      }
      
      $.each(array_groupBy(message.choices, 'group'), function(i, vals)
      {
        container = $('<div>')
          .addClass('img-select-grouping')
          .data('group', vals.key)
          .append('<div>');

        $.each(vals.values, function(i,e)
        {
          container.append(
            $('<img>')
              .attr('src', e.src)
              .data('value', e.choice_id)
              .data('group', e.group)
              .css({height: me.data('heightChoice'), width: me.data('widthChoice')})
          );
        });
        b.append(container);
      });
    }
    
    b.find('div.img-select-grouping').each(function()
    {
      var g = $(this);
      if(me.data('group_options').hasOwnProperty($(this).data('group')))
      {
        $.each(me.data('group_options')[$(this).data('group')], function(k,v)
        {
          if(k === 'class'){g.attr(k,'img-select-grouping ' + v);}
          else if(k === 'label'){ g.find('div:first-child').text(v)}
          else g.attr(k,v);
        });
      }
    });
    
    if(message.hasOwnProperty('selected')) 
    { 
      if(Array.isArray(message.selected))
      {
        b.find('img')
          .filter(function(){return $(this).data('value') == message.selected[0] && $(this).data('group') == message.selected[1]})
          .eq(0).trigger('click');
      }
      else
      {
        b.find('img').filter(function(){return $(this).data('value') == message.selected}).eq(0).trigger('click');
      }
    }
  }
});

Shiny.inputBindings.register(ims_binding);

