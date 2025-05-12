
var isFirefox = typeof InstallTrigger !== 'undefined';

// browser incompatibility fixes //
String.prototype.format = function() {
    var s = this,
        i = arguments.length;

    while (i--) 
    {
        s = s.replace(new RegExp('\\{' + i + '\\}', 'gm'), arguments[i]);
    }
    return s;
};

 
if (!String.prototype.repeat) {
  String.prototype.repeat = function(count) {
    'use strict';
    if (this == null) {
      throw new TypeError('can\'t convert ' + this + ' to object');
    }
    var str = '' + this;
    count = +count;
    if (count != count) {
      count = 0;
    }
    if (count < 0) {
      throw new RangeError('repeat count must be non-negative');
    }
    if (count == Infinity) {
      throw new RangeError('repeat count must be less than infinity');
    }
    count = Math.floor(count);
    if (str.length == 0 || count == 0) {
      return '';
    }
    // Ensuring count is a 31-bit integer allows us to heavily optimize the
    // main part. But anyway, most current (August 2014) browsers can't handle
    // strings 1 << 28 chars or longer, so:
    if (str.length * count >= 1 << 28) {
      throw new RangeError('repeat count must not overflow maximum string size');
    }
    var rpt = '';
    for (var i = 0; i < count; i++) {
      rpt += str;
    }
    return rpt;
  }
}  

// object.assign patch for IE, from mozilla developers network
if (typeof Object.assign != 'function') {
  // Must be writable: true, enumerable: false, configurable: true
  Object.defineProperty(Object, "assign", {
    value: function assign(target, varArgs) { // .length of function is 2
      'use strict';
      if (target == null) { // TypeError if undefined or null
        throw new TypeError('Cannot convert undefined or null to object');
      }

      var to = Object(target);

      for (var index = 1; index < arguments.length; index++) {
        var nextSource = arguments[index];

        if (nextSource != null) { // Skip over if undefined or null
          for (var nextKey in nextSource) {
            // Avoid bugs when hasOwnProperty is shadowed
            if (Object.prototype.hasOwnProperty.call(nextSource, nextKey)) {
              to[nextKey] = nextSource[nextKey];
            }
          }
        }
      }
      return to;
    },
    writable: true,
    configurable: true
  });
}

// misc functions //
function common_ancestor(a, b)
{
    $parentsa = $(a).parents();
    $parentsb = $(b).parents();

    var found = null;

    $parentsa.each(function() {
        var thisa = this;

        $parentsb.each(function() {
            if (thisa == this)
            {
                found = this;
                return false;
            }
        });

        if (found) return false;
    });

    return $(found);
}

array_groupBy = function(arr, key) { 
  var out = {};
  
  for(var i = 0; i< arr.length; i++)
  {
    if(out[arr[i][key]] === undefined) out[arr[i][key]] = [];
    out[arr[i][key]].push(arr[i]);
  }
  
  return $.map(out, function(o,k){
    return {key:k, values:o};
  });
};

Array.prototype.nthIndexOf = function(e, n) {
    var index = -1;
    for (var i = 0, len = this.length; i < len; i++) {
        if (i in this && e === this[i] && !--n) {
            index = i;
            break;
        }
    }
    return index;
};

range = function()
{
  var f, t;
  if(arguments.length == 1)
  {
    f = 0; t = arguments[0];
  } else
  {
    f = arguments[0]; t = arguments[1];
  }
  
  var a = new Array(t-f);
  
  for(var i = 0; i < a.length; i++)
  {
    a[i] = f + i;
  }
  return a;
};


// textarea vertical autogrow, as basic as possible
(function(){
	$.fn.autogrow = function(options)
	{
		var opts = $.extend({maxHeight: 200, reset_on_leave: false}, options);
		
		return this.each(function()
		{
			if($(this).is('textarea'))
			{
				var offset = this.offsetHeight - this.clientHeight + 2;			
				$(this).on('keyup input focus',function()
				{					
					$(this).css('height', 'auto').css('height', Math.min(this.scrollHeight + offset, opts.maxHeight));				
				});
				if(opts.reset_on_leave)
				{
					$(this).change(function()
					{
						$(this).css('height', 'auto')
					});
				}
			}
		});
	}		
}( jQuery ));



// Jquery fixes //

// Textarea and select clone() bug workaround | Spencer Tipping
// Licensed under the terms of the MIT source code license
(function (original) {
  jQuery.fn.clone = function () {
    var result           = original.apply(this, arguments),
        my_textareas     = this.find('textarea').add(this.filter('textarea')),
        result_textareas = result.find('textarea').add(result.filter('textarea')),
        my_selects       = this.find('select').add(this.filter('select')),
        result_selects   = result.find('select').add(result.filter('select'));

    for (var i = 0, l = my_textareas.length; i < l; ++i) $(result_textareas[i]).val($(my_textareas[i]).val());
    for (var i = 0, l = my_selects.length;   i < l; ++i) result_selects[i].selectedIndex = my_selects[i].selectedIndex;

    return result;
  };
})(jQuery.fn.clone);


// named array
(function ( $ ) {
  $.fn.serializeObject = function()
  {
    var a = {};
	console.log(this.find('input, select, textarea').add(this.filter('input, select, textarea')));
    $.each(this.find('input, select, textarea').add(this.filter('input, select, textarea')),
      function(i, r)
      {
        a[r.name] = r.value;
      });
    return(a);
  };
}( jQuery ));


