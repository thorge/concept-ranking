"use strict";

(function () {
  $(document).ready(function () {
    var generateTooltip, getJSON, i, instance, mergeNames, parse, tabs;
    console.log('Up and running..');
    tabs = document.querySelectorAll('.tabs');
    i = 0;
    instance = [];

    while (i < tabs.length) {
      instance[i] = M.Tabs.init(tabs[i]);
      i++;
    }

    $('body').on('focus', '[contenteditable]', function () {
      var $this;
      $this = $(this);
      $this.data('before', $this.html());
    }).on('blur keyup paste input', '[contenteditable]', function () {
      var $this;
      $this = $(this);

      if ($this.data('before') !== $this.html()) {
        $this.data('before', $this.html());
        $this.trigger('change');
      }
    });
    $('#content').on('change', function () {
      return $('#content').addClass("lime lighten-5");
    });

    getJSON = function getJSON(url, callback) {
      var xhr;
      xhr = new XMLHttpRequest();
      xhr.open('GET', url, true);
      xhr.responseType = 'json';

      xhr.onload = function () {
        var status;
        status = xhr.status;

        if (status === 200) {
          callback(null, xhr.response);
        } else {
          callback(status, xhr.response);
        }
      };

      xhr.send();
    };

    mergeNames = function mergeNames(names) {
      names.sort(function (a, b) {
        return b.original.length - a.original.length;
      });
      return names;
    };

    generateTooltip = function generateTooltip(results) {
      var j, len, result, tooltip;
      tooltip = "";

      for (j = 0, len = results.length; j < len; j++) {
        result = results[j];
        tooltip += "".concat(result.label, " (").concat(result.item, ")<br>");
      }

      return tooltip;
    };

    parse = function parse(text, data) {
      var j, len, mapObj, names, person, regexp;

      if (data.names.length > 0) {
        names = mergeNames(data.names);
      } else {
        names = data.names;
      }

      regexp = new RegExp(names.reduce(function (acc, cur) {
        return acc.concat(cur.original.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, "\\$&"));
      }, []).join('|'), 'gi');
      mapObj = {};

      for (j = 0, len = names.length; j < len; j++) {
        person = names[j];

        if (person.results.length === 0) {
          mapObj[person.original] = '<span class="lime accent-1">' + person.original + '</span>';
        } else {
          mapObj[person.original] = '<span class="light-green accent-1 tooltipped" data-tooltip="' + generateTooltip(person.results) + '">' + person.original + '</span>';
        }
      }

      text = text.replace(regexp, function (matched) {
        return mapObj[matched];
      });
      return text;
    };

    return $('#retrieve').click(function () {
      var text;
      $('#json').parent().addClass('disabled');
      $('#content').attr('contenteditable', false);
      $('#content').fadeTo("slow", 0.3);
      text = document.getElementById('content').innerText;

      if (text[text.length - 1] === '\n') {
        text = text.slice(0, -1);
      }

      getJSON('http://localhost:8081/api/parse?text=' + encodeURIComponent(text), function (err, data) {
        if (err !== null) {
          console.log("(".concat(err, ") Something went wrong: ").concat(data));
        } else {
          console.log(data);
          $('.tab').removeClass('disabled');
          document.getElementById('content').innerHTML = parse(text, data);
          $('#code').html(JSON.stringify(data, null, 2));
          $('.tooltipped').tooltip();
        }

        $('#content').attr('contenteditable', true);
        $('#content').removeClass("lime");
        $('#content').addClass("light-green");
        $('#content').fadeTo("slow", 1);
        instance[0].select('output');
      });
    });
  });
}).call(void 0);
//# sourceMappingURL=application.js.map
