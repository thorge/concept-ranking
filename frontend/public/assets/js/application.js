"use strict";

(function () {
  $(document).ready(function () {
    var getJSON, i, instance, parse, tabs;
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

    parse = function parse(text, data) {
      var j, len, person, ref;
      ref = data.names;

      for (j = 0, len = ref.length; j < len; j++) {
        person = ref[j];
        text = text.replace(new RegExp(person.original, 'gi'), '<span class="lime accent-1">' + person.original + '</span>');
      }

      return text;
    };

    return $('#retrieve').click(function () {
      var text;
      $('#json').parent().addClass('disabled');
      $('#content').attr('contenteditable', false);
      $('#content').removeClass("lime");
      $('#content').addClass("grey");
      text = document.getElementById('content').innerText;

      if (text[text.length - 1] === '\n') {
        text = text.slice(0, -1);
      }

      getJSON('http://localhost:8081/api/parse?text=' + text, function (err, data) {
        if (err !== null) {
          console.log('Something went wrong: ' + err);
        } else {
          $('.tab').removeClass('disabled');
          $('#content').attr('contenteditable', true);
          $('#content').removeClass("grey");
          $('#content').addClass("light-green");
          instance[0].select('output');
          document.getElementById('content').innerHTML = parse(text, data);
          $('#code').html(JSON.stringify(data, null, 2));
        }
      });
    });
  });
}).call(void 0);
//# sourceMappingURL=application.js.map
