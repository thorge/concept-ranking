$(document).ready ->
  console.log 'Up and running..'
  tabs = document.querySelectorAll('.tabs')
  i = 0
  instance = []
  while i < tabs.length
    instance[i] = M.Tabs.init tabs[i]
    i++
  
  $('body').on('focus', '[contenteditable]', ->
    $this = $(this)
    $this.data 'before', $this.html()
    return
  ).on 'blur keyup paste input', '[contenteditable]', ->
    $this = $(this)
    if $this.data('before') != $this.html()
      $this.data 'before', $this.html()
      $this.trigger 'change'
    return
  
  $('#content').on 'change', ->
    $('#content').addClass "lime lighten-5"
  
  getJSON = (url, callback) ->
    xhr = new XMLHttpRequest
    xhr.open 'GET', url, true
    xhr.responseType = 'json'

    xhr.onload = ->
      status = xhr.status
      if status == 200
        callback null, xhr.response
      else
        callback status, xhr.response
      return

    xhr.send()
    return

  parse = (text, data) ->
    for person in data.names
      text = text.replace new RegExp(person.original, 'gi'), '<span class="lime accent-1">' + person.original + '</span>'
    return text
    
  $('#retrieve').click ->
    $('#json').parent().addClass 'disabled'
    $('#content').attr 'contenteditable', false
    $('#content').removeClass "lime"
    $('#content').addClass "grey"
    text = document.getElementById('content').innerText
    if text[text.length-1] is '\n'
      text = text.slice 0,-1
    getJSON 'http://localhost:8081/api/parse?text=' + text, (err, data) ->
      if err != null
        console.log 'Something went wrong: ' + err
      else
        $('.tab').removeClass 'disabled'
        $('#content').attr 'contenteditable', true
        $('#content').removeClass "grey"
        $('#content').addClass "light-green"
        instance[0].select('output')
        document.getElementById('content').innerHTML = parse(text, data)
        $('#code').html JSON.stringify(data, null, 2)
      return
    return
