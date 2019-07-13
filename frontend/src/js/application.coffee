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
    
  mergeNames = (names) ->
    names.sort (a, b) ->
      b.original.length - (a.original.length)
    return names

  generateTooltip = (results) ->
    tooltip = ""
    for result in results
      tooltip += "#{result.label} (#{result.item})<br>"
    return tooltip
  
  parse = (text, data) ->
    if data.names.length > 0
      names = mergeNames data.names
    else 
      names = data.names      
    regexp = new RegExp(names.reduce(((acc, cur) ->
      acc.concat cur.original.replace(/[\-\[\]{}()*+?.,\\\^$|#\s]/g, "\\$&")
    ), []).join('|'), 'gi')
    mapObj = {}
    for person in names
      if person.results.length is 0
        mapObj[person.original] = '<span class="lime accent-1">' + person.original + '</span>'
      else
        mapObj[person.original] = '<span class="light-green accent-1 tooltipped" data-tooltip="' + generateTooltip(person.results) + '">' + person.original + '</span>' 
    text = text.replace(regexp, (matched) ->
      mapObj[matched]
    )
    return text
    
  $('#retrieve').click ->
    $('#json').parent().addClass 'disabled'
    $('#content').attr 'contenteditable', false
    $('#content').fadeTo "slow" , 0.3
    text = document.getElementById('content').innerText
    if text[text.length-1] is '\n'
      text = text.slice 0,-1
    getJSON 'http://localhost:8081/api/parse?text=' + encodeURIComponent(text), (err, data) ->
      if err != null
        console.log "(#{err}) Something went wrong: #{data}"
      else
        console.log data
        $('.tab').removeClass 'disabled'
        document.getElementById('content').innerHTML = parse(text, data)
        $('#code').html JSON.stringify(data, null, 2)
        $('.tooltipped').tooltip()
      $('#content').attr 'contenteditable', true
      $('#content').removeClass "lime"
      $('#content').addClass "light-green"
      $('#content').fadeTo "slow" , 1
      instance[0].select('output')
      return
    return
