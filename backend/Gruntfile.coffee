module.exports = (grunt) ->
  require('load-grunt-tasks')(grunt);
  # configure tasks
  grunt.initConfig
    pkg: grunt.file.readJSON "package.json"
    src_path: "src"
    build_path: "build"
    coffee:
      compile:
        files:
          "<%= build_path %>/backend.js": ["<%= src_path %>/backend.coffee"]
          "<%= build_path %>/routes.js": ["<%= src_path %>/routes.coffee"]
          "<%= build_path %>/controller.js": ["<%= src_path %>/controller.coffee"]
    babel:
      options:
        sourceMap: true,
        presets: ['@babel/preset-env']
      dist:
        files:
          '<%= build_path %>/backend.js': '<%= build_path %>/backend.js'
          '<%= build_path %>/routes.js': '<%= build_path %>/routes.js'  
          '<%= build_path %>/controller.js': '<%= build_path %>/controller.js'  
    uglify:
      options:
          sourceMap: true,
          sourceMapName: '<%= build_path %>/<%= pkg.name %>.js.map'
      dist:
        files:
          '<%= build_path %>/backend.min.js': ['<%= build_path %>/backend.js']
          '<%= build_path %>/routes.min.js': ['<%= build_path %>/routes.js']
          '<%= build_path %>/controller.min.js': ['<%= build_path %>/controller.js']

  # define tasks
  grunt.registerTask 'default', [
    'coffee'
    'babel'
    'uglify:dist'
  ]
  return
