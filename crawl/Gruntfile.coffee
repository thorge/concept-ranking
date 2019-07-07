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
          "<%= build_path %>/<%= pkg.name %>.js": ["<%= src_path %>/*.coffee"]
    babel:
      options:
        sourceMap: true,
        presets: ['@babel/preset-env']
      dist:
        files:
          '<%= build_path %>/<%= pkg.name %>.js': '<%= build_path %>/<%= pkg.name %>.js'  
    uglify:
      options:
          sourceMap: true,
          sourceMapName: '<%= build_path %>/<%= pkg.name %>.js.map'
      dist:
        files:
          '<%= build_path %>/<%= pkg.name %>.min.js': ['<%= build_path %>/<%= pkg.name %>.js']

  # define tasks
  grunt.registerTask 'default', [
    'coffee'
    'babel'
    'uglify:dist'
  ]
  return
