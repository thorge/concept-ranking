module.exports = (grunt) ->
  require('load-grunt-tasks')(grunt);
  # configure tasks
  grunt.initConfig
    pkg: grunt.file.readJSON "package.json"
    src_path: "src"
    build_path: "public/assets"
    coffee:
      compile:
        files:
          "<%= build_path %>/js/<%= pkg.name %>.js": ["<%= src_path %>/js/*.coffee"]
    babel:
      options:
        sourceMap: true,
        presets: ['@babel/preset-env']
      dist:
        files:
          '<%= build_path %>/js/<%= pkg.name %>.js': '<%= build_path %>/js/<%= pkg.name %>.js'  
    uglify:
      options:
          sourceMap: true,
          sourceMapName: '<%= build_path %>/js/<%= pkg.name %>.js.map'
      dist:
        files:
          '<%= build_path %>/js/<%= pkg.name %>.min.js': ['<%= build_path %>/js/<%= pkg.name %>.js']

  # define tasks
  grunt.registerTask 'default', [
    'coffee'
    'babel'
    'uglify:dist'
  ]
  return
