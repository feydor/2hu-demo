project('2hudemo', 'c',
  version : '0.1',
  default_options : ['warning_level=3'])

cc = meson.get_compiler('c')
math_dep = cc.find_library('m', required : false)

# Get SDL2 dependencies
# dep = dependency('foo', fallback : [subproject_name, variable_name])
sdl2_dep = dependency('sdl2', fallback: ['sdl2', 'sdl2_dep'])
sdl2_image_dep = dependency('SDL2_image', fallback: ['sdl2_image', 'sdl2_image_dep'])
sdl2_mixer_dep = dependency('SDL2_mixer', fallback: ['sdl2_mixer', 'sdl2_mixer_dep'])
sdl2_ttf_dep = dependency('SDL2_ttf', fallback: ['sdl2_ttf', 'sdl2_ttf_dep'])

# Gather source, header files, dependencies
src = ['src-old/new/game.c', 'src-old/new/entity.c', 'src-old/new/twohu.c', 'src-old/new/input.c', 'src-old/new/bullet.c']
test_src = ['test/sarray_test.c', 'src-old/safe_array.c']
depends = [sdl2_dep, sdl2_mixer_dep, sdl2_image_dep, sdl2_ttf_dep, math_dep]

exe = executable('2hudemo',
          sources: src, 
          dependencies: depends, 
          install : true)

sarray_test_exe = executable('sarray_test',
                      sources: test_src)

test('sarray init works', sarray_test_exe, args: ['test_init'])
test('sarray isempty works', sarray_test_exe, args: ['test_isempty'])
test('sarray pushback works', sarray_test_exe, args: ['test_pushback'])
test('sarray foreach works', sarray_test_exe, args: ['test_foreach'])
test('sarray add queue work', sarray_test_exe, args: ['test_addqueue'])
test('sarray remove queue work', sarray_test_exe, args: ['test_removequeue'])
test('sarray add/remove queues work', sarray_test_exe, args: ['test_queues'])
