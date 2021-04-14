project('2hudemo', 'c',
  version : '0.1',
  default_options : ['warning_level=3'])

sdl2 = dependency('sdl2')
sdl2_mixer = dependency('SDL2_mixer')
sdl2_image = dependency('SDL2_image')
sdl2_ttf = dependency('SDL2_ttf')

src = ['src/game.c', 'src/common.c', 'src/entity.c', 'src/transient.c', 'src/safe_array.c']
test_src = ['test/sarray_test.c', 'src/safe_array.c']
incdir = include_directories('include')

exe = executable('2hudemo', sources: src, include_directories: incdir, dependencies: [sdl2, sdl2_mixer, sdl2_image, sdl2_ttf], install : true)
sarray_test_exe = executable('sarray_test', sources: test_src, include_directories: incdir)

#test('basic', exe)

test('sarray init works', sarray_test_exe, args: ['test_init'])
test('sarray isempty works', sarray_test_exe, args: ['test_isempty'])
test('sarray pushback works', sarray_test_exe, args: ['test_pushback'])
test('sarray foreach works', sarray_test_exe, args: ['test_foreach'])
test('sarray add queue work', sarray_test_exe, args: ['test_addqueue'])
test('sarray remove queue work', sarray_test_exe, args: ['test_removequeue'])
test('sarray add/remove queues work', sarray_test_exe, args: ['test_queues'])
