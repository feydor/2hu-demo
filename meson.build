project('2hudemo', 'c',
  version : '0.1',
  default_options : ['warning_level=3'])

#src = ['src/safe_array.c']
test_src = ['test/sarray_test.c', 'src/safe_array.c']
incdir = include_directories('include')

#exe = executable('sarray_example', sources: src, include_directories: incdir,
#  install : true)
sarray_test_exe = executable('sarray_test', sources: test_src, include_directories: incdir)

#test('basic', exe)

test('sarray init works', sarray_test_exe, args: ['test_init'])
test('sarray isempty works', sarray_test_exe, args: ['test_isempty'])
test('sarray pushback works', sarray_test_exe, args: ['test_pushback'])
test('sarray foreach works', sarray_test_exe, args: ['test_foreach'])
test('sarray add queue work', sarray_test_exe, args: ['test_addqueue'])
test('sarray remove queue work', sarray_test_exe, args: ['test_removequeue'])
test('sarray add/remove queues work', sarray_test_exe, args: ['test_queues'])
