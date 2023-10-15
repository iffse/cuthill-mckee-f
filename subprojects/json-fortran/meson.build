project('json-fortran', 'fortran', default_options: ['warning_level=0'])
sources = run_command('find', './mod/src/', '-maxdepth', '1', '-name', '*.F90', check: true).stdout().strip().split('\n')

lib = shared_library('json-fortran', sources)
dep = declare_dependency(link_with: lib)
