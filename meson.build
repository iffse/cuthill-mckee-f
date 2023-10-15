project('cuthil-mckee-example', 'fortran')

json = subproject('json-fortran').get_variable('dep')


sources = [
	'./src/main.f90',
	'./src/cuthil_mckee.f90'
	]

add_project_arguments('-Wno-tabs', language: 'fortran')

dependencies = [
	json,
	]

executable('cuthil-mckee-example', sources, dependencies: dependencies)

