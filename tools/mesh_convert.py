import sys
import json
import meshio

def print_help():
	print('''
Usage: python mesh_convert.py [options] filename
Options:
	-p, --pretty: Using pretty output
	-o, --output: Output file name, defaults to input file name with json extension name
	-h, --help: Show this help
''')

output_name = ''
filename = ''
pretty = False

index = 1
while index < len(sys.argv):
	match sys.argv[index]:
		case '-h' | '--help':
			print_help()
			exit()
		case '-p' | '--pretty':
			pretty = True
			index += 1
		case '-o' | '--output':
			output_name = sys.argv[index + 1]
			index += 2
		case _:
			filename = sys.argv[index]
			index += 1

if filename == '':
	print('No input file specified. Use -h or --help for help')
	exit()

try:
	mesh = meshio.read(filename)
except:
	print('Error reading file')
	exit()

data = {
	'point': [],
	'line': [],
	'triangle': [],
	'tetra': [],
	'quad': [],
	'edge': [],
}

conditions = {
	'boundary': [],
	'recession': [],
}

print('Reading mesh data...')

for entry in mesh.cells_dict:
	if entry not in data:
		continue
	data[entry]= mesh.cells_dict[entry].tolist()


data['point'] = mesh.points.tolist()


triangle = {}

for entry, triangle in enumerate(data['triangle']):
	triangle = [tuple(sorted([triangle[i], triangle[(i + 1) % 3]])) for i in range(3)]

# index corrections
for triangle in data['triangle']:
	triangle[:] = list(map(lambda x: x + 1, triangle))

meshOut = {
	'metaData': {
		'nodes': len(data['point']),
		'triangles': len(data['triangle']),
		'version': '0.1'
	},
	'mesh': {
		'nodes': data['point'],
		'triangles': data['triangle'],
		'edges': data['edge']
	},
}

if output_name == '':
	output_name = filename[:filename.rfind('.')] + '.json'


with open(output_name, 'w') as file:
	if pretty:
		json.dump(meshOut, file, indent=4)
	else:
		json.dump(meshOut, file)

print('Mesh converted to ' + output_name + ' successfully')
