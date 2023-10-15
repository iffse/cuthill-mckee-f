import matplotlib.pyplot as plt
import numpy as np
import json
import sys

input_file = sys.argv[1]

with open(input_file) as f:
	data = json.load(f)

original = data['original']
ordered = data['ordered']
perm = data['permutation']

rev_perm = [0] * len(perm)
for i in range(len(perm)):
	rev_perm[perm[i] - 1] = i + 1

fig, ax = plt.subplots()

x = []
y = []

for i in range(len(ordered)):
	for j in range(len(ordered[i])):
		if ordered[i][j] == True:
			x.append(i)
			y.append(j)

ax.scatter(x, y, c='r', marker='x', label='ordered')
plt.show()

fig, ax = plt.subplots()

x = []
y = []

for i in range(len(original)):
	for j in range(len(original[i])):
		if original[i][j] == True:
			x.append(i)
			y.append(j)

ax.scatter(x, y, c='b', marker='o', label='original')
plt.show()

