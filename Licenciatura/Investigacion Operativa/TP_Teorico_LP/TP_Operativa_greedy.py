from _collections import defaultdict
import random

FOLDER_PATH = "/home/cherny"
ZIMPL_SOLUTION_FILE = "/one_solution.txt"


def _get_territories_to_locations(territories):
	territory_to_locations = {}
	for territory, amount in territories.iteritems():
		territory_to_locations[territory] = _get_locations(territory, amount)
	return territory_to_locations

def _get_locations(territory, amount):
	locations = []
	#with open(FOLDER_PATH + "/territory_{}.txt".format(territory)) as f:
	#	for line in f:
	#		locations.append((int(line.split(",")[0]), int(line.split(",")[1])))
	for _ in xrange(amount):
		locations.append((random.randint(1, 5000), random.randint(1, 5000)))
	return locations

def _get_solution():
	X = defaultdict(dict)
	with open(FOLDER_PATH + ZIMPL_SOLUTION_FILE) as f:
		for line in f:
			company, territory, amount = line.split(" ")
			X[company][int(territory)-1] = int(amount)
	return X

def _check_if_longer_from_east_to_west(locations):
	leftmost, lowmost = float('inf'), float('inf')
	rightmost, upmost = float('-inf'), float('-inf')
	for location in locations:
		leftmost = min(leftmost, location[0])
		rightmost = max(rightmost, location[0])
		upmost = max(upmost, location[1])
		lowmost = min(lowmost, location[1])
	return rightmost - leftmost > upmost - lowmost


def _get_territories(zimpl_solution):
	territories = defaultdict(int)
	for company, ts in zimpl_solution.iteritems():
		for t, amount in ts.iteritems():
			territories[t] += amount
	return territories

def _main():
	zimpl_solution = _get_solution()
	territories = _get_territories(zimpl_solution)
	territory_to_locations = _get_territories_to_locations(territories)
	territory_to_company_and_schools = {}
	how_to_sort = {}
	for territory, locations in territory_to_locations.iteritems():
		longer_horizontal = _check_if_longer_from_east_to_west(locations)
		if longer_horizontal:
			locations = sorted(locations)
			how_to_sort[territory] = "horizontalmente"
		else:
			locations = sorted(locations, key=lambda pos: (pos[1], pos[0]))
			how_to_sort[territory] = "verticalmente"
		loc_ind = 0
		company_to_schools = defaultdict(list)
		for company, territories_amounts in zimpl_solution.iteritems():
			for _ in xrange(territories_amounts[territory]):
				company_to_schools[company].append(locations[loc_ind])
				loc_ind += 1
		territory_to_company_and_schools[territory] = company_to_schools
	for territory, company_to_schools in territory_to_company_and_schools.iteritems():
		print "Territorio {} (ordenado {}):".format(territory, how_to_sort[territory])
		for company, company_locations in company_to_schools.iteritems():
			print "A la empresa {} le asigno las escuelas:\n{}".format(company, company_locations)
			print "-" * 20
		print "+" * 30

_main()
