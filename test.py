import argparse
import subprocess
import sys
import random as r
import cmath as c
import numpy as np
import numpy.polynomial.polynomial as p


# OBJECTS
class TestPoly:
	def __init__(self, roots):
		self.roots = roots
		self.poly = p.Polynomial.fromroots(roots)
		self.tested = False

	def testPoly(self, exe):
		print(subprocess.check_output([exe, str(self.roots)]))
		self.tested = True
		return(0)


# SETTING PARAMETERS
argParser = argparse.ArgumentParser(description = 'Set parameters for Jenkins-Traub testing')
argParser.add_argument('--exe',
	action = 'store',
	nargs = 1)
argParser.add_argument('--bounds',
	action = 'store',
	nargs = 4,
	type = float,
	default = [-100, 100, -100, 100])
argParser.add_argument('--sample',
	action = 'store',
	nargs = 1,
	type = int,
	default = [1000])
argParser.add_argument('--deg',
	action = 'store',
	nargs = 1,
	type = int,
	default = [15])
args = argParser.parse_args()

print "=========================================================================================="

if args.exe == None:
	sys.exit("No executable given, exiting")
exeFile = args.exe[0]
print "Executable: {0}".format(exeFile)

[lR, uR, lI, uI] = args.bounds
if lR >= uR or lI >= uI:
	print "Invalid bounds: using defaults instead"
	[lR, uR, lI, uI] = [-100, 100, -100, 100]
print 'Real bounds: {0}, {1}; Imaginary bounds {2}, {3}'.format(lR, uR, lI, uI)

n = args.sample[0]
if n <= 0:
	print "Invalid sample size: using defaults instead"
	n = 1000
print "Sample size: {0}".format(n)

mDeg = args.deg[0]
if mDeg <= 1:
	print "Invalid max degree: using defaults intead"
	mDeg = 15
print "Maximum degree: {0}".format(mDeg)


# GENERATING POLYNOMIALS
polyList = []
while n > 0:
	deg = r.randint(2, mDeg)
	roots = []
	while deg > 0:
		roots.append(complex(r.uniform(lR, uR), r.uniform(lI, lR)))
		deg -= 1
	polyList.append(TestPoly(roots))
	n -= 1

print "=========================================================================================="

for i in polyList:
	i.testPoly(exeFile)