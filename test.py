import argparse
import subprocess
import sys
import random as r
import cmath as c
import numpy as np
import numpy.polynomial.polynomial as p
import contextlib


def parseComplex(str):
	split = "".join("".join(str.split('(')).split(')')).split(':+')
	return(float(split[0]) + float(split[1])*1j)

def testF(ro, re, err):
	if len(ro) != len(re): return(False)
	while re != []:
		flag = False
		for i in range(len(ro)):
			if abs(re[0] - ro[i]) < err:
				flag = True
				break
		if flag == False: return(False)
		re = re[1:]
	return(True)

def runTest(polyList, exe, err):
	total = len(polyList)
	sucCount = 0
	for i in polyList:
		if i.testPoly(exe, err): sucCount += 1
	return ({'total' : 0,
			'sucRate' : float(sucCount) / total})


# OBJECTS
class TestPoly:
	def __init__(self, roots):
		self.roots = roots
		self.coeffs = [1]
		for i in roots:
			self.coeffs = np.convolve(self.coeffs, [-i, 1])
		self.coeffs = self.coeffs.tolist()
		self.tested = False

	def testPoly(self, exe, err):
		formattedStr = ', '.join(['{0:f}'.format(i) for i in self.coeffs])
		self.returned = subprocess.check_output([exe, formattedStr])
		self.returned = [parseComplex(i) for i in "".join(self.returned[1:-2].split()).split(',')]
		self.results = testF(self.roots, self.returned, err)
		self.tested = True
		return(self.results)


#################### MAIN ####################
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
argParser.add_argument('--err',
	action = 'store',
	nargs = 1,
	type = float,
	default = [0.00000001])
args = argParser.parse_args()

print "=========================================================================================="

if args.exe == None:
	sys.exit("No executable given, exiting")
exeFile = args.exe[0]
print "Executable: {0}".format(exeFile)

err = args.err[0]
print "Using error value: {0}".format(err)

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
if mDeg <= 2:
	print "Invalid max degree: using defaults intead"
	mDeg = 15
print "Maximum degree: {0}".format(mDeg)


# GENERATING POLYNOMIALS
polyList = []
while n > 0:
	deg = r.randint(3, mDeg)
	roots = []
	while deg > 0:
		roots.append(complex(r.uniform(lR, uR), r.uniform(lI, uI)))
		deg -= 1
	polyList.append(TestPoly(roots))
	n -= 1

print "=========================================================================================="

testResults = runTest(polyList, exeFile, err)
print ("Test performed with parameters above")
print ("Success rate: {0}".format(testResults["sucRate"]))