#!/usr/bin/python
import os, sys,re

def getOrbitalArray(x):
    return {
            "s" : [2],
            "p" : [3,4,5],
            "eg": [8,10],
            "t2g": [6,7,9],
            "py" : [3],
            "pz" : [4],
            "px" : [5],
            "dxy" : [6],
            "dyz" : [7],
            "dz2" : [8],
            "dxz" : [9],
            "dx2My2" : [10]
    }.get(x,"d")

namafilein = sys.argv[1]
atomInt = sys.argv[2]
namaOrbital = sys.argv[3]
#orbitalArray = sys.argv[4:] 
orbitalArray = getOrbitalArray(namaOrbital);
print ' s  py pz  px  dxy dyz dz2 dxz dx2-y2  f-3 f-2 f-1 f0  f1  f2  f3  ' 
print ' 2  3  4   5   6   7   8   9   10      11  12  13  14  15  16  17 '
print ' p   3 4 5'
print ' t2g 6 7 9'
print ' eg  8 10'

namafileout = 'bw.'+atomInt+'.'+namaOrbital+'.'+namafilein+'.dat'
f1 = open(namafilein, 'r')
fo = open(namafileout, 'w')
lines = f1.readlines()
nline= len(lines)
kali3 = False
if "g" in namaOrbital : kali3 = False 
#print nline
ionline=0
for iline in lines:
	if 'k-point' in iline: 
		x = (iline.split('x =')[1]).split('\n')[0]
		iband=0
		#print
		fo.write('\n')
		#print '# ' + iline
		fo.write( '# ' + iline + '\n')
		ionline=0
	if 'band' in iline:
		if iband>0:
			www= '%12.5f' % weight
			#print x +' '+y + www   # NOTE: this is for the previous band, where
			fo.write( x +' '+y + www + '\n')   # NOTE: this is for the previous band, where
		                               # we already 'weight' calculated.
		ionline=0
		weight=0.0
		iband=iband+1
		y= iline.split('energy')[1].split('#')[0]

	if ionline ==1:
		ilines=re.split('\s+',iline)
		if 'tot' in iline : continue #continue goto next iteration. Different from continue of 
		if len(ilines)<3  : continue
		#print ilines,len(ilines)
		if(int(ilines[1])==int(atomInt)) : #oxygen site --- ini kudu disesuaikan sitenya
#                        print ilines[3] + ' ' + ilines [4] + ' ' + ilines[5]
                        cek = ' '
                        for oo in orbitalArray:
                            weight = weight + float(ilines[int(oo)])
#                            cek = cek , oo , ' '+ atomInt + ' '
#                        print cek
#                        if kali3 : weight = weight * 3
                        weight = weight if (weight > 0.05) else 0
#			weight = weight + float(ilines[3])+float(ilines[4])+float(ilines[5])  #py + pz +px
	if 'ion' in iline:
		ionline=1

f1.close
fo.close
sys.exit()
