#!/usr/bin/python
import string, re
"""
Throws dumped data from R tests/evaluations of clusters and stores into
tabulation data-structure.
"""
def parsefile(filepath, algoName, rowSize=11):
  checkID = re.compile('\[1\]\s\"[\w\#]+\"')
  checkValue = re.compile('\[1\]\s\d(.\d*)?')
  checkTimeID = re.compile('\s*user\s*system\s*elapsed\s*')
  checkTimeValue = re.compile('\s*\d(.\d*)?\s*\d(.\d*)?\s*\d(.\d*)?\s*')
  checkFile = re.compile('.*\.csv')
  temp = 0
  theData={}
  theData['algo']=algoName
  returnData =[]
  f = open(filepath,'r')
  for line in f:
    if checkFile.match(line):
      theData['file']=string.split(line)[1]
    elif checkID.match(line):
      temp = string.split(line)[1]
    elif checkValue.match(line):
      theData[temp] = string.split(line)[1]
    elif checkTimeID.match(line):
      temp = string.split(line)[-1]
    elif checkTimeValue.match(line):
      theData[temp] = string.split(line)[-1]
    if len(theData.values()) == rowSize:
      #print(theData.keys())
      returnData.append(theData.values())
      temp = theData['file']
      temp2 = theData['algo']    
      theData = {}
      theData['file']=temp
      theData['algo']=temp2
  return(returnData)
"""
Ouputs tabulated data as .csv to stdin
"""
def printData(theData):
  for row in theData:
    for cell in range(0,len(row)):
      print row[cell] ,
      if cell < len(row)-1:
        print ",",
    print ""    
"""
Module code.
"""
if __name__ == "__main__":
  """
  Appending tabulations and then displaying as .csv
  Ordered as such:
  ['"Cluster#"', '"PCPr"', '"K"', '"VI"', '"CPr"', '"theta"', '"Re"', 'elapsed',
  'algo', 'file', '"Pr"']
  """
  output = parsefile('../test.partition.Rout', 'partition')
  output = output + parsefile('../test.center.Rout', 'center')
  output = output + parsefile('../test.center.degree.Rout', 'center-degree')
  output = output + parsefile('../test.center.sum.Rout', 'center-sum')
  output = output + parsefile('../test.center.mean.Rout', 'center-mean')
  output = output + parsefile('../test.center.evcent.Rout', 'center-evcent')
  output = output + parsefile('../test.center.kleinberg.Rout'
                              , 'center-kleinberg')
  output = output + parsefile('../test.center.markov.Rout'
                              , 'center-markov')

  output = output + parsefile('../test.merge.center.Rout', 'merge-center')
  output = output + parsefile('../test.star.degree.non.overlap.Rout'
    , 'star-degree-non-overlap')
  output = output + parsefile('../test.star.evcent.non.overlap.Rout'
    , 'star-evcent-non-overlap')
  output = output + parsefile('../test.star.kleinberg.non.overlap.Rout'
    , 'star-kleinberg-non-overlap')
  output = output + parsefile('../test.star.markov.non.overlap.Rout'
    , 'star-markov-non-overlap')
  output = output + parsefile('../test.star.sum.non.overlap.Rout'
    , 'star-sum-non-overlap')
  output = output + parsefile('../test.star.mean.non.overlap.Rout'
    , 'star-mean-non-overlap')

  output = output + parsefile('../test.star.degree.overlap.Rout'
    , 'star-degree-overlap')
  output = output + parsefile('../test.star.evcent.overlap.Rout'
    , 'star-evcent-overlap')
  output = output + parsefile('../test.star.kleinberg.overlap.Rout'
    , 'star-kleinberg-overlap')
  output = output + parsefile('../test.star.markov.overlap.Rout'
    , 'star-markov-overlap')
  output = output + parsefile('../test.star.sum.overlap.Rout'
    , 'star-sum-overlap')
  output = output + parsefile('../test.star.mean.overlap.Rout'
    , 'star-mean-overlap')
  output = output + parsefile('../test.markov.Rout'
    , 'mcl')
  output = output + parsefile('../test.affinity.propagation.prelim.Rout'
    , 'affinity-propagation-uniform(=0)')
  output = output + parsefile('../test.affinity.propagation.uniform.1.0.Rout'
    , 'affinity-propagation-uniform(=1)')
  output = output + parsefile('../test.affinity.propagation.sparse.median.Rout'
    , 'affinity-propagation-sparse-median')
  output = output + parsefile('../test.affinity.propagation.sparse.min.Rout'
    , 'affinity-propagation-sparse-min')
  output = output + parsefile('../test.affinity.propagation.mean.Rout'
    , 'affinity-propagation-mean')
  output = output + parsefile('../test.cc.pivot.Rout'
    , 'cc-pivot')
  output = output + parsefile('../test.min.cut.random.non.overlap.Rout'
    , 'cut')
  output = output + parsefile('../test.articulation.point.Rout'
    , 'articulation-point')
  output = output + parsefile('../test.ricochet.SR.mean.Rout'
    , 'ricochet-sr')
  output = output + parsefile('../test.ricochet.BSR.mean.Rout'
    , 'ricochet-bsr')
  output = output + parsefile('../test.ricochet.CR.mean.Rout'
    , 'ricochet-cr')
  output = output + parsefile('../test.ricochet.OCR.mean.Rout'
    , 'ricochet-ocr')
  output = output + parsefile('../test.affinity.propagation.uniform.0.zipf.Rout'
    , 'affinity-propagation-uniform(=0)')
  output = output + parsefile('../test.ricochet.SR.mean.leftover.Rout'
    , 'ricochet-sr')
  output = output + parsefile('../test.ricochet.BSR.mean.leftover.Rout'
    , 'ricochet-bsr')


  printData(output)
