#!/usr/bin/python
import string, re
"""
Throws dumped data from R tests/evaluations of clusters and stores into
tabulation data-structure.
"""
def parsefile(filepath, algoName, rowSize=5):
  checkID = re.compile('\[1\]\s\"[\w\#]+\"')
  checkValue = re.compile('\[1\]\s\d(.\d*)?')
  checkTimeID = re.compile('\s*user\s*system\s*elapsed\s*')
  checkTimeValue = re.compile('\s*\d(.\d*)?\s*\d(.\d*)?\s*\d(.\d*)?\s*')
  checkFile = re.compile('.*\.csv')
  temp = 0
  theData={}
  #theData['algo']=algoName
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
      #temp2 = theData['algo']    
      theData = {}
      theData['file']=temp
      #theData['algo']=temp2
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
  ['"LocalAverageGraphDegreeDensity"', '"LocalAverageTransitivity"'
  , '"GraphEdgeDensity"', '"theta"', '"GraphStrengthDensity"', '"StdMarkov"'
  , '"LocalAverageGraphStrengthDensity"', 'file', '"AvgEVCent"'
  , '"MeanNeighbourhoodMeanStrength"', '"GlobalTransitivity"']
  """
  output = parsefile('../test.graph.scale.Rout', 'not used')
  printData(output)
