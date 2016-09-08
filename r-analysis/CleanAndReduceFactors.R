

#
# Remove nulls from risk factors matrix and reduce by PCA
#

# functions are available in source file
factorGroupFileName="CleanAndReduceFunctions.R"
source(factorGroupFileName)


keyColumnName="valueDate"
dsCodeColumnName="dsCode"

# Source and destination file names
factorGroupFileName="dataframes/factor.groups.csv"
riskFactorFileName="dataframes/factors.csv"
resolvedFactorsFileName="dataframes/factors.clean.csv"


# PCA groups
groupsDF = read.csv(factorGroupFileName)

# Market risk factors
factorsDF = read.csv(riskFactorFileName)


# Clean up all the columns first 
allColumnNames=names(factorsDF)
withoutValueDate=allColumnNames[!allColumnNames %in% c(dsCodeColumnName, keyColumnName)] # : Remove unwanted columns
nComponents=length(withoutValueDate)
dataLength=nrow(factorsDF)
resolvedDataFrame=data.frame(matrix(rep(0,nComponents*dataLength), ncol = nComponents))
names(resolvedDataFrame) = withoutValueDate

for (i in seq(1:length(withoutValueDate))) {
  
  col.i=factorsDF[,withoutValueDate][i]
  col.i=resolveNulls(col.i)
  colName=withoutValueDate[i]
  resolvedDataFrame[eval(colName)]=col.i
  print(paste("Completed", colName))
}

# Take a backup of the clean data...
backupDF=resolvedDataFrame

# ... so PCA can be run from this point
resolvedDataFrame= backupDF

# perform PCA on all the mappings
for (p in names(groupsDF)) {
  
  componentSubset=which(groupsDF[,p] %in% names(resolvedDataFrame))
  if (length(componentSubset) > 1) {
    print(paste("PCA group:", p))
    resolvedDataFrame <- reduceDataFrame(resolvedDataFrame, as.character(groupsDF[componentSubset,p]), p)
  } 
}

# Add the value date back into the resolved data frame
resolvedDataFrame[keyColumnName]=factorsDF[keyColumnName]

# Save the resolved dataset
write.table(resolvedDataFrame, file=resolvedFactorsFileName, append = FALSE, quote = FALSE, sep = ",", row.names = FALSE)

