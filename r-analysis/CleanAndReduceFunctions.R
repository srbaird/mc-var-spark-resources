#
# Resolve nulls by rolling forward previous value
# 
resolveNulls <- function(df) {
  
  if (is.null(dim(df))) {
    print("Resolve single dimension")
    df=data.frame(df)
  }
  dfRows=nrow(df)
  for (r in 1:dfRows) {
    
    if (r==1 & df[r,1]=="NULL") {
      index=2
      while(df[index,]=="NULL") {
        index=index+1
      }
      df[1,]=df[index,]
    }
    if (df[r,]=="NULL") {
      df[r,]=lastVal
    }
    lastVal=df[r,]
  }
  return(as.numeric(as.character(df[,1])))
}

#
#  Perform PCA on supplied columns
# 
performPCA <- function(df, componentNames) {
  
  nComponents=length(componentNames)
  dataLength=nrow(df)

  # The amount of variance to explain
  ratioToExplain=0.9  
  
  # Turn the selected factor columns into a data frame with NULL values resolved
  resolvedDataFrame=data.frame(matrix(rep(0,nComponents*dataLength), ncol = nComponents))
  names(resolvedDataFrame) = componentNames
  
  for (i in seq(1:nComponents)) {
    
    col.i=df[,componentNames][i]
    col.i=resolveNulls(col.i)
    colName=componentNames[i]
    resolvedDataFrame[eval(colName)]=col.i
  }

  # Perform PCA on the data frame
  component.pc=prcomp(resolvedDataFrame, scale = TRUE)

  # How many of the eigenvectors to use
  component.pc.var=component.pc$sdev^2
  
  component.pc.pve=component.pc.var/sum(component.pc.var)
  
  # TEMP TEMP TEMP TEMP TEMP TEMP 
  print(cumsum(component.pc.pve))
  # END OF TEMP -----------------
  
  numOfPCs=min(which(cumsum(component.pc.pve) > ratioToExplain))

  # Take the first n PCs
  pcs=matrix(component.pc$rotation[,1:numOfPCs], ncol=numOfPCs)

  # Transform the resolved data frame into a matrix 
  resolvedMatrix=data.matrix(resolvedDataFrame)

  # Combine the matrix and vectors into a single observation
  resultObservation=resolvedMatrix%*%pcs
}

#
# Reduce the dataframe by combining the factor groups into PCA columns
# 
reduceDataFrame <- function(df, componentNames, name , reduce=TRUE) {
  
  pcaVectors = performPCA(df , componentNames)
  
  # Initially add all returned vectors
  pcaVectorsLength=dim(pcaVectors)[2]
  for (i in 1:pcaVectorsLength) {
    pcaVectorName=paste(name, "_", i, sep="")
    df[eval(pcaVectorName)]=pcaVectors[,i]
  }
  
  # Remove original columns
  if (reduce) {
    df <- df[ , !(names(df) %in% componentNames)]
  } 
  df
}

