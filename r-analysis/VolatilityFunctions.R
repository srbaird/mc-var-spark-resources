#
# Perform h-day sliding function
#
hDayVolatility <- function(days,df) {
  
  # Generate an empty dataframe
  hDayValueDateColumnName="valueDate"
  dsCodeColumnName="dsCode"
  allColumnNames=names(df)
  withoutValueDate=allColumnNames[!allColumnNames %in% c(dsCodeColumnName,hDayValueDateColumnName)] # : Remove the dsCode and value date
  nComponents=length(withoutValueDate)
  dataLength=nrow(df)-(days)
  
  hdayDataFrame=data.frame(matrix(rep(0,nComponents*dataLength), ncol = nComponents))
  names(hdayDataFrame) = withoutValueDate
  
  if (nrow(df) >= days) {
    
    for (i in seq(1:length(withoutValueDate))) {
      
      col.i=df[,withoutValueDate][i]
      colName=withoutValueDate[i]
      rowindex=1
      for (j in seq(1:nrow(hdayDataFrame))) {
        
        hdayDataFrame[rowindex,eval(colName)] = (df[j+days,colName] - df[j,colName])
        rowindex=rowindex+1
      }
      print(paste("Completed", colName))
    }
  }
  return(hdayDataFrame)
}

