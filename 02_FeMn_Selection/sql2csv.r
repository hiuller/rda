  sql2csv <- function(sqlFileName)
  {
    fileName <- paste(sqlFileName, ".sql", sep="")
    sqlCnn <- file(fileName, "r")
    sqlVec <- readLines(sqlCnn, encoding="UTF-8", warn=FALSE)
    sql    <- paste(sqlVec, sep=" ", collapse="\n")
    sql    <- substr(sql, 2, nchar(sql))
    closeAllConnections()
    rm(sqlCnn, sqlVec, fileName)
    
    library("RODBC")
    b30 <- odbcConnect("OUI01B30", uid="servaccess01", pwd="servaccess01") # open connection
    odbcGetInfo(b30)
    data <- sqlQuery(b30, sql)
    close(b30)
    rm(b30); rm(sql)
    
    outputFileName <- paste(sqlFileName, ".csv", sep="")
    write.csv(data, outputFileName, fileEncoding="UTF-8")
  }


