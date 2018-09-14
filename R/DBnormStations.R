#' @export
DBnormStations<- function(inputFile = "dbInputStations.csv")
{
  
  # inputs----
  
  # input 
  input = LtoC(read.csv(paste0("raw/stations/",inputFile),na.strings = ""))
  
  for(i in 1:nrow(input)){
    
    
    #if(file.exists(paste0(input$name[i],"_norm.csv"))){next}
    
 
    sheetTemp = do.call(rbind, strsplit(LtoC(input[i, "sheet"]), ";"))
    if(!is.na(sheetTemp)){if(sheetTemp=="NA"){sheetTemp=NA}}
    lineSkip=input[i, "lineSkip"]
    fileType=input[i, "type"]
    
    if(length(grep("csv",LtoC(input[i, "path"])))!=0){fileType="csv"}
    if(length(grep("xl",LtoC(input[i, "path"])))!=0){fileType="xls"}
    
    # For xlsx if multiple sheets need to be rbind, sep = ';' and the
    # columns of the first sheet are used in the rbind
    # time the loop
    
    #log input
    fileName=paste0("logs/",as.character(Sys.Date()),".log")
    cat(as.character(Sys.time()), file=fileName, append=T, sep = "\n")
    
    
    time=gsub(" EDT","",gsub(" ","_",Sys.time()))
    
    if (fileType == "xls") {
      first = T
      for (w in sheetTemp) {
        if (!is.na(w)) {
          sheet = w
        } else {
          sheet = 1
        }
        if (first)
        {
          
          db = readxl::read_excel(paste( LtoC(input[i, "path"]),sep=""), sheet = sheet,skip = input$lineSkip[i])}
        if (!first)
        {db = rbind(db, readxl::read_excel(paste( LtoC(input[i, "path"]), sep = ""), sheet = sheet,skip = input$lineSkip[i])[, colnames(db)])}
        first = F
      }
    }
    
    if (fileType == "csv")
    {db = read.csv(paste( LtoC(input[i, "path"]), sep = ""),
                   1 ,skip = input$lineSkip[i],na.strings = c("", "NA"),stringsAsFactors = F)}
    
    db=as.data.frame(db)
    

    
   
    # this is for db with only one
    if (is.na(input[i, "stationID"]) | input[i, "stationID"] == "NA") {
      db$stationId = "A"
      stationId = "stationId"
    } else {
      stationId = LtoC(input[i, "stationID"])
    }
    
    dateId=input$dateID[i]
    
    
    #norm colnames
    colnames(db)[which(colnames(db)==input$stationID[i])]="station"
    colnames(db)[which(colnames(db)==input$latitude[i])]="latitude"
    colnames(db)[which(colnames(db)==input$longitude[i])]="longitude"


    
    if(i==1)dbMerged=db[,c("station","latitude","longitude")]
    if(i!=1)dbMerged=rbind(dbMerged,db[,c("station","latitude","longitude")])
  
  }
  write.csv(x = dbMerged,paste0("data/stations_norm.csv"),row.names = F)
}



