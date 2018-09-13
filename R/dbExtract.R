#' dbExtract main function
#'
#' This function is designed to explore databases and summarize
#' the spatial and temporal coverage of pre-selected varaibles (need to fill the input.xls file)
#' @param inputFile inputFile filename, needs to be in the working directory
#' @param dirPath   the path to the databases (see input file)
#' @param startAt at which line to start in input
#' @param append = F,
#' @param lineSkip
#' @param lvl
#' @keywords cats
#' @export
#' @examples
#' dbExtract ()
dbExtract<- function(inputFile = "dbInput.csv",catFile="categories.csv",output="dbExtractOutput")
{

  # inputs----

  # input
  input = LtoC(read.csv(paste0("inputs/",inputFile),na.strings = ""))

  #input categories to identified should also be a csv
  categories = LtoC(read.csv(paste0("inputs/",catFile),na.strings = ""))



  #exclu =  read.csv("inputs/exclu.csv",header=F)
  i=1

  for(i in 1:nrow(input)){

    print(input[i,"path"])
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

    db = db[rowSums(is.na(db)) != ncol(db), ]  #remove columns with only NAs
    db = db[, colSums(is.na(db)) != nrow(db)]  #remove rows with only NAs
    db = db[!is.na(db[, dateId]), ]  #remove rows with only NAs

    db[, dateId]=as.data.frame(do.call(rbind,strsplit(LtoC(db[, dateId])," ")))[,1]

    tryYMD <- tryCatch(lubridate::ymd(db[, dateId]),error=function(e) e, warning=function(w) w)
    tryMDY <- tryCatch(lubridate::mdy(db[, dateId]),error=function(e) e, warning=function(w) w)
    tryDMY <- tryCatch(lubridate::dmy(db[, dateId]),error=function(e) e, warning=function(w) w)


    if(!is( tryYMD ,"warning")){db[, dateId]= tryYMD }
    if(!is( tryMDY ,"warning")){db[, dateId]= tryMDY }
    if(!is( tryDMY ,"warning")){db[, dateId]= tryDMY }


    if (input[i, "dateFormat"] == "C") {
      db$date2 = NA
      db$date2[grep("/", db[, dateId])] = LtoC(lubridate::ymd(lubridate::parse_date_time(LtoC(db[,
                                                                                                 dateId][grep("/", db[, dateId])]), orders = "mdy H:M")))
      db$date2[grep("-", db[, dateId])] = LtoC(lubridate::ymd(lubridate::parse_date_time(LtoC(db[,
                                                                                                 dateId][grep("-", db[, dateId])]), orders = "ymd H:M")))
      db[, dateId] = db$date2
      db$date2 = NULL
    }

    if (input[i, "dateFormat"] == "D") {
      db$date2 = NA
      db$date2[grep("/", db[, dateId])] = LtoC(lubridate::ymd(lubridate::parse_date_time(LtoC(db[,
                                                                                                 dateId][grep("/", db[, dateId])]), orders = "mdy")))
      db$date2[grep("-", db[, dateId])] = LtoC(lubridate::ymd(lubridate::parse_date_time(LtoC(db[,
                                                                                                 dateId][grep("-", db[, dateId])]), orders = "ymd")))
      db[, dateId] = db$date2
      db$date2 = NULL
    }
    if (input[i, "dateFormat"] == "E")
    {db[, dateId] = LtoC(lubridate::ymd(LtoC(db[, dateId])))}

    if (input[i, "dateFormat"] == "F") {
      y = "YEAR"
      d = "DAY"
      m = "MONTH"
      db[, dateId] = lubridate::ymd((paste(db[, c(y)], db[, c(m)], db[, c(d)],
                                           sep = "-")))
    }

    if (input[i, "dateFormat"] == "G") {
      db[, dateId] = lubridate::ymd(lubridate::parse_date_time(LtoC(db[, dateId]), orders = "y"))
    }


    db$ym=paste0(format(as.Date(db[, dateId], format="%Y-%m-%d"),"%Y"),format(as.Date(db[, dateId], format="%Y-%m-%d"),"%m"))

    ## import for different types of file
   if(!is.na(input[i, "importFunction"])&input[i, "importFunction"]=="genie"){
     db$VariablePhase=paste(db$VariableEn,db$PhaseEn)
   }


    j = "doc"
    lvl="Lvl2"
    cats=LtoC(unique(categories[,lvl]))
    lvl1=(categories[,"Lvl1"])

    #-  j=selCat[1]
    c2=1
    parameters=data.frame(param=NA,ctrl=NA,KeyW=NA)
    #params=unique(db[,input$wideVar[i]])
    c3=1

    if(!is.na(input[i, "wideVar"])){
      searchVec=LtoC(unique(db[,LtoC(input$wideVar[i])]))
    }
    if(is.na(input[i, "wideVar"])){
      searchVec=colnames(db)
    }


    # store all variables name in a vector for future reference
    if(i==1)write.csv(unique(db[,input[i, "wideVar"]]),"data/colNames.csv",row.names = F)
    if(i!=1){
      colNames=read.csv("data/colNames.csv",stringsAsFactors = F)
       write.csv(unique(c(colNames[,1],unique(db[,input[i, "wideVar"]]))),"data/colNames.csv",row.names = F)}


    rowSel=NULL
    j="dicamba"
    for (j in cats) {
      # loop to search for the kerwords in order, maybe switch from | to  ; between keywords

      # create a list of pattern to look for
      pattTemp = paste(categories[categories[,lvl]==j, "Keywords"], collapse = ";")
      #pattTemp = gsub("\\|",";",pattTemp)
      pattList=unlist(strsplit(pattTemp,";"))

      # loop to look for patterns, first before
      colsTemp=NULL
      for(r in 1:length(pattList))
      {
        colsTemp = grep(pattern = pattList[r], searchVec , ignore.case = TRUE,perl=T)
        #colsTemp = grep(pattern = pattList[r], "Fecal coliforms not applicable" , ignore.case = TRUE,perl=T)

        if(length(colsTemp)>0){break}
      }

      if (length(colsTemp) == 0) {c2=c2+1;next}

      # store params name, category and keyword in params
      parameters[c3,"param"]=searchVec[colsTemp[1]]
      parameters[c3,"KeyW"]=pattList[r]
      parameters[c3,"ctrl"]=j


      #change in the db

      rowSel= c(rowSel,which(db[,input$wideVar[i]]%in% searchVec[colsTemp]))

      db[(db[,input$wideVar[i]])%in% searchVec[colsTemp],input$wideVar[i]]=parameters[c3,"ctrl"]
      c3=c3+1
      #log names
      #print(paste("\t",j,":",pattList[r]))
      #print(paste("\t\t",searchVec[colsTemp]))

      cat(paste("\t",j,":",pattList[r]), file=fileName, append=T, sep = "\n")
      cat(paste("\t\t",searchVec[colsTemp]), file=fileName, append=T, sep = "\n")
    }

    #select only relevant rows
    db=db[rowSel,]
    db=db[!is.na(db[,input$wideResults[i]]),]
    db=db[(db[,input$wideResults[i]])!="ND",]


    db=db[db[,input$wideResults[i]]>0,]


    #norm colnames
    colnames(db)[which(colnames(db)==input$dateID[i])]="date"
    colnames(db)[which(colnames(db)==input$units[i])]="units"
    colnames(db)[which(colnames(db)==input$wideVar[i])]="variable"
    colnames(db)[which(colnames(db)==input$wideResults[i])]="value"
    colnames(db)[which(colnames(db)==input$stationID[i])]="station"




    if (!is.na(input[i, "NAvalue"]))
    {db[db$value == input[i, "NAvalue"],"value"] = NA}



    lat=grep("lat",colnames(db),ignore.case = T)
    long=grep("long",colnames(db),ignore.case = T)

    if(length(lat)>0)colnames(db)[lat]="lat"
    if(length(long)>0)colnames(db)[long]="long"


    db=norm.units(mat=db,conc ="value",units = "units")

    if(i==1)dbMerged=db[,c("station","date","variable",'value',"units","ym")]
    if(i!=1)dbMerged=rbind(dbMerged,db[,c("station","date","variable",'value',"units","ym")])

  }
  return(dbMerged)
}

#' @export
dbExtract_init()
dbExtract_init<-function(){
  dir.create("raw")}


