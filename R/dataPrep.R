# to add detect duplicated guidelines
#' @export
dataPrep <- function(stationsPath="data/dbExtract_stationsDB.csv",guidePath="raw/criteria/guidelinesNorm2.csv",jurisdiction="world",guide.year=NULL,temporalPath="data/dbExtract_temporalDB.csv",by="ym")
{
  ## Read files
  #source("R/functions.R")
  db=read.csv(temporalPath,stringsAsFactors = F)

  #-stations=read.csv(stationsPath,stringsAsFactors = F,row.names = 1)
  stations=read.csv(stationsPath,stringsAsFactors = F)
  #-stations[which(duplicated(stations$station)),]
  #-length(unique(stations$station))

  guide=read.csv(guidePath,stringsAsFactors=FALSE)
  guide=guide[grep(jurisdiction,guide$jurisdiction,ignore.case = T,perl = T),]
  if(length(guide.year)>0)guide=guide[guide$Date%in%guide.year,]


  ##Normalize datasets
  guide$Pollutant=tolower(guide$Pollutant)
  guide$Pollutant=gsub(" ","",guide$Pollutant)
  guide=norm.units(guide)

  ##Check units  (add to a log)

  #log input
  fileName=paste0("logs/dataPrep",format(Sys.time(), "%Y-%m-%d_%H%M"),".log")
  cat(as.character(Sys.time()), file=fileName, append=T, sep = "\n")

  cat(paste("\t Time range \n"), file=fileName, append=T, sep = "\n")

  range(db$date)

  cat(paste("\t \nBefore \n"), file=fileName, append=T, sep = "\n")

  cat(paste("\t \nBefore \n"), file=fileName, append=T, sep = "\n")


  selVar=unique(db$parameter)
  for(i in 1:length(selVar)){
    un=(paste0(unique(db$units[grep(selVar[i],db$parameter,ignore.case = T)]),collapse=","))
    cat(paste("\t",selVar[i],":", un), file=fileName, append=T, sep = "\n")
  }


  ##Remove undersired units (add to a log)
  db=db[grep("ug/g",db$units,ignore.case = T,invert = T),]

  cat(paste("\t \nAfter\n"), file=fileName, append=T, sep = "\n")

  for(i in 1:length(selVar)){
    un=(paste0(unique(db$units[grep(selVar[i],db$parameter,ignore.case = T)]),collapse=","))
    cat(paste("\t",selVar[i],":", un), file=fileName, append=T, sep = "\n")
  }


  ## db mean by year+month, by month and by day

  mo=LtoN(stringr::str_sub(db$ym, start= -2))
  mo=formatC(mo, width = 2, format = "d", flag = "0")


if(by=="ym"){
  db_mean_ym<- plyr::ddply(db, c("station","ym","parameter"), plyr::summarise,
                           value    = mean(value))

  db_wide<- tidyr::spread(data = db_mean_ym,
                             key = parameter,
                             value = value)

  rownames(db_wide)=paste0(db_wide$station,db_wide$ym)
  db_wide=db_wide[,-c(1,2)]
}
  if(by=="d"){

  db_mean_d<- plyr::ddply(db, c("station","date","parameter"), plyr::summarise,
                          value    = mean(value))

  db_wide<- tidyr::spread(data = db_mean_d,
                            key = parameter,
                            value = value)

  rownames(db_wide)=paste0(db_wide$station,db_wide$d)
  db_wide=db_wide[,-c(1,2)]
  }

  if(by=="m"){

  db_mean_m<- plyr::ddply(cbind(db,mo), c("station","mo","parameter"), plyr::summarise,
                          value    = mean(value))
  db_wide_m<- tidyr::spread(data = db_mean_m,
                            key = parameter,
                            value = value)
  rownames(db_wide)=paste0(db_wide$station,db_wide$m)
  db_wide=db_wide[,-c(1,2)]
}



  #-sites=unique(stringr::str_sub(rownames(db_wide),start=0,end=-7))

  #-location=matrix(NA,length(sites),2,dimnames=list(sites,c("long","lat")))

  #-for(i in rownames(location))
  #-{
    #-if(length(stations[stations$station==i,"latitude"])==0)next
    #-location[i,"lat"]=stations[stations$station==i,"latitude"]
    #-location[i,"long"]=stations[stations$station==i,"longitude"]
  #-}
#-  location=as.data.frame(location)

 write.csv(db_wide,"data/temporalDBwide.csv",row.names = F)
 write.csv(guide,"data/guidelines_norm.csv",row.names = F)


  #save(db_wide_m,db_wide_d,db_wide_ym,stations,guide,location,file=paste0("data/dataPrep.RData"))
}


