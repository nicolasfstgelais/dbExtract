#' @export
dataPrep <- function(stationsPath="data/stations_norm.csv",guidePath="raw/criteria/guidelines.csv")
{
  ## Read files
  #source("R/functions.R")
  db=read.csv("data/dbMerged.csv",stringsAsFactors = F)
  range(db$date)
  stations=read.csv(stationsPath,stringsAsFactors = F,row.names = 1)
  stations=read.csv(stationsPath,stringsAsFactors = F)
  stations[which(duplicated(stations$station)),]
  length(unique(stations$station))

  guide=read.csv(guidePath,stringsAsFactors=FALSE)

  ##Normalize datasets
  guide$Pollutant=tolower(guide$Pollutant)
  guide$Pollutant=gsub(" ","",guide$Pollutant)
  guide=norm.units(guide)

  ##Check units  (add to a log)

  #log input
  fileName=paste0("logs/dataPrep",format(Sys.time(), "%Y-%m-%d_%H%M"),".log")
  cat(as.character(Sys.time()), file=fileName, append=T, sep = "\n")

  cat(paste("\t \nBefore \n"), file=fileName, append=T, sep = "\n")

  selVar=unique(db$variable)
  for(i in 1:length(selVar)){
    un=(paste0(unique(db$units[grep(selVar[i],db$variable,ignore.case = T)]),collapse=","))
    cat(paste("\t",selVar[i],":", un), file=fileName, append=T, sep = "\n")
  }


  ##Remove undersired units (add to a log)
  db=db[grep("ug/g",db$units,ignore.case = T,invert = T),]

  cat(paste("\t \nAfter\n"), file=fileName, append=T, sep = "\n")

  for(i in 1:length(selVar)){
    un=(paste0(unique(db$units[grep(selVar[i],db$variable,ignore.case = T)]),collapse=","))
    cat(paste("\t",selVar[i],":", un), file=fileName, append=T, sep = "\n")
  }


  ## db mean by year+month, by month and by day

  mo=LtoN(stringr::str_sub(db$ym, start= -2))
  mo=formatC(mo, width = 2, format = "d", flag = "0")

  db_mean_ym<- plyr::ddply(db, c("station","ym","variable"), plyr::summarise,
                           value    = mean(value))

  db_mean_d<- plyr::ddply(db, c("station","date","variable"), plyr::summarise,
                          value    = mean(value))


  db_mean_m<- plyr::ddply(cbind(db,mo), c("station","mo","variable"), plyr::summarise,
                          value    = mean(value))


  db_wide_d<- tidyr::spread(data = db_mean_d,
                            key = variable,
                            value = value)

  db_wide_ym<- tidyr::spread(data = db_mean_ym,
                             key = variable,
                             value = value)

  db_wide_m<- tidyr::spread(data = db_mean_m,
                            key = variable,
                            value = value)


  rownames(db_wide_ym)=paste0(db_wide_ym$station,db_wide_ym$ym)
  db_wide_ym=db_wide_ym[,-c(1,2)]

  rownames(db_wide_d)=paste0(db_wide_d$station,db_wide_d$date)
  db_wide_d=db_wide_d[,-c(1,2)]

  rownames(db_wide_m)=paste0(db_wide_m$station,db_wide_m$mo)
  db_wide_m=db_wide_m[,-c(1,2)]


  sites=unique(stringr::str_sub(rownames(db_wide_ym),start=0,end=-7))

  location=matrix(NA,length(sites),2,dimnames=list(sites,c("long","lat")))

  for(i in rownames(location))
  {
    if(length(stations[stations$station==i,"latitude"])==0)next
    location[i,"lat"]=stations[stations$station==i,"latitude"]
    location[i,"long"]=stations[stations$station==i,"longitude"]
  }
  location=as.data.frame(location)



  save(db_wide_m,db_wide_d,db_wide_ym,stations,guide,location,file=paste0("data/dataPrep.RData"))
}


