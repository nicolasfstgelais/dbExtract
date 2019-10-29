
#' @export
transform_temporal <- function(stationsPath="data/dbExtract_stationsDB.csv",temporalPath="data/dbExtract_temporalDB.csv",by="ym",outputFile="data/processed/dbExtract_output.csv")
{
  ## Read files
  db=read.csv(temporalPath,stringsAsFactors = F)
  stations=read.csv(stationsPath,stringsAsFactors = F)

  ##Check units  (add to a log)
  #guide$Pollutant=tolower(guide$Pollutant)
  #guide$Pollutant=gsub(" ","",guide$Pollutant)
  #guide=norm.units(guide)

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
 db$y=lubridate::year(db$date)

 if(by=="y"){
   db_mean_y<- plyr::ddply(db, c("station","y","parameter"), plyr::summarise,
                            value    = mean(value))

   db_wide<- tidyr::spread(data = db_mean_y,
                           key = parameter,
                           value = value)

   rownames(db_wide)=paste0(db_wide$station,db_wide$y,"0000")
   db_wide=db_wide[,-c(1,2)]
 }

if(by=="ym"){
  db_mean_ym<- plyr::ddply(db, c("station","ym","parameter"), plyr::summarise,
                           value    = mean(as.numeric(as.character(value))))

  db_wide<- tidyr::spread(data = db_mean_ym,
                             key = parameter,
                             value = value)

  rownames(db_wide)=paste0(db_wide$station,db_wide$ym,"00")
  db_wide=db_wide[,-c(1,2)]
}

 if(by=="ymd"){
   db_mean_ymd<- plyr::ddply(db, c("station","date","parameter"), plyr::summarise,
                            value    = mean(as.numeric(as.character(value))))

   db_wide<- tidyr::spread(data = db_mean_ymd,
                           key = parameter,
                           value = value)

   rownames(db_wide)=paste(db_wide$station,db_wide$date,sep="-")
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

 write.csv(db_wide,outputFile)

}


norm.units<-function(mat,conc="value",units="units")
{
  mat[,units]=gsub("MILLIGRAM PER LITER","mg/L", mat[,units])
  mat[,units]=gsub("NANOGRAM PER LITER","ng/L", mat[,units])
  mat[,units]=gsub("MICROGRAM PER LITER","ug/L", mat[,units])

  mat[,units]=gsub("µ","u", mat[,units])

  mgL=grep("mg/L",mat[,units],ignore.case = T)
  ngL=grep("ng/L",mat[,units],ignore.case = T)
  mat[mgL,conc]=as.numeric(mat[mgL,conc])*1000
  mat[ngL,conc]=as.numeric(mat[ngL,conc])/1000
  ugL=grep("ug/L",mat[,units],ignore.case = T)

  mat[c(mgL,ngL,ugL),units]="ug/L"

  fileName=paste0("logs/normUnits",format(Sys.time(), "%Y-%m-%d"),".log")
  cat(as.character(Sys.time()), file=fileName, append=T, sep = "\n")
  cat(as.character(unique(mat[,units])), file=fileName, append=T, sep = "\n")


  return(mat)
}
