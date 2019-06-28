#' @export
load_ES<-function(stationPath="data/dbExtract_stationsDB.csv",temporal_widePath="data/temporalDBwide.csv",guidePath="data/guidelines_norm.csv",selSpaces=c("irrigation","livestock","drink","aquatic","recreational","oligotrophic","mesotrophic","eutrophic"),jurisdiction="Canada",guide.year=NULL,outputPath="data/processed")
{

  db_wide=read.csv(temporal_widePath,header=T,stringsAsFactors = F,row.names=1)
  selVar=colnames(db_wide)

  stations= read.csv(stationPath,header=T,stringsAsFactors = F)


  guide= read.csv(guidePath,stringsAsFactors = F)

  #select guidelines
  guide=guide[grep(jurisdiction,guide$jurisdiction,ignore.case = T,perl = T),]
  if(length(guide.year)>0)guide=guide[guide$Date%in%guide.year,]


  #select guidelines
  stations.sel=stations[which(stations[stations$parameter=="location","value"]=="Canada"),"station"]
  db_wide=db_wide[(stringr::str_sub(rownames(db_wide), 0, -9)%in%stations$station),]


  #-selSpaces=unique(guide$ES)


  # forced to have fc measures
  #db_wide=db_wide[!is.na(db_wide$fc),]
  #import="data/dataCDN.RData"

  #load(paste0("data/sitesClass",selOut,".RData"))


  if (file.exists(paste0(outputPath,"/sitesClass.csv"))) file.remove(paste0(outputPath,"/sitesClass.csv"))

  sitesClass=matrix(NA,nrow(db_wide),length(selSpaces),dimnames=list(rownames(db_wide),selSpaces))
  limFreqTable=matrix(0,length(selVar),length(selSpaces),dimnames=list(selVar,selSpaces))
  limTotTable=matrix(0,length(selVar),length(selSpaces),dimnames=list(selVar,selSpaces))

  measFreq=NA
  limFreq=NA

  c=1
  #pb <- txtProgressBar(min = 0, max = nrow(sitesClass), style = 3)

  class=rep(NA,length(selSpaces));names(class)=selSpaces

  sitesClass_raw=list()
  measFreq=matrix(NA,length(selSpaces),length(selVar),dimnames=list(selSpaces,selVar))
  limFreq=matrix(NA,length(selSpaces),length(selVar),dimnames=list(selSpaces,selVar))

  db_wide=as.matrix(db_wide)

  j="aquatic"
  for(j in selSpaces){


      guideUpper=guide[guide$Pollutant%in%selVar&guide$ES==j&guide$Limit=="upper",c("Pollutant","Concentration")]
      rn=guideUpper$Pollutant;guideUpper=unlist(guideUpper[,2]);names(guideUpper)=rn


      guideLower=guide[guide$Pollutant%in%selVar&guide$ES==j&guide$Limit=="lower",c("Pollutant","Concentration")]
      rn=guideLower$Pollutant;guideLower=unlist(guideLower[,2]);names(guideLower)=rn



      out=rep(NA,nrow(db_wide))
      names(out)=rownames(db_wide)
      m="secchi"
      #x <-foreach(i=rownames(db_wide), .combine='rbind',.options.snow = opts) %:%
      for(m in selVar){
        if(m==selVar[1])out=apply(db_wide[,m,drop=F],1,evalLim,upper=as.numeric(guideUpper[m]),lower=as.numeric(guideLower[m]))
        if(m!=selVar[1])out=cbind(out,apply(db_wide[,m,drop=F],1,evalLim,upper=as.numeric(guideUpper[m]),lower=as.numeric(guideLower[m],evalLim)))
      }
      colnames(out)=selVar
    #meso=out[596,]

    sitesClass_raw[[j]]=out

    out_colap=matrix(do.call(paste0, as.data.frame(out)))


    if(file.exists(paste0(outputPath,"/sitesClass.csv"))){
      sitesClass=read.csv(paste0(outputPath,"/sitesClass.csv"),row.names=1)
    }
    if(j%in%colnames(sitesClass))
    {
      sitesClass[grepl(pattern = "w" ,out_colap),j]=1
      sitesClass[grepl(pattern = "u" ,out_colap),j]=0
      sitesClass[grepl(pattern = "o" ,out_colap),j]=0
      sitesClass[is.infinite(as.matrix(sitesClass))]=NA
    }
    if(!j%in%colnames(sitesClass))
    {
      sitesClass=cbind(sitesClass,apply(out,1,min,na.rm=T))
      colnames(sitesClass)[ncol(sitesClass)]=j
      sitesClass[grepl(pattern = "w" ,out_colap),j]=1
      sitesClass[grepl(pattern = "u" ,out_colap),j]=0
      sitesClass[grepl(pattern = "o" ,out_colap),j]=0
      sitesClass[is.infinite(sitesClass)]=NA
    }



    #-save(sitesClass,sitesClass_raw,critLim,file=paste0("data/sitesClass",selOut,".RData"))
    if(!file.exists(paste0(outputPath,"/sitesClass.csv"))){
      write.csv(sitesClass,paste0(outputPath,"/sitesClass.csv"),row.names = T )
    }
    if(file.exists(paste0(outputPath,"/sitesClass.csv"))){
      write.csv(sitesClass,paste0(outputPath,"/sitesClass.csv"),row.names = T )
    }

    #-indic<-function(x)length(which(is.nan(x)))/(length(which(is.nan(x)))+(length(which(is.infinite(x)))))

    measFreqF<-function(x)(length(which(!is.na(x))))/length(x)
    limFreqF<-function(x)length(grep("o|u",x))/(length(which(!is.na(x))))
    temp.measFreq=apply(sitesClass_raw[[j]],2,measFreqF)
    temp.limFreq=apply(sitesClass_raw[[j]],2,limFreqF)



    #-s=sweep(sitesClass_raw[[j]],MARGIN=1,sitesClass[,j],`/`)
    #- temp=apply(s,2,indic)
    #-temp[is.nan(temp)]=NA
    measFreq[j,]=temp.measFreq
    limFreq[j,]=temp.limFreq
    print(j)
  }

  save(sitesClass_raw, file = paste0(outputPath,"/sitesClass_raw"))
  #write.csv(sitesClass_raw,"data/sitesClass_raw.csv")
  write.csv(measFreq,paste0(outputPath,"/measFreq.csv"))
  write.csv(limFreq,paste0(outputPath,"/limFreq.csv"))

}

#rm(list=ls(all=TRUE))

evalLim<-function(env, upper,lower){

  incLow=NA
  incUp=NA

  if(is.na( upper)&is.na(lower))return(NA)
  if(is.na(env))return(NA)

  if(is.na(upper))upper=Inf
  if(is.na(lower))lower=0

  if(env<upper&env>=lower){out="w"}
  if(env<lower){out="u"}
  if(env>=upper){out="o"}

  return(out)
}


