#' @export
sitesClassification<-function(db_wide="db_wide_ym")
{
  load(paste0("data/dataPrep.RData"))
  db_wide=get(db_wide)
  selVar=colnames(db_wide)
  selSpaces=unique(guide$ES)


  # forced to have fc measures
  #db_wide=db_wide[!is.na(db_wide$fc),]
  #import="data/dataCDN.RData"

  #load(paste0("data/sitesClass",selOut,".RData"))




  sitesClass=matrix(NA,nrow(db_wide),length(selSpaces),dimnames=list(rownames(db_wide),selSpaces))
  limFreqTable=matrix(0,length(selVar),length(selSpaces),dimnames=list(selVar,selSpaces))
  limTotTable=matrix(0,length(selVar),length(selSpaces),dimnames=list(selVar,selSpaces))

  measFreq=NA
  limFreq=NA

  c=1
  #pb <- txtProgressBar(min = 0, max = nrow(sitesClass), style = 3)

  pb <- txtProgressBar(max = nrow(sitesClass)*length(selVar), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  class=rep(NA,length(selSpaces));names(class)=selSpaces

  sitesClass_raw=list()
  measFreq=matrix(NA,length(selSpaces),length(selVar),dimnames=list(selSpaces,selVar))
  limFreq=matrix(NA,length(selSpaces),length(selVar),dimnames=list(selSpaces,selVar))

  db_wide=as.matrix(db_wide)

  j="oligotrophic"
  for(j in selSpaces){

    # Initiate cluster
    # cl <- makeCluster(no_cores)
    #registerDoSNOW(cl)

    guideUpper=guide[guide$Pollutant%in%selVar&guide$ES==j&guide$Limit=="upper",c("Pollutant","Concentration")]
    rn=guideUpper$Pollutant;guideUpper=unlist(guideUpper[,2]);names(guideUpper)=rn


    guideLower=guide[guide$Pollutant%in%selVar&guide$ES==j&guide$Limit=="lower",c("Pollutant","Concentration")]
    rn=guideLower$Pollutant;guideLower=unlist(guideLower[,2]);names(guideLower)=rn



    out=rep(NA,nrow(db_wide))
    names(out)=rownames(db_wide)
    #x <-foreach(i=rownames(db_wide), .combine='rbind',.options.snow = opts) %:%
    for(m in selVar){
      if(m==selVar[1])out=apply(db_wide[,m,drop=F],1,evalLim,upper=as.numeric(guideUpper[m]),lower=as.numeric(guideLower[m]))
      if(m!=selVar[1])out=cbind(out,apply(db_wide[,m,drop=F],1,evalLim,upper=as.numeric(guideUpper[m]),lower=as.numeric(guideLower[m],evalLim)))
    }
    colnames(out)=selVar

    sitesClass_raw[[j]]=out

    if(file.exists(paste0("data/sitesClass.RData"))){
      load(paste0("data/sitesClass.RData"))
    }
    if(j%in%colnames(sitesClass))
    {
      sitesClass[,j]=apply(out,1,min,na.rm=T)
      sitesClass[is.infinite(sitesClass)]=NA

    }
    if(!j%in%colnames(sitesClass))
    {
      sitesClass=cbind(sitesClass,apply(out,1,min,na.rm=T))
      colnames(sitesClass)[ncol(sitesClass)]=j
      sitesClass[is.infinite(sitesClass)]=NA
    }



    #-save(sitesClass,sitesClass_raw,critLim,file=paste0("data/sitesClass",selOut,".RData"))

    save(sitesClass,file=paste0("data/sitesClass.RData"))

    #-indic<-function(x)length(which(is.nan(x)))/(length(which(is.nan(x)))+(length(which(is.infinite(x)))))

    measFreqF<-function(x)(length(which(x==1))+length(which(x==0)))/length(x)
    limFreqF<-function(x)length(which(x==0))/(length(which(x==1))+length(which(x==0)))
    temp.measFreq=apply(sitesClass_raw[[j]],2,measFreqF)
    temp.limFreq=apply(sitesClass_raw[[j]],2,limFreqF)



    #-s=sweep(sitesClass_raw[[j]],MARGIN=1,sitesClass[,j],`/`)
    #- temp=apply(s,2,indic)
    #-temp[is.nan(temp)]=NA
    measFreq[j,]=temp.measFreq
    limFreq[j,]=temp.limFreq
    print(j)
  }

  save(sitesClass_raw,file=paste0("data/sitesClass_raw.RData"))
  save( measFreq,limFreq,file=paste0("data/critLim.RData"))
  close(pb)


}

#rm(list=ls(all=TRUE))

evalLim<-function(env, upper,lower){

  incLow=NA
  incUp=NA

  if(is.na( upper)&is.na(lower))return(NA)
  if(is.na(env))return(NA)

  if(is.na(upper))upper=Inf
  if(is.na(lower))lower=0

  if(env<upper){incUp=min(incUp,1,na.rm = T)}
  if(env<lower){
    incLow=max(incLow,0,na.rm = T)
  }
  if(env>=upper){
    incUp=min(incUp,0,na.rm = T)
  }
  if(env>=lower){incLow=max(incLow,1,na.rm = T)}


  min=min(incUp,incLow)
  return(min)
}
