# to add detect duplicated guidelines
# log changes
#select guidelines in tansformES
#' @export

transform_guidelines <- function(guidePath="raw/criteria/guidelinesNorm2.csv")
{
  
  guide=read.csv(guidePath,stringsAsFactors=FALSE)
  #-guide=guide[grep(jurisdiction,guide$jurisdiction,ignore.case = T,perl = T),]
  #-if(length(guide.year)>0)guide=guide[guide$Date%in%guide.year,]
  
  ##to lower for pollutant
  guide$Pollutant=tolower(guide$Pollutant)
  ##remove extra spaces
  guide$Pollutant=gsub(" ","",guide$Pollutant)
  ##normalize nits
  guide=norm.units(guide)
  
  write.csv(guide,"data/guidelines_norm.csv",row.names = F)
}

transformES<- function(guidePath="raw/criteria/guidelinesNorm2.csv",jurisdiction="world",guide.year=NULL,temporalPath="data/dbExtract_temporalDB.csv",by="ym")
{
  #-guide=read.csv(guidePath,stringsAsFactors=FALSE)
  
  ##Normalize datasets
  #-guide$Pollutant=tolower(guide$Pollutant)
  #-guide$Pollutant=gsub(" ","",guide$Pollutant)
  #-guide=norm.units(guide)
guide=guide[grep(jurisdiction,guide$jurisdiction,ignore.case = T,perl = T),]
if(length(guide.year)>0)guide=guide[guide$Date%in%guide.year,]
}
