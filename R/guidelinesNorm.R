#' @export
guidelinesNorm <- function(guidePath="raw/criteria/guidelines_extended.csv")
{

guide=read.csv(guidePath,stringsAsFactors = F)

# revome spaces in pollutant
guide$Pollutant=gsub(pattern = " ","",x = guide$Pollutant)
# to lower
guide$Pollutant=tolower(guide$Pollutant)

adist=adist(guide$Pollutant)
adist[1,]

sim.index=which(adist==1,arr.ind = TRUE)
sim.index[,1]=guide$Pollutant[sim.index[,1]]
sim.index[,2]=guide$Pollutant[as.numeric(sim.index[,2])]

sim.index=unique(paste(sim.index[,1],sim.index[,2],sep=": "))
i="turbidity"

fileName=paste0("logs/guidelinesNorm",as.character(Sys.Date()),".log")
cat(as.character(Sys.time()), file=fileName, append=T, sep = "\n")


i="copper"
for (i in unique(guide$Pollutant)){
  
  aspect=unique(guide[guide$Pollutant==i,"Aspect"])
  aspect=aspect[aspect!=""]
  
  if(length(aspect)!=0){
  if(length(aspect)!=1){
    cat(paste0("More than one aspect for ",i,": ",paste(aspect,collapse=", ")), file=fileName, append=T, sep = "\n")
  } else{guide[guide$Pollutant==i,"Aspect"]=aspect}}
  
 
 
  group=unique(guide[guide$Pollutant==i,"group"])
  group=group[group!=""]
  if(length(group)!=0){
  if(length(group)!=1){
    cat(paste0("More than one group for ",i,": ",paste(group,collapse=", ")), file=fileName, append=T, sep = "\n")
  } else{guide[guide$Pollutant==i,"group"]=group}}
}

#next to make sure that this function work before implementing
g=norm.units(guide,conc="Concentration",units="Units")

write.csv(guide,"data/guidelinesNorm.csv",row.names = F)
}
  