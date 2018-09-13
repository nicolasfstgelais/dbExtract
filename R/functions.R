ReporteRsWrap<- function(FUN,filename="Figure1"){
  # Create a new powerpoint document
  doc <- ReporteRs::pptx()
  # Add a new slide into the ppt document
  doc <- ReporteRs::addSlide(doc, "Two Content" )
  # add a slide title
  #doc<- addTitle(doc, "Editable vector graphics format versus raster format" )
  # A function for creating a box plot
  #-plotTreeFunc<- function(){
  #- plot(ctree, main="Conditional Inference Tree for Kyphosis")
  #-}
  # Add an editable box plot
  doc <- ReporteRs::addPlot(doc, FUN, vector.graphic = TRUE )
  # Add a raster box plot
  #doc <- addPlot(doc, boxplotFunc, vector.graphic = FALSE )
  # write the document to a file
  ReporteRs::writeDoc(doc, file = paste0("../figures/",filename,".pptx"))
}

#guide=CDN_wide_sel
#db=dbSel_wide

completeGuidelines<-function(guide,db){
  for(i in rownames(guide)){
    guide[i,is.na(guide[i,])]=max(c(as.numeric(db[,i]),as.numeric(guide[i,])),na.rm = T)
  }
  return(guide)}

norm.units<-function(mat,conc="Concentration",units="Units")
{
  mat[,units]=gsub("MILLIGRAM PER LITER","mg/L", mat[,units])
  mat[,units]=gsub("NANOGRAM PER LITER","ng/L", mat[,units])
  mat[,units]=gsub("MICROGRAM PER LITER","ug/L", mat[,units])
  
  mat[,units]=gsub("µ","u", mat[,units])
  
  mgL=grep("mg/L",mat[,units],ignore.case = T)
  ngL=grep("ng/L",mat[,units],ignore.case = T)
  mat[mgL,conc]=as.numeric(mat[mgL,conc])*1000
  mat[ngL,conc]=as.numeric(mat[ngL,conc])/1000
  mat[c(mgL,ngL),units]="ug/L"
  
  
  return(mat)
}

#mat=CDN_wide_sel
#col="drink"
create.edges<-function(mat,col)
{
  
  # mat=log10(mat)
  
  #centrer reduire les donnees par pollutant
  matScaled=(cbind((mat),0))
  #matScaled= t(scale(rbind(t(mat),0),scale=T))

  mat=split(cbind(matScaled[,ncol(mat)+1],matScaled[,col]),1:nrow(mat))
  mat=unname(mat)
  mat=expand.grid(mat)
  colnames(mat)=rownames( matScaled)
  return(mat)
}

firstAsRowNames <- function(mat)
{
  mat=as.data.frame(mat)
  rownames(mat) = mat[, 1]
  mat[, 1] = NULL
  return(mat)
}


# function to convert levels to numeric or characters
LtoN <- function(x) {as.numeric(as.character(x))}
LtoC <- function(x) {
  if(!is.null(dim(x))){
    for (i in 1:ncol(x)){
      if(class(x[,i])=="factor")x[,i]=as.character(x[,i])}
  }
  if(is.null(dim(x))){x=as.character(x)}
  return(x)
}

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

guide.samp<-function(x){
  sample(seq(0,x,x/100),1000000,replace=T)
}