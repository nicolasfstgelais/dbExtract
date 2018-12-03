#' @export
# This function uses the ES space DB to calculate the emperical overlap between ecosystem services in canadian rivers
overlapES<-function(sitesClass,rd=2,type=1,perm=100)
{
  incMatrix=matrix(NA,ncol(sitesClass),ncol(sitesClass),dimnames=list(colnames(sitesClass),colnames(sitesClass)))
  diff=matrix(NA,ncol(sitesClass),ncol(sitesClass),dimnames=list(colnames(sitesClass),colnames(sitesClass)))
  p=matrix(NA,ncol(sitesClass),ncol(sitesClass),dimnames=list(colnames(sitesClass),colnames(sitesClass)))
  
  for(i in colnames(incMatrix)){
    for(j in colnames(incMatrix)){
      if(i==j){incMatrix[i,j]=round(sum(sitesClass[,i],na.rm = T)/nrow(sitesClass[!is.na(sitesClass[,i]),]),rd)}  else{
        subM=sitesClass[sitesClass[,i]==1&!is.na(sitesClass[,i])&!is.na(sitesClass[,j]),]
        subN=sitesClass[sitesClass[,i]==0&!is.na(sitesClass[,i])&!is.na(sitesClass[,j]),]
        
        subT=sitesClass[!is.na(sitesClass[,i])&!is.na(sitesClass[,j]),]
        incMatrix[i,j]=round(sum(subM[,j],na.rm = T)/nrow( subM[!is.na(subM[,j]),]),rd)
        #diff[i,j]= round(length(which(subM[,j]==1))-length(which(subM[,j]==0)),rd)

        diff[i,j]=  round(((length(which(subM[,j]==1))/nrow(subM))-(length(which(subN[,j]==1)))/nrow(subN)),rd)
        
        res=NA
        if(type==3){
        for(k in 1:perm){
          sitesClass_rand=apply(sitesClass,2,function(x)x[sample(length(x))])
          subM= sitesClass_rand[ sitesClass_rand[,i]==1&!is.na( sitesClass_rand[,i]),]
          subN=sitesClass[sitesClass_rand[,i]==0&!is.na(sitesClass_rand[,i])&!is.na(sitesClass_rand[,j]),]
          res[k]=  round(((length(which(subM[,j]==1))/nrow(subM))-(length(which(subN[,j]==1)))/nrow(subN)),rd)
        }
        p[i,j]=length(which(abs(res)>abs(diff[i,j])))/perm
        }
        }
    }}
  if(type==1)return(incMatrix)
  if(type==2)return(diff)
  if(type==3)return(p)
  
}

