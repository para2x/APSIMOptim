############# SA techniques
#########1
ANOVA.SA<-function(Result.sim){
  sapply(names(Result.sim$Simulation_agg),function(col){
 #   print(is.na(all(col)))
   # print(eval(parse(text=paste0("Result.sim$Simulation_agg$col"))))
 if(!is.na(all(Result.sim$Simulation_agg[col]))){
        ## bringing params
    tmpdf<-as.data.frame(Result.sim$Param)
    names(tmpdf)<-Result.sim$var
    ##making the df for linear model
    lmdf<-cbind(Result.sim$Simulation_agg[col],tmpdf)
    anovobj<-aov(lm(as.formula(paste(eval(parse(text="col")),"~.^2")),data=lmdf))
    allssq<-summary(anovobj)[[1]][,2]
    varn<-names((anovobj)$coefficients)[-1]

    ##########
    sesivities<-sapply(Result.sim$var,function(nn){

      sum(allssq[which(grepl(nn, varn, fixed=F))],na.rm = T)/sum(allssq,na.rm = T)
    },USE.NAMES = T)
 }else{
   NULL
 }

  },USE.NAMES = T,simplify = T)
}

Corr.SA<-function(Result.sim){
  sapply(names(Result.sim$Simulation_agg),function(col){
    ## bringing params
    tmpdf<-as.data.frame(Result.sim$Param)
    names(tmpdf)<-Result.sim$var
    ##making the df for linear model
    lmdf<-cbind(Result.sim$Simulation_agg[col],tmpdf)
    ##########
    rpearson<-c()
    rspearman<-c()
    rkendall<-c()
    for(i in 2:ncol(lmdf)){
     rpearson<-c(rpearson,cor(lmdf[,1],lmdf[,i],method = "pearson"))
     rspearman<-c(rspearman,cor(lmdf[,1],lmdf[,i],method = "spearman"))
     rkendall<-c(rkendall,cor(lmdf[,1],lmdf[,i],method = "kendall"))
    }
    out<-as.data.frame(t(cbind(rpearson,rspearman,rkendall)))
    names(out)<-Result.sim$var
    return(out)
  },USE.NAMES = T,simplify = F)
}


Reg.SA<-function(Result.sim){
  sapply(names(Result.sim$Simulation_agg),function(col){
    ## bringing params
    tmpdf<-as.data.frame(Result.sim$Param)
    names(tmpdf)<-Result.sim$var
    ##making the df for linear model
    lmdf<-cbind(Result.sim$Simulation_agg[col],tmpdf)
    anovobj<-lm(as.formula(paste(eval(parse(text="col")),"~.^2")),data=lmdf)
    coeffs<-coefficients(anovobj)[-1]
    namecoef<-names(coefficients(anovobj))[-1]
    vary<-sd(lmdf[,1])
    ##########
    sesivities<-sapply(Result.sim$var,function(nn){
        allind<-which(grepl(nn, namecoef, fixed=F))
      sum(coeffs[allind])*(sd(lmdf[col][,1])/vary)
    },USE.NAMES = T)

  },USE.NAMES = T,simplify = T)
}


