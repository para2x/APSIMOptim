### read outputfile
read_out<-function(out_file){
  tryCatch({
    res<-data.table::fread(out_file)[-1,]
    eval(parse(text = paste0("res$",names(res)[1],"<-dmy(res$",names(res)[1],")") ))
  },error=function(cond){
    res=data.frame()
  })

  return(res)

}

### read outputfile
read_out_sen<-function(out_file){
  tryCatch({
    res<-data.table::fread(out_file)[-1,]
    },error=function(cond){
      res=data.frame()
    })

  return(res)

}

## edit apsim xml file
edit_apsimxml<-function (file, wd = getwd(), var, value, overwrite = FALSE,XMLPOS)
{
  oldWD <- getwd()
  setwd(wd)
  if (!(file %in% list.files())) {
    stop("Specified file could not be found in the current working directory.")
  }
  if (length(grep(".apsim$", file)) > 0) {
    warning("Specified file is an APSIM simulation file and will be passed to edit_apsim.")
    return(edit_apsim(file = file, wd = wd, var = var, value = value,
                      overwrite = overwrite))
  }
  pXML <- xmlParse(file)

  for (i in 1:length(var)) {

    vari <- pXML[paste("//", var[i], sep = "")]
    lengthVari <- xmlSize(vari)
    lReplace <- length(value[[i]])

    newVar <- as.character(value[[i]])

############################################# Changing the value
    for (k in 1:lengthVari) {
       ll<-strsplit(xmlValue(vari[[k]])," ")
       ll<-unlist(ll)
       ll<-ll[nchar(ll)>0]
     #  cat(length(ll),"-","\n")

       if(length(ll)>1){
         ll[XMLPOS[i]]<-newVar
       }else{
         ll[1]<-newVar
       }

      xmlValue(vari[[k]]) <- paste(ll,collapse = " ")
    }
    ###############################
  }
  addWd <- paste(wd, file, sep = "/")
  if (overwrite) {
    setwd(oldWD)
    return(saveXML(pXML, file = addWd))
  }
  else {
    newName <- paste(gsub(".xml", "", addWd), "-edited",
                     sep = "")
    setwd(oldWD)
    return(saveXML(pXML, file = paste(newName, ".xml", sep = "")))
  }
}


### editing the apsim file
edit_apsim<-function(file, wd = getwd(), var, value, varType, Elpos=NULL, tag="-edited",paddok=1,
                     overwrite = FALSE){

  oldWD<-getwd()
  setwd(wd)

  if(length(grep(".xml$",file))>0){
    warning("Specified file is an xml and will be passed to edit_sim_file.")
    return(edit_sim_file(file = file, wd = wd, var = var, value = value, overwrite = overwrite))
  }

  fileNames <- dir(pattern=".apsim$",ignore.case=TRUE)

  if(length(fileNames)==0){
    stop("There are no .apsim files in the specified directory 'wd' to edit.")
  }

  file<-match.arg(file,fileNames,several.ok=TRUE)

  pXML<-xmlParse(file)

  ### It does it for each varibale
  for(i in 1:length(var)){

    vari<-pXML[[paste(var[i],sep="")]]

    #If supplied length is shorter then length to replace, then
    #leave the remaining values unchanged
    lToReplace<-xmlSize(vari)
    lReplace<-length(value[[i]])
    lenDiff<-lToReplace-lReplace

    if(lenDiff>0 & varType[i] != "Element"){
      #value[[i]]<-c(value[[i]],rep(value[[i]][lReplace],lenDiff))
      warning(paste("Only the first",lReplace,"of the",lToReplace,"elements of",var[i],"were changed",sep=" "))
    }
    ############################################
    ###### This was modified by Hamze
    #############################################
    if(varType[i] != "Element") {
      for(j in 1:lReplace){
        xmlValue(vari[[j]])<-as.character(value[[i]][j])
      }
    }else{
      xmlValue(vari[[Elpos[i]]])<-as.character(value[[i]][1])
    }
  }

  ###################################################
  ##### Up to here
  ###################################################

  #Be sure the edited file is written to the specified wd and not the current wd
  addWd <- paste(wd,file,sep="/")

  if(overwrite){
    setwd(oldWD)
    return(saveXML(pXML,file=addWd))
  }else{

    oldFileName <- gsub(".APSIM$","",gsub(".apsim$","",addWd))
    newFileName <- paste0(oldFileName,tag,".apsim")

    #Rename the simulation
    wholeSim<-pXML["//simulation"]
    for(i in 1:length(wholeSim)){
      newName <- paste0(xmlAttrs(wholeSim[[i]]),tag)
      xmlAttrs(wholeSim[[i]])<-c(name=newName)
    }

    #Rename the output filename to match the new file name
    outName<-pXML["//outputfile/filename"]
    for(i in 1:length(outName)){
      newName <- paste0(gsub(".out$","",xmlValue(outName[[i]])),tag)
      xmlValue(outName[[i]])<-paste(newName,".out",sep="")
    }

    #Also update title for output file
    outTitle<-pXML["//outputfile/title"]
    for(i in 1:length(outTitle)){
      newName <- paste0(xmlValue(outTitle[[i]]),tag)
      xmlValue(outTitle[[i]])<-newName
    }

    setwd(oldWD)

    saveXML(pXML,file=newFileName)
    simname<-pXML[["//simulation/@name"]]
    #
    rm(vari,wholeSim,outName,outTitle)
    if(exists("pXML")){
      #free(pXML)
      .Call("RS_XML_forceFreeDoc", pXML,PACKAGE="XML")
      rm(pXML)
      gc()
    }
    #
    return(simname)
  }
}

###### I use this function to cleanup the address
addCommas<-function(str){
  #if str includes spaces but isn't surrounded by commas then the commas are added

  if( length(grep(" ",str))>0  & length(grep("\"",str))==0 ){
    str <- paste("\"",str,"\"",sep="")
  }

  return(str)

}
################## plot output
plot.apsimOptim<-function(x,type="Posterior",burnin=0,cols=-1,namesv=NULL,cexi=NULL,chains=NULL){
  if(length(cols)==1 & cols[1]==-1){
    cols<-1:length(x$var)
    }
  if(type=="Posterior_Combo"){
      params<-as.data.frame(x$Param)
      names(params)<-x$var
      params<-params[-c(1:burnin),cols]
      mcmc_combo(params)
  }else if (type=="Posterior"){
    params<-as.data.frame(x$Param)
    names(params)<-x$var
    params<-params[-c(1:burnin),cols]
    mcmc_hist(params)
 }else if (type=="Interval"){
    params<-as.data.frame(x$Param)
    names(params)<-x$var
    params<-params[-c(1:burnin),cols]
    mcmc_intervals(params)
  }else if (type=="Cost"){
    cost<-x$Param[-c(1:burnin),length(x$var)+1]
    plot(cost)
    lines(cost)
  }else if (type=="Simulations"){

if(burnin>ncol(x$Simulations))stop("Burnin is greater than the number of accepted sampels.")
x$Simulations<-x$Simulations[,-c(1:burnin)] #taking out burnin

upper<-apply(x$Simulations,1,max)
lower<-apply(x$Simulations,1,min)
median<-apply(x$Simulations,1,median)
mean<-apply(x$Simulations,1,mean)
names(x$Obs)[2]<-"Obs"
names(x$Obs)[1]<-"Date"
plotdf<-cbind(x$Obs,upper,lower,median,mean)

plotdf%>%ggplot(aes(Date))+
  geom_ribbon(aes(ymin=lower,ymax=upper), fill = "grey85")+
  geom_line(aes(y=mean),linetype=2,col="red",size=1.02)+
  geom_line(aes(y=median),size=1.02)+
  geom_point(aes(y=Obs),size=3)+
  theme_bw(base_size = 18)+
  theme(legend.position="none")



}else if (type=="Multivariate"){
  library(psych)
  ## getting posterior
  params<-as.data.frame(x$Param)
  names(params)<-x$var
  params<-params[-c(1:burnin),cols]
  if(!is.null(namesv)) names(params)<-namesv ## changing the names
  if(!is.null(cexi))sizesi<-cex
  sizesi<-2
  psych::pairs.panels(params,scale = F, hist.col="steelblue4",cex.cor=sizesi/2,
                      cex=sizesi,rug=F, cex.labels = sizesi, gap=2, cex.axis = sizesi,mgp=c(2,2,0))
}else if (type=="ChainComparison"){
  if(!is.list(x)) stop("Your input needs to be a list with different ApsimOptim objects")
output<-x
  All<-lapply(output,function(x){
    params<-as.data.frame(x$Param)
    params<-params[-c(1:burnin),cols]
    names(params)<-x$var[cols]
    return(params%>%mutate(Simname=x$`Simulation name`))
  })
  All<-do.call("rbind",All)%>%gather(Param,Value,-c(Simname))

  All%>%
    ggplot(aes(y=Value,x=Param))+
    # geom_violin(aes(fill=Simname),size=1)+
    geom_boxplot(aes(fill=Simname),position =position_dodge(width = 0.9),size=1)+
    #geom_jitter(aes(color=Simname))+
    theme_bw(base_size = 18)+
    scale_fill_brewer(palette="Greys")+
    coord_flip()+
    theme(legend.title = element_blank(),
          legend.position = "top")


}
}

summary.apsimOptim<-function(x,burnin=0){
cat("Total iteration=",x$Itration,"\n")
cat("Accepted samples=",nrow(x$Param),"\n")
cat("Simulation name=",as.character(x[9]),"\n")
cat("--- \n")
params<-as.data.frame(x$Param[-c(1:burnin),])
names(params)<-c(x$var,"Cost")
summary(params)
}

