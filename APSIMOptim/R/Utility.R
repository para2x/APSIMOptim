### read outputfile
read_out<-function(out_file){

  skipline<-1

  res<-read.table(out_file,skip =4,header = FALSE,colClasses=c("character","numeric"))
  res$V1<-dmy(res$V1)
  return(res)

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

    vari<-pXML[[paste("//area[",paddok,"]/",var[i],sep="")]]

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
    simname<-pXML[["/folder/simulation/@name"]]
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
plot.apsimOptim<-function(x){
  params<-as.data.frame(x$Param)
  names(params)<-x$var
  boxplot(params)
}
