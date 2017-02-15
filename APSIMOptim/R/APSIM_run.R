################## Ready for the run and estimating the cost
APSIM.RUN<-function(apsimWd,apsimExe,apsimFile,apsimVarL,VarT,tag,unlinkf=F,Varvalues){
  nvar<-length(apsimVarL)
  news <- matrix(NA, 1, nvar)
  tagc<-tag

  apsimVar<-names(apsimVarL) # finding the name of variables
  elp <- unlist(apsimVarL)   # finding the element which is the value in the list
  varnames<-unlist(lapply(apsimVar,function(x){tail(strsplit(x,split="/")[[1]],1)}))  # fidning the variables names
  varnames<-paste0(varnames,elp) # making the name to lookup in the dataframe
  news<-Varvalues ## no fancy stuff staright the starting value if no value is provide
  apsimValue <- as.list((news))

  Fname<-paste0(unlist(strsplit(apsimFile, "[.]"))[1],"_edited",tagc,".apsim") #edited file
  ## Edit file
  simname<-(edit_apsim(file = apsimFile, wd = apsimWd, var = apsimVar, tag=paste0("_edited",tagc),
                       value = apsimValue, varType = VarT, Elpos = elp, overwrite = FALSE))

  outFname<-paste0(unclass(simname),".out") #outfile
  sumFname<-paste0(unclass(simname),".sum") #outfile
  # Find the dited file location
  system(paste(addCommas(apsimExe),Fname, sep = " "), show.output.on.console = TRUE)
  # run the edited file
  results <- (read_out(paste0(getwd(),"/",outFname)))
  if(unlinkf){
    unlink(c(Fname,outFname,sumFname), recursive = FALSE)
  }
  return(results)
}
