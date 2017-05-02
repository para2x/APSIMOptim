################## Ready for the run and estimating the cost
apsimRun<-function(apsimWd, apsimExe, apsimFile, apsimVarL=NULL, VarT, tag, unlinkf=F, Varvalues, show.output = FALSE,
                   xmlfile=NULL, xmlvarL=NULL, xmlvarValue=NULL,xmlelp=NULL){

  nvar<-length(apsimVarL)
  news <- matrix(NA, 1, nvar)
  tagc<-tag
  apsimVar<-names(apsimVarL) # finding the name of variables
  elp <- unlist(apsimVarL)   # finding the element which is the value in the list
  varnames<-unlist(lapply(apsimVar,function(x){tail(strsplit(x,split="/")[[1]],1)}))  # fidning the variables names
  varnames<-paste0(varnames,elp) # making the name to lookup in the dataframe
  news<-Varvalues ## no fancy stuff staright the starting value if no value is provide
  apsimValue <- as.list((news))
################################ editting and runnng
  Fname<-paste0(unlist(strsplit(apsimFile, "[.]"))[1],"_edited",tagc,".apsim") #edited file
  ####################### editing xml file if it's necessary
  pXML <- xmlParse(apsimFile)
  outFname<-unlist(lapply(pXML["//filename[@output='yes']/text()"],function(x){xmlValue(x)}))
   if(!is.null(xmlfile)){
    inputsxml<-pXML["//filename[@input='yes']/text()"]
    inputs<-unlist(lapply(inputsxml,function(x){xmlValue(x)}))
    pos<-which(inputs%in%xmlfile)
    if(length(pos)==0) stop('No xml file was found.')
     xminput<-inputs[pos]
    ############ edit xml
     s<-edit_apsimxml(xminput, wd = getwd(), xmlvarL, xmlvarValue, overwrite = FALSE,XMLPOS=xmlelp)
     xmlValue(inputsxml[[pos]]) <- as.character(s) ## putting back the name of edited xml
  }
  saveXML(pXML, file = paste(Fname, sep = ""))
####################### editing apsim file if it's necessary
    if(!is.null(apsimVarL)){
    ## Edit file
    simname<-(edit_apsim(file = Fname, wd = apsimWd, var = apsimVar, tag=paste0("",tagc),
                         value = apsimValue, varType = VarT, Elpos = elp, overwrite = TRUE))
  }
  # Find the dited file location
  system(paste(addCommas(apsimExe),Fname, sep = " "), show.output.on.console = show.output)
  ##########################
  ########################################## Reading the outputs
  #########################
  # run the edited file

  results <- read_out(outFname[1])
  #results<-NULL
  if(unlinkf){
    unlink(c(Fname,outFname,sumFname), recursive = FALSE)
  }
  ##### cleaning XML
  if(exists("pXML")){
    #free(pXML)
    .Call("RS_XML_forceFreeDoc", pXML,PACKAGE="XML")
    rm(pXML)
    gc()
  }
  return(results)
}
