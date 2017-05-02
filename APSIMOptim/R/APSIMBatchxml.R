################## Ready for the run and estimating the cost
apsimBatchXML<-function(apsimWd, apsimExe, apsimFile, apsimVarL, Varinfo, tag, unlinkf=F, nitr=10,
                     show.output = TRUE,verbose=T,Simname="Simulation",
                     xmlfile=NULL, VarinfoXML=NULL){
  options(warn=-1) #supress warnings resulting from cbind and join
  nvar<-length(apsimVarL) # number of variables
  ntotal<-ifelse(is.null(VarinfoXML),nvar,nvar+length(VarinfoXML[,1]))
  news <- matrix(NA, 1, nvar) # this vector keeps the values for the new set of poposed samples
  tagc<-tag # it will be added to the edited apsim file
  apsimVar<-names(apsimVarL) # finding the name of variables
  elp <- unlist(apsimVarL)   # finding the element which is the value in the list
  varnames<-unlist(lapply(apsimVar,function(x){tail(strsplit(x,split="/")[[1]],1)}))  # fidning the variables names
  varnames<-paste0(varnames,elp) # making the name to lookup in the dataframe
  ###################################################
  ## simulation variables
  ###################################################
  sws <- data.frame() ## this matrix keeps the results of simulations
  out <- matrix(NA, nitr, ntotal) # output which contains all the parameters posterior
  outallres <- matrix() # output which contains all the parameters posterior
  Pmeans<-Varinfo[which(varnames%in%Varinfo$Variable),2] ## finding starting values
  Psds<-Varinfo[which(varnames%in%Varinfo$Variable),3] ## finding starting values
  VarT<-Varinfo[which(varnames%in%Varinfo$Variable),4] # variable type - from dataframe
  boundU<-Varinfo[which(varnames%in%Varinfo$Variable),5] # upper bound for check the new sample
  boundL<-Varinfo[which(varnames%in%Varinfo$Variable),6]# lower bound for check the new sample
  Funcs<-as.character(Varinfo[which(varnames%in%Varinfo$Variable),7]) # functions for  sample generation
  # check to see if we have information for all variables
  if(nvar!=length(Pmeans)) stop("At least one of the parameters is not in the Variableinfo dataframe.")
  if(!(Funcs[1]%in%c("Normal","Uniform"))) stop("Your function needs to be either normal or uniform. Make sure the column is char not factor.")
  ## finding the starting and end date for filtering simulation results
  j<-1 # this counts the number of accepted samples and put it on matrix
  tsimstart<-proc.time()
  #################### Stating the loop

  for (i in 1:nitr) {
    ###############################################################################
    ###### Doing the random sampling
    ###############################################################################
    for(turn in 1:nvar){

      repeat {
        ## give a random walk to the one whos is has turn
        if(Funcs[turn]=="Normal"){
          news[turn] <- rnorm(1, Pmeans[turn], Psds[turn])
        }else if(Funcs[turn]=="Uniform"){
          news[turn] <- runif(1, Pmeans[turn], Psds[turn])
        }

     #    cat(turn,"-",Funcs[turn],"\n ----",Pmeans[turn],"\n ----",Psds[turn],"\n ----", news[turn],"---- \n",boundL[turn],"\n")
        if(is.nan(news[turn] )) stop("Somthing is wrong wth Varinfo. Second column should be lower bound for uiform and mean for normal.")
        # check to see if the new sample is in specified range.
        if(news[turn]<boundU[turn] & news[turn]>boundL[turn]){break;}

      }
    }
    apsimValue <- as.list((news)) ##Takes the values form new generated samples
    ###############################################################################
    ###### XML PART
    ###############################################################################

    xmlvarL<-VarinfoXML[,1]
    varnamesXML<-xmlvarL
    elpXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),2]
    PmeansXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),3] ## finding starting values
    PsdsXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),4] ## finding starting values
    boundUXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),5] # upper bound for check the new sample
    boundLXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),6]# lower bound for check the new sample
    FuncsXML<-as.character(VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),7]) # functions for  sample generation
    newsXML <- matrix(NA, 1, length(xmlvarL)) # th

    for(turn in 1:length(xmlvarL)){

      repeat {
        ## give a random walk to the one whos is has turn
        if(Funcs[turn]=="Normal"){
          newsXML[turn] <- rnorm(1, PmeansXML[turn], PsdsXML[turn])
        }else if(Funcs[turn]=="Uniform"){
          newsXML[turn] <- runif(1, PmeansXML[turn], PsdsXML[turn])
        }

        #    cat(turn,"-",Funcs[turn],"\n ----",Pmeans[turn],"\n ----",Psds[turn],"\n ----", news[turn],"---- \n",boundL[turn],"\n")
        if(is.nan(newsXML[turn] )) stop("Somthing is wrong wth Varinfo. Second column should be lower bound for uiform and mean for normal.")
        # check to see if the new sample is in specified range.
        if(newsXML[turn]<boundUXML[turn] & newsXML[turn]>boundLXML[turn]){break;}

      }
    }
    xmlvarval<- as.list((newsXML)) ##Takes the values form new generated samples
    ###############################################################################
    ###### Running the model
    ###############################################################################
    results<-apsimRun(apsimWd=apsimWd, apsimExe=apsimExe, apsimFile=apsimFile,
                      apsimVarL,VarT, Varvalues=apsimValue,
                      xmlfile = xmlfile,xmlvarL=xmlvarL, xmlvarValue=xmlvarval,
                      tag="",unlinkf=F,xmlelp=elpXML)


    #print(results)
    results <- apply(results, 2, as.numeric)
    results <-as.data.frame(results)
    ###############################################################################
    ################### Calculating the prior information piece
    ###############################################################################
    if(nrow(results>0)){

      sws<-rbind(sws,results%>%summarise_all(funs(mean,var)))
      outallres<-cbind(outallres,results)
      if(!is.null(VarinfoXML))tline<-c(news,newsXML)
      tline<-news
      out[i,]<-tline
    }

    ###############################################################################
    #######################Doing the math for sensitivity
    ###############################################################################
    if(verbose)  {
      cat(i,"===================","\n")
    }
    ###############################################################################
    ################################
    ############ Killing the job ruuner of APSIM
    ###############################################################################
    job <- grep("^JobRunner", readLines(textConnection(system('tasklist', intern = TRUE))), value = TRUE) # finding the pid of the job runners
    if(length(job)>0){ # if there is job runner running
      PIDS<-(read.table(text = job))
      if (nrow(PIDS)>20) {
        pskill(head(PIDS$V2,nrow(PIDS)-10))
      }
    }
    if(unlinkf){
      unlink(c(Fname,outFname,sumFname), recursive = FALSE)
    }
  }# end e loop

  #building output
  output<-list("Param" = out, "Simulation_agg" = sws,"Simulation"=as.data.frame(outallres[,-c(1)]),
               "Variable Info" = Varinfo,"var"=c(varnames,varnamesXML),"Itration"=i,"APfile"=apsimFile,"Simulation name"=Simname,
               "Elapsed time"=proc.time()-tsimstart)

  class(output)<-"apsimBatch"
  return(output)
}
