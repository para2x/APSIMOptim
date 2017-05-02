################## Ready for the run and estimating the cost
apsimOptimxml<-function(apsimWd, apsimExe, apsimFile, apsimVarL, Varinfo, tag, unlinkf=F, nitr=10,
                      obs=NULL, Gibbs=T, vc=NULL, show.output = TRUE,verbose=T,Simname="Simulation",
                      usePrior=T, xmlfile=NULL, VarinfoXML=NULL){
  options(warn=-1) #supress warnings resulting from cbind and join
  nvar<-length(apsimVarL) # number of variables
  ntotal<-ifelse(is.null(VarinfoXML),nvar,nvar+length(VarinfoXML[,1]))
  sws <- matrix(NA, nrow(obs), nitr) ## this matrix keeps the results of simulations from accepted samples
  out <- matrix(NA, nitr, ntotal + 1) # output which contains all the parameters posterior


  tagc<-tag # it will be added to the edited apsim file
  apsimVar<-names(apsimVarL) # finding the name of variables
  elp <- unlist(apsimVarL)   # finding the element which is the value in the list
  varnames<-unlist(lapply(apsimVar,function(x){tail(strsplit(x,split="/")[[1]],1)}))  # fidning the variables names
  varnames<-paste0(varnames,elp) # making the name to lookup in the dataframe
  ###################################################
  ## simulation variables
  ###################################################
  oldCost=1e6 ## starting cost for the psudo liklihood
  news <- matrix(NA, 1, nvar) # this vector keeps the values for the new set of poposed samples
  olds<-Varinfo[which(varnames%in%Varinfo$Variable),2] ## finding starting values

  Pmeans<-Varinfo[which(varnames%in%Varinfo$Variable),3] ## finding starting values
  Psds<-Varinfo[which(varnames%in%Varinfo$Variable),4] ## finding starting values
  stepsU<-Varinfo[which(varnames%in%Varinfo$Variable),5] # proposal - step size
  VarT<-Varinfo[which(varnames%in%Varinfo$Variable),6] # variable type - from dataframe
  stepsL<-rep(0,length(varnames)) # it's not being used for now.
  boundU<-Varinfo[which(varnames%in%Varinfo$Variable),7] # upper bound for check the new sample
  boundL<-Varinfo[which(varnames%in%Varinfo$Variable),8]# lower bound for check the new sample
  Funcs<-Varinfo[which(varnames%in%Varinfo$Variable),9] # functions for  sample generation
  # check to see if we have information for all variables
  if(nvar!=length(olds)) stop("At least one of the parameters is not in the Variableinfo dataframe.")
  if(!Gibbs & is.null(vc)) stop("If gibbs is false, you need to provide the variance covarinace matrix.")
  ## finding the starting and end date for filtering simulation results
  Start.date <-obs[1,1]
  End.date <-  obs[nrow(obs),1]
  j<-1 # this counts the number of accepted samples and put it on matrix
  tsimstart<-proc.time()


  ###############################################################################
  ###### XML PART
  ###############################################################################

  xmlvarL<-VarinfoXML[,1]
  varnamesXML<-xmlvarL
  elpXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),2]
  oldsXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),3] ## finding starting values
  PmeansXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),4] ## finding starting values
  PsdsXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),5] ## finding starting values
  stepsUXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),6] # proposal - step size
  stepsLXML<-rep(0,length(varnamesXML)) # it's not being used for now.
  boundUXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),7] # upper bound for check the new sample
  boundLXML<-VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),8]# lower bound for check the new sample
  FuncsXML<-as.character(VarinfoXML[which(varnamesXML%in%VarinfoXML$Variable),9]) # functions for  sample generation
  newsXML <- matrix(NA, 1, length(xmlvarL)) # th

  for (i in 1:nitr) {
    ###############################################################################
    ###### Doing the random sampling
    ###############################################################################
    turn <- sample(1:ntotal, replace = TRUE)[1] ### Who's trun is that ? TAKING TURN FOR CHANGING THE PARAMETERS
   varbosename<-""
    if(turn<=nvar){
      varbosename<-varnames[turn]
  #################### Stating the loop
  cat("APSIM part  => \n")
    repeat {
        news <- olds
        ## give a random walk to the one whos is has turn
       if(Funcs[turn]=="Normal"){
        news[turn] <- olds[turn] + rnorm(1, stepsL[turn], stepsU[turn])
       }else if(Funcs[turn]=="Uniform"){
         news[turn] <- olds[turn] + runif(1, stepsL[turn], stepsU[turn])
       }
        #print(news)
        # check to see if the new sample is in specified range.
        if(news[turn]<boundU[turn] & news[turn]>boundL[turn]){break;}

    }

    newsXML <- oldsXML#keeping xml unchanged

    }else{

      cat("XML part =>  \n")
      turnxml<-turn-nvar
      varbosename<-varnamesXML[turnxml]
      repeat {
        newsXML <- oldsXML
     #   cat(oldsXML[turn],"-",stepsLXML[turn],"-",stepsUXML[turn])
          ## give a random walk to the one whos is has turn
          if(FuncsXML[turnxml]=="Normal"){
            newsXML[turnxml] <- oldsXML[turnxml] + rnorm(1, stepsLXML[turnxml], stepsUXML[turnxml])
          }else if(FuncsXML[turn]=="Uniform"){
            newsXML[turnxml] <- oldsXML[turnxml] + runif(1, stepsLXML[turnxml], stepsUXML[turnxml])
          }

          # check to see if the new sample is in specified range.
          if(newsXML[turnxml]<boundUXML[turnxml] & newsXML[turnxml]>boundLXML[turnxml]){break;}

      }

      news <- olds #keeping apsim unchanged
    }

    apsimValue <- as.list((news)) ##Takes the values form new generated samples
    xmlvarval<- as.list((newsXML)) ##Takes the values form new generated samples
    ###############################################################################
    ###### Running model
    ###############################################################################

    results<-apsimRun(apsimWd=apsimWd, apsimExe=apsimExe, apsimFile=apsimFile,
                      apsimVarL,VarT, Varvalues=apsimValue,
                      xmlfile = ,xmlvarL=xmlvarL, xmlvarValue=xmlvarval,
                      tag="",unlinkf=F,xmlelp=elpXML)


    ## joining the simulation results with observed
    names(results)[1]<-names(obs)[1]
    n1<-names(results)[1]
    n2<-names(obs)[1]
    attr(n2,"names")<-n1



    results<-results%>%right_join(obs,by=n2)
    results[,2]<-as.numeric(results[,2])
    results[,3]<-as.numeric(results[,3])
    #print(str(results))
    #print(results)
    #cat(dim(sws),"--",dim(results),"--",dim(obs),"\n")
    ### estimating the cost for liklihood
    cost_c <- 0.5 * (sum((results[,2] - results[,3]) ^ 2)) / var(results[,2])
    #cat("HERE3\n")
    ###############################################################################
    ################### Calculating the prior information piece
    ###############################################################################
    pratio<-1
    if(turn<=nvar){
    pratio <- pratio * ((dnorm(news[turn], PmeansXML[turn], PsdsXML[turn])) / (dnorm(olds[turn], PmeansXML[turn], PsdsXML[turn])))
    }else{
      pratio <- pratio * ((dnorm(newsXML[turnxml], PmeansXML[turnxml], PsdsXML[turnxml])) / (dnorm(oldsXML[turnxml], PmeansXML[turnxml], PsdsXML[turnxml])))
    }
    if(is.nan(pratio)) {
      print("Combination of prior mean and sd results in 0 probability. It needs to be refined.\n")
      next}
    ###############################################################################
    ################################
    ############ Making desicion about the propsed sample
    ################################
   if(usePrior){
     Desi<-log(runif(1, 0, 1)) < ((oldCost- cost_c) + (log(pratio)))
   }else{
     Desi<-log(runif(1, 0.8, 1)) < (oldCost- cost_c)
   }
  if(verbose)  {
    cat(i,"/",nitr,"-> Variable:",varbosename,"\n",
        #  "      -New sample:",news[turn],"\n",
         # "      -Old sample:",olds[turn],"\n",
          "      -Current cost:",cost_c,"\n",
          "      -Old cost:",oldCost,"\n",
          "      -Accetpted:",Desi,"\n")
  }
  if (Desi) { #log form
      olds <- news#updating
      oldCost <- cost_c	#cost
      sws[,j] <- results[,2]
      j<-j+1
      if(!is.null(VarinfoXML)){tline<-c(news,newsXML,cost_c)}else{ tline<-c(news, cost_c)}
      out[j,]<-tline
  }
    #print(str(out))
    cat("===================\n")
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
      #unlink(c(Fname,outFname,sumFname), recursive = FALSE)
    }
  }# end e loop

  #building output
    output<-list("Param" = out[which(complete.cases(out)),], "Simulations" = sws[, colSums(is.na(sws)) != nrow(sws)]
, "Variable Info" = Varinfo,"var"=c(varnames,varnamesXML),"Obs"=obs,"Itration"=i,"APfile"=apsimFile,"Gsampling"=Gibbs,"Simulation name"=Simname,
"Elapsed time"=proc.time()-tsimstart)

    class(output)<-"apsimOptim"
  return(output)
}
