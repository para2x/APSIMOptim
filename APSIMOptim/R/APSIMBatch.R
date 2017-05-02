################## Ready for the run and estimating the cost
apsimBatch<-function(apsimWd, apsimExe, apsimFile, apsimVarL, Varinfo, tag, unlinkf=F, nitr=10,
                    show.output = TRUE,verbose=T,Simname="Simulation"){
  options(warn=-1) #supress warnings resulting from cbind and join
  nvar<-length(apsimVarL) # number of variables
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
  out <- matrix(NA, nitr, nvar) # output which contains all the parameters posterior
  outallres <- matrix() # output which contains all the parameters posterior
  Pmeans<-Varinfo[which(varnames%in%Varinfo$Variable),2] ## finding starting values
  Psds<-Varinfo[which(varnames%in%Varinfo$Variable),3] ## finding starting values
  VarT<-Varinfo[which(varnames%in%Varinfo$Variable),4] # variable type - from dataframe
  boundU<-Varinfo[which(varnames%in%Varinfo$Variable),5] # upper bound for check the new sample
  boundL<-Varinfo[which(varnames%in%Varinfo$Variable),6]# lower bound for check the new sample
  Funcs<-as.character(Varinfo[which(varnames%in%Varinfo$Variable),7]) # functions for  sample generation
  # check to see if we have information for all variables
  if(nvar!=length(Pmeans)) stop("At least one of the parameters is not in the Variableinfo dataframe.")
  if(!(tolower(Funcs[1])%in%c("normal","uniform"))) stop("Your function needs to be either normal or uniform. Make sure the column is char not factor.")
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
          #  cat(turn,"-",Funcs[turn],"\n ----")
              ## give a random walk to the one whos is has turn
             if(tolower(Funcs[turn])=="normal"){
              news[turn] <- rnorm(1, Pmeans[turn], Psds[turn])
             }else if(tolower(Funcs[turn])=="uniform"){
               news[turn] <- runif(1, Pmeans[turn], Psds[turn])
             }
           # cat(Pmeans[turn],"\n ----",Psds[turn],"\n ----",boundL[turn],"\n",news[turn],"\n")

            if(is.nan(news[turn] )) stop("Somthing is wrong wth Varinfo. Second column should be lower bound for uiform and mean for normal.")
              # check to see if the new sample is in specified range.
              if(news[turn]<boundU[turn] & news[turn]>boundL[turn]){break;}

          }
      }
    ###############################################################################
    ###### Running model
    ###############################################################################
    apsimValue <- as.list(round(news,5)) ##Takes the values form new generated samples
    Fname<-paste0(unlist(strsplit(apsimFile, "[.]"))[1],"_edited",tagc,".apsim") #edited file
    ## Edit file
    simname<-(edit_apsim(file = apsimFile, wd = apsimWd, var = apsimVar, tag=paste0("_edited",tagc),
                         value = apsimValue, varType = VarT, Elpos = elp, overwrite = FALSE))
 # cat("----------",simname,"\n")
    outFname<-paste0(unclass(simname),".out") #outfile
    sumFname<-paste0(unclass(simname),".sum") #sumfile

    # Find the dited file location
    system(paste(addCommas(apsimExe),Fname, sep = " "), show.output.on.console = show.output)
    Sys.sleep(0.1)# giving enough time to let the file go
    # run the edited file
    results <- (read_out_sen(paste0(getwd(),"/",outFname)))
#print(results)
    results <- apply(results, 2, as.numeric)
    results <-as.data.frame(results)
    ###############################################################################
    ################### Calculating the prior information piece
    ###############################################################################
    if(nrow(results>0)){
      tryCatch({
         sws<-rbind(sws,results%>%summarise_all(funs(mean,var)))
         outallres<-cbind(outallres,results)
         out[i,]<-news
      },error=function(cond){
   print(cond)
      })


    }

    ###############################################################################
    #######################Doing the math for sensitivity
    ###############################################################################
    if(verbose)  {
      cat("=============>",i,"\n")
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
      "Variable Info" = Varinfo,"var"=varnames,"Itration"=i,"APfile"=apsimFile,"Simulation name"=Simname,
      "Elapsed time"=proc.time()-tsimstart)

    class(output)<-"apsimBatch"
  return(output)
}
