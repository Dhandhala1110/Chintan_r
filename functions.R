#############################################
# Function file, several function definitions
#############################################

#####
## Functions: Parameter learning, get data
##################################################################################
## Read in data and learn parameters for the bayesian network
## uses data stored at WD
## hand over: DAG (untrained bayesian network), netname(Vorformling, Blasformteil)
## return: bayes net class bn.fit
trainBN <- function(dag,netname){
  if(netname == "Vorformling")
    data <- read.csv2("Data_archive/data_download_Vorformling.csv", header = TRUE, 
                      colClasses = "factor")
  else if(netmane == "Blasformteil")
    data <- read.csv2("Data_archive/data_download_Blasformteil.csv", header = TRUE, 
                      colClasses = "factor")
  else
  {
    cat("Fehler bei Auswertung des Bayes Netzes!")
    return()
    }
  data <- data[,-1] # remove first column observation counter
  bayesnet <- bn.fit(x = dag, data =  data, method = "mle") # learn
  
  return(bayesnet)
}

## save manual inputs and machine parameters at a data store
## hand over: manual datas
## return: none
saveDataTuble <- function(manualdata){
  datastore <- read.csv2("Data_archive/datastore.csv") 
  datastore[nrow(datastore)+1,1] <- nrow(datastore)+1
  for (i in 1:length(manualdata)) {
    datastore[nrow(datastore),colnames(manualdata)[i]] <- manualdata[1,i]
  }
  cpudata <- read.csv2("Data_archive/machineparameters.csv", sep=",")
  datastore[nrow(datastore),colnames(cpudata)[1]] <- cpudata[nrow(cpudata),1]
  for (i in 3:length(cpudata)){
    datastore[nrow(datastore),colnames(cpudata)[i]] <- cpudata[nrow(cpudata),i]
  }
  write.csv2(datastore, "Data_archive/datastore.csv", quote = FALSE,
             row.names = FALSE)
}

## call python file to extract data from CPU and stores into a csv-file
## file name machineparameters.csv
extractDataFromCPU <- function(){
  py_run_file("Snap7Tests/main.py")
}

## select manual parameters for an error
## hand over: prepared excel list (customizeExcellist()), bayes net and error
## return: vector with manual parameters
manParaFault <- function(excellist, bayesnet, error){
  # initialize list for manual parameters
  manlist <- NULL
  # extract manual parameters for selected error
  nodes <- InfluNodes(bayesnet, error)
  # check if parameter is a manual one
  for(i in 1:length(nodes)){
    index <- which(nodes[i] == excellist[,"Netzname"])
    if(excellist[index,"Erfassung"] == "manuell")
      manlist <- c(manlist, nodes[i])
  }
  return(manlist)
}

## Select the influence manual parameters from a fault !!! old parameter list !!!!
## hand over: error name, parameter list from prepareExcellist() and
## nametyp: 1 or "Parameter"=long name,
##          3 or "Name im Bayes-Netz"= clear name bayes net
## return: array with parameter names
selectManualParametersFromFault <- function(fault,parameterlist,nametyp){
  parameter <- NULL # initialize
  n <- 1 # initialize index counter
  # Loop through each row
  for (i in 1:nrow(parameterlist)) {
    # check if the parameter is a cause of the error and is a manual parameter
    if(parameterlist[i,fault]=="x" & parameterlist[i,"Erhebung"]=="visuell"){
      parameter[n] <- parameterlist[i,nametyp]
      n <- n + 1
      #cat(parameterlist[i,1])
      #cat("\n")
    }
  }
  return(parameter)
}

## prepare excel list for usage
## delete unused columns and rows, names header
## hand over: excel list
## return: new excel list
prepareExcellist <- function(excellist){
  excellist <- as.data.frame(excellist) # convert to a data frame
  excellist[is.na(excellist)] <- 0      # set NA fields to 0
  colnames(excellist) <- excellist[3,]  # set column names
  delete <- c(1,2,3) # delete rows
  excellist <- excellist[-delete,] # delete rows
  excellist <- subset(excellist,select=-c(Kommentar,Wert,Bearbeitungsstand))
  
  return(excellist)
}

## select default level from manual input parameter for chosen fault
## hand over: parameter list from prepareExcellist() and error name
## return: array with default levels
selectDefaultLevel <- function(parameterlist, fault){
  # manual input parameters for chosen fault
  parameters <- selectManualParametersFromFault(fault,parameterlist,"Parameter")
  defaultlevel = c(NULL)
  # select default levels
  for (i in 1:length(parameters)) {
    # row index from parameter
    indexrow = which(parameterlist == parameters[i], arr.ind=TRUE)[1,1]
    # set default level
    defaultlevel[i] <- parameterlist[indexrow,"Zustand_default"]
  }  
  return(defaultlevel)
}

#####
## Functions: To calculate inference (old ones) and help functions
##################################################################################
## estimate inference from one fault to all influence nodes
# Parameters to pass: Bayesian Net, Event Parameter, List with level 
# of each node
# select influence nodes to a chosen node; calculate inference
# for each influence node as evidence and for each level of a
# node; calculate difference between each level of a node; create 
# a list with Difference of probabilities, from level, to level and 
# influence node; at the end sort the hole list descending
estimateInference <- function(bn, event_var, bn_level){
  # define variables
  event_lvl <- "yes"  # fault level
  event_lvl <- "Ja"   # TODO das muss noch sauber gelöst werden
  #event_lvl <- "Schlecht"
  
  probs <- list(NULL) # initialize probability list
  
  # empty data frame, probability list with node and from state to state
  differences_prob_list <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(differences_prob_list) <- c('prob', 'from', 'to', 'node')
  n <- 1 # initialize as index
  m <- 1 # initialize as index
  
  # select possible influence node to chosen fault
  possiblenodes <- InfluNodes(bn, event_var)
  
  # calculate inference for every possible influence node as evidence
  for(i in 1:length(possiblenodes)){  
    # extract one node
    evidence_var <- possiblenodes[i]
    # extract levels from select node; bn_level is a list with node and level
    evidence_lvl <- c(eval(parse(text = paste("bn_level$", evidence_var, 
                                              sep="")))) 
    
    # estimate inference
    cat("Call function inference with evidence_var:", evidence_var, 
        "and evidence_lvl:", evidence_lvl, "\n")
    # one evidence per row
    probs[[i]] <- inferenceOneNode(event_var, event_lvl, evidence_var,
                                   evidence_lvl, bn) 
    
    # calculate difference between all levels of a node
    temp_diff <- diffInference(probs[[i]], evidence_lvl)
    # switch states when negative
    temp_diff <- change2Positive(temp_diff)
    
    # put probdiff, from state, to state and node into a list
    for(j in n:(n+nrow(temp_diff)-1)){
      temp_diff[m,1] <- round(as.numeric(temp_diff[m,1]), digits = 4)
      differences_prob_list[j,] <- c(temp_diff[m,], evidence_var)
      m <- m + 1
    }
    n <- n + nrow(temp_diff)
    m <- 1
    
    
    # add next row
    probs[i+1] <- list(NULL)
  }
  # delete last empty row
  probs[length(possiblenodes)+1] <- NULL
  
  # from decimal to percent
  differences_prob_list[,1] <- (as.numeric(differences_prob_list[,1]) *100)
  
  # Sort by decreasing probability
  differences_prob_list <- differences_prob_list[order(differences_prob_list$prob,
                                                       decreasing = TRUE),]
  
  return(differences_prob_list)
}

## calculate difference between each prob from one node
## hand over: list of states
## return: list with differences due to state change (from to to)
diffInference <- function(probabilitys, level){
  # empty data frame row 'prob_diff', 'from level', 'to level'
  diff_list <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(diff_list) <- c('prob_diff', 'from', 'to')
  
  n <- 1 # initialize index counter
  for(j in 1:(length(probabilitys)-1)){
    for(i in j:(length(probabilitys)-1)){
      # Difference of probabilities, between from state ... and to state ...
      diff_list[n,] <- c((probabilitys[j] - probabilitys[i+1]), level[j], 
                         level[i+1])
      n <- n + 1
    }
  }
  
  return(diff_list)
}

## extract level of each node
## hand over: bayesian network 
## return: list with arrays pair of node name and state/ level 
extractLevelsFromBN <- function(bayesnet){
  # initialize two empty lists
  nodelevel <- list(NULL)
  nodename  <- list(NULL)
  for(i in 1:length(bayesnet)){
    # select node name
    nodename[i]  <- bayesnet[[i]][[1]]
    # select node levels
    nodelevel[i] <- ifelse(TRUE,dimnames(bayesnet[[i]][[4]]))
  }
  # name lists with node name
  names(nodelevel) <- nodename
  
  return(nodelevel)
}

## change negative probability into positive and switch levels
## check every probability; if <0 switch from and to level and 
## switch to positive
## hand over: probability list
## return: customized probability list
change2Positive <- function(probabilities){
  # runs through every row
  for(i in 1:nrow(probabilities)){
    # check for negative probability, switch if necessary 
    if(probabilities[i,1] < 0){
      probabilities[i,1] <- abs(as.numeric(probabilities[i,1]))
      temp <- probabilities[i,2] # save from level
      probabilities[i,2] <- probabilities[i,3] # switch from and to
      probabilities[i,3] <- temp
    }
  }
  return(probabilities)
}

## estimate inference from one evidence var and all its levels/ states
## hand over: event variable (error), event state, evidence variable and a list
##            of evidence states, bayes net as bn.fit
## return: probability list
inferenceOneNode <- function(event_var, event_lvl, evi_var, evi_lvl, bayes_net){
  prob_list <- rep(0, length(evi_lvl))
  # string for event
  str_event <- paste("(", event_var, " == '", event_lvl, "')", sep = "")
  # calculate inference for every evidence level
  for(i in 1:length(evi_lvl)){
    # string for evidence
    str_evide <- paste("(", evi_var, " == '", evi_lvl[i], "')",
                       sep = "")
    # merge strings
    cmd = paste("cpquery(bayes_net, ", str_event, ", ", str_evide, 
                ", n=10^6)", sep = "")
    # calculate
    prob <- eval(parse(text = cmd))
    prob_list[i] <- prob
    # print out
    cat("P(", event_var, "=", event_lvl , "|", evi_var, "=",
        evi_lvl[i], ") =", prob, "\n")
  }
  
  return(prob_list)
}

## estimate inference from one event var and a pair of evidence var and level
## hand over: event variable (error), event state, to lists with the same length
##            evidence variable and evidence state, bayes net as bn.fit
## return: probability list
inferenceMultipleNode <- function(event_var,event_lvl,evi_var,evi_lvl,bayes_net){
  prob_list <- rep(0, length(evi_var))
  # string for event
  str_event <- paste("(", event_var, " == '", event_lvl, "')", sep = "")
  # calculate inference for every pair of evidence
  for(i in 1:length(evi_var)){
    # string for evidence
    str_evide <- paste("(", evi_var[i], " == '", evi_lvl[i], "')",
                       sep = "")
    # merge strings
    cmd = paste("cpquery(bayes_net, ", str_event, ", ", str_evide, 
                ", n=10^6)", sep = "")
    # calculate
    prob <- eval(parse(text = cmd))
    prob_list[i] <- prob
    # print out
    cat("P(", event_var, "=", event_lvl , "|", evi_var[i], "=",
        evi_lvl[i], ") =", prob, "\n")
  }
  
  return(prob_list)
}

## string for changing parameter
## hand over: probability list (probability, from state, to state, cause name)
## return: string
strParameterChange <- function(problist){
  string <- paste0("Betrachten Sie Parameter '", 
                   as.character(problist[1,4]), 
                   "' und wechseln vom Zustand '",
                   as.character(problist[1,2]), 
                   "' in den Zustand '", as.character(problist[1,3]), 
                   "', die Wahrscheinlichkeit des Fehlers sinkt somit um ", 
                   as.numeric(problist[1,1]), "%\n", sep ="")
  
  return(string)
}

#####
## Functions: Different types how to select nodes
##################################################################################
## get nodes from a bayesian network
## hand over: bayes net as bn or bn.fit
## return array with nodes
getNodes <- function(bayesnet){
  node <- nodes(bayesnet)
  return(node)
}

## get nodes from a bayesian network without root nodes
## hand over: bayes net as bn or bn.fit
## return: array with nodes
getNodesWithoutRoot <- function(bayesnet){
  node <- nodes(bayesnet) ##get nodes
  rootnode <- root.nodes(bayesnet) # get root nodes
  # remove root nodes
  for(i in 1:length(rootnode)){
    index <- match(rootnode[i], node)
    node <- node[-index]
  }
  return(node)
}

## get leaf nodes from a bayesian network
## hand over: bayes net as bn or bn.fit
## return: array with nodes
getLeafnodes <- function(bayesnet){
  node <- leaf.nodes(bayesnet)
  
  return(node)
}

## select influence nodes to a chosen node
## check if their is a path between the main node and an other one
## hand over: bayes net as bn or bn.fit and name of the main node i.e. error name
## return: array, nodes with a path to the chosen node
InfluNodes <- function(bayesnet, selectnode){
  allnodes <- nodes(bayesnet) 
  influencenodes <- c("") # empty array for nodes
  n <- 1 # counter
  # go through each node
  for(i in 1:length(allnodes)){
    if(allnodes[i] != selectnode){ # remove selected note
      if(path.exists(bayesnet, allnodes[i], selectnode)){
        influencenodes[n] <- allnodes[i]
        n <- n + 1
      } # end if
    } # end if
  } # end for
  return(influencenodes)
}

#####
## Functions: Plot and highlight DAG or  bayes net
##################################################################################
## draw bayesian network
## hand over: bayesian network as bn or bn.fit
## return: customized plot for renderGraph()
drawBayesNet<- function(bayesnet){
  bn_plot <- graphviz.plot(bayesnet,
                           shape = "ellipse",
                           #layout = "neato", # alternative layout
                           layout = "dot", 
                           render = FALSE)
  nodeRenderInfo(bn_plot) <- list(lwd = 3)
  edgeRenderInfo(bn_plot) <- list(lwd = 3)
  
  return(bn_plot)
}

## highlight bayesian network
## highlight error (lightblue), cause (pink) and edge (bold black)
## hand over: the return from graphviz.plot() and error name, error name and
##            cause names
## return: customized plot for renderGraph()
highlightBN <- function(plot_bn, node_fault, node_para){
  # create list <error name> % <color>
  fault <- "lightblue"
  names(fault) <- node_fault
  # create list <cause name> % <color>
  para <- rep("pink", length(node_para))
  names(para) <- node_para
  # create list <edge> % <color>
  col <- rep("black", length(node_para))
  for(i in 1:length(node_para)){
    names(col)[i] <- paste0(node_para[i],"~",node_fault, sep = "")  
  }
  # set colors
  nodeRenderInfo(plot_bn) <- list(fill = fault, col = fault)
  nodeRenderInfo(plot_bn) <- list(fill = para, col = para)
  edgeRenderInfo(plot_bn) <- list(col = col)
  
  return(plot_bn)
}

## highlight faults 'light blue'
## hand over: the return from graphviz.plot() and error name
## return: customized plot for renderGraph()
highlightFault <- function(plot_bn, node_fault){
  # create list <error name> % <color>
  fault <- rep("lightblue", length(node_fault))
  names(fault) <- node_fault
  # set color
  nodeRenderInfo(plot_bn) <- list(fill = fault, col = fault)
  nodeRenderInfo(plot_bn) <- list(lwd = 1)
  edgeRenderInfo(plot_bn) <- list(lwd = 1)
  
  return(plot_bn)
}


#####
# Functions: Creation of a bayesian network structure and parts of it
##################################################################################
## prepare excel list for use to create bayesian structure
## hand over: excel list with bayesian network structure
## return: customized excel list
customizeExcellist <- function(excelliste){
  excelliste <- as.data.frame(excelliste) # convert to a data frame
  excelliste[is.na(excelliste)] <- "-"    # set NA fields with -
  return(excelliste)
}

## create model string from excel list with structure matrix
## hand over: prepared excel list (customizeExcellist()) and fault number (column 
##            number) or name
## return: model string 
createMStrings <- function(liste,faultnumber){
  # initialize equations
  node_factors <- "" # for influential variables
  # for error
  node_fault <- paste0("[", colnames(liste[faultnumber]), "|", sep="")
  # create error and influential variables equations
  for(i in 1:nrow(liste)){
    # check if variable is relevant for this error
    if(liste[i,colnames(liste[faultnumber])] != "-"){
      # check if variable has a direct arc to this error
      if(liste[i,colnames(liste[faultnumber])] == "x"){
        # set variable as parent
        #node_fault <- paste0(node_fault, liste[i,"Knoten"], ":", sep="")}
        node_fault <- paste0(node_fault, liste[i,"Netzname"], ":", sep="")}
      # set variable 'x' at first position in equation [x
      #node_factors <- paste0(node_factors,"[", liste[i,"Knoten"])
      node_factors <- paste0(node_factors,"[", liste[i,"Netzname"])
      # check if variable has parents
      if(liste[i,"Elternknoten1"] != "-") {
        node_factors <- paste0(node_factors,"|",liste[i,"Elternknoten1"],sep ="")}
      if(liste[i,"Elternknoten2"] != "-") {
        node_factors <- paste0(node_factors,":",liste[i,"Elternknoten2"],sep="")}
      if(liste[i,"Elternknoten3"] != "-") {
        node_factors <- paste0(node_factors,":",liste[i,"Elternknoten3"],sep="")}
      if(liste[i,"Elternknoten4"] != "-") {
        node_factors <- paste0(node_factors,":",liste[i,"Elternknoten4"],sep="")}
      if(liste[i,"Elternknoten5"] != "-") {
        node_factors <- paste0(node_factors,":",liste[i,"Elternknoten5"],sep="")}
      # close influential variable equation 
      node_factors <- paste0(node_factors, "]", sep = "")
    }
  }
  # remove last character ':' of the string and add ']' at the end
  node_fault <- paste0(substr(node_fault,1,nchar(node_fault)-1), "]", sep="")
  # merge influential variables and fault to one string
  mstring <- paste0(node_factors, node_fault)
  
  return(mstring)
}

## create multiple model strings from excel list and save as txt-file
## hand over: prepared excel list (customizeExcellist()) with structure 
##            matrix, file/net name and name of the first error 
## return: none, save txt-file at WD, as table error name and modelstring
excel2ms <- function(excellist, netname, firstfault){
  # column number of first error
  k <- which(colnames(excellist)==firstfault)
  # minus (k-1) because first error at column k
  mslist <- matrix(nrow = 1, ncol = (ncol(excellist)-(k-1)))
  # create model string and save at array
  for (i in k:ncol(excellist)) {
    mstring <- createMStrings(excellist,i)
    mslist[i-(k-1)] <- mstring  
  }  
  # names columns with error names
  colnames(mslist) <- colnames(excellist[k:ncol(excellist)])
  # save model strings as <netname>.txt
  write.table(mslist, file = paste0("MS", netname, ".txt", sep = ""))
}

## create DAG from model string file
## hand over: model string file with multiple model strings
##            i.e. read.table('MSVorformling.txt')
## return: DAG
createDAG <- function(msfile){
  msstring <- ""
  # extract to a single string
  for (i in 1:length(msfile)) {
    msstring <- paste0(msstring, msfile[i], sep = "")
  }
  # convert model string to DAG
  msstring <- collapsModelstring(msstring)
  dag <- collapsedMs2DAG(msstring)
  
  return(dag)
}

#####
# Functions: Root cause analysis
##################################################################################
## calculate the most probable cause
## probability: how likely it is that the cause will occur
## return: a decreasing probability list
## hand over: bayesian network and one error name (evidence_var)
## P(cause=TRUE | error=TRUE)
causeanalysis_likeliest <- function(bayesnet, evidence_var){
  # Initialization
  evidence_lvl <- "Ja" # error = TRUE
  evidence <- paste("(",evidence_var,"=='",evidence_lvl,"')",sep="") #event string
  wkt_list <- data.frame(matrix(ncol = 3, nrow = 0)) # initialize matrix
  colnames(wkt_list) <- c('causeName','causeState','probability') # names columns 
  j <- 0 # set index counter for matrix
  nodelist <-  InfluNodes(bayesnet,evidence_var) # all cause nodes
  
  for (i in 1:length(nodelist)) { # for every cause node
    expression <- paste0("bayesnet[['", nodelist[i], "']][['prob']]")
    problist <- eval(parse(text = expression)) # select node states
    statenames <- rownames(problist) # select state names
    for (n in 1:length(statenames)) { # for every cause node state
      if (statenames[n] != "Nein" & statenames[n] != "iO") {
        event <- paste("(",nodelist[i],"== '",statenames[n],"')",sep = "")
        cmd = paste("cpquery(bayesnet,",event,", ",evidence,", n=10^4)",sep = "")
        prob <- eval(parse(text = cmd)) # calculate inference
        prob <- round(as.numeric(prob),4)
        cat("P(",nodelist[i],"=",statenames[n] ,"|",evidence_var,"=",evidence_lvl,
            ") =",prob,"\n")
        j <- j + 1 # increment index counter
        wkt_list[j,] <- c(nodelist[i],statenames[n],prob)
      } # end if
    } # end for
  } # end for
  # Sort list in descending order
  wkt_list <- wkt_list[order(as.numeric(wkt_list$probability),decreasing = TRUE),]
  return(wkt_list)
}

## calculate the most probable cause, can consider known good cause condition
## probability: how likely it is that the cause will occur
## hand over: bayes net, one error name, list of known good cause conditions 
## return: a decreasing probability list
## P(cause=TRUE | error=TRUE : cause_i=TRUE...)
causeanalysis_likeliestdyn <- function(bayesnet,error,knoEvi=NULL,knoState=NULL){
  # Initialization
  error_lvl <- "Ja" # error = TRUE
  evidence <- paste("(",error," == '",error_lvl,"'",sep="") # evidence string
  wktlist <- data.frame(matrix(ncol = 3, nrow = 0)) # initialize matrix
  colnames(wktlist) <- c('causeName','causeState','probability') # names columns 
  j <- 0 # set index counter for matrix
  
  nodelist <-  InfluNodes(bayesnet,error) # all cause nodes
  if (!is.null(knoEvi)) {
    # remove known causes from list
    nodelist <- nodelist [!nodelist  %in% knoEvi]
    # create string for causes with known state
    knowledge <- knownCauses(knoEvi, knoState)
    # add to evidence string (formula)
    evidence <- paste(evidence, knowledge, sep="")
  } # end if
  evidence <- paste(evidence, ")",sep = "") # close evidence string
  
  for (i in 1:length(nodelist)) { # for every cause node
    expression <- paste0("bayesnet[['", nodelist[i], "']][['prob']]")
    problist <- eval(parse(text = expression)) # select node states
    statenames <- rownames(problist) # select state names
    for (n in 1:length(statenames)) { # for every cause node state
      if (statenames[n]!="Nein" & statenames[n]!="iO" & statenames[n]!="mittel"){
        event <- paste("(",nodelist[i],"== '",statenames[n],"')",sep = "")
        cmd = paste("cpquery(bayesnet,",event,",",evidence,", n=10^5)",sep = "")
        prob <- eval(parse(text = cmd)) # calculate inference
        prob <- round(as.numeric(prob),4)
        cat("P(",nodelist[i],"=",statenames[n],"|",error,"=",error_lvl,")=",prob,"\n")
        #cat(evidence,"\n")
        j <- j + 1 # increment index counter
        wktlist[j,] <- c(nodelist[i],statenames[n],prob) # set to probability list
      } # end if
    } # end for
  } # end for
  # Sort list in descending order
  wktlist <- wktlist[order(as.numeric(wktlist$probability),decreasing = TRUE),]
  return(wktlist)
}
## P(cause=TRUE | error=TRUE : cause_i=FALSE...)
causeanalysis_likeliestdyn_old <- function(bayesnet,error,knownEvidence=NULL){
  # Initialization
  error_lvl <- "Ja" # error = TRUE
  evidence <- paste("(",error," == '",error_lvl,"'",sep="") # evidence string
  wktlist <- data.frame(matrix(ncol = 3, nrow = 0)) # initialize matrix
  colnames(wktlist) <- c('causeName','causeState','probability') # names columns 
  j <- 0 # set index counter for matrix
  
  nodelist <-  InfluNodes(bayesnet,error) # all cause nodes
  if (!is.null(knownEvidence)) {
    # remove known causes from list
    nodelist <- nodelist [!nodelist  %in% knownEvidence]
    # create string for causes with known state
    knowledge <- evidenceStr(bayesnet, knownEvidence)
    # add to evidence string (formula)
    evidence <- paste(evidence, knowledge, sep="")
  } # end if
  evidence <- paste(evidence, ")",sep = "") # close evidence string
  
  for (i in 1:length(nodelist)) { # for every cause node
    expression <- paste0("bayesnet[['", nodelist[i], "']][['prob']]")
    problist <- eval(parse(text = expression)) # select node states
    statenames <- rownames(problist) # select state names
    for (n in 1:length(statenames)) { # for every cause node state
      if (statenames[n] != "Nein" & statenames[n] != "iO") {
        event <- paste("(",nodelist[i],"== '",statenames[n],"')",sep = "")
        cmd = paste("cpquery(bayesnet,",event,",",evidence,", n=10^4)",sep = "")
        prob <- eval(parse(text = cmd)) # calculate inference
        prob <- round(as.numeric(prob),4)
        cat("P(",nodelist[i],"=",statenames[n],"|",error,"=",error_lvl,")=",prob,"\n")
        #cat(evidence,"\n")
        j <- j + 1 # increment index counter
        wktlist[j,] <- c(nodelist[i],statenames[n],prob) # set to probability list
      } # end if
    } # end for
  } # end for
  # Sort list in descending order
  wktlist <- wktlist[order(as.numeric(wktlist$probability),decreasing = TRUE),]
  return(wktlist)
}

## calculate the most probable cause, can consider known good cause condition
## Removes intermediate variables (first letter "z") and non-modifiable ones
## probability: how likely it is that the cause will occur
## hand over: bayes net, one error name, list of known good cause conditions 
## return: a decreasing probability list
## P(cause=TRUE | error=TRUE : cause_i!=TRUE...)
causeanalysis_likeliestdyn_v1 <- function(bayesnet,error,knoEvi=NULL,knoState=NULL){
  # Initialization
  error_lvl <- "Ja" # error = TRUE
  evidence <- paste("(",error," == '",error_lvl,"'",sep="") # evidence string
  wktlist <- data.frame(matrix(ncol = 3, nrow = 0)) # initialize matrix
  colnames(wktlist) <- c('causeName','causeState','probability') # names columns 
  j <- 0 # set index counter for matrix
  
  nodelist <-  InfluNodes(bayesnet,error) # all cause nodes
  len <- length(nodelist)
  i <- 1
  # remove intermediate variables from list
  while (i <= len) {
    if(substr(nodelist[i],1,1)=="z"){
      nodelist <- nodelist[-i]
      len <- length(nodelist)
    }
    i <- i +1
  }
  
  if (!is.null(knoEvi)) {
    # remove known causes from list
    nodelist <- nodelist [!nodelist  %in% knoEvi]
    # create string for causes with known state
    knowledge <- knownCauses(knoEvi, knoState)
    # add to evidence string (formula)
    evidence <- paste(evidence, knowledge, sep="")
  } # end if
  evidence <- paste(evidence, ")",sep = "") # close evidence string
  
  for (i in 1:length(nodelist)) { # for every cause node
    expression <- paste0("bayesnet[['", nodelist[i], "']][['prob']]")
    problist <- eval(parse(text = expression)) # select node states
    statenames <- rownames(problist) # select state names
    for (n in 1:length(statenames)) { # for every cause node state
      if (statenames[n] != "Nein" & statenames[n] != "iO") {
        event <- paste("(",nodelist[i],"== '",statenames[n],"')",sep = "")
        cmd = paste("cpquery(bayesnet,",event,",",evidence,", n=10^4)",sep = "")
        prob <- eval(parse(text = cmd)) # calculate inference
        prob <- round(as.numeric(prob),4)
        #cat("P(",nodelist[i],"=",statenames[n],"|",error,"=",error_lvl,")=",prob,"\n")
        #cat(evidence,"\n")
        j <- j + 1 # increment index counter
        wktlist[j,] <- c(nodelist[i],statenames[n],prob) # set to probability list
      } # end if
    } # end for
  } # end for
  # Sort list in descending order
  wktlist <- wktlist[order(as.numeric(wktlist$probability),decreasing = TRUE),]
  return(wktlist)
}

## calculate the strongest cause
## probability: how likely it is that the error will occur
## return: a decreasing probability list
## hand over: bayesian network and one error name (event_var)
## P(error=TRUE | cause=TRUE)
causeanalysis_strongest <- function(bayesnet, event_var){
  # Initialization
  event_lvl <- "Ja" # error = TRUE
  event <- paste("(",event_var," == '",event_lvl,"')",sep = "") # event string
  wkt_list <- data.frame(matrix(ncol = 3, nrow = 0)) # initialize matrix
  colnames(wkt_list) <- c('causeName','causeState','probability') # names columns 
  j <- 0 # set index counter for matrix
  nodelist <-  InfluNodes(bayesnet,event_var) # all cause nodes
  
  for (i in 1:length(nodelist)) { # for every cause node
    expression <- paste0("bayesnet[['", nodelist[i], "']][['prob']]")
    problist <- eval(parse(text = expression)) # select node states
    statenames <- rownames(problist) # select state names
    for (n in 1:length(statenames)) { # for every cause node state
      if (statenames[n] != "Nein" & statenames[n] != "iO") {
        evidence <- paste("(",nodelist[i],"== '",statenames[n],"')",sep = "")
        cmd = paste("cpquery(bayesnet,",event,", ",evidence,", n=10^4)",sep = "")
        prob <- eval(parse(text = cmd)) # calculate inference
        prob <- round(as.numeric(prob),4)
        cat("P(",event_var,"=",event_lvl ,"|",nodelist[i],"=",statenames[n],
            ") =",prob,"\n")
        j <- j + 1 # increment index counter
        wkt_list[j,] <- c(nodelist[i],statenames[n],prob)
      } # end if
    } # end for
  } # end for
  # Sort list in descending order
  wkt_list <- wkt_list[order(as.numeric(wkt_list$probability),decreasing = TRUE),]
  return(wkt_list)
}

## calculate the strongest cause, can consider known good cause conditions
## probability: how likely it is that the error will occur
## hand over: bayes net, one error name, list of known good cause conditions 
## return: a decreasing probability list
## P(error=TRUE | cause=TRUE : cause_i=TRUE...)
causeanalysis_strongestdyn <- function(bayesnet,event,knoEvi=NULL,knoState=NULL){
  start <- Sys.time()
  # Initialization
  event_lvl <- "Ja" # error = TRUE
  eventstr <- paste("(", event, " == '", event_lvl, "')", sep="") # event string
  wktlist <- data.frame(matrix(ncol = 3, nrow = 0)) # initialize matrix
  colnames(wktlist) <- c('causeName','causeState','probability') # names columns 
  j <- 0 # set index counter for matrix
  
  nodelist <-  InfluNodes(bayesnet,event) # all cause nodes
  if (!is.null(knoEvi)) {
    # remove known causes from list
    nodelist <- nodelist [!nodelist  %in% knoEvi]
    # create string for causes with known state
    knowledge <- knownCauses(knoEvi, knoState)
  } # end if
  
  for (i in 1:length(nodelist)) { # for every cause node
    expression <- paste0("bayesnet[['", nodelist[i], "']][['prob']]")
    problist <- eval(parse(text = expression)) # select node states
    statenames <- rownames(problist) # select state names
    for (n in 1:length(statenames)) { # for every cause node state
      if (statenames[n]!="Nein" & statenames[n]!="iO" & statenames[n]!="mittel") {
        evidence <- paste("(",nodelist[i],"== '",statenames[n],"'",sep = "")
        if (!is.null(knoEvi)){
          evidence <- paste(evidence, knowledge, sep="")
        } # end if
        evidence <- paste(evidence, ")",sep = "")
        cmd = paste("cpquery(bayesnet,",eventstr,", ",evidence,", n=10^5)",sep = "")
        prob <- eval(parse(text = cmd)) # calculate inference
        prob <- round(as.numeric(prob),4)
        #cat("P(",event,"=",event_lvl,"|",nodelist[i],"=",statenames[n],")=",prob,"\n")
        #cat(evidence,"\n")
        j <- j + 1 # increment index counter
        wktlist[j,] <- c(nodelist[i],statenames[n],prob) # set to probability list
      } # end if
    } # end for
  } # end for
  # Sort list in descending order
  wktlist <- wktlist[order(as.numeric(wktlist$probability),decreasing = TRUE),]
  print(Sys.time() - start)
  return(wktlist)
}
## P(error=TRUE | cause=TRUE : cause_i=FALSE...)
causeanalysis_strongestdyn_old <- function(bayesnet,event_var,knownEvidence=NULL){
  # Initialization
  event_lvl <- "Ja" # error = TRUE
  event <- paste("(", event_var, " == '", event_lvl, "')", sep="") # event string
  wktlist <- data.frame(matrix(ncol = 3, nrow = 0)) # initialize matrix
  colnames(wktlist) <- c('causeName','causeState','probability') # names columns 
  j <- 0 # set index counter for matrix
  
  nodelist <-  InfluNodes(bayesnet,event_var) # all cause nodes
  if (!is.null(knownEvidence)) {
    # remove known causes from list
    nodelist <- nodelist [!nodelist  %in% knownEvidence]
    # create string for causes with known state
    knowledge <- evidenceStr(bayesnet, knownEvidence)
  } # end if
  
  for (i in 1:length(nodelist)) { # for every cause node
    expression <- paste0("bayesnet[['", nodelist[i], "']][['prob']]")
    problist <- eval(parse(text = expression)) # select node states
    statenames <- rownames(problist) # select state names
    for (n in 1:length(statenames)) { # for every cause node state
      if (statenames[n] != "Nein" & statenames[n] != "iO") {
        evidence <- paste("(",nodelist[i],"== '",statenames[n],"'",sep = "")
        if (!is.null(knownEvidence)){
          evidence <- paste(evidence, knowledge, sep="")
        } # end if
        evidence <- paste(evidence, ")",sep = "")
        cmd = paste("cpquery(bayesnet,",event,", ",evidence,", n=10^4)",sep = "")
        prob <- eval(parse(text = cmd)) # calculate inference
        prob <- round(as.numeric(prob),4)
        cat("P(",event_var,"=",event_lvl,"|",nodelist[i],"=",statenames[n],")=",prob,"\n")
        #cat(evidence,"\n")
        j <- j + 1 # increment index counter
        wktlist[j,] <- c(nodelist[i],statenames[n],prob) # set to probability list
      } # end if
    } # end for
  } # end for
  # Sort list in descending order
  wktlist <- wktlist[order(as.numeric(wktlist$probability),decreasing = TRUE),]
  return(wktlist)
}

## calculate the strongest cause, can consider known good cause conditions
## Removes intermediate variables (first letter "z") and non-modifiable ones
## probability: how likely it is that the error will occur
## hand over: bayes net, one error name, list of known good cause conditions 
## return: a decreasing probability list
## P(error=TRUE | cause=TRUE : cause_i!=TRUE...)
causeanalysis_strongestdyn_v1 <- function(bayesnet,event,knoEvi=NULL,knoState=NULL){
  # Initialization
  event_lvl <- "Ja" # error = TRUE
  eventstr <- paste("(", event, " == '", event_lvl, "')", sep="") # event string
  wktlist <- data.frame(matrix(ncol = 3, nrow = 0)) # initialize matrix
  colnames(wktlist) <- c('causeName','causeState','probability') # names columns 
  j <- 0 # set index counter for matrix
  
  nodelist <-  InfluNodes(bayesnet,event) # all cause nodes
  len <- length(nodelist)
  i <- 1
  # remove intermediate variables from list
  while (i <= len) {
    if(substr(nodelist[i],1,1)=="z"){
      nodelist <- nodelist[-i]
      len <- length(nodelist)
    }
    i <- i +1
  }
  
  if (!is.null(knoEvi)) {
    # remove known causes from list
    nodelist <- nodelist [!nodelist  %in% knoEvi]
    # create string for causes with known state
    knowledge <- knownCauses(knoEvi, knoState)
  } # end if
  
  for (i in 1:length(nodelist)) { # for every cause node
    expression <- paste0("bayesnet[['", nodelist[i], "']][['prob']]")
    problist <- eval(parse(text = expression)) # select node states
    statenames <- rownames(problist) # select state names
    for (n in 1:length(statenames)) { # for every cause node state
      if (statenames[n] != "Nein" & statenames[n] != "iO") {
        evidence <- paste("(",nodelist[i],"== '",statenames[n],"'",sep = "")
        if (!is.null(knoEvi)){
          evidence <- paste(evidence, knowledge, sep="")
        } # end if
        evidence <- paste(evidence, ")",sep = "")
        cmd = paste("cpquery(bayesnet,",eventstr,", ",evidence,", n=10^4)",sep = "")
        prob <- eval(parse(text = cmd)) # calculate inference
        prob <- round(as.numeric(prob),4)
        #cat("P(",event,"=",event_lvl,"|",nodelist[i],"=",statenames[n],")=",prob,"\n")
        #cat(evidence,"\n")
        j <- j + 1 # increment index counter
        wktlist[j,] <- c(nodelist[i],statenames[n],prob) # set to probability list
      } # end if
    } # end for
  } # end for
  # Sort list in descending order
  wktlist <- wktlist[order(as.numeric(wktlist$probability),decreasing = TRUE),]
  return(wktlist)
}

## create a evidence string with cause are false (Nein or iO)
## i.e. & cause1=='Nein' & cause2=='iO'
## hand over: bayes net and list of know good cause conditions
## return: evidence string for causeanalysis_strongestdyn()
evidenceStr <- function(bayesnet, evidence_var){
  knowledge <- NULL # initialize
  for (k in 1:length(evidence_var)) {
    knowledge <- paste(knowledge, "&", evidence_var[k],sep = "")
    comand <- paste0("bayesnet[['", evidence_var[k], "']][['prob']]")
    state <- eval(parse(text = comand))
    if ("Nein" %in% rownames(state)){
      knowledge <- paste(knowledge, "=='Nein'",sep = "")
    } # end if
    else if("iO" %in% rownames(state)) {
      knowledge <- paste(knowledge, "=='iO'",sep = "")
    } # end else if
    else {
      knowledge <- paste(knowledge, "!='",rownames(state)[1],"'",sep = "")
    } # end else
  }# end for
  return(knowledge)
}

## build an evidence string for causeanalysis_strongestdyn()/_likeliestdyn()
## i.e. &(<cause name> = <cause state>); knows the cause is in this state
## hand over: list of cause names and list of cause state known is in this
## return: evidence string
knownCauses <- function(causename,causestate){
  strCauses <- c() # initialize string for evidence
  # build evidence string i.e. (<cause name>=<cause state>)
  for(i in 1:length(causename)){
    strCauses <- paste0(strCauses,"&",causename[i],"=='",causestate[i],"'",sep="")
  }
  return(strCauses)
}

#####
## Functions: check bayes nets on directory, build DAG from directory, conversions
##################################################################################
## check for available Bayes nets in the directory
## return: array with filename.net
findBNAtWD <- function(){
  filelist <- list.files(path = paste0("Bayesnetze/Gelernte_Subnetze"))
  if(length(filelist)!=0){
    newfilelist <- NULL
    for(i in 1:length(filelist)){
      ext <- tools::file_ext(filelist[i])
      if(ext == "net"){
        newfilelist[i] <- filelist[i]
      }  
    }
    newfilelist <- newfilelist[!is.na(newfilelist)]
  }else{
    cat("Es konnten keine BayesNetze gefunden werden!")
  }
  return(newfilelist)
}

## select bayesnet name from filename with .net
## return: array with bayes net names without .net
findBNNamesAtWD <- function(){
  baynetlist <- findBNAtWD()
  filelist <- strsplit(baynetlist, split = '.net')
  name_array <- ""
  for(i in 1:length(filelist)){
    name_array[i] <- filelist[[i]][1]
  }
  return(name_array)
}

## Read in Bayesnet from directory
## hand over: bayesian network name
## return bayes net class bn.fit (full trained bayes net)
readinBN <- function(bn_name){
  cmd = paste0("read.net('Bayesnetze/Gelernte_Subnetze/",bn_name,".net')",sep = "")
  bn <- eval(parse(text = cmd))
  
  return(bn)
}

## read in main model string and create dag
## return: DAG
readInMainDAG <- function(){
  mstring <- as.character(read.table(file = "mainmodelstring.txt"))
  dag <- model2network(mstring)
  return(dag)
}

## read in multiple bayes nets from directory
## hand over: list of bayes net names
## return: list with multiple bayes nets
readInmultiNet <- function(net_names){
  bnet <- list(NULL)
  for(i in 1:length(net_names)){
    # merge strings
    cmd = paste("read.net('Bayesnetze/Gelernte_Subnetze/", net_names[i],
                ".net')", sep = "")
    # execute
    bnet[[i]] <- eval(parse(text = cmd))
  }
  return(bnet)
}

## build a modelstring from multiple bayes nets
## hand over: list of bayesian nets
## return: string with a big modelstring over all bayes nets
bigModelString <- function(bn_list){
  stringBN <- modelstring(bn_list[[1]]) # model string from first bayes net
  for (i in 2:length(bn_list)) {
    stringBN <- paste0(stringBN, modelstring(bn_list[[i]]), sep = "")
  }
  return(stringBN)
}

## separate each brackets (node formula) and put together as an array
## hand over: modelstring (bayes net structure formula)
## return: separated strings as array
collapsModelstring <- function(modelstring){
  string <- strsplit(modelstring, split = ']')
  col_string <- ""
  for(i in 1:length(string[[1]])){
    temp <- string[[1]][i]
    temp <- paste0(temp, "]", collapse = "")
    col_string[i] <- temp
  }
  return(col_string)
}

## remove duplicates from array and build a DAG
## hand over: a collapsed model string as array
## return: DAG
collapsedMs2DAG <- function(modelstring){
  mstring <- unique(modelstring) # remove duplicates
  mstring <- paste0(mstring, collapse = "") # build one string
  dag <- model2network(mstring) # model string to DAG
  return(dag)
}

## create main dag from multiple sub nets
## return: DAG
createMainDAG <- function(){
  # select available sub nets
  bn_names <- findBNNamesAtWD()
  # read in sub bayes nets
  bayesnets <- readInmultiNet(bn_names)
  # build modelstring with doublets
  bigstring <- bigModelString(bayesnets)
  # build modelstring without doublets
  modelstring <- collapsModelstring(bigstring)
  # modelstring to dag
  dag <- collapsedMs2DAG(modelstring)
  return(dag)
}

## Bayesnet to DAG
## hand over: bayesian network
## return: DAG class bn
bnfit2DAG <- function(bayesnet){
  string <- modelstring(bayesnet)
  dag <- model2network(string)
  
  return(dag)
}
#####
## Functions: generate synthetic data
##################################################################################
## Generator for random synthetic data set for one error net
## hand over: excel list (customizeExcellist()), error name and length of data set
## return: data set as data frame with factors
simulateRandDataset <- function(paralist, errorname, n){
  # create DAG from error with modelstring from 'paralist'
  modelstring <- createMStrings(paralist, errorname)
  dag <- model2network(modelstring)
  # select all nodes
  nodes <- data.frame(matrix(ncol = length(nodes(dag)), nrow = 3))
  colnames(nodes) <- nodes(dag)
  # initialize data
  data <- data.frame(matrix(ncol = length(nodes(dag)), nrow = n))
  colnames(data) <- nodes(dag)
  # simulate random data n observation of each node
  for (i in 1:length(nodes)) {
    # extract node state
    if(colnames(nodes)[i] == errorname)
      nodes[,i] <- c("Nein","Ja","-")
    else{
      index <- match(colnames(nodes)[i],paralist[,"Netzname"])
      nodes[1,i] <- paralist[index,"Zustand_default"]
      nodes[2,i] <- paralist[index,"Zustand_2"]
      nodes[3,i] <- paralist[index,"Zustand_3"]
    }
    # generate a random data set of one node
    prob <- runif(1, min= 0.1, max = 0.9)
    if(nodes[3,i] != "-")
      rand <- sample(1:3, n, replace=TRUE, prob=c(prob,(1-prob)*0.3,(1-prob)*0.7))
    else
      rand <- sample(1:2, n, replace=TRUE, prob=c(prob,(1-prob)))
    data[,i] <- as.factor(nodes[rand,i])
  }
  return(data)
}

## Generator for random synthetic data set for the hole network
## hand over: excel list (customizeExcellist()), length of data set, first error
## i.e. "Kratzer" or "Butzenabfall"
## return: data set as data frame with factors
simulateRandDatasetBig <- function(paralist, n, firsterror){
  # set indexes
  firsterr <- match(firsterror,colnames(paralist))
  lasterr  <- length(colnames(paralist))
  # extract all node names
  names <- c(colnames(paralist)[firsterr:lasterr],paralist[,"Netzname"])
  nodes <- data.frame(matrix(ncol = length(names), nrow = 3,
                             dimnames = list(c(NULL),names)))
  # initialize data set
  data <- data.frame(matrix(ncol = length(names), nrow = n,
                            dimnames = list(c(NULL),names)))
  # simulate random data n observation of each node
  for (i in 1:length(nodes)) {
    # extract node state
    if(!is.na(match(colnames(nodes)[i],colnames(paralist))))
      nodes[,i] <- c("Nein","Ja","-")
    else{
      index <- match(colnames(nodes)[i],paralist[,"Netzname"])
      nodes[1,i] <- paralist[index,"Zustand_default"]
      nodes[2,i] <- paralist[index,"Zustand_2"]
      nodes[3,i] <- paralist[index,"Zustand_3"]
    }
    # generate a random data set of one node
    prob <- runif(1, min= 0.1, max = 0.9)
    if(nodes[3,i] != "-")
      rand <- sample(1:3, n, replace=TRUE, prob=c(prob,(1-prob)*0.3,(1-prob)*0.7))
    else
      rand <- sample(1:2, n, replace=TRUE, prob=c(prob,(1-prob)))
    data[,i] <- as.factor(nodes[rand,i])
  }
  return(data)
}

###### old versions with a long calculation time

## Generate random synthetic data for a selected fault
## hand over: excel list (customizeExcellist()), error name and quantity of data
## return: data as data frame factors
creatRandomData <- function(excellist, errorname, num){
  # create model string
  string <- createMStrings(excellist,errorname)
  # create DAG
  dag <- model2network(string)
  # select node names from DAG
  nodenames <- nodes(dag)
  # initialize data table
  datas <- data.frame(matrix(ncol = length(nodenames), nrow = 0))
  colnames(datas) <- nodenames
  # set random states 
  for (i in 1:length(datas)) {
    # random vectors for two and three states
    prob <- runif(1, min = 0.05, max = 0.8)
    two <- sample(1:2, num, replace=TRUE, prob=c(prob,(1-prob)*0.3))
    prob <- runif(1, min = 0, max = 0.6)
    three <- sample(1:3,num,replace=TRUE,prob=c(prob,(1-prob)*0.4,(1-prob)*0.8))
    # set random states for error
    if(colnames(datas)[i]==errorname){
      states <- c("Ja","Nein")
      for (j in 1:num) {
        datas[j,i] <- states[two[j]]
      }
    }
    # set random states for causes
    else{
      row <- match(colnames(datas)[i],excellist[,2])
      col1 <- match("Zustand_default", colnames(excellist))
      col2 <- match("Zustand_3", colnames(excellist))
      # select states from excel list
      states <- excellist[row,c(col1:col2)]
      for (j in 1:num) {
        if(states[3]!="-"){
          datas[j,i] <- states[three[j]]
        }
        else{
          datas[j,i] <- states[two[j]]
        }
      }
    }
  }
  # convert into factors
  for (k in 1:length(datas)) {
    datas[,k] <- as.factor(datas[,k])
  }
  return(datas)
}

## Generate random synthetic data for the hole bayesian network
## hand over: excel list (customizeExcellist()) and quantity of data
## return: data as data frame factors
creatRandomDataBig <- function(excellist, num){
  firsterror <- match("Kratzer",colnames(excellist))
  lasterror  <- length(colnames(excellist))
  names <- colnames(excellist)[firsterror:lasterror]
  names <- c(names,excellist[,"Netzname"])
  # initialize data table
  datas <- data.frame(matrix(ncol = length(names), nrow = 0))
  colnames(datas) <- names
  # set random states 
  for (i in 1:length(datas)) {
    # random vectors for two and three states
    prob <- runif(1, min = 0.05, max = 0.8)
    two <- sample(1:2, num, replace=TRUE, prob=c(prob,(1-prob)*0.3))
    prob <- runif(1, min = 0, max = 0.6)
    three <- sample(1:3,num,replace=TRUE,prob=c(prob,(1-prob)*0.4,(1-prob)*0.8))
    # set random states for error
    if(i<=(lasterror-firsterror+1)){
      states <- c("Ja","Nein")
      for (j in 1:num) {
        datas[j,i] <- states[two[j]]
      }
    }
    # set random states for causes
    else{
      row <- match(colnames(datas)[i],excellist[,2])
      col1 <- match("Zustand_default", colnames(excellist))
      col2 <- match("Zustand_3", colnames(excellist))
      # select states from excel list
      states <- excellist[row,c(col1:col2)]
      for (j in 1:num) {
        if(states[3]!="-"){
          datas[j,i] <- states[three[j]]
        }
        else{
          datas[j,i] <- states[two[j]]
        }
      }
    }
  }
  # convert into factors
  for (k in 1:length(datas)) {
    datas[,k] <- as.factor(datas[,k])
  }
  return(datas)
}

#####
## Functions: data preparation
##################################################################################
## classify numeric data set into three classes
## classes: niederig,  mittel, hoch
## hand over: data set as data frame
## return: classified data set 
classifyInto3 <- function(data){
  # find min value
  min <- min(data)
  # find max value
  max <- max(data)
  # data width
  width <- (max - min)
  # lower limit
  lower <- (min + (width*0.1))
  # upper limit
  upper <- (max - (width*0.1))
  # initialize new data set
  newdata <- data
  # classify
  for (j in 1:ncol(data)) {
    for (i in 1:nrow(data)) {
      if ((data[i,j] >= min) && (data[i,j] < lower)){
        newdata[i,j] <- "niedrig"}
      else if((data[i,j] >= lower) && (data[i,j] < upper)){
        newdata[i,j] <- "mittel"}
      else if((data[i,j] >= upper) && (data[i,j] <= max)){
        newdata[i,j] <- "hoch"}
      else
        cat("Datum nicht zuordenbar: ", data[i,j])
    }
  }
  return(newdata)
}

## fill missing value in the data set
## to complete data set after a RCA
## hand over: data set with missing values
## return: complete data set
filldataset <- function(data, excellist){
  # number of rows and cols
  dim = dim(data)
  # loop through each column 
  for (i in 1:dim[2]) {
    # index where is a NA value
    temp <- which(!is.na(data[,i]))
    # when only found one NA
    if(length(temp) == 1){
      # search line index of parameter in parameter list
      index <- which(colnames(data)[i]==excellist[,"Netzname"])
      # when state found in the last observation, set all obs to this state
      if(temp == dim[1]){
        data[,i] <- data[temp,i]}
      # when state not found in the last observation and not default state
      else if(data[temp,i] != excellist[index,"Zustand_default"]){
        # set observations to the found index to the found state
        data[1:temp,i] <- data[temp,i]
        # set observations from found index to end on default state
        data[(temp+1):dim[1],i] <- excellist[index,"Zustand_default"]}
      else{
        # when state not found in the last observation but default state, all def
        data[,i] <- data[temp,i]}}
  }
  return(data)
}


## Funktion, welche Zwischenvariablen beurteilt
## diese muss kontinuerlich an die Struktur angepasst werden
classifynodes <- function(dataset){
  # convert TempDuese
  if(!is.na(match("TempDuese",colnames(dataset)))){
    for (i in 1:nrow(dataset)) {
      if(dataset[i,"Heizzone14"] != "mittel"){
        dataset[i,"TempDuese"] <- "niO"}
      else{
        dataset[i,"TempDuese"] <- "iO"}}}
  
  # convert Temp Kopf
  else if(!is.na(match("TempKopf",colnames(dataset)))){
    for (i in 1:nrow(dataset)) {
      if(dataset[i,"Heizzone10"]!="mittel" | dataset[i,"Heizzone11"]!="mittel" |
         dataset[i,"Heizzone12"]!="mittel" | dataset[i,"Heizzone13"]!="mittel"){
        dataset[i,"TempKopf"] <- "niO"}
      else{
        dataset[i,"TempKopf"] <- "iO"}}}
  
  # convert TempDueseKopfAussen
  else if(!is.na(match("TempDueseKopfAussen",colnames(dataset)))){
    for (i in 1:nrow(dataset)) {
      if(dataset[i,"Heizzone12"]!="mittel" | dataset[i,"Heizzone13"]!="mittel" |
         dataset[i,"Heizzone14"]!="mittel"){
        dataset[i,"TempDueseKopfAussen"] <- "niO"}
      else{
        dataset[i,"TempDueseKopfAussen"] <- "iO"}}}
  
  # convert TempExtrudereinfuellzone
  else if(!is.na(match("TempExtrudereinfuellzone",colnames(dataset)))){
    for (i in 1:nrow(dataset)) {
      if(dataset[i,"Heizzone1"]!="mittel"){
        dataset[i,"TempExtrudereinfuellzone"] <- "niO"}
      else{
        dataset[i,"TempExtrudereinfuellzone"] <- "iO"}}}
  
  # convert TempDueseKopf
  else if(!is.na(match("TempDueseKopf",colnames(dataset)))){
    for (i in 1:nrow(dataset)) {
      if(dataset[i,"TempDuese"]!="iO" | dataset[i,"TempKopf"]!="iO"){
        dataset[i,"TempDueseKopf"] <- "niO"}
      else{
        dataset[i,"TempDueseKopf"] <- "iO"}}}
  
  # convert TempExtrudereinzug
  else if(!is.na(match("TempExtrudereinzug",colnames(dataset)))){
    for (i in 1:nrow(dataset)) {
      if(dataset[i,"Heizzone1"]!="mittel" | dataset[i,"Heizzone2"]!="mittel"){
        dataset[i,"TempExtrudereinzug"] <- "niO"}
      else{
        dataset[i,"TempExtrudereinzug"] <- "iO"}}}
  
  # convert TempExtruder
  else if(!is.na(match("TempExtruder",colnames(dataset)))){
    for (i in 1:nrow(dataset)) {
      if(dataset[i,"Heizzone1"]!="mittel" | dataset[i,"Heizzone2"]!="mittel" |
         dataset[i,"Heizzone3"]!="mittel" | dataset[i,"Heizzone4"]!="mittel"){
        dataset[i,"TempExtruder"] <- "niO"}
      else{
        dataset[i,"TempExtruder"] <- "iO"}}}
  
  return(dataset)
}