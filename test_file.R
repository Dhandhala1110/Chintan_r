# load packages
library(bnlearn)
library(Rgraphviz)

source("functions.R")



## Aus Subnetzen ein gemeinsames großes Bayesnetz/ DAG erstellen
# bayesnets <- readIn_multiNet(get_available_nets())
# bigstring <- buildBNstring(bayesnets)
# modelstring <- collaps_modelstring(bigstring)
# dag <- modelstring_to_dag(modelstring)



## read in DAG
# bnfit <- readin_dag("test_bn")
# 
# ## transform bn.fit to bn
# dag <- bnfit2dag(bayesnet =  bnfit)
# 
# ## learn parameters
# bn <- learnpara(dag)
# 
# ## save new bayesnet
# write.net("actual_bn.net", bn)


# bn_names <- c("Kratzer","Schmelzbruch","Schlierenbildung","Vorformling_glaenzend","Blasenbildung","Rauch_Qualm","Gardineneffekt")
# bayesnets <- readIn_multiNet(bn_names)
# bigstring <- buildBNstring(bayesnets)
# modelstring <- collaps_modelstring(bigstring)
# dag <- modelstring_to_dag(modelstring)
# pdf("InteractiveBN.pdf", height = 3)
# graphviz.plot(dag,shape = "ellipse",main= "Diskretes Bayes Netz - Gesamtübersicht")
# dev.off()


## Temperaturwerte in Bereiche einteilen
# library(arules)
# temp_data <- rnorm(100,100,3)
# temp_data
# temp_data <- round(temp_data, 1)
# temp_data
# temp_data_1 <- bnlearn::discretize(data = as.data.frame(temp_data), breaks = 4,  method = "interval")
# temp_data_2 <- arules::discretize(x = temp_data, breaks = c(90,95,100,105,110), method = "fixed", labels = c("90to95","95to100","100to105","105to110"))
# temp_data_2 <- as.data.frame(temp_data_2)
# temp_data_1
# temp_data_2


## select influence nodes to a chosen node
# transferred Bayesian and selected node for example a fault, check if path
# between selected node and node from bayesnet is true
# return nodes with a path to the chosen node


######

# ## Nur betreffende Knoten zu einem Fehelr extrahieren
# bn_names <- get_available_nets()
# bayesnets <- readIn_multiNet(bn_names)
# bigstring <- buildBNstring(bayesnets)
# modelstring <- collaps_modelstring(bigstring)
# dag <- modelstring_to_dag(modelstring)
# 
# fault <- "Regeneratanteil"
# plot(bn.net(read.net("Schlierenbildung.net")))
# testnodes <- selectAllInfluenceNodes(dag, fault)
# testnodes

#######
# Suche nach möglichen Subnetzen im Ordner und lese sie automatisch ein, 
# ohne dass eine Liste erstellt werden muss

# zeigt alle Dateien im Pfad an 
# findbayesnetsAtDirectory <- function(){
#   filelist <- list.files(path = paste0("C:/Users/Julian/Documents/Studium/",
#                                        "Semester_7/BPP/Git/",
#                                        "Blasformen_Wissensspeicher/Bayesnetze"))
#   if(length(filelist)!=0){
#     newfilelist <- NULL
#     for(i in 1:length(filelist)){
#       ext <- tools::file_ext(filelist[i])
#       if(ext == "net"){
#         newfilelist[i] <- filelist[i]
#       }  
#     }
#     newfilelist <- newfilelist[!is.na(newfilelist)]
#   }else{
#     cat("Es konnten keine BayesNetze gefunden werden!")
#   }
#   return(newfilelist)
# }



#data <- read.csv2("manualinputs.csv")
#saveDataTuble(data) 

# string <- paste0("[VisitAsia][Tuberculosis|VisitAsia][Age][Smoking]",
#                  "[LungCancer|Smoking:Age][TuorCa|LungCancer:Tuberculosis]",
#                  "[Bronchitis|Smoking][Dyspnea|TuorCa:Bronchitis]",
#                  "[XRayResult|TuorCa]", sep = "")
# dag <- model2network(string)
# graphviz.plot(dag)
# dag <- read.net("Kratzer.net")
# fault <- "lightblue"
# names(fault) <- "Kratzer"
# 
# nodeRenderInfo(plot_bn) <- list(fill = fault, col = fault)
# nodeRenderInfo(plot_bn) <- list(lwd = 1)
# edgeRenderInfo(plot_bn) <- list(lwd = 1)
# plot_bn <- graphviz.plot(dag, shape = "ellipse")
# renderGraph(plot_bn)




#write.table(mainstring, file = "mainmodelstring.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)




##################################################

# read in excel list
excelliste <- readxl::read_xlsx("Parameterliste.xlsm", sheet = "Fehler Blasformteil")
excelliste <- readxl::read_xlsx("Parameterliste.xlsm", sheet = "Fehler Vorformling")
excelliste <- as.data.frame(excelliste)
excelliste[is.na(excelliste)] <- "-"

# create sub model strings and sub DAGs
bigstring <- ""
for (i in which(colnames(excelliste)=="Kratzer"):ncol(excelliste)) {
  cat(i)
  cat("\r")
  testdag <- model2network(createMStrings(excelliste,i))
  graphviz.plot(testdag, shape = "ellipse")
  bigstring <- paste0(bigstring,createMStrings(excelliste,i), sep="")
  Sys.sleep(0.5) # sleep time
}


# print big DAG
bigstring <- collapsModelstring(bigstring)
testdag <- collapsedMs2DAG(bigstring)
bnviewer::viewer(testdag,
                 bayesianNetwork.width = "100%",
                 bayesianNetwork.height = "90vh",
                 bayesianNetwork.layout = "layout_with_dh",
                 bayesianNetwork.title = "Diskretes Bayes Netz - 
                                                     Gesamtübersicht",
                 node.shape = "ellipse",
                 node.font = list(color = "black", face="Arial", 
                                  size = 25),
                 edges.width = 3,
                 options.highlightNearest = TRUE)


##################################################

# read in model strings from 'Vorformling'
vorformling <- read.table(file = "MSVorformling.txt")
vfstring <- ""
# extract to a single string
for (i in 1:length(vorformling)) {
  vfstring <- paste0(vfstring,vorformling[i], sep="")
}

# read in model strings from 'Blasformteil'
blasformteil <- read.table(file = "MSBlasformteil.txt")
bfstring <- ""
# extract to a single string
for (i in 1:length(blasformteil)) {
  bfstring <- paste0(bfstring,blasformteil[i], sep="")
}
# merge both single strings
bigstring <- paste0(vfstring,bfstring)

# convert model string to DAG
bigstring <- collapsModelstring(bigstring)
testdag <- collapsedMs2DAG(bigstring)

# plot DAG
bnviewer::viewer(testdag,
                 bayesianNetwork.width = "100%",
                 bayesianNetwork.height = "90vh",
                 bayesianNetwork.layout = "layout_with_dh",
                 bayesianNetwork.title = "Diskretes Bayes Netz - 
                                                     Gesamtübersicht",
                 node.shape = "ellipse",
                 node.font = list(color = "black", face="Arial", 
                                  size = 25),
                 edges.width = 3,
                 options.highlightNearest = TRUE)

##################################################
# interactive graph
install.packages("visNetwork")
require(visNetwork, quietly = TRUE)
# minimal example
nodes <- data.frame(id = 1:3)
edges <- data.frame(from = c(1,2), to = c(2,3))
visNetwork(nodes, edges, width = "100%")

net <- eval(parse(text = paste0("read.net('Bayesnetze/", "Kratzer",".net')")))

nodes <- data.frame(id = nodes(net), label = nodes(net), shape = "ellipse", color = c("lightgreen","lightgreen","orange","lightgreen","lightgreen"))
edges <- as.data.frame(arcs(net))
visNetwork(nodes, edges, height = "500px", width = "100%") %>% 
  visPhysics(stabilization = FALSE) %>%
  visEdges(arrows = "to")


##################################################
# Test verschiedener Inferenzberechnungen
bn <- read.net("Network2.net")
# Ursache Duesentemperatur
wkt1 <- cpquery(bn, event = (Kratzer=="Ja") ,evidence = (Duesentemperatur=="zuhoch"),n=10^6)
wkt2 <- cpquery(bn, event = (Kratzer=="Ja") ,evidence = (Duesentemperatur=="iO"),n=10^6)
wkt3 <- cpquery(bn, event = (Kratzer=="Ja") ,evidence = (Duesentemperatur=="zuniedrig"),n=10^6)
# Ursache BeschaedigungDuese
wkt4 <- cpquery(bn, event = (Kratzer=="Ja") ,evidence = (BeschaedigungDuese=="Ja"),n=10^6)
wkt5 <- cpquery(bn, event = (Kratzer=="Ja") ,evidence = (BeschaedigungDuese=="Nein"),n=10^6)
# Ursache VerunreinigterRohstoff
wkt6 <- cpquery(bn, event = (Kratzer=="Ja") ,evidence = (VerunreinigterRohstoff=="Ja"),n=10^6)
wkt7 <- cpquery(bn, event = (Kratzer=="Ja") ,evidence = (VerunreinigterRohstoff=="Nein"),n=10^6)

#Differenzen
# Ursache Duesentemperatur
dif1 <- wkt1 - wkt2 # von zuhoch in iO
dif2 <- wkt1 - wkt3 # von zuhoch in zuniedrig
dif3 <- wkt2 - wkt3 # von iO in zuniedrig
# Ursache BeschaedigungDuese
dif4 <- wkt4 - wkt5 # von Ja zu Nein
# Ursache VerunreinigterRohstoff
dif5 <- wkt6 - wkt7 # von Ja zu Nein


# Fall 1
cpquery(bn, event = (BeschaedigungDuese=="Ja") ,evidence = (Kratzer=="Ja"& VerunreinigterRohstoff=="Nein"),n=10^6)
cpquery(bn, event = (VerunreinigterRohstoff=="Ja") ,evidence = (Kratzer=="Ja"),n=10^6)
cpquery(bn, event = (Duesentemperatur=="zuhoch") ,evidence = (Kratzer=="Ja"),n=10^6)
cpquery(bn, event = (Duesentemperatur=="zuniedrig") ,evidence = (Kratzer=="Ja"),n=10^6)


cpquery(bn, event = (VerunreinigterRohstoff=="Ja") ,evidence = (Kratzer=="Ja"&Duesentemperatur=="iO"),n=10^6)


cpquery(bn, event = (Duesentemperatur=="zuniedrig") ,evidence = (Kratzer=="Ja"),n=10^6)



##################################################

bn <- read.net("Network2.net")
bn <- read.net("actual_bn.net")

## Fall 1: Current machine state unknown
# perform root cause analysis
causelist <- causeanalysis_likeliest(bn,"Kratzer") # inference P(cause=true|error=true)
# print out results
causelist
# print out most probable cause
probablecause <- as.character(causelist[1,1:2])
str <- paste0("Die wahrscheinlichste Ursache ist: ",probablecause[1], ", Zustand: ", probablecause[2], sep="")
cat(str)


## Fall 2: Current machine state known
# perform root cause analysis
causelist <- causeanalysis_strongest(bn,"Kratzer") # inference P(error=true|cause=true)
# print out results
causelist
# print out most probable cause
probablecause <- as.character(causelist[1,3:4])
# comparison with current machine state
# if current machine state == probablecause --> fix it
# if current machine state != probablecause --> next cause from list and comparison
str <- paste0("Die wahrscheinlichste Ursache nach Abgleich mit Maschinenzustand ist: ",probablecause[1], ", Zustand: ", probablecause[2], sep="")
cat(str)

##################################################
# new root cause algorithm, a dynamic variant

# create a evidence string with cause are false (Nein or iO)
# i.e. & cause1=='Nein' & cause2=='iO'
evidenceStr <- function(bayesnet, evidence_var){
  knowledge <- NULL # initialize
  for (k in 1:length(evidence_var)) {
    knowledge <- paste(knowledge, "&", evidence_var[k], "==",sep = "")
    comand <- paste0("bayesnet[['", evidence_var[k], "']][['prob']]")
    state <- eval(parse(text = comand))
    if ("Nein" %in% rownames(state)){
      knowledge <- paste(knowledge, "'Nein'",sep = "")
    } # end if
    else {
      knowledge <- paste(knowledge, "'iO'",sep = "")
    } # end else
  }# end for
  return(knowledge)
}

## function test
evidenceStr(bn,c("BeschaedigungDuese", "Duesentemperatur"))



# P(error=true | cause=true)
# besitzt die Möglichkeit bekannte Ursachenzustände mit zu übergeben --> Für dynamisch
causeAnalysisStateKnown <- function(bayesnet, event_var, knownEvidence = NULL){
  # Initialization
  event_lvl <- "Ja" # error = TRUE
  event <- paste("(",event_var," == '",event_lvl,"')",sep = "") # event string
  wkt_list <- data.frame(matrix(ncol = 5, nrow = 0)) # initialize matrix
  colnames(wkt_list) <- c('event_name','event_state','evidence_name',
                          'evidence_state','probability') # names columns 
  j <- 0 # set index counter for matrix
  
  nodelist <-  selectAllInfluenceNodes(bayesnet,event_var) # all cause nodes
  if (!is.null(knownEvidence)) { # remove known causes
    nodelist <- nodelist [!nodelist  %in% knownEvidence]
  } # end if
  
  # create string for causes with known state
  if (!is.null(knownEvidence)){
    knowledge <- evidenceStr(bayesnet, knownEvidence)
  }
  
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
        cmd = paste("cpquery(bayesnet,",event,", ",evidence,", n=10^6)",sep = "")
        prob <- eval(parse(text = cmd)) # calculate inference
        cat("P(",event_var,"=",event_lvl ,"|",nodelist[i],"=",statenames[n],
            ") =",prob,"\n")
        cat(evidence,"\n")
        j <- j + 1 # increment index counter
        wkt_list[j,] <- c(event_var,event_lvl,nodelist[i],statenames[n],prob)
      } # end if
    } # end for
  } # end for
  # Sort list in descending order
  wkt_list <- wkt_list[order(as.numeric(wkt_list$probability),decreasing = TRUE),]
  return(wkt_list)
}


# 1. Try
causelist <- causeAnalysisStateKnown(bn,"Kratzer")
# print out results
causelist
# print out most probable cause
probablecause <- as.character(causelist[1,3:4])
str <- paste0("Die wahrscheinlichste Ursache nach Abgleich mit Maschinenzustand ist: ",probablecause[1], ", Zustand: ", probablecause[2], sep="")
cat(str)

# 2. Try
causelist <- causeAnalysisStateKnown(bn,"Kratzer",probablecause[1])
# print out results
causelist
# print out most probable cause
probablecause <- c(probablecause, as.character(causelist[1,3:4]))
str <- paste0("Die wahrscheinlichste Ursache nach Abgleich mit Maschinenzustand ist: ",probablecause[3], ", Zustand: ", probablecause[4], sep="")
cat(str)

# 3. and last Try
causelist <- causeAnalysisStateKnown(bn,"Kratzer",probablecause[c(1,3)])
# print out results
causelist
# print out most probable cause
probablecause <- c(probablecause, as.character(causelist[1,3:4]))
str <- paste0("Die wahrscheinlichste Ursache nach Abgleich mit Maschinenzustand ist: ",probablecause[5], ", Zustand: ", probablecause[6], sep="")
cat(str)

##################################################
## test create MS 
excellist <- readxl::read_xlsx("Parameterliste.xlsm", sheet = "Fehler Vorformling")
excellist <- customizeExcellist(excellist)
createMStrings(excellist, "Kratzer")







##################################################
# Test cause analysis, strongest or likeliest cause
net <- read.net("actual_bn.net")

causelist <- causeanalysis_strongestdyn(net,"Kratzer")
causelist
causelist <- causeanalysis_strongestdyn(net,"Kratzer","BeschaedigungDuese")
causelist


causelist <- causeanalysis_likeliestdyn(net,"Kratzer")
causelist
causelist <- causeanalysis_likeliestdyn(net,"Kratzer","BeschaedigungDuese")
causelist

##################################################
# Test Erfassen der inkorrekten Ursachen
net <- read.net("actual_bn.net")


causeanalysis_likeliestdyn(net,"Kratzer","Temp_Duese","190")
causeanalysis_likeliestdyn_old(net,"Kratzer","Temp_Duese")


causeanalysis_strongestdyn(net,"Kratzer","Temp_Duese","190")
causeanalysis_strongestdyn_old(net,"Kratzer","Temp_Duese")


##################################################################################
## Bayes Netz Strukturen für Vorformling in PDF plotten
excelliste <- customizeExcellist(readxl::read_xlsx("Parameterliste.xlsm",
                                                   sheet = "Fehler Vorformling"))
excel2ms(excellist = excelliste, netname = "Vorformling", firstfault = "Kratzer")
ms<-read.table("MSVorformling.txt")
#Pfad zum Speichern der PDF-Datei angeben
destination = paste0("C:/Users/Julian/Documents/Studium/Semester_7/BPP/Git/",
                     "Blasformen_Wissensspeicher/Netze.pdf", sep ="")
#PDF öffnen
pdf(file=destination)
#Plots als PDF speichern
for (i in 1:length(ms)) {
  graphviz.plot(model2network(as.character(ms[i])),shape = 'ellipse')}
#PDF-Plot deaktivieren
dev.off() 

##################################################################################
## Test Generierung synthetischer Daten
excelliste <- customizeExcellist(readxl::read_xlsx("Parameterliste.xlsm",
                                                   sheet = "Fehler Vorformling"))

## Synthetische Daten für ein Fehelr
testdata <- creatRandomData(excelliste,"Kratzer",10000)
bayesnet <- bn.fit(x = dag, data =  testdata, method = "mle") # learn
bayesnet[["BeschaedigungDuese"]][["prob"]]

string <- createMStrings(excelliste,"Kratzer")
dag <- model2network(string)
write.net("Kratzer.net",bayesnet)


## Synthetische Daten für gesamtes Bayes Netz
testtest <- creatRandomDataBig(excelliste,10000)
mstring <- read.table("MSVorformling.txt")
testdag <- createDAG(mstring)
testbn <- bn.fit(x = testdag, data =  testtest, method = "mle") # learn
write.net("testbn.net",testbn)



bn <- read.net("testbn.net")
faults <- leaf.nodes(bn)
for(i in 1:length(faults)){
  ms <- createMStrings(liste = excelliste,faultnumber = faults[i])
  dag <- model2network(ms)
  data <- creatRandomData(excelliste,faults[i],10000)
  BN <- bn.fit(x = dag, data =  data, method = "mle")
  cmd <- paste0("write.net('Bayesnetze/Gelernte_Subnetze/",faults[i],".net',BN)", sep = "")
  eval(parse(text = cmd))
}

##################################################################################
monteCarlo <-  function() {
  # We do this “forever” until we find a qualifying random value.
  while (TRUE) {
    # Pick a random value.
    r1 <- runif(1, min = 1, max = 2)
    # Assign a probability.
    probability <- r1
    # Pick a second random value.
    r2 <- runif(1, min = 1, max = 2)
    # Does it qualify? If so, we’re done!
      if (r2 < probability) {
        return(r1)
      }
  }
}
num <- NULL
for (i in 1:10000) {
  num <- c(num,monteCarlo())
}
mean(num)

##################################################################################
## Ursachenanalyse Tests
bayesnet <- read.net("Testnetze/VorformlingGlaenzend.net")
bayesnet <- read.net("Testnetze/Kratzer.net")
bayesnet <- read.net("Testnetze/SchlauchlaufGeradeauslauf.net")

##### VorformlingGlaenzend
## wahrscheinlichste Ursache dynamisch
causeanalysis_likeliestdyn(bayesnet,"VorformlingGlaenzend")
causeanalysis_likeliestdyn(bayesnet,"VorformlingGlaenzend","RegeneratAnteil","hoch")
causeanalysis_likeliestdyn(bayesnet,"VorformlingGlaenzend",c("RegeneratAnteil","TempKopf"),c("hoch","niO"))
## wahrscheinlichste Ursache statisch
causeanalysis_likeliest(bayesnet,"VorformlingGlaenzend")
## einflussstärkste Ursache dynamisch
causeanalysis_strongestdyn(bayesnet,"VorformlingGlaenzend")
causeanalysis_strongestdyn(bayesnet,"VorformlingGlaenzend","Extruderdruck","niedrig")
causeanalysis_strongestdyn(bayesnet,"VorformlingGlaenzend",c("Extruderdruck","ExtruderRPM"),c("niedrig","niedrig"))
## einflussreichste Ursache statisch
causeanalysis_strongest(bayesnet,"VorformlingGlaenzend")


##### Kratzer
## Mit Zwischenvariable
causeanalysis_likeliestdyn(bayesnet,"Kratzer")
causeanalysis_likeliestdyn(bayesnet,"Kratzer","Heizzone14","mittel")
## Ohne Zwischenvariable
causeanalysis_likeliestdyn_v1(bayesnet,"Kratzer")
causeanalysis_likeliestdyn_v1(bayesnet,"Kratzer","Heizzone14","mittel")

##### SChlauchlaufGeradeauslauf
causeanalysis_likeliestdyn(bayesnet,"SchlauchlaufGeradeauslauf")
causeanalysis_likeliestdyn(bayesnet,"SchlauchlaufGeradeauslauf","TempKopf","niO")




estimateInference(bayesnet, "Kratzer", extractLevelsFromBN(bayesnet))
causeanalysis_strongest(bayesnet,"Kratzer")
causeanalysis_likeliest(bayesnet,"Kratzer")

#################################################################################
string <- read.table("MSVorformling.txt")
dag <- createDAG(string[c(10,4)])
graphviz.plot(dag,shape = "ellipse")

# read in structure matrix
excellist <- readxl::read_xlsx("Parameterliste.xlsm", sheet="Fehler Vorformling")
# prepare excel list
excellist <- customizeExcellist(excelliste = excellist)
# build formale Gleichungen and save as .txt
excel2ms(excellist = excellist, netname = "Gesamt", firstfault = "Kratzer")
# read in formale Gleichungen
msGesamt <- read.table("MSGesamt.txt")
# create DAG
dag <- createDAG(msfile = msGesamt[1:3])



##################################################################################
# generate synthetic data
bn <- read.net("actual_bn_onlyKratzer.net")
data <- rbn(bn,100)
table(data["VerunreinigungRohstoff"])["Ja"]
dag <- model2network(modelstring(bn))
bn_learnd <- bn.fit(dag,data)
data_learnd <- rbn(bn_learnd,10000)
bn_learnd2 <- bn.fit(dag,data_learnd)
data_learnd2 <- rbn(bn_learnd2,10000000)
bn_learnd3 <- bn.fit(dag,data_learnd2)

## old version
paralist <- readxl::read_xlsx("Parameterliste.xlsm", sheet="Fehler Vorformling")
paralist <- customizeExcellist(excelliste = paralist)
randDataset <- creatRandomData(excellist = paralist, errorname = "Kratzer", num = 10)
head(randDataset)








## new version
paraliste <- readxl::read_xlsx("Parameterliste.xlsm", sheet="Fehler Vorformling")
paraliste <- customizeExcellist(excelliste = paraliste)


modelstring <- createMStrings(paraliste, "Kratzer")
dag <- model2network(modelstring)
testdata <- simulateRandDataset(paraliste,"Kratzer", 1000000)
testdata1 <- simulateRandDatasetBig(paraliste, 1000000)
bn <- bn.fit(dag,testdata)
pdf(file = "BarchartKratzer.pdf")
graphviz.chart(bn, type = "barprob", bar.col = "gold",
               strip.bg = "lightskyblue")
dev.off()


write.csv2(x = testdata, file = "Testdaten_rand_Kratzer.csv", row.names = FALSE)





rbn(bn, 9)


TempDuese <- floor(runif(1000,181,204))


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

TempDuese_classifyed <- classifyInto3(as.data.frame(TempDuese))

dataset <- matrix(nrow = length(TempDuese), ncol = 2)

dataset[,1] <- TempDuese
dataset[,2] <- TempDuese_classifyed[,1]
colnames(dataset) <- c("TempDuese","TempDuse_class")








##################################################################################
## fill missing value in the data set
## hand over: data set with missing values
## return: complete data set
filldataset_second <- function(data){
  dim = dim(data)
  for (i in 1:dim[2]) {
    temp <- which(!is.na(data[,i]))
    if(length(temp) == 2){
      data[1:temp[1],i] <- data[temp[1],i]
      data[temp[2]:dim[1],i] <- data[temp[2],i]}
    else
      data[,i] <- data[temp,i]
  }
  return(data)
}







# Testfall 1
testvec <- data.frame(matrix(nrow=10,ncol=1,dimnames = list(NULL,"TempDuese")))
testvec[4,1] <- "niO"
testvec[5,1] <- "iO"
testvec_complete <- filldataset_second(testvec)
test <- data.frame(matrix(nrow=10,ncol=1,dimnames = list(NULL,"TempDuese")))
test[,1] <- testvec
test[,2] <- testvec_complete
test
# Testfall 2
testvec <- data.frame(matrix(nrow=10,ncol=1,dimnames = list(NULL,"TempDuese")))
testvec[5,1] <- "iO"
testvec_complete <- filldataset_second(testvec)
test <- data.frame(matrix(nrow=10,ncol=1,dimnames = list(NULL,"TempDuese")))
test[,1] <- testvec
test[,2] <- testvec_complete
test
# Testfall 3
testvec <- data.frame(matrix(nrow=10,ncol=1,dimnames = list(NULL,"TempDuese")))
testvec[10,1] <- "iO"
testvec_complete <- filldataset_second(testvec)
test <- data.frame(matrix(nrow=10,ncol=1,dimnames = list(NULL,"TempDuese")))
test[,1] <- testvec
test[,2] <- testvec_complete
test

# Testfall 4
testvec <- data.frame(matrix(nrow=10,ncol=2,dimnames = list(NULL,c("TempDuese","Kratzer"))))
testvec[4,1] <- "niO"
testvec[5,1] <- "iO"
testvec[6,2] <- "Nein"
testvec_complete <- filldataset_second(testvec)
test <- data.frame(matrix(nrow=10,ncol=4,dimnames = list(NULL,c("TempDuese","Kratzer","TempDuese","Kratzer"))))
test[,1] <- testvec[,1]
test[,2] <- testvec_complete[,1]
test[,3] <- testvec[,2]
test[,4] <- testvec_complete[,2]
test




test <- data.frame(matrix(nrow=10,ncol=4,dimnames = list(NULL,c("TempDuese","Kratzer","TempDuese","Kratzer"))))
test[1,1] <- 4
test[2,1] <- 5
bn <- read.net("actual_bn.net")
InfluNodes(bn, "Kratzer")
all(is.na(test))





msfile <- read.table("MSVorformling.txt")
dag <- createDAG(msfile)
datensatz <- simulateRandDatasetBig(paraliste,10000)
bn_fitted <- bn.fit(dag,datensatz,method = "bayes")
write.net("actual_bn.net",bn_fitted)



bn <- read.net("actual_bn.net")
dataset <- rbn(bn,n=10000)
write.csv2(dataset, "Traingsdatensatz_BN3F.csv",row.names = FALSE)
data <- read.csv2("Traingsdatensatz_BN3F.csv")

msfile <- read.table("MSBlasformteil.txt")
dag <- createDAG(msfile)

paralist <- readxl::read_xlsx("Parameterliste.xlsm", sheet="Fehler Blasformteil")
paralist <- customizeExcellist(excelliste = paralist)
data <- simulateRandDatasetBig(paralist,10000,"Blasformteil")
bn_fitted <- bn.fit(dag,data,method = "bayes")
write.net("actual_bn_Blasformteil.net",bn_fitted)





















testdata <- read.csv2("Data_collection/edited/Data_2022_02_03_12_07_18.csv")[-1]

newdata <- data.frame(list("Heizzone14"=as.numeric(testdata[,"Heizzone14"])))

testdata[,"Heizzone14"] <- classifyInto3(newdata)



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


classifynodes(testdata)








net <- read.net("actual_bn_Blasformteil.net")
graphviz.plot(net, shape = "ellipse")


viewer(model2network(modelstring(net)),node.shape = "ellipse",
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "90vh")







excellist <- readxl::read_xlsx("Parameterliste.xlsm", sheet="Fehler Vorformling")
excellist <- customizeExcellist(excelliste = excellist)
dataset <- simulateRandDataset(paralist = excellist , errorname = "Kratzer", n = 1000000)






# read in Bayes Net with three errors
bayesnet <- read.net(file = "actual_bn_3Fehler.net")
# simulate synthetic data set
dataset <- rbn(x = bayesnet, n = 10000)

class(dataset)
dim(dataset)






# read in Bayes Net with three errors
bayesnet <- read.net(file = "actual_bn_3Fehler.net")
# estimate the most probable cause
causeanalysis_likeliestdyn(bayesnet = bayesnet, error = "Kratzer")

causeanalysis_likeliestdyn(bayesnet = bayesnet, error = "Kratzer", knoEvi = "Heizzone14", knoState = "mittel")


# read in Bayes Net with three errors
bayesnet <- read.net(file = "actual_bn_3Fehler.net")
# estimate the strongest cause
causeanalysis_strongestdyn(bayesnet = bayesnet, event = "Kratzer")



use_virtualenv('Pythonanbindung/venv', required = TRUE)
py_run_file("Pythonanbindung/main.py")
pydaten <- py$data
source_python("Pythonanbindung/main.py")
testdata <- read_data()

for(i in 1:ncol(pydaten)){
  if(colnames(pydaten)[i] %in% colnames(obs$list)){
    obs$list[counter$obs,colnames(pydaten)[i]] <- pydaten[i] }
}
nodeslist <- c("Heizzone1","Heizzone4","Heizzone11","Heizzone10","Kratzer")
obslist <- data.frame(matrix(nrow = 1, ncol = length(nodeslist),
                             dimnames = list(NULL, nodeslist)))




paranames <- c('Datum','Ausstossdruck','Extruderdruck','ExtruderRPM',
               'Ausstossgeschw','Taktzeit','GegendruckAKKU','Heizzone1',
               'Heizzone2','Heizzone3','Heizzone4','Heizzone10','Heizzone11',
               'Heizzone12','Heizzone13','Heizzone14')

dataset <- data.frame(matrix(nrow=1, ncol=length(paranames), 
                             dimnames = list(NULL, paranames)))
for (i in 1:100) {
  Sys.sleep(round(runif(1, min = 1, max = 30)))
  dataset["Datum"] <- substr(Sys.time(),1,19)
  dataset["Ausstossdruck"] <- round(runif(1, min = 1, max = 4))
  dataset["Extruderdruck"] <- round(runif(1, min = 1, max = 8))
  dataset["ExtruderRPM"] <- round(runif(1, min = 40, max = 85))
  dataset["Ausstossgeschw"] <- round(runif(1, min = 2, max = 6))
  dataset["Taktzeit"] <- round(runif(1, min = 10, max = 43))
  dataset["GegendruckAKKU"] <- round(runif(1, min = 1, max = 5))
  dataset["Heizzone1"] <- round(runif(1, min = 193, max = 203))
  dataset["Heizzone2"] <- round(runif(1, min = 193, max = 203))
  dataset["Heizzone3"] <- round(runif(1, min = 193, max = 203))
  dataset["Heizzone4"] <- round(runif(1, min = 193, max = 203))
  dataset["Heizzone10"] <- round(runif(1, min = 193, max = 203))
  dataset["Heizzone11"] <- round(runif(1, min = 193, max = 203))
  dataset["Heizzone12"] <- round(runif(1, min = 193, max = 203))
  dataset["Heizzone13"] <- round(runif(1, min = 193, max = 203))
  dataset["Heizzone14"] <- round(runif(1, min = 193, max = 203))
  
  write.table(dataset, file = "Data_collection/cpu/vorformling.csv", sep = ",", 
              append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)
}


write.table(dataset, file = "Data_collection/cpu/vorformling.csv", sep = ",", 
            append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE)







test <- read.table(header= T, file = "Data_collection/cpu/vorformling.csv",
                   sep = ",")




net <- createDAG(read.table("MSBlasformteil.txt"))
excelliste <- readxl::read_xlsx("Parameterliste.xlsm",
                                sheet = "Fehler Blasformteil")
excelliste <- customizeExcellist(excelliste)
data <- simulateRandDatasetBig(excelliste, 100000,"Butzenabfall")
bnfitted <- bn.fit(net,data)
write.net("actual_bn_Blasformteil.net", bnfitted)



renewBN <- function(bnname){
  if(bnname == "Vorformling"){
    cat("Lade Bayes Netz 'Vorformling'...\n")
    return(read.net("actual_bn_Vorformling.net"))
  }
  else if(bnname == "Blasformteil"){
    cat("Lade Bayes Netz 'Blasformteil'...\n")
    return(read.net("actual_bn_Blasformteil.net"))
  }
  cat("Die Auswahl des Bayes Netzes hat nicht funktioniert\n")
  return(NULL)
}


    dag <- bnfit2DAG(renewBN("Blasformteil"))
    nodelist <- as.list(leaf.nodes(dag))
    nchighlight <- list(background = "yellow", border = "black")
    bnviewer::viewer(dag,
                     bayesianNetwork.width = "100%",
                     bayesianNetwork.height = "90vh",
                     #bayesianNetwork.layout = "layout_with_dh",
                     bayesianNetwork.layout = "layout_on_grid",
                     bayesianNetwork.title = "Diskretes Bayes Netz - 
                                                     Gesamtübersicht",
                     node.shape = "ellipse",
                     node.colors = list(background = "#00a65a",
                                        border = "black",
                                        highlight = nchighlight),
                     node.font = list(color = "black", face="Arial", 
                                      size = 25),
                     edges.width = 3,
                     options.highlightNearest = TRUE,
                     clusters = list(
                       list(label = "Fehler",
                            shape = "icon",
                            icon = list(color = "orange"),
                            nodes = nodelist))
    )

    
net <- read.net("actual_bn.net")
data <- rbn(net, 100)
pdf(file = "BarchartLernprozess.pdf")
net_new <- bn.fit(bnfit2DAG(net),data)
graphviz.chart(net, type = "barprob", bar.col = "gold", layout = "dot",
               strip.bg = "lightskyblue")
graphviz.chart(net_new, type = "barprob", bar.col = "gold", layout = "dot",
               strip.bg = "lightskyblue")
dev.off()



bn <- read.net("actual_bn_Vorformling.net")
dataset <- rbn(bn,10000)
names <- colnames(dataset)
write.table(dataset, file = "Testdata.csv", quote = FALSE, sep = ";", col.names = names, row.names = FALSE)
write.net("actual_bn_Vorformling.net", bn)























library(gridExtra)



EqDatedf <- as.data.frame(data[1,])

pdf(file = "q.pdf")

for (i in 2:nrow(data)) {
  {EqDatedf <- rbind(EqDatedf, data[i,])}
}

grid.table(EqDatedf, rows=NULL)
dev.off()


pdf("q.pdf", height=28, width=30)
grid.table(data)
dev.off()








ms <- read.table("MSBlasformteil.txt")

for (i in 1:ncol(ms)) {
  dag <- createDAG(ms[i])
  graphviz.plot(dag, shape = "ellipse")
  Sys.sleep(1)
}


pdf("Blasformteil_Sub.pdf", height = 1.9, width = 3)
dag <- createDAG(ms["ArtikelhaengtFrom"])
graphviz.plot(dag, shape = "ellipse")
dev.off()


net <- read.net("actual_bn_Blasformteil.net")
list <- causeanalysis_strongestdyn(bayesnet=net,event="ArtikelhaengtFrom",knoEvi=NULL,knoState=NULL)
pdf("q1.pdf", height=5, width=5)
grid.table(list)
dev.off()


net <- read.net("actual_bn.net")
levels <- extractLevelsFromBN(bayesnet = net)
error <- "SchlauchlaufGeradeauslauf"
error <- "Kratzer"
error <- "VorformlingGlaenzend"
start <- Sys.time()
estimateInference(net,error,levels)
print(Sys.time() - start)



net <- read.net("actual_bn.net")
pdf("CPT.pdf", height=5, width=5)
bn.fit.barchart(net$Kratzer)
dev.off()


  