# set true when use the first time 
firstuse <- FALSE
##################################################################################
# only use the first time
if(firstuse == "TRUE"){
    # installation path
    library.path <- .libPaths()[1]
    # install packages
    install.packages("shiny", repos = "http://cran.us.r-project.org",
                     lib = library.path)
    install.packages("shinydashboard", repos = "http://cran.us.r-project.org",
                     lib = library.path)
    install.packages("shinyBS", repos = "http://cran.us.r-project.org",
                     lib = library.path)
    install.packages("shinyjqui", repos = "http://cran.us.r-project.org",
                     lib = library.path)
    install.packages("shinyjs", repos = "http://cran.us.r-project.org",
                     lib = library.path)
    install.packages("bnlearn", repos = "http://cran.us.r-project.org",
                     lib = library.path)
    install.packages("readxl", repos = "http://cran.us.r-project.org",
                     lib = library.path)
    install.packages("reticulate", repos = "http://cran.us.r-project.org",
                     lib = library.path)
    install.packages("bnviewer", repos = "http://cran.us.r-project.org",
                     lib = library.path)
    install.packages("shinybusy", repos = "http://cran.us.r-project.org",
                     lib = library.path)
    if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager", repos = "http://cran.us.r-project.org")
    BiocManager::install("Rgraphviz")
}
##################################################################################

library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjqui)
library(shinyjs)
library(shinybusy)

## call main.R
source("main.R")

## read in actual bayesnet --> Serves only for test purposes
# If the function is deleted adjust function name in source code below --> renewBN
renewBN_test1 <- function(){
    return(read.net("actual_bn.net"))
}

## read in actual bayes net, can distinguish which type 
renewBN <- function(bnname){
    # Vorformling
    if(bnname == "Vorformling"){
        cat("Lade Bayes Netz 'Vorformling'...\n")
        return(read.net("actual_bn_Vorformling.net"))}
    # Blasformteil
    else if(bnname == "Blasformteil"){
        cat("Lade Bayes Netz 'Blasformteil'...\n")
        return(read.net("actual_bn_Blasformteil.net"))}
    # Error occurred
    cat("Die Auswahl des Bayes Netzes hat nicht funktioniert\n")
    return(NULL)
}


##################################
##  Define UI for application   ##
##################################
ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Wissensspeicher Extrusionsblasformen", 
                    titleWidth = 400),
    
    ## tabs on the left side
    dashboardSidebar(
        sidebarMenu(id = "tabs",     
            menuItem("Gesamt Struktur", tabName = "overview", 
                     icon = shiny::icon("home")),
            menuItem("Prozesseingriff", tabName = "errorcause",
                     icon = shiny::icon("arrow-right")),
            menuItem("Fehlerübersicht", tabName = "bayesnetz", 
                     icon = shiny::icon("globe")),
            menuItem("Parameter Lernen", tabName = "paralearn", 
                     icon = shiny::icon("chart-bar"))
        )# end sidebar menu
    ), # end sidebar
    # main part of the page
    dashboardBody(
        shinyjs::useShinyjs(), # for disable and enable action buttons
        tabItems(
            ## Tab OVERVIEW
            tabItem(tabName = "overview",
                    # choose between 'Vorformling' and 'Blasformteil'
                    selectInput(inputId = "idi_bayesnettype",
                                label = "Auswahl Vorformling oder Blasformteil",
                                choices = c("Vorformling", "Blasformteil")),
                    # refresh button
                    actionButton(inputId = "idi_ac_refreshBN", "Aktualisieren"),
                    # bayes net plot
                    uiOutput(outputId = "ido_bayesnetz_gesamte_plot"),
                    # charging circuit
                    add_busy_spinner(spin="fading-circle",timeout=100)
                    ), # end tab overview
            ######################################################################
            ## Tab Error Cause
            tabItem(tabName = "errorcause",
                    fluidRow(
                    ## left column
                    column(width = 4,
                           ## type cause analysis
                           box(width = NULL, title = "Art der Ursachenanalyse",
                               uiOutput(outputId = "ido_causetyp")
                               ),
                           ## fault box
                           box(width = NULL, title = "Vorliegendes Fehlerbild",
                               # drop-down main bayes net
                               selectInput(inputId = "idi_bayesnetart",
                                           label = "Vorformling/ Blasformteil",
                                           choices = c("Vorformling", 
                                                       "Blasformteil")),
                               # drop-down error pattern
                               uiOutput(outputId = "ido_errorpattern"),
                               # button start prediction/ calculation
                               actionButton(inputId = "idi_ab_errorcausecal",
                                            label = "Fehlerursachenermittlung"),
                               # button cancel process 
                               actionButton(inputId = "idi_ab_stop",
                                            label = "Abbrechen",
                                            icon = shiny::icon("times")),
                               # charging circuit
                               add_busy_spinner(spin="fading-circle",timeout=100)
                               ),
                          ## fault causes, suggestion process intervention
                          box(width = NULL, title = "Prozesseingriff",
                              # text output error cause
                              htmlOutput(outputId = "ido_errorcausetext"),
                              br(),
                              # text output error fixed
                              htmlOutput(outputId = "ido_pintervention"),
                              # button error fixed
                              actionButton(inputId = "idi_pintervention_y",
                                           label = "Ja"),
                              # button error not fixed
                              actionButton(inputId = "idi_pintervention_n",
                                           label = "Nein")
                              ),
                          ## fault causes list descending
                          box(width = NULL, title = "Fehlerursachenliste",
                              dataTableOutput(outputId = "ido_errorcauselist")
                              )
                          ),
                    ## right column
                    column(width = 8,
                           ## plot with highlighted error cause
                           box(width = NULL, title = "Plot mit hervorgehobener 
                               Fehlerursache",
                               uiOutput(outputId = "ido_bn_faultplot")
                               )
                           )
                    )),
            ######################################################################
            ## Tab BAYESNET
            tabItem(tabName = "bayesnetz",
                    fluidPage(
                        column(width = 12,
                               h1("Anzeige einzenler Fehlerbilder"),
                               ## chose bayesian net
                               box(width = NULL,
                                   # drop-down main bayes net
                                   selectInput(inputId = "idi_bntype",
                                               label="Vorformling/ Blasformteil",
                                               choices = c("Vorformling",
                                                           "Blasformteil")),
                                   # drop-down error pattern 
                                   uiOutput(outputId = "bayesnet_errorpattern"),
                                   helpText("Ausgabe Fehler-Bayes-Netz"),
                                   # button refresh plot
                                   actionButton(inputId = "idi_ab_bayes_refresh",
                                                label = "Ausgabe", 
                                                icon = icon("sync"), width = 100),
                                   # charging circuit
                                   add_busy_spinner(spin="fading-circle",
                                                    timeout=100)
                               ),
                               ## plot bayesian net
                               box(width = NULL,
                                   plotOutput(outputId = "ido_bayesnet_solo_plot", 
                                              height = 1000)
                               )))
                    ),# end Tab Bayes net
            ######################################################################
            ## Tab PARAMETER LEARNING
            tabItem(tabName = "paralearn",
                    fluidPage(
                        column(width = 12,
                               h2("Parameter Bayes Netz lernen"),
                               # instructions
                               helpText("1. Laden Sie die neuen Daten als .csv 
                                        Datei hoch"),
                               helpText("2. Lernen Sie die Parameter mittels 
                                        'Parameter Lernen'"),
                               helpText("3. Automatisches Speicher des Datensatzes"),
                               box(width = NULL,
                                   # select bayes net
                                   selectInput(inputId = "idi_bnart",
                                               label = "Vorformling/Blasformteil",
                                               choices = c("Vorformling",
                                                           "Blasformteil")),
                                   # upload file
                                   fileInput(inputId = "idi_para_in", 
                                             label = "Lade Parameterliste", 
                                             buttonLabel = "Durchsuchen...", 
                                             placeholder = "No file selected",
                                             accept = ".csv"),
                                   # learn button
                                   actionButton(inputId = "idi_learn",
                                                label = "Parameter lernen"),
                                   textOutput(outputId = "ido_startlearn")),
                               # charging circuit
                               add_busy_spinner(spin="fading-circle",timeout=100),
                               h2("Liste eingelesener Daten"),
                               # output data set as table
                               dataTableOutput(outputId = "ido_paralist")
                               )
                        )
                    )# end Tab parameter learning
        )# end Tab Items
    )# end Body
)# end Page

##################################
## Define server logic required ##
##################################
server <- function(input, output, session){
    ##############################################################################
    ##### TAB OVERVIEW #####
    
    ## Detect if tab changed or button for refresh pressed
    toListen <- reactive({
        list(input$tabs,input$idi_ac_refreshBN)
        })
    
    ## draw bayes net interactive
    observeEvent(toListen(),{
        output$ido_bayesnetz_gesamte_plot <- renderUI({
            # create DAG
            isolate(dag <- bnfit2DAG(renewBN(input$idi_bayesnettype)))
            # create list with error nodes 
            nodelist <- as.list(leaf.nodes(dag))
            # highlight cause nodes
            causehighlight <- list(background = "yellow", border = "black")
            # plot bayes net
            bnviewer::viewer(dag,
                             bayesianNetwork.width = "100%",
                             bayesianNetwork.height = "90vh",
                             bayesianNetwork.layout = "layout_with_dh",
                             bayesianNetwork.title = "Diskretes Bayes Netz - 
                                                     Gesamtübersicht",
                             node.shape = "ellipse",
                             node.colors = list(background = "#00a65a",
                                                border = "black",
                                                highlight = causehighlight),
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
            })
        })
    ##############################################################################
    ##### TAB ERROR CAUSE #####
    
    ## key to enable or disable buttons
    button <- reactiveValues(pcintervention = FALSE, RCA = TRUE)
    
    ## load current bayes net for the first launch 
    current <- reactiveValues(BN = renewBN_test1())
    
    ## load excel list for the first launch
    para <- reactiveValues(
        list = customizeExcellist(readxl::read_xlsx("Parameterliste.xlsm",
                                                    sheet = "Fehler Vorformling"))
        )
    
    ## drop down menu to choose type of cause analysis
    output$ido_causetyp <- renderUI({
        selectInput(inputId = "idi_causetype",
                    label = "Wählen Sie Art der Analyse aus:",
                    choices = c("wahrscheinlichste","einflussstärkste"))
        })
    
    ## drop down menu to choose current fault pattern
    output$ido_errorpattern <- renderUI({
        selectInput(inputId = "idi_curerror",
                    label = "Wählen Sie aus",
                    choices = leaf.nodes(current$BN))
        })
    
    ## initialize knowledge about predicted causes
    knowhow <- reactiveValues(causes = NULL, states = NULL)
    
    ## initialize list for nodes, observations, manual parameters
    nodes <- reactiveValues(list = NULL)
    obs <- reactiveValues(list = NULL)
    man <- reactiveValues(list = NULL)
    
    ## initialize counter for trails, observations and numbers of RCA
    # maxnumoftrails: max number of trails equal to the number of causes
    # numoftrails: number of predicted causes, increment after each forecast
    # obs: number of observations in collected data set
    # rca: number of RCA-runs since last system shutdown
    counter <- reactiveValues(maxnumoftrails=1, numoftrails=0, obs=1, rca=0)
    
    ## load correct excel list and bayes net, when main Bayesnet was selected
    observeEvent(input$idi_bayesnetart,{
        # when 'Vorformling' was selected
        if(input$idi_bayesnetart == "Vorformling"){
            # load parameter list
            para$list <- readxl::read_xlsx("Parameterliste.xlsm",
                                           sheet = "Fehler Vorformling")
            para$list <- customizeExcellist(para$list)
            cat("Parameter Liste für Vorformling wird geladen\n")
            # load bayes net
            #current$BN <- renewBN(input$idi_bayesnetart)
            current$BN <- renewBN_test1() # delete when use main bayes net
            cat("Bayes Netz für Vorformling wird geladen\n")}
        # when 'Blasformteil' was selected
        else if(input$idi_bayesnetart == "Blasformteil"){
            # load parameter list
            para$list <- readxl::read_xlsx("Parameterliste.xlsm",
                                           sheet = "Fehler Blasformteil")
            para$list <- customizeExcellist(para$list)
            cat("Parameter Liste für Blasformteil wird geladen\n")
            # load bayes net
            current$BN <- renewBN(input$idi_bayesnetart)
            cat("Bayes Netz für Blasformteil wird geladen\n")}
        else{cat("Fehler bei Auswertung Bayesnetz Typ\n")}
    })
    
    ## initialization after start root-cause-analysis (RCA)
    observeEvent(input$idi_ab_errorcausecal,{
        # list over all nodes influence to the error plus error
        nodes$list <- c(input$idi_curerror,
                        InfluNodes(current$BN,input$idi_curerror))
        # set max numbers of trails (number of causes)
        counter$maxnumoftrails = (length(nodes$list)-1)
        # a empty list for observations
        obs$list <- data.frame(matrix(nrow = 1, ncol = length(nodes$list),
                                      dimnames = list(NULL, nodes$list)))
        # a list over all manual parameter to a selected error
        man$list <- manParaFault(para$list,current$BN,input$idi_curerror)
        # set error state true
        obs$list[1,input$idi_curerror] <- "Ja"
        ############################################################################################
        # Replacement for capturing the machine data
        py_run_file("Pythonanbindung/main.py")
        pydaten <- py$data
        for(i in 1:ncol(pydaten)){
            if(colnames(pydaten)[i] %in% colnames(obs$list)){
                obs$list[1,colnames(pydaten)[i]] <- pydaten[i] }
        }
        ###
        # query machine data
        ###
        ###########################################################################################
        # disable drop down menu RCA type
        disable("ido_causetyp")
        # disable drop down menu to switch bayes net
        disable("idi_bayesnetart")
        # disable drop down menu error pattern
        disable("ido_errorpattern")
    })
    
    ## initialize inference list as empty data.frame
    inference <- reactiveValues(list=data.frame(list(cause=NA,state=NA,prob=NA)))
    
    ## observe if button "error not fixed" pressed
    observeEvent(input$idi_pintervention_n,{
        # set actual suggested cause
        cause <- inference$list[1,1]
        if(!is.na(match(cause,man$list))){
            # Query manual parameters
            index <- which(cause==para$list[,"Netzname"])
            # possible states for a node
            choise <- para$list[index,c("Zustand_default","Zustand_2",
                                            "Zustand_3")]
            # when only two possible states remove the third one
            if(choise[3] == "-")
                choise <- choise[-3]
            # show modal dialog for manual parameters
            showModal(modalDialog(
                title = "Vorgefundener Parameterzustand",
                # query state of manual parameter
                radioButtons(inputId = "idi_para_state",
                             label = paste("Parameter:", cause),
                             choices = as.character(choise), inline = TRUE),
                footer = tagList(actionButton("close_errortrue", "Bestätigen"))
            ))}
        else{
            # show modal dialog for automatic parameter
            showModal(modalDialog(
                title = "Bestätigung - Fehler nicht behoben",
                h4("Bestätigen Sie, dass der Fehler weiterhin besteht."),
                footer = tagList(actionButton("close_errortrue", "Bestätigen"))
            ))}
        
        # add new observation (increment counter)
        counter$obs <- (counter$obs + 1)
        # set error state true
        obs$list[counter$obs,input$idi_curerror] <- "Ja"
        ###########################################################################################
        # Replacement for capturing the machine data
        py_run_file("Pythonanbindung/main.py")
        pydaten <- py$data
        for(i in 1:ncol(pydaten)){
            if(colnames(pydaten)[i] %in% colnames(obs$list)){
                obs$list[counter$obs,colnames(pydaten)[i]] <- pydaten[i] }
        }
        ###
        # query machine data
        ###
        ###########################################################################################
        # set state from cause (machine parameter) when error not fixed
        if(!(inference$list[1,1] %in% man$list)){
            # insert current machine state
            knowhow$states <- c(knowhow$states,pydaten[inference$list[1,1]])}
        })
    
    ## determine cause of error
    observeEvent(c(input$idi_ab_errorcausecal,input$close_errortrue),{
        # initialization event "Start RCA" came without pushing the button
        if(input$idi_ab_errorcausecal==0){
            cat("Initialisierung","\n")
            return()}
        # event button close modal window came while opening window, suppress
        if((input$idi_ab_errorcausecal==counter$rca) &&
           (req(input$close_errortrue)<=0)){
            cat("Modalfenster wurde geöffnet")
            return()}
        # print starts calculation
        cat("Ursachenermittlung gestartet...")
        # reset after the maximum number of attempts has been reached
        if(counter$numoftrails >= counter$maxnumoftrails){
            # reset counter trails
            counter$numoftrails <- 0
            # rest known good cause conditions
            knowhow$causes <- NULL
            knowhow$states <- NULL
            # disable buttons to confirm process intervention
            button$pcintervention <- FALSE
            # enable button to start cause analysis
            button$RCA <- TRUE
            # show modal dialog no cause found
            showModal(modalDialog(
                title = "Keine Ursache gefunden",
                footer = tagList(
                    modalButton("Bestätigen")),
                paste0("Es konnte keine Ursache gefunden werden",
                       "Beheben Sie den Fehler eigenständig!",
                       sep = "")))
            return()
        }
        # calculate inference
        isolate(if(input$idi_causetype == "wahrscheinlichste"){
            # most probable cause
            inference$list <- causeanalysis_likeliestdyn(current$BN,
                                                         input$idi_curerror,
                                                         knowhow$causes,
                                                         knowhow$states)}
        else if(input$idi_causetype == "einflussstärkste"){
            # strongest cause
            inference$list <- causeanalysis_strongestdyn(current$BN,
                                                         input$idi_curerror,
                                                         knowhow$causes,
                                                         knowhow$states)})
        # set predicted causes into array
        knowhow$causes <- c(knowhow$causes, inference$list[1,1])
        # highlight error cause at plot
        output$ido_bn_faultplot <- renderUI({
            isolate(faultnet <- createDAG(createMStrings(para$list,
                                                         input$idi_curerror)))
            isolate(bnviewer::viewer(faultnet,
                                     bayesianNetwork.width = "100%",
                                     bayesianNetwork.height = "70vh",
                                     bayesianNetwork.layout = "layout_with_lgl",
                                     bayesianNetwork.title = "Fehlerursache",
                                     node.shape = "ellipse",
                                     node.colors = list(background = "white",
                                                        border = "black"),
                                     edges.width = 3,
                                     options.highlightNearest = TRUE,
                                     clusters = list(
                                         list(label = "Fehler", shape = "icon",
                                              icon = list(color = "#98FB98"),
                                              nodes = input$idi_curerror),
                                         list(label = "Fehlerursache",
                                              shape = "icon",
                                              icon = list(color = "#FF4500"),
                                              nodes = inference$list[1,1]))))
            })
        # show descending list of error causes 
        output$ido_errorcauselist <- renderDataTable(
            options = list(pageLength = 5),{inference$list
            })
        # print what is the cause of the error
        output$ido_errorcausetext <- renderText({
            paste("<font size=3><b>Vorhergesagte Fehlerursache</b>: ",
                  inference$list[1,1], "<br>",
                  "<b>Im Zustand</b>: ", inference$list[1,2],
                  "</font>")
            })
        # process intervention possible or not possible
        output$ido_pintervention <- renderText({
            paste("<font size=3>",
                  "Konnte der Fehler vollständig behoben werden?",
                  "</font")
            })
        # enable buttons to confirm process intervention
        button$pcintervention <- TRUE
        # disable button to start cause analysis
        button$RCA <- FALSE
        # increment number of trails
        counter$numoftrails <- counter$numoftrails + 1
        # increment number of RCA runs
        counter$rca <- input$idi_ab_errorcausecal
        # print Calculation finished
        cat("beendet!\n")
        })
    
    ## enable or disable buttons for process intervention
    observe(
        if(button$pcintervention){
            enable("idi_pintervention_y")
            enable("idi_pintervention_n")}
        else if(!button$pcintervention){
            disable("idi_pintervention_y")
            disable("idi_pintervention_n")}
        )
    
    ## enable or disable button to start cause analysis
    observe(
        if(button$RCA){
            enable("idi_ab_errorcausecal")
            enable("ido_causetyp")}
        else if(!button$RCA){
            disable("idi_ab_errorcausecal")
            disable("ido_causetyp")}
        )
    
    ## set different variables
    observeEvent(input$idi_pintervention_y, {
        # rest known good cause conditions
        knowhow$causes <- NULL
        knowhow$states <- NULL
        # rest counter of trials
        counter$numoftrails <- 0
        # enable button to start cause analysis
        button$RCA <- TRUE
        # disable buttons to confirm process intervention
        button$pcintervention <- FALSE
        # add new observation (increment counter)
        counter$obs <- (counter$obs + 1)
        # when predicted cause was a manual one, set predicted state into obs list
        if((inference$list[1,1] %in% man$list)){
            obs$list[counter$obs-1,inference$list[1,1]] <- inference$list[1,2]
        }
        # show modal dialog error fixed, quire unknown manual parameters
        showModal(modalDialog(
            title = "Der Fehler konnte vollständig behoben werden",
            h4('Eingabe der offenen Parameter'),
            ## after error fixed, Query manual parameters
            lapply(1:length(man$list), function(i){
                if(all(is.na(obs$list[,man$list[i]]))){
                    index <- which(man$list[i]==para$list[,"Netzname"])
                    choise <- para$list[index,c("Zustand_default","Zustand_2",
                                                    "Zustand_3")]
                    if(choise[3] == "-"){
                        choise <- choise[-3]}
                    radioButtons(inputId = paste("idi_para",i, sep = ""),
                                 label = paste("Parameter:", man$list[i]),
                                 choices = as.character(choise), inline = TRUE)
                    }}),
            footer = tagList(actionButton("close_errorfix", "Bestätigen")),
            ))
        # reset counter push button no, "error no fixed"
        counter$rca <- 0
        # enable drop down menu error pattern
        enable("ido_errorpattern")
        # enable drop down menu RCA type
        enable("ido_causetyp")
        # enable drop down menu to switch bayes net
        enable("idi_bayesnetart")
        })
    
    ## read parameter state after error is not fixed and close modal window
    observeEvent(input$close_errortrue,{
        # close open modal window
        removeModal()
        # if suggested cause is manual parameter, set into observation list
        if((inference$list[1,1] %in% man$list)){
            obs$list[counter$obs,inference$list[1,1]] <- input[["idi_para_state"]]
            knowhow$states <- c(knowhow$states, input[["idi_para_state"]])
            #cat(input[["idi_para_state"]],"\n")
            }
        })
    
    ## after error fixed read man parameters, save data set and close modal window
    observeEvent(input$close_errorfix,{
        # close open modal window
        removeModal()
        # set at modal window queried parameter states into data set observation
        for (i in 1:length(man$list)) {
            if(all(is.na(obs$list[,man$list[i]]))){
                obs$list[counter$obs,man$list[i]] <- input[[paste0("idi_para",i,
                                                                   sep="")]]
                #cat(input[[paste0("idi_para" ,i, sep="")]],"\n")
            }}
        ###########################################################################################
        # Replacement for capturing the machine data
        py_run_file("Pythonanbindung/main.py")
        pydaten <- py$data
        for(i in 1:ncol(pydaten)){
            if(colnames(pydaten)[i] %in% colnames(obs$list)){
                obs$list[counter$obs,colnames(pydaten)[i]] <- pydaten[i] }
        }
        ###
        # query machine data
        ###
        ###########################################################################################
        # set error state false
        obs$list[counter$obs,input$idi_curerror] <- "Nein"
        # save RCA data set 
        write.csv2(obs$list,"Testfile.csv")
        write.csv2(obs$list,paste0("Data_collection/raw/Data_",
                                   format(Sys.time(),"%Y_%m_%d_%H_%M_%S"), ".csv",
                                   sep = ""))
        # complete data set and save 
        dataset <- filldataset(obs$list,para$list)
        write.csv2(dataset,paste0("Data_collection/edited/Data_",
                                  format(Sys.time(),"%Y_%m_%d_%H_%M_%S"), ".csv",
                                  sep = ""))
        # reset observation counter
        counter$obs <- 1
    })
    
    ## cancel root cause analysis and reset all
    observeEvent(input$idi_ab_stop,{
        # rest known good cause conditions
        knowhow$causes <- NULL
        knowhow$states <- NULL
        # rest counter of trials
        counter$numoftrails <- 0
        # enable button to start cause analysis
        button$RCA <- TRUE
        # disable buttons to confirm process intervention
        button$pcintervention <- FALSE
        # reset counter for RCA runs
        counter$rca <- 0
        # reset counter for observations of the last RCA run
        counter$obs <- 1
        # enable drop down menu error pattern
        enable("ido_errorpattern")
        # enable drop down menu RCA type
        enable("ido_causetyp")
        # enable drop down menu to switch bayes net
        enable("idi_bayesnetart")
    })
    ##############################################################################
    ##### TAB BAYESIAN NETWORKS #####
    ## drop down menu to choose fault pattern
    output$bayesnet_errorpattern <- renderUI({
        selectInput(inputId = "bayesnet", 
                    label = "Fehlerbild auswählen",
                    choices = c(leaf.nodes(renewBN(input$idi_bntype))))
    })
    
    ## initialize parameter list with matrix structure
    matrix <- reactiveValues(
        structure = customizeExcellist(read_xlsx("Parameterliste.xlsm",
                                                 sheet = "Fehler Vorformling")))
    
    ## load correct excel list and bayes net, select with drop-down menu
    observeEvent(input$idi_bntype,{
        # when 'Vorformling' was selected
        if(input$idi_bntype == "Vorformling"){
            # load parameter list
            matrix$structure <- readxl::read_xlsx("Parameterliste.xlsm",
                                                  sheet = "Fehler Vorformling")
            matrix$structure <- customizeExcellist(matrix$structure)
            cat("Parameter Liste für Vorformling wird geladen\n")}
        # when 'Blasformteil' was selected
        else if(input$idi_bntype == "Blasformteil"){
            # load parameter list
            matrix$structure <- readxl::read_xlsx("Parameterliste.xlsm",
                                                  sheet = "Fehler Blasformteil")
            matrix$structure <- customizeExcellist(matrix$structure)
            cat("Parameter Liste für Blasformteil wird geladen\n")}
        else{cat("Fehler bei Auswertung Bayesnetz Typ\n")}
    })
    
    ## plot selected error pattern
    observeEvent(input$idi_ab_bayes_refresh,{
        output$ido_bayesnet_solo_plot <- renderPlot({
        # create a DAG for selected error
        isolate(dag <- createDAG(createMStrings(matrix$structure,input$bayesnet)))
        # plot bayes net structure
        isolate(renderGraph(highlightFault(drawBayesNet(dag), input$bayesnet)))
        })
    })
    ##############################################################################
    ##### TAB PARAMETER LEARNING #####
    ## variable for disabling and enabling buttons
    loadDataReady <- reactiveValues(ok = FALSE)
    
    ## load data input and check if csv file
    data <- reactive({
        # upload file
        file <- input$idi_para_in
        # return file extension (i.e. csv)
        ext <- tools::file_ext(file$datapath)
        # check file exists
        req(file)
        # check file is a csv file, otherwise warning message
        validate(need(ext == "csv", "Bitte verwenden Sie eine '.csv' Datei"))
        # enable button learn
        loadDataReady$ok <- TRUE
        # load uploaded file into data
        read.csv2(file$datapath, header = TRUE)
        })
    
    ## display uploaded data
    output$ido_paralist <- renderDataTable({data()})
    
    ## disable or enable button 'learn'
    observe(
        if(loadDataReady$ok){
            enable("idi_learn")}
        else{
            disable("idi_learn")}
        )
    
    ## load current bayes net for the first launch 
    actual <- reactiveValues(bn = renewBN("Vorformling"))
    
    ## load select bayes net 'Vorformling' or 'Blasformteil'
    observeEvent(input$idi_bnart,{
        if(input$idi_bnart == "Vorformling"){
            actual$bn <- renewBN(input$idi_bnart)}
        else if(input$idi_bnart == "Blasformteil"){
            actual$bn <- renewBN(input$idi_bnart)}
    })
    
    ## learning was started
    observeEvent(input$idi_learn,{
        output$ido_startlearn <- renderText({
            isolate(if(input$idi_bnart == "Vorformling"){
                        # save data input as .csv
                        write.csv2(data(),"Data_archive/data_download_Vorfromling.csv")
                        # learn parameters
                        actual$bn <- trainBN(bnfit2DAG(actual$bn),"Vorformling")
                        # save new bayes net
                        write.net("actual_bn_Vorformling.net",actual$bn)
                        "Das Lernen wurde ausgeführt...."}
                    else if(input$idi_bnart == "Blasformteil"){
                        # save data input as .csv
                        write.csv2(data(),"Data_archive/data_download_Blasformteil.csv")
                        # learn parameters
                        actual$bn <- trainBN(bnfit2DAG(actual$bn),"Blasformteil")
                        # save new bayes net
                        write.net("actual_bn_Blasformteil.net",actual$bn)
                        "Das Lernen wurde ausgeführt...."})
            })
        # hint after learning parameters
        showModal(modalDialog(
            title = "Neue Parameter",
            footer = modalButton("Bestätigen"),
            h4('ACHTUNG, die Parameter wurden neu gelernt.')))
        # disable button learn
        loadDataReady$ok <- FALSE
        })
##################################################################################
    ## general commands
    # close session
    session$onSessionEnded(function() {
        cat("Die Session wird geschlossen")
        stopApp()
        cat(": Schließen war erfolgreich\n")
    })
}
##################################
##     Run the application      ##
##################################
shinyApp(ui = ui, server = server)