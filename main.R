library(bnlearn)    # to create and use bayesian networks 
library(Rgraphviz)  # to plot bayesian networks
library(bnviewer)   # for interactive plots
library(readxl)     # to read in .xlsm file
library(reticulate) # to execute python file

# include functions
source("functions.R")

# config and start python at the beginning, only once
if(exists("pythonstarted") == FALSE ){
  # set path to the python executable file
  use_python("Pythonanbindung/venv/Scripts/python.exe", required=F)
  # check version of python
  py_config()
  # set variable TRUE, because runs only once
  pythonstarted <- TRUE
}

# create model string 'Vorformling', when file doesn't exist on working directory
if(!(file.exists("MSVorformling.txt"))){
  # read in excel list 'Vorformling'
  excelliste <- readxl::read_xlsx("Parameterliste.xlsm",
                                  sheet = "Fehler Vorformling")
  # prepare excel list
  excelliste <- customizeExcellist(excelliste)
  # create model string and save on working directory
  excel2ms(excellist = excelliste, netname = "Vorformling",
           firstfault = "Kratzer")
}

# create model string 'Blasformteil', when file doesn't exist on working directory
if(!(file.exists("MSBlasformteil.txt"))){
  # read in excel list 'Blasformteil'
  excelliste <- readxl::read_xlsx("Parameterliste.xlsm",
                                  sheet = "Fehler Blasformteil")
  # prepare excel list
  excelliste <- customizeExcellist(excelliste)
  # create model string and save on working directory
  excel2ms(excellist = excelliste, netname = "Blasformteil",
           firstfault = "Butzenabfall")
}