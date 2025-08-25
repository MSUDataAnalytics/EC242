source("renv/activate.R")

library(data.table)
library(tidyverse)
library(here)


filter <- dplyr::filter


copyFromOneDrive<-function(){
  fromOneDrive = "C:\\Users\\jkirk\\OneDrive - Michigan State University\\Teaching\\OneDrive_to_EC242_Portal"
  toD = 'D:\\Courses\\EC242\\Images'
  allFilesInOneDrive = list.files(fromOneDrive, full.names = T)
  allFilesInD = list.files(toD, full.names=T)
  
  filesToCopy = allFilesInOneDrive[!basename(allFilesInOneDrive) %in% basename(allFilesInD)]
  print('Copying the following files')
  print(filesToCopy)
  
  for(ff in filesToCopy){
    file.copy(ff, file.path(toD, basename(ff)), overwrite = TRUE )
  }
}


