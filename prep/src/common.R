#common.R

#Common libraries and directories that we use for OHI Norway project


# set the mazu data_edit share based on operating system
dir_M             <- c('Windows' = '//ftp.imr.no', #main directory to get to 
                       'Darwin'  = '/Volumes/ftp.imr.no')  


# WARN rather than stop if directory doesn't exist
if (!file.exists(sprintf('%s/',dir_M))){
  warning(sprintf("The directory for variable dir_M set in src/R/common.R does not exist. Do you need to mount %s?", dir_M))
  
}

# install (if necessary) and load commonly used libraries
packages <- c('tidyverse','dplyr', 'tidyr', 'stringr', 'readr', 'ggplot2')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  cat(sprintf("Installing %s\n", setdiff(packages, rownames(installed.packages()))))
  install.packages(setdiff(packages, rownames(installed.packages())))
}

#round everything to 2 decimal places
options(digits=3, scipen = 999)

#libraries
library(readxl)
library(tidyverse)
library(stringr)
library(RColorBrewer)
library(rgdal)
library(raster)

rm(packages)
