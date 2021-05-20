
# Import dependencies
require(tidyverse)
#require(patchwork)

# Imports all source code for a variable depth of directory.
.src <- function(depth = 1, r_folder = "../R/"){
  # Get directory path based on directory depth for use in directories with given variable depth
  dir_path <- paste(c(rep("../",depth-1),r_folder), sep = "", collapse = "")
  # Get script file names and get their path
  script_files <- dir(path = dir_path, pattern = "\\.R$")
  # Helper function to parse string name
  .ADDpref <- function(filename, dirpath){paste(dirpath, filename, sep = "", collapse = "")}
  script_paths <- as.list(purrr::map_chr(script_files, .ADDpref, dir_path))
  sapply(script_paths, source, .GlobalEnv)
}

# Imports a preset of source code for a variable depth of directory.
.srcp <- function(depth = 1, preset, r_folder = "../R/"){
  # Get directory path based on directory depth for use in directories with given variable depth
  dir_path <- paste(c(rep("../",depth-1),r_folder), sep = "", collapse = "")
  # Get script file names and get their path
  script_files <- dir(path = dir_path, pattern = "\\.R$")
  # Script file presets
  preset1 <- script_files[which(stringr::str_detect(script_files, pattern = "krn"))] # KRN Files
  preset2 <- c("misty.R","incidental.R","enharmonic.R","interval.R","scale.R")
  preset3 <- c(preset1,preset2)
  # Set preset
  if(preset == 1){script_files <- preset1}
  if(preset == 2){script_files <- preset2}
  # Helper function to parse string name
  .ADDpref <- function(filename, dirpath){paste(dirpath, filename, sep = "", "")}
  script_paths <- as.list(purrr::map_chr(script_files, .ADDpref, dir_path))
  sapply(script_paths, source, .GlobalEnv)
}