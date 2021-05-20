
# Returns a list of kern scores by composer
byComposer <- function(composer, dir_path = "../data/composer/", bad_idx = NA, loud = T){
  # Go to the composer file folder
  dir_path <- paste(dir_path, composer, "/", sep = "")
  # Get score file names and get their path
  composer_scores <- dir(path = dir_path, pattern = "\\.krn$")
  # Take out the bad scores
  if(class(bad_idx) != "logical"){ composer_scores <- composer_scores[-bad_idx] }
  # Print the piece names
  if(loud) { print(composer_scores) }
  # Helper function to parse string name
  .ADDprefix <- function(filename, dirpath){paste(dirpath, filename, sep = "", collapse = "")}
  score_paths <- as.list(purrr::map_chr(composer_scores, .ADDprefix, dir_path))
  # Read the kern files and return a list 
  purrr::map(score_paths, kern2df)
}

# List the scores found in the directory by the composer
find_composer_scores <- function(composer){
  # Find composer-sorted directory
  dir_path = "../data/composer/"
  # Go to the composer file folder
  dir_path <- paste(dir_path, composer, "/", sep = "")
  # Get script file names and get their path
  composer_scores <- dir(path = dir_path, pattern = "\\.krn$")
  # Return composer scores
  composer_scores
}

# Using the composer score filename, outputs the relevant kern file
composer_score <- function(composer, krn_file){
  # Find composer-sorted directory
  dir_path = "../data/composer/"
  # Go to the composer file folder
  dir_path <- paste(dir_path, composer, "/", sep = "")
  # Get the kern score for the composer's piece
  score_path <- paste(dir_path, krn_file, sep = "")
  # Get the data frame for the score
  kern2df(score_path)
}

.labelComposer <- function(array, composer){
  array$composer <- rep(composer,nrow(array))
  array
}
.composerModal <-  function(scores, composer){
  map_dfr(scores, analyzeScore) %>% .labelComposer(composer)
}