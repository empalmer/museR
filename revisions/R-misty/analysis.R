
#============================================#
#               SCORE ANALYSIS
#============================================#

# Given a krn file, analyze the score and return the summary statistics
analyzeKRN <- function(filename){ analyzeScore(kern2df(filename)) }

#============================================#
# After splitting the score into subscores (accounting for key changes), analyze each subscore and obtain score summary 
analyzeScore <- function(score, bars = F){
  # Filter for notes
  score <- getScoreNotes(score)
  # Split into subscores (accounting for key changes)
  subscores <- score %>% split(score$key_p)
  # Get the modality of each subscore
  modal <- map_dfr(subscores, subscoreModality)
  if(bars){ return(modal) }
  # Remove bars
  modal <- modal %>% select(!contains("bar"))
  # Return the summary statistics
  purrr::map_dbl(modal, mean)
}
#============================================#
# Analyze the modal features of a subscore (same key)
subscoreModality <- function(subscore){
  # Get the tonic and quality from the key signature
  key_sig <- relative_key(subscore)
  tonic <- str_to_upper(key_sig[[1]])
  quality <- str_to_lower(key_sig[[2]])
  # Get the number of bars
  bars <- levels(as.factor(as.numeric(subscore$measure_p)))
  num_bars <- length(bars)
  # Create a dataframe with a row for each bar
  modal <- data.frame(bar = 1:num_bars)
  modal <- cbind(modal, .modalRow())
  # Get the modal features of each bar
  for(i in 1:num_bars){
    # Get the measure
    measure <- getMeasures(measure = bars[i], score = subscore)
    # Obtain the notes and their sd freq
    measure_notes <- collapseNotes(measure)
    freqs <- SD_freq(measure_notes, tonic)
    modal[i,2:6] <- modality(freqs, quality)
  }
  return(modal)
}
#============================================#
# Helper function for subscoreModality
.modalRow <- function(){
  data.frame(blues = NA, interchange = NA, borrowed_sub = NA, dissonance = NA, consonance = NA)
}

#============================================#
#               MODAL ANALYSIS
#============================================#

# Get the frequency table of the scale degrees
SD_freq <- function(notes, tonic){
  table(map(notes, function(x,y){ scaleDegree(y,x) }, tonic) %>% unlist())
}
#============================================#
modality <- function(freqs, quality = NA){
  # Use the quality to study modality
  if(quality == "major"){ return (.modalityMajor(freqs)) }
  else if(quality == "minor"){ return (.modalityMinor(freqs)) }
}
#============================================#
.modalityMinor <- function(freqs){
  # Get the total number of notes
  total <- sum(freqs)
  # Create a data frame of features
  modality <- data.frame(blues = 0)
  # Blues
  modality$blues <- .countFreqs(c("3","#4","-5"), freqs)
  # Interchange
  modality$interchange <- .countFreqs(c("-2","3","6","7"), freqs)
  # Borrowed subdominant 
  modality$borrowed_sub <- .countFreqs(c("6"), freqs)
  # Consonance
  modality$consonance <- .countFreqs(c("1","-3","5","-6"), freqs)
  # Dissonance
  modality$dissonance <- .countFreqs(c("-2","#2","#4","-5","-7","7"), freqs)
  # Return the features
  modality/total
}
#============================================#
.modalityMajor <- function(freqs){
  # Get the total number of notes
  total <- sum(freqs)
  # Create a data frame of features
  modality <- data.frame(blues = 0)
  # Blues
  modality$blues <- .countFreqs(c("-3","#4","-5","-6"), freqs)
  # Interchange
  modality$interchange <- .countFreqs(c("-3","#4","-6","-7"),freqs)
  # Borrowed subdominant 
  modality$borrowed_sub <- .countFreqs(c("-6","#5"), freqs)
  # Consonance
  modality$consonance <- .countFreqs(c("1","3","5","6"), freqs)
  # Dissonance
  modality$dissonance <- .countFreqs(c("-2","#2","#4","-5","-7","7"), freqs)
  # Return the features
  modality/total
}
#============================================#
.countFreqs <- function(scale_degrees, freqs){
  count <- 0
  for(i in 1:length(scale_degrees)){
    freq <- freqs[scale_degrees[i]] %>% unname()
    if(!is.na(freq)){ count <- count + freq }
  }
  count
}

#============================================#
#               SCORE WRANGLING
#============================================#

# Remove unwanted columns from a score dataframe
getScoreNotes <- function(score){
  score %>% select(contains(c("key","measure","name")))
}
#============================================#
# Get a measure or range of measures of the score notes dataframe
getMeasures <- function(measure, score){
  score %>% filter(measure_p %in% c(measure)) #%>% select(!contains("measure_p"))
}
#============================================#
# Given a score (or subscore) dataframe, collapse the score into a vector of the notes in the note columns
collapseNotes <- function(score){
  score_notes <- score %>% select(!contains(c("key","measure_p")))
  score_notes <- map(score_notes, .cleanNoteColumn) %>% unlist() %>% unname()
  score_notes <- score_notes[which(!is.na(score_notes))]
  score_notes <- score_notes[which(score_notes %in% POSSIBLE_NOTES)]
}
#*******************************************#
# Helper function: .obtainNotes()
.cleanNoteColumn <- function(note_column){
  notes <- as.vector(note_column)
  rests_idx <- c(which(notes == "."), (which(notes == "rest")))
  notes[rests_idx] <- NA
  return(notes)
}
#============================================#



