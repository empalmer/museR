#========================================================#
#                       PIECE_DF
#========================================================#
#' piece_df
#' Using multiple .krn files, calls kern2df to create piece_df: a dataframe for the entire piece
#'
#' @param krn_files string vector of .krn filenames
#' @param instruments string vector of instruments
#' @return a dataframe containing all instruments for the whole piece

piece_df <- function(krn_files, instruments){
  # Get the piece info from one of the files ONCE
  piece_info <- .piece_df_pieceinfo(krn_files)
  # Get the labeled note columns for each instrument by column binding each one iteratively
  notes_df <- purrr::map2_dfc(krn_files, instruments, .kern2df_instrument)
  # Generate the piece_df by column binding the two dataframes
  piece_df <- cbind(piece_info, notes_df)
  return(piece_df)
}
#========================================================#
# Given a vector of krn files, get their common piece info once
.piece_df_pieceinfo <- function(krn_files){
  # Get one of the pieces (doesn't matter which)
  piece_df <- kern2df(krn_files[1])
  # Obtain the piece information columns using the _p regex
  piece_info <- piece_df %>% select(contains("_p"))
  return(piece_info)
}
#========================================================#
# Given a filename, returns the note columns labelled with that instrument
.kern2df_instrument <- function(spline, instrument){
  # Get the current dataframe
  piece_df <- kern2df(spline)
  # Remove the piece information columns
  notes_df <- piece_df %>% select(!contains("_p"))
  # Apply the instrument name to the note columns
  colnames(notes_df) <- paste(instrument, colnames(notes_df), sep = ".")
  return(notes_df)
}

#========================================================#
#                         KERN2DF
#========================================================#
#' kern2df
#' Takes a .krn spline and converts into tidy dataframe of note rows
#'
#' @param spline a .krn score file
#' @return tidy frame of note rows
#
# 
kern2df <- function(spline){
  raw_data <- readLines(spline) # Read the lines of the spline as a vector of strings
  data <- raw_data[-grep("^!|\\*", raw_data)] # Removes extra text and information - lines that start with *
  notes_df <- .k2df_tidy_measures(data) # Get dataframe with tidy rows - move measure lines and make them columns
  # Tidy the dataframe
  notes_df <- .k2df_resolve_chords(notes_df) # Resolve the note pattern chords into seperate tidy note columns
  # Useful values
  max_notes <- ncol(notes_df) - 1 # Get the number of notes in the largest chord  
  num_notes <- nrow(notes_df) # Get the number of notes
  # Add piece information to the dataframe
  notes_df$key_p <- .k2df_get_key(raw_data, notes_df)
  notes_df$meter_p <- .k2df_get_meter(raw_data)
  notes_df <- notes_df[,c(c((ncol(notes_df) - 1):(ncol(notes_df)), 1:(1+max_notes)))] # Reorder the columns
  # Save the key, meter, and measure columns
  piece_info <- notes_df[,1:3]
  # Resolve each note column into rhythm and note value/name
  note_columns <- notes_df %>% select(contains("note"))
  notes_df <- map2_dfc(note_columns, 1:ncol(note_columns), .k2df_resolve_notes)
  notes_df <- as.data.frame(lapply(notes_df, function(y) gsub("K", "", y)))
  # Return the piece info and the resolved note columns
  cbind(piece_info, notes_df)
}

#========================================================#
#                K2DF: PIECE INFORMATION
#========================================================#
# Gets the key of a .krn piece
.k2df_get_key <- function(data, notes_df){
  measure_no <- 0
  keys <- c()
  keys_idx <- c()
  for (line in data) {
    if (str_detect(line, "=[:digit:]")) {
      measure_no <- measure_no + 1
    }
    if (str_detect(line, "\\*{1}k\\[")) {
      key_sig <- str_extract(line, "\\[.*?\\]")
      key_sig <- str_replace_all(key_sig, "\\[|\\]", "")
      key <- "C"
      if (key_sig != "") {
        get_key <- KRN_KEYS$key
        names(get_key) <- KRN_KEYS$key_sig
        key <- as.character(get_key[key_sig])
      }
      keys <- c(keys,key)
      keys_idx <- c(keys_idx, measure_no)
    }
  }
  keys_idx[1] <- 1
  measures <- notes_df$measure_p
  all_keys <- c()
  count <- 1
  for (m in measures) {
    low <- keys_idx[count]
    high <- keys_idx[count+1]
    if (!is.na(high) && as.numeric(m) == high) {
      count <- count + 1
    }
    all_keys <- c(all_keys, keys[count])
  }
  all_keys
  # Old Code
  #key_v <- grep("^\\*.*[^I]:$",data,value = T)
  ## In case not immediately found
  #if(identical(key_v,character(0))){
  #  key_v <- grep("\\*{1}k\\[",data,value = T)
  #}
  #key_v<- gsub("\t.*","",key_v)
  #key_v
}
#========================================================#
# Gets the meter of a .krn piece
.k2df_get_meter <- function(data){
  meter <- grep("\\*{1}M[0-9]",data,value = T)
  meter
}
#========================================================#
#                K2DF: NOTE WRANGLING
#========================================================#
# Tidies the rows of the lines from a .krn piece into a tidy dataframe with a measure column 
.k2df_tidy_measures <- function(data){
  # Helper: Returns the interval of rows for measure i
  .measure_interval <- function(i, measure_bds){ (1 + measure_bds[i]) : measure_bds[i+1] }
  # Get rows with measure boundaries - lines that start with an =
  measure_bds <- grep("=", data, value = F)
  # Get the length of each measure by calling length() on the helper function
  measure_lengths <- map_dbl(1:(length(measure_bds)-1), function(i){length(.measure_interval(i, measure_bds))})
  # Get number of measures
  num_measures <- length(measure_lengths)
  # Generate the column designating which measure a note is in by using rep(measure_number, measure_length)
  measure_col <- do.call("c",lapply(1:num_measures, function(m){rep(m, measure_lengths[m])}))
  # Remove the measure boundaries
  measure_col <- measure_col[-measure_bds]
  # Make sure we only have notes
  note_patterns <- data[-measure_bds]
  # Get rid of the leading note
  if (min(measure_bds) > 1) {
    note_patterns <- note_patterns[-1]
  }
  # If the composer is in the note patterns, remove them
  if(str_detect(note_patterns[1], "COM")){ note_patterns <- note_patterns[-c(1)] }
  # Get column of notes
  notes_df <- data.frame(measure_p = measure_col, note_pattern = note_patterns)
  return(notes_df)
}
#========================================================#
# Resolve the note pattern chords into seperate tidy note columns
.k2df_resolve_chords <- function(notes_df){
  # Helper: Resolves a .krn string with multiple notes (a chord) into a vector of single note patterns
  .resolve_chord_notes <- function(chord){unlist(strsplit(chord,"\\s"))}
  
  # Resolve the .krn strings with multiple notes (a chord) into a list of vectors of single note patterns
  resolved_notes <- map(as.character(notes_df$note_pattern), .resolve_chord_notes)
  max_notes <- max(lengths(resolved_notes)) # Get the number of notes in the largest chord  
  num_notes <- nrow(notes_df) # Get the number of notes
  
  # Intialize an empty array with number of columns to fit maximum number of notes needed
  notes_df_ <- map_dfc(1:max_notes, function(foo){data.frame(V = rep(NA_character_, num_notes))})
  # Populate the dataframe
  for(i in 1:num_notes){
    vec_notes <- str_replace_all(resolved_notes[[i]], "<|>|_|\\^|(|)", "")
    idx <- str_detect(vec_notes, "")
    vec_notes[!idx] <- NA
    notes_df_[i,1:length(vec_notes)] <- vec_notes
  }
  
  # Relabel each note column
  colnames(notes_df_) <- purrr::map_chr(1:max_notes, .f = function(i){paste("note",i,sep="")})
  # Add the measure column
  notes_df_ <- cbind(notes_df$measure_p, notes_df_); colnames(notes_df_)[1] <- "measure_p"
  # Purify the note pattern 
  notes_df_ <- as.data.frame(lapply(notes_df_, function(y) gsub("L|J|K", "", y)))
  notes_df_ <- as.data.frame(lapply(notes_df_, function(y) gsub("'", "", y)))
  notes_df_ <- as.data.frame(lapply(notes_df_, function(y) gsub("\\[|\\]|\\\\|\\/", "", y)))
  # Return the tidied dataframe
  return(notes_df_)
}
#========================================================#
.k2df_resolve_notes <- function(note_column, i){
  # Initialize the dataframe of note with resolved rhythm and note 
  resolved_note_df <- data.frame(pattern = note_column)
  # Get the rhythm value
  resolved_note_df$rhythm <- stringr::str_extract(note_column,"[0-9]{1,2}(\\.*)")
  # Get the note pattern strings
  notei <- stringr::str_extract(note_column,"[A-z]{1,2}.*")
  # Parse the note pattern strings for a name and value (ex. 'ee' = E, 9)
  parsed_notes <- do.call("rbind",lapply(notei, function(i){krn_note(i)}))
  # Add the columns to the dataframe 
  resolved_note_df$note_name <- parsed_notes[,1]
  resolved_note_df$note_value <- parsed_notes[,2]
  # Drop the pattern string column
  resolved_note_df <- resolved_note_df %>% select(!contains("pattern"))
  # Paste the note index onto the column names
  colnames(resolved_note_df) <- paste(colnames(resolved_note_df), i, sep="")
  # Return the tidy note with its resolved values of rhythm, note name, and note value
  resolved_note_df 
}



#========================================================#
#                       .KRN NOTE
#========================================================#
#### PREVIOUSLY CALLED: n.v_n.n #### 
## krn_note: Identifies the note and note value of a .krn note using the note_hashtable()
#'  
#' @param note .krn note pattern string 
#' @return note name and note value
#' 
# Input a krn note string
krn_note <- function(note){
  # Check edge cases first
  if(is.na(note)){ return(rep(NA, 2)) }
  # Iterate over our hash table to try and find a match
  for(i in 1:nrow(KRN_NOTES)){
    pattern <- as.character(KRN_NOTES$pattern[i])
    # Match found! Return note name and note value.
    if(stringr::str_detect(note, pattern)){ return(c(KRN_NOTES$label[i], KRN_NOTES$value[i])) }
  }
  if(stringr::str_detect(note, "r")){ return(c("rest", NA)) }
  else if(stringr::str_detect(note, "\\.")){ return(rep(".", 2)) }
  else{ return(c(note, NA)) } # If no edge cases or notes can be matched, catch corner cases by returning raw note with NA
}

#========================================================#
#                      RELATIVE KEY
#========================================================#
#' Figure out the relative key of a piece (previously called Major_minor)
#' 
#' @param piece A piece of .krn music
#' @import tidyverse
#' 
relative_key <- function(piece){
  # Get the single letter key name
  krn_key <- piece[1,1]
  if(stringr::str_detect(krn_key,"\\:")){
    krn_key <- gsub("\\*","",krn_key); krn_key <- gsub("\\:","",krn_key)
    # If the key is uppercase; it is a major key. Otherwise, minor.
    if(krn_key == toupper(krn_key)){ return(c(krn_key,"Major")) } else { return(c(krn_key,"minor")) }
  } 
  # If the key signature is a plain signature, use note frequencies
  else{
    # Get the major and relative minor keys
    major_key <- krn_key
    minor_key <- KRN_KEYS$relative_minor[which(KRN_KEYS$key == major_key)]
    # Select the note columns
    note_df <-  piece %>% select(contains("note"))
    # For both keys, get the tonic and fifth (degree 1 and 5)
    RELKEYS <- c(major_key, minor_key)
    tonics <- .getScaleDegreez(degree = 1, scale = RELKEYS)
    fifths <- .getScaleDegreez(degree = 5, scale = RELKEYS)
    # Count the frequency of the tonic shells 
    tonics_fifths_count <- rep(0,2)
    names(tonics_fifths_count) <- tonics
    # Iterate over each note column
    for(i in 1:ncol(note_df)){
      # Get the note frequency in the column
      curr_col <- table(note_df[,i])
      # Get the frequency of each key shell (root and fifth)
      mmtonic <- curr_col[tonics] %>% as.vector
      mmtonic[is.na(mmtonic)]<- 0
      mmfifth <- curr_col[fifths] %>% as.vector
      mmfifth[is.na(mmfifth)]<- 0
      tonics_fifths_count <- tonics_fifths_count + mmtonic + mmfifth
    }
    # Get the "votes" for each relative key
    Major <- tonics_fifths_count[1] %>% unname; minor <- tonics_fifths_count[2] %>% unname
    # Return the key with the more votes
    if(Major >= minor){ return(c(tonics[1],"Major")) } else { return(c(tonics[2],"minor")) }
  }
}

# Helper function for relative_key
.getScaleDegreez <- function(degree, scale){
  KRN_SCALES[degree,scale] %>% unlist() %>% unname() %>% as.character()
}


