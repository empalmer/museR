#========================================================#
#                       CHORDS
#========================================================#
#' Extract_chord_name_beat
#' This function gives all the harmonic notes that are attacked
#' at the same time
#' @param piece A piece of music in R's piece_df
#' @return A list of all the row by voiced chords
extract_chord_name_beat <- function(piece){
  note_cols <- grep("_name\\d", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  chords <- list()
  note_df <- map_df(note_df,function(x){gsub("rest",NA,x)})
  x <- !map_df(note_df,is.na)
  for(i in 1:nrow(x)){
    chords[[i]] <- note_df[i,x[i,]] %>%unname() %>% as.character()
  }
  chords
}

#=========================================================
#=========================================================
#' extract_chord_value_beat
#' Extracts..
#' @param piece
#' @return list of all note value chords

extract_chord_value_beat <- function(piece, asd = TRUE){
  if(asd){piece <- assign_sd(piece)}
  note_cols <- grep("_value\\d", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  chords <- list()
  note_df <- map_df(note_df,function(x){gsub("rest",NA,x)})
  x <- !map_df(note_df,is.na)
  for(i in 1:nrow(x)){
    chords[[i]] <- note_df[i,x[i,]] %>%unname() %>% as.numeric()
  }
  return(chords)
}

#=========================================================
#=========================================================
#' one_chord_harms
#' Given a chord, this returns the intervals between chord notes.
#' @param chord A vector of chord notes
#' @return A vector of the intervals between chord notes
one_chord_harms <- function(chord){
  l <- length(chord)
  if(l < 2){return(chord)}
  intervals <- c()
  for(i in 2:l){
    intervals[i-1] <- (chord[i] - chord[i-1]) %% 12
  }
  intervals
}

#=========================================================
#' chord_harms
#' Given a piece, this returns a list of intervals between chord notes.
#' @param piece
#' @return A list of chords spelled out by interval
chord_harms <- function(piece){
  chords <- extract_chord_value_beat(piece)
  harm_form <- map(chords,one_chord_harms)
  harm_form
}

#=========================================================
#' harm_ints (intervals)
#'
#' @param piece
#' @return

harm_ints <- function(piece){
  chords <- chord_harms(piece)
  lengths <- map(chords, length) %>% unlist
  harms <- which(lengths == 1)
  twos <- chords[harms] %>% unlist
  ints <- c("unison","m2", "M2","m3", "M3","p4","tt",
            "p5", "m6","M6","m7","M7")
  int_freq <- data.frame(int_val = 0:11, freq = rep(0, 12))
  # Iterate over twos and obtain freq of each interval
  for(i in 1:12){
    count_i <- length(which(twos == int_freq$int_val[i]))
    freq_i <- count_i / length(twos)
    int_freq$freq[i] <- freq_i
  }
  int_freq
}


#=========================================================
#' freq_chord_size
#'
#' @param piece
#' @param type harmonic interval = 2, triad = 3, seventh = 4
#' @return
#'
freq_chord_size <- function(piece,type){
  chords <- chord_harms(piece)
  l <- length(chords)
  ls <- map(chords,length) %>% unlist
  two <- sum(ls ==type)
  freq <- two/l
  freq
}


#========================================================#
#                    SCALE DEGREE
#========================================================#
#' scale_degree_freq
#' @param piece
#' @return the frequency of occurance of all the scale degrees
#'
scale_degree_freq <- function(piece){
  note_cols <- grep("_name\\d", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  keyx <- relative_key(piece)[1]
  # KRN_SCALES is a global variable
  scale_degrees <- KRN_SCALES[,keyx] %>% as.vector()
  degrees_count <- rep(0,length(scale_degrees))
  tot <- 0
  for(i in 1:ncol(note_df)){
    a <- table(note_df[,1]) # is this really a better useage?
    tot <- tot + sum(a, na.rm = T)
    scale_deg <- a[scale_degrees] %>% as.vector
    scale_deg[is.na(scale_deg)]<- 0
    degrees_count <- degrees_count + scale_deg
  }
  freq <- degrees_count/tot
  data.frame(scale_degrees,degrees_count,freq)
}

#' scale_degree_freq
# (DEPRECATED BUT SEEMS TO QUELL WARNINGS FOR EXTRACT_CHORD_VALUE_BEAT)
#' @param piece
#' @return the scale degrees for each note
#'

assign_sd <- function(piece){
  key2 <- relative_key(piece)[1]
  scalez <- c("G#","A-","A","A#","B-","B","B#","C-","C","C#",
              "D-","D","D#","E-","E","E#","F-","F","F#","G-","G")
  scalez <- c(scalez,scalez)
  start <- min(which(scalez == key2))
  scale_deg_s <- scalez[start:(start+20)]
  scdv <- c(0,0,1,2,2,3,4,3,4,5,5,6,7,7,8,9,8,9,10,10,11) + 1
  scdv <- c(scdv,scdv+scdv[start])
  scdv_key <- scdv[start:(start+20)]
  scdv_key <- scdv_key - (scdv_key[1]-1)
  
  scale_deg_values <- rbind(scale_deg_s,scdv_key)
  scale_deg_values <- cbind(scale_deg_values,c(".",NA),c("rest",NA))
  colnames(scale_deg_values) <- c(scale_deg_s,".","rest")
  
  note_name_cols <- grep("_name\\d", colnames(piece),value = T)
  note_value_cols <- grep("_value\\d", colnames(piece),value = T)
  for(j in 1:length(note_name_cols)){
    for(i in 1:nrow(piece)){
      if(is.na(piece[i,note_name_cols[j]])){
        sd <- NA
      }else{
        note <- piece[i,note_name_cols[j]] %>% unname() %>% as.character()
        sd <- scale_deg_values[2,note]
      }
      piece[i,note_value_cols[j]] <- sd
      #print(sd)
    }
  }
  piece
}

