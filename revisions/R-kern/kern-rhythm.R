#========================================================#
#                   RYHTHM FREQUENCY
#========================================================#
#' rythem freq
#'
#' @param piece
#' @return Vector of frequencies for all rhythmic values
#'
rhythm_freq <- function(piece){
  # Declare "global" variable
  RHYTHMS <- c("2","2.","4","4.","8","8.","16","16.","32")
  # Create empty rhythm count vector
  tot <- rep(0,10)
  names(tot) <- RHYTHMS
  # Get the rhythm columns
  rhy_df <- piece %>% select(contains("rhythm"))
  # Collapse all columns into single vector
  rhy <- c()
  for(i in seq_along(rhy_df)){ rhy <- c(rhy, rhy_df[[i]]) }
  rhy <- rhy[-which(is.na(rhy))] # drop NAs
  # Count rhythms
  freqs <- table(rhy)/length(rhy)
  freqs
}

#==============================================================
#' dot freq
#'
#' @param piece
#' @return Frequency of dotted rhythms in the piece
#'
dotted_freq <- function(piece){
  # Declare dotted rhythms
  DOTTED_RHYTHMS <- RHYTHMS[which(str_detect(RHYTHMS, pattern = "\\.{1}"))]
  # Subset frequency table of dotted rhythms
  freqs <- top_rhythm_freq(piece)
  dotted_freqs <- freqs[names(freqs) %in% DOTTED_RHYTHMS]
  # Return freq table of dotted rhythms
  dotted_freqs
}

#========================================================#
#                         DENSITY
#========================================================#
#' beat_density
#'
#' @param piece in raw data frame
#' @return density for each measure
#'

beat_density <- function(piece){
  note_cols <- grep("measure|_name\\d", colnames(piece) ,value = T)
  notes <- piece[,note_cols]
  note_df <- notes[,-1]
  density <- vector()
  for(i in 1:nrow(note_df)){
    density[i] <- sum(!is.na(note_df[i,]))
  }
  density <- density[which(density != 0)]
  c(mean(density),sd(density))
}


#========================================================#
#                     RHYTHM ENTROPY
#========================================================#
#' rhythm entropy
#'
#' @param piece
#' @return Time signature of the piece
#'
rhythm_entropy <- function(piece){
  r_cols <- measure_col <- grep("rhythm\\d", colnames(piece),value = T)
  r_df <-  piece[,r_cols]
  changes <- 0
  for(j in 1:ncol(r_df)){
    rv <- as.numeric(as.vector(na.omit(r_df[,j])))
    changes_j <- 0
    for(i in 2:length(rv)){
      c <- rv[i]-rv[i-1]
      changes_j[i-1] <-ifelse(c ==0,0,1)
    }
    changes[j] <- mean(changes_j)
  }
  changes <- mean(changes,na.rm = T)
  changes
}
