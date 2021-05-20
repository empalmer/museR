
#============================================#
#               COMMON SCALES
#============================================#
# Given a tonic, returns the diatonic major scale
major_scale <- function(tonic){ diatonic_scale(tonic, "major") }
#============================================#
# Given a tonic, returns the diatonic minor scale
minor_scale <- function(tonic){ diatonic_scale(tonic, "minor") }
#============================================#
# Given a tonic and a diatonic mode, returns the corresponding diatonic scale
diatonic_scale <- function(tonic, mode){
  # Get the scale degrees from the DIATONIC_SCALES list
  scale_degrees <- DIATONIC_SCALES[[mode]]
  # Return the diatonic scale
  scale(tonic, scale_degrees)
}
#============================================#
# Given a tonic, returns the major blues scale
blues_scale <- function(tonic, quality){
  if(quality == "minor"){
    scale(tonic, OTHER_SCALES$minor_blues)
  }
  else if(quality == "major"){
    scale(tonic, OTHER_SCALES$major_blues)
  }
}

#============================================#
#                   SCALES
#============================================#
# Given a tonic and a vector of scale degrees, spells the scale and returns it
scale <- function(tonic, scale_degrees){
  # Get the note letter and incidental
  note_letter <- .dropIncidental(tonic)
  incidental <- .detectIncidental(tonic)
  # Get the white key scale (i.e. using just the note letter)
  base_scale <- .scaleBase(note_letter, scale_degrees)
  # If the incidental is a natural (white key), return the white key scale
  if(incidental == "natural"){ return(base_scale) }
  # Otherwise, apply the incidental over the base scale to return the correct scale
  purrr::map_chr(base_scale, .addIncidental, incidental)
}
#============================================#
# Given a white key, return the scale base prior to incidentalization
.scaleBase <- function(tonic, scale_degrees){
  c(tonic, purrr::map2_chr(rep(tonic,length(scale_degrees)), scale_degrees, .intervalNote))
}

#============================================#
#              SCALE ORDERING
#============================================#

# # Given an unordered scale, makes it a proper scale (for circle of fifths)
# .orderScale <- function(tonic, scale){
#   # Get parameters
#   num_notes <- length(scale)
#   # Make a sample tonic-ordered scale
#   tonic_scale <- MUS_ALPH[.ALPH_RANGE(note_index(tonic), 7)]
#   # Create a stripped version of the scale
#   stripped <- c()
#   for(i in 1:num_notes){
#     stripped[i] <- .dropIncidental(scale[i])
#   }
# }
#============================================#
# Ensure the scale degrees are enharmonically correct by finding the enharmonic of the first repeated letter
# .scaleEnharmonics <- function(scale){
#   for(i in 1:num_notes){
#     if()
#   }
# }

#============================================#
#              SCALE ARITHMETIC
#============================================#

`%STEPUP%` <- function(scale, note){
  idx <- note_index(note)
  scale[mod7(7 + idx) + 1]
}
#============================================#
`%STEPDOWN%` <- function(scale, note){
  idx <- note_index(note)
  idx <- mod7(-(8 - idx) - 1) + 1
  scale[idx]
}
#============================================#
`%STEP+%` <- function(note, steps){
  for(i in 1:steps){ note <- MUS_ALPH %STEPUP% note }
  note
}
#============================================#
`%STEP-%` <- function(note, steps){
  for(i in 1:steps){ note <- MUS_ALPH %STEPDOWN% note }
  note
}
