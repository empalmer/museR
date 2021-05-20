
#============================================#
#               SCALE DEGREE
#============================================#

# Generalize the scale degree function for non-white keys; assume the tonic has a simple incidental
scaleDegree <- function(tonic, note){
  # Get tonic incidental
  tonic_incidental <- .detectIncidental(tonic)
  tonic_letter <- .dropIncidental(tonic)
  # If the note is itself, return tonic scale degree
  if(tonic == note){ return("1") }
  # If the tonic is a white key, then just call that function
  if(tonic_incidental == "natural") { return(.scaleDegreeWK(tonic, note)) }
  # Try to find a white enharmonic first
  if(tonic_incidental == "flat" && !(tonic_letter %in% HAS_FLATS)){
    # Get the white key enharmonic
    tonic <- (MUS_ALPH %STEPDOWN% tonic_letter)
    # Try to preserve the scale degree (doesn't quite do the trick; misses white keys...)
    if(.isBlackNote(note)){ 
      note <- .cycle_enharmonic(note)
    } else{
      note <- enharmonic(note, above = FALSE)
    }
    # Find the scale degree using the white key enharmonic
    return(.scaleDegreeWK(tonic, note))
  }
  # Repeat for the sharp white enharmonics
  if(tonic_incidental == "sharp" && !(tonic_letter %in% HAS_SHARPS)){
    # Get the white key enharmonic
    tonic <- (MUS_ALPH %STEPUP% tonic_letter)
    # Try to preserve the scale degree (doesn't quite do the trick; misses white keys...)
    if(.isBlackNote(note)){ 
      note <- .cycle_enharmonic(note)
    } else{
      note <- enharmonic(note, above = TRUE)
    }
    return(.scaleDegreeWK(tonic, note))
  }
  # Otherwise, flatten sharp keys and sharpen flat keys
  else if(tonic_incidental == "flat"){
    tonic <- tonic %INC% "#"; note <- note %INC% "#"
    return(.scaleDegreeWK(tonic, note)) 
  }
  else if(tonic_incidental == "sharp"){
    tonic <- tonic %INC% "b"; note <- note %INC% "b"
    return(.scaleDegreeWK(tonic, note)) 
  }
  NA
}
#=======================================================#
# Given a note and a white key tonic, find the scale degree the note is to the tonic (inverse of build interval)
.scaleDegreeWK <- function(tonic, note){
  # Get a base scale of the tonic (to obtain letters for each degree)
  base_scale <- .baseScale(tonic)
  # Get the note letter and note incidental
  note_letter <- .dropIncidental(note)
  note_inc <- .detectIncidental(note)
  # Handle improperly written steps; otherwise, the note letter is certainly not the same as the tonic
  if(note_letter == tonic){ return(.improperStepDegree(tonic, note)) }
  # Using the note letter only, get the (unaltered) scale degree and the # of semitones
  deg_unaltered <- which(base_scale == note_letter)
  # Get the number of semitones to the unaltered degree (white key semitones)
  semitones_unalt <- .WKsemitones(tonic, deg_unaltered)
  # Get the number of semitones between the unaltered note and the altered note
  semitone_alt_diff <- .semitoneDifference(note_letter, note)
  # Get the total semitone distance between the tonic and the observed note
  semitone_total <- semitone_alt_diff + semitones_unalt
  # Compare the semitones from the unaltered degree to the found degree
  deg_semitones <- SCALE_DEGREES[[as.character(deg_unaltered)]]
  # From the observed semitones and pure degree semitones, get the degree alteration by the semitone difference
  deg_alt_semitones <- semitone_total - deg_semitones
  # Find the appopriate degree alteration incidental
  deg_incidental <- .incidentalSemitones(deg_alt_semitones)
  # Return the scale degree
  paste(deg_incidental, deg_unaltered, sep = "")
}
#=======================================================#
# Given a tonic and a note of the same letter, find the '2nd' scale degree quality (improper half step or whole step)
.improperStepDegree <- function(tonic, note){
  # Get the number of semitones between the tonic and altered tonic
  semitones <- .semitoneDifference(tonic, note)
  #print(semitones)
  if(length(semitones) == 0){ return(NA) } # Quell numeric(0) bug
  # Depending on number of semitones, return the scale degree
  if(abs(semitones) == 1){ # (Half Step)
    # Based on whether the note is below or above the tonic, return the appopriate degree
    if(semitones > 0){ return("b2") } else { return("7") }
  }
  if(abs(semitones) == 2){ # (Whole Step)
    # Based on whether the note is below or above the tonic, return the appopriate degree
    if(semitones > 0){ return("2") } else { return("b7") }
  }
  NA
}

#============================================#
#        SCALE DEGREE HELPER FUNCTIONS
#============================================#

.incToSymbol <- function(incidental){
  symbol <- INC_HASH$symbol[which(INC_HASH$incidental == incidental)[1]]
  if(is.na(symbol)){ symbol <- "" }
  symbol
}
#=======================================================#
.semitoneDifference <- function(note, altered_note){
  # Get the incidentals of both notes
  note_incidental <- .incToSymbol(.detectIncidental(note))
  altr_incidental <- .incToSymbol(.detectIncidental(altered_note))
  # Get the semitone equivalent of each incidental
  inc0_semitones <- .incidentalSemitones(note_incidental, F) 
  inc1_semitones <- .incidentalSemitones(altr_incidental, F)
  # Return semitone difference
  inc1_semitones - inc0_semitones
}
#=======================================================#
.baseScale <- function(tonic){
  MUS_ALPH[.ALPH_RANGE(note_index(tonic), length = 7)]
}
