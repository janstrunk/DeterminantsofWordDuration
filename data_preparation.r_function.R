############################################################################################################################
###                                                                                                                      ###
### R functions required for the data preparation steps for the analyses presented in the paper:                         ###
###                                                                                                                      ###
### by Jan Strunk, Frank Seifart, ...                                                                                    ###
###                                                                                                                      ###
############################################################################################################################

## Most of these functions assume a data.frame as input that is obtained by reading in Toolbox files using Taras Zakharko's
## R library ToolboxSearch and subsequently converting the resulting corpus object into a data frame using as.data.frame(...)

## Function to count the number of morphs for each word token in a corpus data frame
count.morphemes.perword = function (corpus, morph_tier, compound_marker="-") {
  
  results = c()
  
  for (word_id in corpus$word.id) {
  
    cur_number_of_morphemes = 0
  
    for (morpheme in corpus[corpus$word.id==word_id, morph_tier]) {
    
      if (!is.na(morpheme)) {
    
        if (morpheme != compound_marker) {

          cur_number_of_morphemes = cur_number_of_morphemes + 1
        }
      }
    }

    results = c(results, cur_number_of_morphemes)
  }

  return(results)
}

## Function to determine each word token's position within the surrounding annotation unit (called record in Toolbox)
position.within.annotation.unit = function (corpus, ignore_filled_pauses=FALSE, ignore_false_starts=FALSE) {
  
  positions = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {
  
    position = 0

    for (word_id in unique(na.omit(corpus$word.id[corpus$record.id==record_id]))) {

      fldps = FALSE

      if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("fldps","fldps_new")) {
      
        if (ignore_filled_pauses==FALSE) {
        
          position = position + 1
          
        } else {
        
          fldps = TRUE

        }
      
      } else {
      
        if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("flst","flst_new")) {
      
          if (ignore_false_starts==FALSE) {
        
            position = position + 1
            
          } else {
        
            fldps = TRUE

          }
        
        } else {

          position = position + 1
        }
      }

      if (fldps == TRUE) {
      
        positions[corpus$record.id==record_id & corpus$word.id == word_id] = NA
        
      } else {
      
        positions[corpus$record.id==record_id & corpus$word.id == word_id] = position
        
      }
    }
  }
  
  return(positions)
}


## Function to determine each word token's position within the surrounding annotation unit (called record in Toolbox), couting from the last word
position.within.annotation.unit.from.last = function (corpus, ignore_filled_pauses=FALSE, ignore_false_starts=FALSE) {
  
  positions = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {

    position = 0
    max_position = 0

    for (word_id in unique(na.omit(corpus$word.id[corpus$record.id==record_id]))) {

      fldps = FALSE

      if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("fldps","fldps_new")) {
      
        if (ignore_filled_pauses==FALSE) {
        
          position = position + 1
          
        } else {
        
          fldps = TRUE
        
        }
      
      } else {
      
        if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("flst","flst_new")) {
      
          if (ignore_false_starts==FALSE) {
        
            position = position + 1
            
          } else {
        
            fldps = TRUE
        
          }
        
        } else {

          position = position + 1
        }
      }
      
      if (fldps == TRUE) {
      
        positions[corpus$record.id==record_id & corpus$word.id == word_id] = NA
        
      } else {
      
        positions[corpus$record.id==record_id & corpus$word.id == word_id] = position
        max_position = position
      }
    }
    
    for (word_id in unique(na.omit(corpus$word.id[corpus$record.id==record_id]))) {
    
      if (is.na(unique(positions[corpus$record.id==record_id & corpus$word.id == word_id]))) {
      
        positions[corpus$record.id==record_id & corpus$word.id == word_id] = NA
        
      } else {
    
        positions[corpus$record.id==record_id & corpus$word.id == word_id] = abs(unique(na.omit(positions[corpus$record.id==record_id & corpus$word.id == word_id])) - max_position) + 1
      }
    }
  }
  
  return(positions)
}

## Function to determine each word token's relative position within the surrounding annotation unit (called record in Toolbox)
## by normalizing its absolute by the number of word tokens in the annotation unit
position.within.annotation.unit.percentage = function (corpus, ignore_filled_pauses=FALSE, ignore_false_starts=FALSE) {
  
  positions = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {

    position = 0
    max_position = 0

    for (word_id in unique(na.omit(corpus$word.id[corpus$record.id==record_id]))) {

      fldps = FALSE

      if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("fldps","fldps_new")) {
      
        if (ignore_filled_pauses==FALSE) {
        
          position = position + 1
          
        } else {
        
          fldps = TRUE
        
        }
      
      } else {
      
        if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("flst","flst_new")) {
      
          if (ignore_false_starts==FALSE) {
        
            position = position + 1
            
          } else {
        
            fldps = TRUE
        
          }
        
        } else {

          position = position + 1
        }
      }
      
      if (fldps == TRUE) {
      
        positions[corpus$record.id==record_id & corpus$word.id == word_id] = NA
        
      } else {
      
        positions[corpus$record.id==record_id & corpus$word.id == word_id] = position
        max_position = position
      }
    }
    
    for (word_id in unique(na.omit(corpus$word.id[corpus$record.id==record_id]))) {
    
      if (is.na(unique(positions[corpus$record.id==record_id & corpus$word.id == word_id]))) {
      
        positions[corpus$record.id==record_id & corpus$word.id == word_id] = NA
        
      } else {
    
        positions[corpus$record.id==record_id & corpus$word.id == word_id] = (unique(na.omit(positions[corpus$record.id==record_id & corpus$word.id == word_id])) - 1) / (max_position - 1)
        
        if (max_position == 1) {
        
          positions[corpus$record.id==record_id & corpus$word.id == word_id] = 1
        }
      }
    }
  }
  
  return(positions)
}


## Function to count the number of words in an annotation unit (called record in Toolbox) in a corpus data frame
length.of.annotation.units.in.words = function (corpus, ignore_filled_pauses=FALSE, ignore_false_starts=FALSE) {

  lengths = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {

    length = length(unique(na.omit(corpus$word.id[corpus$record.id==record_id])))

    if (ignore_filled_pauses==T) {
    
      length = length - length(unique(na.omit(corpus$word.id[corpus$record.id==record_id & corpus$fldps %in% c("fldps","fldps_new")])))
    }

    if (ignore_false_starts==T) {
    
      length = length - length(unique(na.omit(corpus$word.id[corpus$record.id==record_id & corpus$fldps %in% c("flst","flst_new")])))
    }

    lengths[corpus$record.id == record_id] = length
  }
  
  return(lengths)
}


## Function to count the number of morphs in an annotation unit (called record in Toolbox) in a corpus data frame
length.of.annotation.units.in.morphemes = function (corpus, ignore_filled_pauses=FALSE, ignore_false_starts=FALSE) {

  lengths = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {

    length = length(unique(na.omit(corpus$morpheme.id[corpus$record.id==record_id])))

    if (ignore_filled_pauses==T) {
    
      length = length - length(unique(na.omit(corpus$morpheme.id[corpus$record.id==record_id & corpus$fldps %in% c("fldps","fldps_new")])))
    }

    if (ignore_false_starts==T) {
    
      length = length - length(unique(na.omit(corpus$morpheme.id[corpus$record.id==record_id & corpus$fldps %in% c("flst","flst_new")])))
    }

    lengths[corpus$record.id == record_id] = length
  }
  
  return(lengths)
}


## Function to normalize combinations of a UTF-8 character with a diacritic in Bora
normalize_bora = function(words) {

   result = c()
   
   for (cur_word in words) {
   
     cur_word = gsub("\U0268\U0301", "\U0268", cur_word)
     cur_word = gsub("\U0197\U0301", "\U0197", cur_word)
   
     result = c(result, cur_word)
   }
   
   return(result)
}


## Function to normalize combinations of a UTF-8 character with a diacritic in Chintang
normalize_chintang = function(words) {

   result = c()
   
   for (cur_word in words) {
   
     cur_word = gsub("a\U0303", "\U00E3", cur_word)
     cur_word = gsub("e\U0303", "\U1EBD", cur_word)
     cur_word = gsub("i\U0303", "\U0129", cur_word)
     cur_word = gsub("o\U0303", "\U00F5", cur_word)
     cur_word = gsub("u\U0303", "\U0169", cur_word)
     cur_word = gsub("A\U0303", "\U00C3", cur_word)
     cur_word = gsub("E\U0303", "\U1EBC", cur_word)
     cur_word = gsub("I\U0303", "\U0128", cur_word)
     cur_word = gsub("O\U0303", "\U00D5", cur_word)
     cur_word = gsub("U\U0303", "\U0168", cur_word)
     
     result = c(result, cur_word)
   }
   
   return(result)
}


## Function to normalize combinations of a UTF-8 character with a diacritic in Hoocąk
normalize_hoocak = function(words) {

   result = c()
   
   for (cur_word in words) {
   
     cur_word = gsub("a\U0328", "\U0105", cur_word)
     cur_word = gsub("e\U0328", "\U0119", cur_word)
     cur_word = gsub("i\U0328", "\U012F", cur_word)
     cur_word = gsub("o\U0328", "\U01EB", cur_word)
     cur_word = gsub("u\U0328", "\U0173", cur_word)
     cur_word = gsub("A\U0328", "\U0104", cur_word)
     cur_word = gsub("E\U0328", "\U0118", cur_word)
     cur_word = gsub("I\U0328", "\U012E", cur_word)
     cur_word = gsub("O\U0328", "\U01EA", cur_word)
     cur_word = gsub("U\U0328", "\U0172", cur_word)
     cur_word = gsub("g\U030C", "\U01E7", cur_word)
     cur_word = gsub("s\U030C", "\U0161", cur_word)
     cur_word = gsub("z\U030C", "\U017E", cur_word)
     cur_word = gsub("G\U030C", "\U01E6", cur_word)
     cur_word = gsub("S\U030C", "\U0160", cur_word)
     cur_word = gsub("Z\U030C", "\U017D", cur_word)
        
     result = c(result, cur_word)
   }
   
   return(result)
}

## YOHO!

## Function to delete ...
normalize_nuu = function(words) {

   result = c()
   
   for (cur_word in words) {
   
     cur_word = gsub("[", "", cur_word, fixed=T)
     cur_word = gsub("]", "", cur_word, fixed=T)
   
     result = c(result, cur_word)
   }
   
   return(result)
}

ignore_chars="[])-=,?!¿¡.:;\"\'([]"

count.characterlength.perword = function (corpus, text_tier, word_tier="word.id", ignore_chars="[])=,?!¿¡.*:;\"\'([-]", type="chars", normalize=FALSE) {
  
  length_results = c()

  for (cur_word in corpus[,text_tier]) {
  
    if (is.na(cur_word)) {
      
      length_results = c(length_results, as.integer(0))
      
    } else {

      if (is.function(normalize)) {
        cur_word = normalize(cur_word)
      }

      cur_word = gsub(ignore_chars, "", cur_word)
      
      cur_word_length = nchar(cur_word, type=type)      
      length_results = c(length_results, as.integer(cur_word_length))
    }
  }

  return(length_results)
}

length.of.annotation.units.in.characters = function (corpus, ignore_filled_pauses=FALSE, ignore_false_starts=FALSE) {

  lengths = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {
  
    word_length = 0
  
    for (word_id in unique(na.omit(corpus$word.id[corpus$record.id == record_id]))) {

      if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("fldps","fldps_new")) {
      
        if (ignore_filled_pauses==FALSE) {
        
          word_length = word_length + unique(corpus$chars_per_word[corpus$record.id==record_id & corpus$word.id == word_id])
        }
      
      } else {
      
        if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("flst","flst_new")) {
      
          if (ignore_false_starts==FALSE) {
        
            word_length = word_length + unique(corpus$chars_per_word[corpus$record.id==record_id & corpus$word.id == word_id])
          }
        
        } else {

          word_length = word_length + unique(corpus$chars_per_word[corpus$record.id==record_id & corpus$word.id == word_id])
        }
      }          
    }
    
    lengths[corpus$record.id == record_id] = word_length
  }
  
  return(lengths)
}

length.of.annotation.units.in.seconds = function (corpus, ignore_filled_pauses=FALSE, ignore_false_starts=FALSE) {

  lengths = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {
    
    if ((ignore_filled_pauses==TRUE) && (ignore_false_starts==TRUE)) {
    
      if (length(corpus$WordEnd[corpus$record.id==record_id & !corpus$fldps %in% c("fldps","fldps_new","flst","flst_new")]) == 0) {
      
        length = NA

      } else {
    
        length = max(corpus$WordEnd[corpus$record.id==record_id & !corpus$fldps %in% c("fldps","fldps_new","flst","flst_new")]) - min(corpus$WordBegin[corpus$record.id==record_id & !corpus$fldps %in% c("fldps","fldps_new","flst","flst_new")])
      
      }
      
    } else {
    
      if (ignore_filled_pauses==TRUE) {
      
        if (length(corpus$WordEnd[corpus$record.id==record_id & !corpus$fldps %in% c("fldps","fldps_new")]) == 0) {
        
          length = NA
        
        } else {
      
          length = max(corpus$WordEnd[corpus$record.id==record_id & !corpus$fldps %in% c("fldps","fldps_new")]) - min(corpus$WordBegin[corpus$record.id==record_id & !corpus$fldps %in% c("fldps","fldps_new")])

        }
        
      } else {
      
        if (ignore_false_starts==TRUE) {
        
          if (length(corpus$WordEnd[corpus$record.id==record_id & !corpus$fldps %in% c("flst","flst_new")]) == 0) {
          
            length = NA
          
          } else {
          
            length = max(corpus$WordEnd[corpus$record.id==record_id & !corpus$fldps %in% c("flst","flst_new")]) - min(corpus$WordBegin[corpus$record.id==record_id & !corpus$fldps %in% c("flst","flst_new")])

          }
          
        } else {
        
          length = max(corpus$WordEnd[corpus$record.id==record_id]) - min(corpus$WordBegin[corpus$record.id==record_id])
        }
      }
    }
      
    lengths[corpus$record.id == record_id] = length
  }
  
  return(lengths)
}

length.of.annotation.units.from.wordtimes = function (corpus, ignore_filled_pauses=FALSE, ignore_false_starts=FALSE) {

  lengths = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {

    word_lengths = 0
  
    for (word_id in unique(na.omit(corpus$word.id[corpus$record.id == record_id]))) {

      if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("fldps","fldps_new")) {
      
        if (ignore_filled_pauses==FALSE) {
        
          word_lengths = word_lengths + unique(corpus$WordLength[corpus$record.id==record_id & corpus$word.id == word_id])
        }
      
      } else {
      
        if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("flst","flst_new")) {
      
          if (ignore_false_starts==FALSE) {
        
            word_lengths = word_lengths + unique(corpus$WordLength[corpus$record.id==record_id & corpus$word.id == word_id])
          }
        
        } else {

          word_lengths = word_lengths + unique(corpus$WordLength[corpus$record.id==record_id & corpus$word.id == word_id])
        }
      }          
    }
    
    lengths[corpus$record.id == record_id] = word_lengths
  }
  
  return(lengths)
}

length.of.sessions.in.words = function (corpus) {

  lengths = corpus$morpheme.id
  
  for (ntvr_file in unique(corpus$ntvr_file)) {
  
    length = length(unique(na.omit(corpus$word.id[corpus$ntvr_file])))
    
    lengths[corpus$ntvr_file == ntvr_file] = length
  }
  
  return(lengths)
}

calculate.code.switching.per.annotation.unit = function (corpus, language_tier, language_code) {

  code_switchings = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {
  
    code_switching = length(corpus$morpheme.id[(corpus[,language_tier] %in% language_code & corpus$record.id == record_id)]) / length(corpus$morpheme.id[corpus$record.id == record_id])

    code_switchings[corpus$record.id == record_id] = code_switching
  }

  return(code_switchings)
}

calculate.pause.before.word = function (corpus) {
  
  pauses = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {

    first_word = min(corpus$word.id[corpus$record.id==record_id])
    
    preceding_word_end = NA
    
    for (word_id in unique(na.omit(corpus$word.id[corpus$record.id==record_id]))) {
    
      if (word_id != first_word) {
        
        current_word_begin = unique(corpus$WordBegin[corpus$record.id==record_id & corpus$word.id==word_id])
        
        pause_before_current_word = current_word_begin - preceding_word_end
        
        if (!is.na(pause_before_current_word)) {
        
            if (pause_before_current_word < 0) {
            
               print("Negative pause before word:")
               print(word_id)
            }
        }
        
        pauses[corpus$record.id==record_id & corpus$word.id == word_id] = pause_before_current_word
        
      } else {
      
        pauses[corpus$record.id==record_id & corpus$word.id == word_id] = NA
      }
      
      preceding_word_end = unique(corpus$WordEnd[corpus$record.id==record_id & corpus$word.id==word_id])
    }
  }
  
  return(pauses)
}

calculate.pause.after.word = function (corpus) {
  
  pauses = corpus$morpheme.id
  
  for (record_id in unique(corpus$record.id)) {

    last_word = max(corpus$word.id[corpus$record.id==record_id])
    
    following_word_begin = NA
    
    for (word_id in rev(unique(na.omit(corpus$word.id[corpus$record.id==record_id])))) {
    
      if (word_id != last_word) {
        
        current_word_end = unique(corpus$WordEnd[corpus$record.id==record_id & corpus$word.id==word_id])
        
        pause_after_current_word = following_word_begin - current_word_end
        
        if (!is.na(pause_after_current_word)) {
        
            if (pause_after_current_word < 0) {
            
               print("Negative pause after word:")
               print(word_id)
            }
        }
        
        pauses[corpus$record.id==record_id & corpus$word.id == word_id] = pause_after_current_word
        
      } else {
      
        pauses[corpus$record.id==record_id & corpus$word.id == word_id] = NA
      }
      
      following_word_begin = unique(corpus$WordBegin[corpus$record.id==record_id & corpus$word.id==word_id])
    }
  }
  
  return(pauses)
}

false.starts.per.record = function (corpus) {

  false_starts = corpus$word.id
  false_starts = 0
  
  for (record_id in unique(na.omit(corpus$record.id))) {
  
    seen_false_start = 0
    
    for (word_id in sort(unique(na.omit(corpus$word.id[corpus$record.id == record_id])))) {
      
      if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("flst","flst_new")) {
                   
        seen_false_start = seen_false_start + 1
      }
    }
            
    false_starts[corpus$record.id == record_id] = seen_false_start
  }
  
  return(false_starts)
}

filled.pauses.per.record = function (corpus) {

  filled_pauses = corpus$word.id
  filled_pauses = 0
  
  for (record_id in unique(na.omit(corpus$record.id))) {
  
    seen_filled_pauses = 0
    
    for (word_id in sort(unique(na.omit(corpus$word.id[corpus$record.id == record_id])))) {
      
      if (unique(na.omit(corpus$fldps[corpus$word.id==word_id])) %in% c("fldps","fldps_new")) {
                   
        seen_filled_pauses = seen_filled_pauses + 1
      }
    }
            
    filled_pauses[corpus$record.id == record_id] = seen_filled_pauses
  }
  
  return(filled_pauses)
}

# Function for calculating a dense ranking
dense_rank = function (x) {

  return(rank(unique(x))[match(x, unique(x))])
}

## Function to list objects with class, size, etc.
## taken from https://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(format(utils::object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

##

# Function to list all logs
list_logs = function () {
  ls(pattern=".parselog.",envir=globalenv())
}

# Function to save all logs
save_logs = function () {
  save(list=ls(pattern=".parselog.",envir=globalenv()),file=paste("backup/logs.",Sys.Date(),".RData",sep=""))
}

# Function to remove all logs from global environment
remove_logs = function () {
  rm(list=ls(pattern=".parselog.",envir=globalenv()),envir=globalenv())
}

# Function to list all models
list_models = function () {
  row.names(subset(lsos(n=10000),subset=Type %in% c("lmerMod","glmerMod","lm","glm","gam")))
}

# Function to save all logs
save_models = function () {
  save(list=row.names(subset(lsos(n=10000),subset=Type %in% c("lmerMod","glmerMod","lm","glm","gam"))),file=paste("backup/models.",Sys.Date(),".RData",sep=""))
}

# Function to remove all models from global environment
remove_models = function () {
  rm(list=row.names(subset(lsos(n=10000),subset=Type %in% c("lmerMod","glmerMod","lm","glm","gam"))),envir=globalenv())
}

# Function to list all ToolboxSearch format definitions
list_formats = function () {
  ls(pattern=".fmt",envir=globalenv())
}

# Function to save all ToolboxSearch format definitions
save_formats = function () {
  save(list=ls(pattern=".fmt",envir=globalenv()),file=paste("backup/formats.",Sys.Date(),".RData",sep=""))
}

# Function to remove all ToolboxSearch format definitions from global environment
remove_formats = function () {
  rm(list=ls(pattern=".fmt",envir=globalenv()),envir=globalenv())
}
