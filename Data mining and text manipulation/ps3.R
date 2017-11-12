library(stringr)
library(methods)
####problem 2a####
download.file('http://www.gutenberg.org/cache/epub/100/pg100.txt', destfile = 'shakes.txt')
#scan the file into characters and seperate by lines
text <- scan(file='shakes.txt', what = character(0), sep = "\n")
#the play always begin with a number of a year with 4 digits
play_start <- grep('^[[:digit:]]{4}', text)
#the plays always end with "THE END"
play_end <- grep('THE END', text)
num_plays <- length(play_start)
#the function that extract each play
extract_play <- function(i){
  return(text[play_start[i]:play_end[i]])
}
#exclude the first piece and last piece as required, and put the rest into a list
plays <- c(2:(num_plays-1))
plays <- lapply(plays, extract_play)

####problem 2b####
plays_num <- length(plays)
#creat an index based on problem 2a
play_index <- c(1:plays_num)

#this function is to find years, note that the year is the first line of each play
find_year <- function(i){
  return(as.integer(plays[[i]][[1]]))
}
play_year <- sapply(play_index, find_year)
#years output
play_year

#this function is to find titles, note that the title is the second line of each play
find_title <- function(i){
  return(as.character(plays[[i]][[2]]))
}
play_title <- sapply(play_index, find_title)
#titles output
play_title

#create a vector which will be used in the following function
index <- c()
#this function searches all of the acts and scenes of each play
index_search <- function(play) {
  #all acts (contains several scences) begin with the following 3 types
  index_lines <- grep("^ACT |SCENE |Scene", plays[[play]])
  index_num <- length(index_lines)
  index <- c(1:index_num)
  #a sub-function that extract the index lines of the acts and scenes
  index_index <- function(i){
    return(as.character(plays[[play]][[index_lines[i]]]))
  }
  index <- sapply(index, index_index)
  return(index)
}
#this function separates and counts the index lines of acts
find_act_num <- function(i){
  #to count acts, we can count scene 1s instead, because each act must has and only has one scene 1
  #we also have to exclude other scenes begin with 'scene 1' such as scene 10 and scene IV
  return(length(grep("(?i)SCENE 1[^(0-9)]\\.?|(?i)SCENE 1$|(?i)SCENE I[^(A-Z)]", index_search(i))))
}

#this function counts the index lines of scenes
find_scene_num <- function(i)
  #this makes sense because we made the index_search() function based on total scenes 
  return(length(index_search(i)))

#put the act numbers and scene numbers into vectors based on the plays
act_num <- sapply(play_index, find_act_num)
scene_num <- sapply(play_index, find_scene_num)

#scene numbers output
scene_num

#act numbers output
act_num

#this part deals with the bodies
body_start <- c()
body_end <- c()
#each body begins with scene: or scene.
body_start <- grep('SCENE(:|\\.)|Scene(:|\\.)', text)
body_end <- grep('THE END', text)
#this function extracts the bodys
find_body <- function(i){
  return(text[body_start[i]:body_end[i+1]])
}
#put the bodies into a list
play_body <- lapply(play_index, find_body)

####problem 2c####
#exclude the 4th play
plays[[4]] <- list()
#this function searches the beginning of each chunk including the corresponding speaker
find_chunk <- function(i){
  #each chunk begins with the speaker and indents 2 spaces
  #the speaker's names are all capital letters in most plays
  #there is only one captial letter (first letter) in the names in some plays
  #some names have spaces (maybe first and last names together)
  #all names end with a dot
  chunks <- grep('^[[:space:]]{2}[[:upper:]]{1,10}[ ]?[[:alpha:]]{1,15}\\.', plays[[i]])
  return(chunks)
}
#put all chunk beginnings into a vector 
chunks <- sapply(play_index, find_chunk)
chunks_length <- as.integer(sapply(chunks, length))
#this function returns the chunks of each play
make_chunks <- function(i){
  #exclude the 4th play
  if(i == 4)
    return(0)
  else
    #this sub-function extracts the chunks
    make_chunks_sub <- function (j){
      #use 2 adjacent chunk beginnings to extract each chunk 
      chunk_temp <- plays[[i]][chunks[[i]][[j-1]]:(chunks[[i]][[j]]-1)]
      return(chunk_temp)
    }
    #beginning with 2 because I used (j-1) in the make_chunks_sub() function
    chunk_length_temp <- c(2:chunks_length[i])
    chunk_all <- sapply(chunk_length_temp, make_chunks_sub)
    return(chunk_all)
}
#put all chunks into a list 
play_chunk <- lapply(play_index, make_chunks)


#an attempt
#find_chunk_end <- grep('^[[:space:]]{4}([[:space:]]|[[:graph:]])*\n[[:graph:]]|[[:space:]]{1,3}|[[:space:]]{4,}$', plays[[i]])

####problem 2d####
#this function counts the unique speakers
unique_speaker <- function(i){
  if(i == 4)
    return(0)
  else
    #search the speaker's name using the same method in problem 2c
    speakers <- gregexpr('^[[:space:]]{2}[[:upper:]]{1,10}[ ]?[[:alpha:]]{1,15}\\.', as.character(make_chunks(i)))
    #extract the matched names
    speaker_name <- regmatches(as.character(make_chunks(i)), speakers)
    #find unique names
    unique_name <- unique(speaker_name)
    return(length(unique_name))
}
#put the speaker numbers into a vector
play_unique_speaker <- sapply(play_index, unique_speaker)

#this function counts the spoken chunks
spoken_chunks <- function(i){
  if(i == 4)
    return(0)
  else
    return(length(make_chunks(i)))
}
#put the chunk numbers into a list
play_chunks <- sapply(play_index, spoken_chunks)

#this function counts the sentences of each play
sentence <- function(i){
  if(i == 4)
    return(0)
  else
    #use periods to detect sentences
    periods <- sum(sapply(gregexpr('\\.', make_chunks(i)), length))
    return(periods)
}
#put sentence numbers into a vector
play_sentence <- sapply(play_index, sentence)

#this function counts the words
word <- function(i){
  if(i == 4)
    return(0)
  else
    #use "\\w+" to detect words
    word_temp <- sum(sapply(gregexpr('\\w+', make_chunks(i)), length))
  return(word_temp)
}
#put the word numbers into a vector
play_word <- sapply(play_index, word)

#this function counts the average number of words per chunk
average_word <- function(i){
  if(i == 4)
    return(0)
  else
    #the number of words divided by the number of chunks of each play
    return(word(i) / chunks_length[i])
}
#put the average numbers of words per chunk into a vector
play_average_word <- sapply(play_index, average_word)

#this function counts the unique words
unique_word <- function(i){
  if(i == 4)
    return(0)
  else
    #search the words
    words <- gregexpr('\\w+', as.character(make_chunks(i)))
    #extract the matched words
    match_words <- regmatches(as.character(make_chunks(i)), words)
    #find unique words
    unique_words <- unique(match_words)
    return(length(unique_words))
}
#put the unique word numbers into a vector
play_unique_word <- sapply(play_index, unique_word)

####problem 2e####
#put the summarized statistics into a data frame
results <- data.frame(play_year, play_title, act_num, scene_num, play_unique_speaker, play_chunks, play_sentence, play_word, play_average_word, play_unique_word)
#output a few lines of the data frame, note that '0's are in the 4th play because I exclued it
head(results)
#plot act numbers, scene numbers, unique speaker numbers, and chunk numbers
plot(results$play_year, results$act_num, xlab='Year', ylab='Act numbers')
plot(results$play_year, results$scene_num, xlab='Year', ylab='Scene numbers')
plot(results$play_year, results$play_unique_speaker, xlab='Year', ylab='Unique speaker numbers')
plot(results$play_year, results$play_chunks, xlab='Year', ylab='Chunk numbers')

#Using S4 approach, suppose that we define a new class called $plays$, 
#the fields of the class $plays$ are $year$, $title$, and $body$.
####problem 3a####
#define a class called "plays" which contains the following slots
setClass("plays",
         representation(
           year = "numeric",
           title = "character",
           act = "character",
           scene = "character",
           body = "list"
         )
)
#then set the validity of each slots
setValidity("plays",
            function(object){
              #William Shakespeara (1564-1616)
              if(!(object@year %in% 1564:1616))
                return("error: not a valid year")
              if(length(grep('[^a-zA-Z ]', object@title)))
                return("error: title contains non-characters")
              if(!length(grep('^ACT ', object@act)))
                return("error: not a valid act")
              if(!length(grep('(?i)SCENE 1[^(0-9)]\\.?|(?i)SCENE 1$|(?i)SCENE I[^(A-Z)]', 
                              object@scene)))
                return("error: not a valid scene")
              if(!length(grep('SCENE(:|\\.)|Scene(:|\\.)', object@body)))
                return("error: not a valid body")
              return(TRUE)
            }
)
#make an object as an example
a_play_act4 <- new('plays', year=1601, title="AS YOU LIKE IT", act="ACT IV.", 
              scene=c("SCENE I.", "SCENE II.", "SCENE III"), 
              body=list("SCENE: OLIVER'S house..."))
a_play_act4

####problem 3b####
#example 1: validate the year
setGeneric("isYear", function(object){
  standardGeneric("isYear")
})
#check whether the year is valid
isYear.plays <- function(object){
  if(object@year %in% 1564:1616)
    cat(object@title, "is of valid year")
  else
    cat(object@title, "is not of valid year")
}
setMethod(isYear, signature=c("plays"), definition=isYear.plays)
#example output
isYear(a_play_act4)
#if we change the number:
a_play_act4@year <- 99999
isYear(a_play_act4)

#example 2: provide the scene number information for an act of a play
setGeneric("scenenum", function(object){
  standardGeneric("scenenum")
})
scenenum.plays <- function(object){
  cat(object@title, object@act, "has", length(object@scene), "scenes")
}
setMethod(scenenum, signature=c("plays"), definition=scenenum.plays)
#example output
scenenum(a_play_act4)


