##########Problem Set 6###########
library(RSQLite)

####Problem 2####

#connect to the database
drv <- dbDriver("SQLite")
dir <- "~/Desktop"
dbFilename <- 'stackoverflow-2016.db'
db <- dbConnect(drv, dbname = file.path(dir, dbFilename))

#check tables in the database
dbListTables(db)

#check the columns of the related tables
dbListFields(db, 'questions')
dbListFields(db, 'questions_tags')

#extract rows with unique users who have asked only R-related questions and no Python-related questions
uni_user <- dbGetQuery(db,"SELECT DISTINCT ownerid FROM(
                       SELECT questions.ownerid, questions_tags.tag FROM questions 
                       JOIN questions_tags 
                       ON questions.questionid = questions_tags.questionid) 
                       WHERE tag = 'r' EXCEPT SELECT DISTINCT ownerid FROM(
                       SELECT questions.ownerid, questions_tags.tag FROM questions 
                       JOIN questions_tags 
                       ON questions.questionid = questions_tags.questionid) 
                       WHERE tag = 'python'")

#count the number of the extracted rows, which is the number of the unique users 
length(unlist(uni_user))

####Problem 4####
#this part should be submitted to Savio 
library(parallel)
library(doParallel)
library(foreach)
library(stringr)
library(readr)
library(dplyr)

#access the number of the available cores
nCores <- as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))
registerDoParallel(nCores)

#code on a quarter of the files
n <- 240

#filter the rows and collect the results into a single data frame
result <- foreach(i = 1:n,
                  .combine = rbind,
                  .verbose = TRUE) %dopar% {
                    #get the file names
                    data <- paste(
                      '/global/scratch/paciorek/wikistats_full/dated_for_R/part-00', 
                      str_pad(i, 3, pad=0), sep='')
                    #read the file
                    tmp <- readr::read_delim(data, ' ', quote='', col_names=FALSE)
                    #filter the rows 
                    output <- tmp %>% dplyr::filter(
                      stringr::str_detect(tmp$X4,'Barack_Obama'))
                    output
                  }

#print part of the result
#since the screen can only display 3 columns, I chose to display the 4th column
#instead of the 3rd because the 4th column contains "Barack Obama" for better view
print(result[1:5,c(1,2,4)])
  
           
          


