# Problem Set 2 R code

library(knitr)
library(XML)
library(stringr)
library(testthat)

##1a
#save letters in text format
chars <- sample(letters, 1e6, replace=TRUE)
write.table(chars, file='tmp1.csv', row.names=FALSE, quote=FALSE,
            col.names=FALSE)
system('ls -l tmp1.csv', intern=TRUE)
chars <- paste(chars, collapse = '')
write.table(chars, file='tmp2.csv', row.names=FALSE, quote=FALSE,
            col.names=FALSE)
system('ls -l tmp2.csv', intern=TRUE)

#save in binary format
nums <- rnorm(1e6)
save(nums, file='tmp3.Rda')
system('ls -l tmp3.Rda', intern=TRUE)
#save in text format
write.table(nums, file='tmp4.csv', row.names=FALSE, quote=FALSE,
            col.names=FALSE, sep=', ')
system('ls -l tmp4.csv', intern=TRUE)
write.table(round(nums, 2), file='tmp5.csv', row.names=FALSE,
            quote=FALSE, col.names=FALSE, sep=', ')
system('ls -l tmp5.csv', intern=TRUE)

##1b
chars <- sample(letters, 1e6, replace=TRUE)
chars <- paste(chars, collapse='')
save(chars, file='tmp6-unc.Rda', compress=FALSE)
system('ls -l tmp6.Rda', intern=TRUE)
chars <- rep('a', 1e6)
chars <- paste(chars, collapse='')
save(chars, file='tmp7.Rda', intern=TRUE)
system('ls -l tmp7.Rda', intern=TRUE)
nums <- rnorm(1e6)
save(nums, file='tmp8-unc.Rda',compress=FALSE)
system('ls -l tmp8.Rda', intern=TRUE)

chars <- sample(letters, 1e6, replace=TRUE)
chars <- paste(chars, collapse='')
save(chars, file='tmp6.Rda')
system('ls -l tmp6.Rda', intern=TRUE)
chars <- rep('a', 1e6)
chars <- paste(chars, collapse='')
save(chars, file='tmp7.Rda')
system('ls -l tmp7.Rda', intern=TRUE)

##2a
getCitations <- function(firstName=NULL, lastName=NULL,
                           extended=FALSE){
    if(is.null(firstName) && is.null(lastName))
      stop("Please provide at least one name.")
    url <- paste0('https://scholar.google.com/scholar?q="',
                  firstName, '+',
                  lastName,
                  '"&btnG=&hl=en&as_sdt=0%2C5')
    researcherInfo <- htmlParse(readLines(url))
    ANodes <- getNodeSet(researcherInfo, "//a[@href]")
    num <- which(sapply(ANodes, function(x)
      str_detect(xmlValue(x), fixed('User profiles for'))))
    if(!length(num))
      stop("No profile found for that researcher")
    if(str_detect(xmlGetAttr(ANodes[[num+2]], 'href'),
                  fixed('citations?user=')))
      warning('Found more than one profile; using the first.')
    num <- num+1 ##next link should be for the researcher
    profileUrl <- xmlGetAttr(ANodes[[num]], 'href')
    str <- str_extract(profileUrl, "user=[A-Za-z0-9]+")
    scholarID <- str_split(str, "=")[[1]][2]
    url <- paste0('http://scholar.google.com/', profileUrl)
    if(extended) ##this solves the extra credit
      url <- paste0(url, 'cstart=0&pagesize=100')
    citations <- htmlParse(readLines(url))
    return(list(id=scholarID, citationsHTML=citations))
  }
result <- getCitations("Geoffrey", "Hinton")
getCitations("Mickey", "Mouse")

##2b
  processCitations <- function(input){
    if(!is.list(input) || names(input) != c('id', 'citationsHTML'))
      stop("'input' should be produced by getCitations")
    authorJournalHTML <- getNodeSet(input$citationsHTML,
                                    "//div[@class='gs_gray']")
    authorJournal <- matrix(sapply(authorJournalHTML, xmlValue),
                            ncol=2, byrow=TRUE)
    titleHTML <- getNodeSet(input$citationsHTML,
                            "//a[@class='gsc_a_at']")
    title <- sapply(titleHTML, xmlValue)
    yearHTML <- getNodeSet(input$citationsHTML,
                           "//td[@class='gsc_a_y']")
    year <- as.numeric(sapply(yearHTML, xmlValue))
    numCitationsHTML <- getNodeSet(input$citationsHTML,
                                   "//td[@class='gsc_a_c']")
    numCitations <- sapply(numCitationsHTML, xmlValue)
    numCitations <- as.numeric(str_replace(numCitations,
                                           "\\*", ""))
    citations <- data.frame(author=authorJournal[, 1],
                            journal=authorJournal[, 2],
                            title=title,
                            year=year,
                            numCitations=numCitations,
                            stringsAsFactors=FALSE)
    return(citations)
  }
citations <- processCitations(result)

##2c
test_that('test Geoffrey Hinton works',
            expect_true(is.list(getCitations('Geoffery', 'Hinton'))))
test_that('test Bugs Bunny fails',
          expect_error(getCitations('Bugs', 'Bunny')))
test_that('test Jordan warns',
          expect_warning(getCitations('Jordan'),
                         'Found more than one profile'))
zhaoCites <- processCitations(getCitations("Franklin", "Zhao"))
test_that('test Zhao data frame',
          expect_true(all(zhaoCites$year > 1970) &
                        sum(str_detect(zhaoCites$author, "Zhao")) > 15))