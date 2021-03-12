#' Title: NBA Fan Engagement Analysis
#' Purpose: Obtain term frequency and explore associations 
#' Name: Gabriela Rosero
#' email: grosero2019@student.hult.edu
#' Date: Jan 20 2021

#Setting directory---------------------------------------------------
setwd("~/Documents/HULT/Spring2021/Text Analytics/hult_NLP_student/cases/NBA Fan Engagement/data")


#loading libraries --------------------------------------------------
library(tm)
library(ggplot2)
library(ggthemes)
library(stringi)
library(qdap)
library(dplyr)
library(lubridate)
library(pbapply)
library(tokenizers)
library(ggdendro)
library(wordcloud)

#Options and Functions ----------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')


#Lower case function ------------------------------------------------
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

#Extra Cleaning function --------------------------------------------
basicSubs <- function(x){
  x <- gsub("[^\x01-\x7F]", "", x) #remove emojis
  x <- gsub('[^ [[:space:]]*', "", x) #remove extra spaces
  return(x)
}

#CleanCorpus function -----------------------------------------------
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  #corpus <- tm_map(corpus, content_transformer(basicSubs))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Bigram token maker -----------------------------------------------
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}


# Create custom stop words ------------------------------------------
stops <- c(stopwords('SMART'), 'nba', 'play','rt','game', 'basketball', 
           'end', 'sports', '%', 'amp', 'aaa','abc', '&', 'boston', 
           'houston', 'utah', 'miami', 'today','ago', 'team','day')


################################### DATASETS ##################################

txtFiles <- list.files(pattern = 'E_Feb2020|F_Mar2020|G_Apr2020.csv|H_May2020|I_June2020|J_July2020.csv|K_Aug2020')

##Read cvs individually function 
for (i in 1:length(txtFiles)){
  assign(txtFiles[i], read.csv(txtFiles[i]))
  cat(paste('read completed:',txtFiles[i],'\n'))
}

##Getting a sample from each dataset to reduce size
set.seed(123)

feb20.txt <- slice_sample(E_Feb2020.csv, prop = 0.05)
mar20.txt <- slice_sample(F_Mar2020.csv, prop = 0.05)
apr20.txt <- slice_sample(G_Apr2020.csv, prop = 0.05)
may20.txt <- slice_sample(H_May2020.csv, prop = 0.05)
jun20.txt <- slice_sample(I_June2020.csv, prop = 0.05)
jul20.txt <- slice_sample(J_July2020.csv, prop = 0.05)
aug20.txt <- slice_sample(K_Aug2020.csv, prop = 0.05)


############################ CLEANING DATASETS ############################

#As of tm version 0.7-3 tabular was deprecated 
names(feb20.txt)[1] <- 'doc_id'
names(mar20.txt)[1] <- 'doc_id'
names(apr20.txt)[1] <- 'doc_id'
names(may20.txt)[1] <- 'doc_id'
names(jun20.txt)[1] <- 'doc_id'
names(jul20.txt)[1] <- 'doc_id'
names(aug20.txt)[1] <- 'doc_id'


#Cleaning before cleancorpus ---------------------------------------
feb20.txt$text <- as.character(feb20.txt$text)
feb20.txt$text <- gsub("[^\x01-\x7F]", '', feb20.txt$text)

mar20.txt$text <- as.character(mar20.txt$text)
mar20.txt$text <- gsub("[^\x01-\x7F]", '', mar20.txt$text)

apr20.txt$text <- as.character(apr20.txt$text)
apr20.txt$text <- gsub("[^\x01-\x7F]", '', apr20.txt$text)

may20.txt$text <- as.character(may20.txt$text)
may20.txt$text <- gsub("[^\x01-\x7F]", '', may20.txt$text)

jun20.txt$text <- as.character(jun20.txt$text)
jun20.txt$text <- gsub("[^\x01-\x7F]", '', jun20.txt$text)

jul20.txt$text <- as.character(jul20.txt$text)
jul20.txt$text <- gsub("[^\x01-\x7F]", '', jul20.txt$text)

aug20.txt$text <- as.character(aug20.txt$text)
aug20.txt$text <- gsub("[^\x01-\x7F]", '', aug20.txt$text)


#Cleaning Data ------------------------------------------------------
txtCorpusfeb <- VCorpus(VectorSource(feb20.txt$text))
txtCorpusfeb <- cleanCorpus(txtCorpusfeb, stops)
txtCorpusfeb <- tm_map(txtCorpusfeb, stripWhitespace)
txtCorpusfeb.df <- as.data.frame(txtCorpusfeb)

txtCorpusMar <- VCorpus(VectorSource(mar20.txt$text))
txtCorpusMar <- cleanCorpus(txtCorpusMar, stops)
txtCorpusMar <- tm_map(txtCorpusMar, stripWhitespace)
txtCorpusMar.df <- as.data.frame(txtCorpusMar)

txtCorpusapr <- VCorpus(VectorSource(apr20.txt$text))
txtCorpusapr <- cleanCorpus(txtCorpusapr, stops)
txtCorpusapr <- tm_map(txtCorpusapr, stripWhitespace)
txtCorpusapr.df <- as.data.frame(txtCorpusapr)

txtCorpusmay <- VCorpus(VectorSource(may20.txt$text))
txtCorpusmay <- cleanCorpus(txtCorpusmay, stops)
txtCorpusmay <- tm_map(txtCorpusmay, stripWhitespace)
txtCorpusmay.df <- as.data.frame(txtCorpusmay)

txtCorpusjun <- VCorpus(VectorSource(jun20.txt$text))
txtCorpusjun <- cleanCorpus(txtCorpusjun, stops)
txtCorpusjun <- tm_map(txtCorpusjun, stripWhitespace)
txtCorpusjun.df <- as.data.frame(txtCorpusjun)

txtCorpusjul <- VCorpus(VectorSource(jul20.txt$text))
txtCorpusjul <- cleanCorpus(txtCorpusjul, stops)
txtCorpusjul <- tm_map(txtCorpusjul, stripWhitespace)
txtCorpusjul.df <- as.data.frame(txtCorpusjul)

txtCorpusaug <- VCorpus(VectorSource(aug20.txt$text))
txtCorpusaug <- cleanCorpus(txtCorpusaug, stops)
txtCorpusaug <- tm_map(txtCorpusaug, stripWhitespace)
txtCorpusaug.df <- as.data.frame(txtCorpusaug)

#Organizing Data ----------------------------------------------------
tweetTDMfeb  <- TermDocumentMatrix(txtCorpusfeb)
tweetTDMmfeb <- as.matrix(tweetTDMfeb)

tweetTDMMar  <- TermDocumentMatrix(txtCorpusMar)
tweetTDMmMar <- as.matrix(tweetTDMMar)

tweetTDMapr  <- TermDocumentMatrix(txtCorpusapr)
tweetTDMmapr <- as.matrix(tweetTDMapr)

tweetTDMmay  <- TermDocumentMatrix(txtCorpusmay)
tweetTDMmmay <- as.matrix(tweetTDMmay)

tweetTDMjun  <- TermDocumentMatrix(txtCorpusjun)
tweetTDMmjun <- as.matrix(tweetTDMjun)

tweetTDMjul  <- TermDocumentMatrix(txtCorpusjul)
tweetTDMmjul <- as.matrix(tweetTDMjul)

tweetTDMaug  <- TermDocumentMatrix(txtCorpusaug)
tweetTDMmaug <- as.matrix(tweetTDMaug)


# Frequency Data Frame ----------------------------------------------
tweetSumsfeb <- rowSums(tweetTDMmfeb)
tweetFreqfeb <- data.frame(word=names(tweetSumsfeb),frequency=tweetSumsfeb)

tweetSumsMar <- rowSums(tweetTDMmMar)
tweetFreqMar <- data.frame(word=names(tweetSumsMar),frequency=tweetSumsMar)

tweetSumsapr <- rowSums(tweetTDMmapr)
tweetFreqapr <- data.frame(word=names(tweetSumsapr),frequency=tweetSumsapr)

tweetSumsmay <- rowSums(tweetTDMmmay)
tweetFreqmay <- data.frame(word=names(tweetSumsmay),frequency=tweetSumsmay)

tweetSumsjun <- rowSums(tweetTDMmjun)
tweetFreqjun <- data.frame(word=names(tweetSumsjun),frequency=tweetSumsjun)

tweetSumsjul <- rowSums(tweetTDMmjul)
tweetFreqjul <- data.frame(word=names(tweetSumsjul),frequency=tweetSumsjul)

tweetSumsaug <- rowSums(tweetTDMmaug)
tweetFreqaug <- data.frame(word=names(tweetSumsaug),frequency=tweetSumsaug)


# Remove the row attributes meta family -----------------------------
rownames(tweetFreqfeb) <- NULL
tweetFreqfeb[50:55,]

rownames(tweetFreqMar) <- NULL
tweetFreqMar[50:55,]

rownames(tweetFreqapr) <- NULL
tweetFreqapr[50:55,]

rownames(tweetFreqmay) <- NULL
tweetFreqmay[50:55,]

rownames(tweetFreqjun) <- NULL
tweetFreqjun[50:55,]

rownames(tweetFreqjul) <- NULL
tweetFreqjul[50:55,]

rownames(tweetFreqaug) <- NULL
tweetFreqaug[50:55,]

######################## PLOTS AND ASSOCIATIONS BY MONTH########################

###### FEBRUARY =================================================

## Bar Chart -----------------------------------------------------------

# Simple barplot; values greater than 15 
topWordsfeb     <- subset(tweetFreqfeb, tweetFreqfeb$frequency >= 400) 
topWordsfeb     <- topWordsfeb[order(topWordsfeb$frequency, decreasing=F),]


# Chg to factor for ggplot 
topWordsfeb$word <- factor(topWordsfeb$word, 
                           levels=unique(as.character(topWordsfeb$word)))

##Plot most frequent words
ggplot(topWordsfeb, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

## String Count object --------------------------------------------------
williams <- sum(stri_count(txtCorpusfeb.df$text, fixed = 'williams'))
russell <- sum(stri_count(txtCorpusfeb.df$text, fixed = 'russell'))
james <- sum(stri_count(txtCorpusfeb.df$text, fixed = 'james'))
kobe <- sum(stri_count(txtCorpusfeb.df$text, fixed = 'kobe'))

##Organize text object on data frame 
peopleFreqfeb <- data.frame(terms = c('williams','russell','james','kobe'),
                            freq  = c(williams, russell, james, kobe))

#Ratio of frequency 
peopleFreqfeb <- peopleFreqfeb %>% 
  mutate(ratio = peopleFreqfeb$freq/nrow(txtCorpusfeb.df)) %>% 
  mutate(month = 'Feb')

ggplot(data = peopleFreqfeb, aes(x = reorder(terms, ratio), y = ratio,fill=ratio)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "done")


## Frequency Association -----------------------------------------------

# Inspect word associations
assoc.feb <- findAssocs(tweetTDMfeb, 'nike', 0.3)
assoc.feb

# Organize the word associations
assocDFfeb <- data.frame(terms=names(assoc.feb[[1]]),
                         value=unlist(assoc.feb))
assocDFfeb$terms <- factor(assocDFfeb$terms, levels=assocDFfeb$terms)
rownames(assocDFfeb) <- NULL
assocDFfeb

# Make a dot plot
ggplot(assocDFfeb, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDFfeb, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" 
            , size=3)


## Dendogram -----------------------------------------------------------

# Reduce TDM
reducedTDMfeb <- removeSparseTerms(tweetTDMfeb, sparse=0.975) #shoot for ~50 
#terms; 1.5% of cells in row have a value  
reducedTDMfeb

# Organize the smaller TDM
reducedTDMfeb <- as.data.frame(as.matrix(reducedTDMfeb))

# Cluster Dendogram
hcMar <- hclust(dist(reducedTDMfeb))
plot(hcMar,yaxt='n')


#Wordcloud -----------------------------------------------------------

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(tweetFreqfeb$word,
          tweetFreqfeb$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

###### MARCH =================================================
## Bar Chart -----------------------------------------------------------

# Simple barplot; values greater than 15 
topWordsMar     <- subset(tweetFreqMar, tweetFreqMar$frequency >= 600) 
topWordsMar     <- topWordsMar[order(topWordsMar$frequency, decreasing=F),]


# Chg to factor for ggplot 
topWordsMar$word <- factor(topWordsMar$word, 
                           levels=unique(as.character(topWordsMar$word)))

##Plot most frequent words
ggplot(topWordsMar, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

## String Count object --------------------------------------------------
williams.mar <- sum(stri_count(txtCorpusMar.df$text, fixed = 'williams'))
russell.mar <- sum(stri_count(txtCorpusMar.df$text, fixed = 'russell'))
james.mar <- sum(stri_count(txtCorpusMar.df$text, fixed = 'james'))
kobe.mar <- sum(stri_count(txtCorpusMar.df$text, fixed = 'kobe'))
shamscharania.mar <- sum(stri_count(txtCorpusMar.df$text, fixed = 'shamscharania'))

##Organize text object on data frame 
peopleFreqmar <- data.frame(terms = c('williams','russell','james','kobe','shamscharania'),
                            freq  = c(williams.mar, russell.mar, james.mar, kobe.mar, shamscharania.mar))

#Ratio of frequency 
peopleFreqmar <- peopleFreqmar %>% 
  mutate(ratio = peopleFreqmar$freq/nrow(txtCorpusfeb.df)) %>% 
  mutate(month = 'March')

ggplot(data = peopleFreqmar, aes(x = reorder(terms, ratio), y = ratio,fill=ratio)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "done")


## Frequency Association -----------------------------------------------

# Inspect word associations
assoc.mar <- findAssocs(tweetTDMMar, 'nike', 0.2)
assoc.mar

# Organize the word associations
assocDFmar <- data.frame(terms=names(assoc.mar[[1]]),
                         value=unlist(assoc.mar))
assocDFmar$terms <- factor(assocDFmar$terms, levels=assocDFmar$terms)
rownames(assocDFmar) <- NULL
assocDFmar

# Make a dot plot
ggplot(assocDFmar, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDFmar, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" 
            , size=3)


## Dendogram -----------------------------------------------------------

# Reduce TDM
reducedTDMmar <- removeSparseTerms(tweetTDMMar, sparse=0.975) #shoot for ~50 
#terms; 1.5% of cells in row have a value  
reducedTDMmar

# Organize the smaller TDM
reducedTDMmar <- as.data.frame(as.matrix(reducedTDMmar))

# Cluster Dendogram
hcMar <- hclust(dist(reducedTDMmar))
plot(hcMar,yaxt='n')


#Wordcloud -----------------------------------------------------------

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(tweetFreqMar$word,
          tweetFreqMar$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

###### APRIL =================================================
## Bar Chart -------------------------------------------------------------

# Simple barplot; values greater than 15 
topWordsapr     <- subset(tweetFreqapr, tweetFreqapr$frequency >= 600) 
topWordsapr     <- topWordsapr[order(topWordsapr$frequency, decreasing=F),]


# Chg to factor for ggplot 
topWordsapr$word <- factor(topWordsapr$word, 
                           levels=unique(as.character(topWordsapr$word)))

##Plot most frequent words
ggplot(topWordsapr, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

## Frequency Association -------------------------------------------------

# Inspect word associations
assoc.apr <- findAssocs(tweetTDMapr, 'nike', 0.3)
assoc.apr

# Organize the word associations
assocDFapr <- data.frame(terms=names(assoc.apr[[1]]),
                         value=unlist(assoc.apr))
assocDFapr$terms <- factor(assocDFapr$terms, levels=assocDFapr$terms)
rownames(assocDFapr) <- NULL
assocDFapr

# Make a dot plot
ggplot(assocDFapr, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDFapr, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" 
            , size=3)


## Dendogram -------------------------------------------------------------

# Reduce TDM
reducedTDMapr <- removeSparseTerms(tweetTDMapr, sparse=0.975) #shoot for ~50 
#terms; 1.5% of cells in row have a value  
reducedTDMapr

# Organize the smaller TDM
reducedTDMapr <- as.data.frame(as.matrix(reducedTDMapr))

# Cluster Dendogram
hcapr <- hclust(dist(reducedTDMapr))
plot(hcapr,yaxt='n')


#Wordcloud ----------------------------------------------------------------

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(tweetFreqapr$word,
          tweetFreqapr$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

###### MAY =================================================

## Bar Chart -------------------------------------------------------------

# Simple barplot; values greater than 15 
topWordsmay     <- subset(tweetFreqmay, tweetFreqmay$frequency >= 600) 
topWordsmay     <- topWordsmay[order(topWordsmay$frequency, decreasing=F),]


# Chg to factor for ggplot 
topWordsmay$word <- factor(topWordsmay$word, 
                           levels=unique(as.character(topWordsmay$word)))

##Plot most frequent words
ggplot(topWordsmay, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

## Frequency Association -----------------------------------------------

# Inspect word associations
assoc.may <- findAssocs(tweetTDMapr, 'nike', 0.3)
assoc.may

# Organize the word associations
assocDFmay <- data.frame(terms=names(assoc.may[[1]]),
                         value=unlist(assoc.may))
assocDFmay$terms <- factor(assocDFmay$terms, levels=assocDFmay$terms)
rownames(assocDFmay) <- NULL
assocDFmay

# Make a dot plot
ggplot(assocDFmay, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDFapr, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" 
            , size=3)


## Dendogram -----------------------------------------------------------

# Reduce TDM
reducedTDMmay <- removeSparseTerms(tweetTDMmay, sparse=0.975) #shoot for ~50 
#terms; 1.5% of cells in row have a value  
reducedTDMmay

# Organize the smaller TDM
reducedTDMmay <- as.data.frame(as.matrix(reducedTDMmay))

# Cluster Dendogram
hcmay <- hclust(dist(reducedTDMmay))
plot(hcmay,yaxt='n')


#Wordcloud -----------------------------------------------------------

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(tweetFreqmay$word,
          tweetFreqmay$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

###### JUN =================================================

## Bar Chart -------------------------------------------------------------

# Simple barplot; values greater than 15 
topWordsjun     <- subset(tweetFreqjun, tweetFreqjun$frequency >= 600) 
topWordsjun     <- topWordsjun[order(topWordsjun$frequency, decreasing=F),]


# Chg to factor for ggplot 
topWordsjun$word <- factor(topWordsjun$word, 
                           levels=unique(as.character(topWordsjun$word)))

##Plot most frequent words
ggplot(topWordsjun, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

## Frequency Association -----------------------------------------------

# Inspect word associations
assoc.jun <- findAssocs(tweetTDMjun, 'nike', 0.3)
assoc.jun

# Organize the word associations
assocDFjun <- data.frame(terms=names(assoc.jun[[1]]),
                         value=unlist(assoc.jun))
assocDFjun$terms <- factor(assocDFjun$terms, levels=assocDFjun$terms)
rownames(assocDFjun) <- NULL
assocDFjun

# Make a dot plot
ggplot(assocDFjun, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDFjun, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" 
            , size=3)


## Dendogram -----------------------------------------------------------

# Reduce TDM
reducedTDMjun <- removeSparseTerms(tweetTDMjun, sparse=0.975) #shoot for ~50 
#terms; 1.5% of cells in row have a value  
reducedTDMjun

# Organize the smaller TDM
reducedTDMjun <- as.data.frame(as.matrix(reducedTDMjun))

# Cluster Dendogram
hcjun <- hclust(dist(reducedTDMjun))
plot(hcjun,yaxt='n')


#Wordcloud -----------------------------------------------------------

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(tweetFreqjun$word,
          tweetFreqjun$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))


###### JUL =================================================

## Bar Chart -------------------------------------------------------------

# Simple barplot; values greater than 15 
topWordsjul     <- subset(tweetFreqjul, tweetFreqjul$frequency >= 600) 
topWordsjul     <- topWordsjul[order(topWordsjul$frequency, decreasing=F),]


# Chg to factor for ggplot 
topWordsjul$word <- factor(topWordsjul$word, 
                           levels=unique(as.character(topWordsjul$word)))

##Plot most frequent words
ggplot(topWordsjul, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

## Frequency Association -----------------------------------------------

# Inspect word associations
assoc.jul <- findAssocs(tweetTDMjul, 'nike', 0.3)
assoc.jul

# Organize the word associations
assocDFjul <- data.frame(terms=names(assoc.jul[[1]]),
                         value=unlist(assoc.jul))
assocDFjul$terms <- factor(assocDFjul$terms, levels=assocDFjul$terms)
rownames(assocDFjul) <- NULL
assocDFjul

# Make a dot plot
ggplot(assocDFjul, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDFjul, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" 
            , size=3)


## Dendogram -----------------------------------------------------------

# Reduce TDM
reducedTDMjul <- removeSparseTerms(tweetTDMjul, sparse=0.975) #shoot for ~50 
#terms; 1.5% of cells in row have a value  
reducedTDMjul

# Organize the smaller TDM
reducedTDMjul <- as.data.frame(as.matrix(reducedTDMjul))

# Cluster Dendogram
hcjul <- hclust(dist(reducedTDMjul))
plot(hcjul,yaxt='n')


#Wordcloud -----------------------------------------------------------

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(tweetFreqjul$word,
          tweetFreqjul$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))


###### AUG =================================================

## Bar Chart -------------------------------------------------------------

# Simple barplot; values greater than 15 
topWordsaug     <- subset(tweetFreqaug, tweetFreqaug$frequency >= 600) 
topWordsaug     <- topWordsaug[order(topWordsaug$frequency, decreasing=F),]


# Chg to factor for ggplot 
topWordsaug$word <- factor(topWordsaug$word, 
                           levels=unique(as.character(topWordsaug$word)))

##Plot most frequent words
ggplot(topWordsaug, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0)

## Frequency Association -----------------------------------------------

# Inspect word associations
assoc.aug <- findAssocs(tweetTDMaug, 'nike', 0.3)
assoc.aug

# Organize the word associations
assocDFaug <- data.frame(terms=names(assoc.aug[[1]]),
                         value=unlist(assoc.aug))
assocDFaug$terms <- factor(assocDFaug$terms, levels=assocDFaug$terms)
rownames(assocDFaug) <- NULL
assocDFaug

# Make a dot plot
ggplot(assocDFaug, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDFaug, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" 
            , size=3)


## Dendogram -----------------------------------------------------------

# Reduce TDM
reducedTDMaug <- removeSparseTerms(tweetTDMaug, sparse=0.975) #shoot for ~50 
#terms; 1.5% of cells in row have a value  
reducedTDMaug

# Organize the smaller TDM
reducedTDMaug <- as.data.frame(as.matrix(reducedTDMaug))

# Cluster Dendogram
hcjul <- hclust(dist(reducedTDMaug))
plot(hcjul,yaxt='n')


#Wordcloud -----------------------------------------------------------

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(tweetFreqaug$word,
          tweetFreqaug$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))


#=================================== ANALYSIS ==================================

######################## MOST FREQUENT TEAMS MENTIONED #########################

unique(feb20.txt$team)

teamFreqfeb <- tweetFreqfeb %>%
  group_by(word) %>% 
  filter(word %in% c('lakers','raptors','pistons','spurs','rockets','hawks','clippers', 'grizzlies','cavaliers','nets','kings','magic','bucks','celtics','thunder','warriors','pelicans','jazz','sixers','knicks','mavericks','suns','blazers','timberwolves','heat','nuggets','pacers','bulls','hornets','wizards')) %>% 
  summarise(freq = sum(frequency)) %>% 
  mutate(month = format(as.Date('2020-02-02'), "%b"))

teamFreqMar <- tweetFreqMar %>%
  group_by(word) %>% 
  filter(word %in% c('lakers','raptors','pistons','spurs','rockets','hawks','clippers', 'grizzlies','cavaliers','nets','kings','magic','bucks','celtics','thunder','warriors','pelicans','jazz','sixers','knicks','mavericks','suns','blazers','timberwolves','heat','nuggets','pacers','bulls','hornets','wizards')) %>% 
  summarise(freq = sum(frequency)) %>% 
  mutate(month = format(as.Date('2020-03-02'), "%b"))

teamFreqapr <- tweetFreqapr %>%
  group_by(word) %>% 
  filter(word %in% c('lakers','raptors','pistons','spurs','rockets','hawks','clippers', 'grizzlies','cavaliers','nets','kings','magic','bucks','celtics','thunder','warriors','pelicans','jazz','sixers','knicks','mavericks','suns','blazers','timberwolves','heat','nuggets','pacers','bulls','hornets','wizards')) %>% 
  summarise(freq = sum(frequency)) %>% 
  mutate(month = format(as.Date('2020-04-02'), "%b"))

teamFreqmay <- tweetFreqmay %>%
  group_by(word) %>% 
  filter(word %in% c('lakers','raptors','pistons','spurs','rockets','hawks','clippers', 'grizzlies','cavaliers','nets','kings','magic','bucks','celtics','thunder','warriors','pelicans','jazz','sixers','knicks','mavericks','suns','blazers','timberwolves','heat','nuggets','pacers','bulls','hornets','wizards')) %>% 
  summarise(freq = sum(frequency)) %>% 
  mutate(month = format(as.Date('2020-05-02'), "%b"))

teamFreqjun <- tweetFreqjun %>%
  group_by(word) %>% 
  filter(word %in% c('lakers','raptors','pistons','spurs','rockets','hawks','clippers', 'grizzlies','cavaliers','nets','kings','magic','bucks','celtics','thunder','warriors','pelicans','jazz','sixers','knicks','mavericks','suns','blazers','timberwolves','heat','nuggets','pacers','bulls','hornets','wizards')) %>% 
  summarise(freq = sum(frequency)) %>% 
  mutate(month = format(as.Date('2020-06-02'), "%b"))

teamFreqjul <- tweetFreqjul %>%
  group_by(word) %>% 
  filter(word %in% c('lakers','raptors','pistons','spurs','rockets','hawks','clippers', 'grizzlies','cavaliers','nets','kings','magic','bucks','celtics','thunder','warriors','pelicans','jazz','sixers','knicks','mavericks','suns','blazers','timberwolves','heat','nuggets','pacers','bulls','hornets','wizards')) %>% 
  summarise(freq = sum(frequency)) %>% 
  mutate(month = format(as.Date('2020-07-02'), "%b"))

teamFreqaug <- tweetFreqaug %>%
  group_by(word) %>% 
  filter(word %in% c('lakers','raptors','pistons','spurs','rockets','hawks','clippers', 'grizzlies','cavaliers','nets','kings','magic','bucks','celtics','thunder','warriors','pelicans','jazz','sixers','knicks','mavericks','suns','blazers','timberwolves','heat','nuggets','pacers','bulls','hornets','wizards')) %>% 
  summarise(freq = sum(frequency)) %>% 
  mutate(month = 'Aug')

#Combining all tweet frequencies in a data frame 

off.season.teams <- rbind(teamFreqfeb, teamFreqMar, teamFreqapr, teamFreqmay, teamFreqjun, teamFreqjul)

off.season.teams$month <-factor(off.season.teams$month, 
                                levels = c("Feb", "Mar", "Apr", "May", "Jun",
                                           "Jul"))

view.teams <- off.season.teams %>% 
  group_by(word, month) %>% 
  filter(word %in% c('lakers','celtics','heat','raptors')) %>% 
  summarise(freq = sum(freq))


ggplot(data = view.teams, aes(x = month, y = freq, colour = word, group = word)) +
  geom_line()+
  geom_point()

##April Most frequent words 

m.jordan <- sum(stri_count(txtCorpusapr.df$text, fixed = 'michael'))
lebron <- sum(stri_count(txtCorpusapr.df$text, fixed = 'lebron'))
kobe <- sum(stri_count(txtCorpusapr.df$text, fixed = 'kobe'))
hist <- sum(stri_count(txtCorpusapr.df$text, fixed = 'history'))
top <- sum(stri_count(txtCorpusapr.df$text, fixed = 'top'))

termsapr.freq <- data.frame(terms = c('michael','lebron','kobe','history', 'top'),
                            freq  = c(m.jordan,lebron,kobe,hist,top))

#Ratio of frequency 
termsapr.freq <- termsapr.freq %>% 
  mutate(ratio = termsapr.freq$freq/nrow(txtCorpusapr.df)) %>% 
  mutate(month = 'Apr')

ggplot(data = termsapr.freq, aes(x = reorder(terms, ratio), y = ratio,fill=ratio)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "done")



########################## NIKE ASSOCIATED WORDS ##############################

nike.assoc <- rbind(assocDFfeb, assocDFmar, assocDFapr, assocDFmay, assocDFjun,assocDFjul, assocDFaug)

nike.assoc.filter <- nike.assoc %>% 
  group_by(terms, value) %>% 
  filter(value > 0.35)

ggplot(nike.assoc.filter, aes(y=terms)) +
  geom_point(aes(x=value), data=nike.assoc.filter, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" 
            , size=5)

########################### CHANGE IN TWEET POSTING  ###########################

oct19.og <- read.csv('A_Oct2019.csv')
oct20.og <- read.csv('M_Oct2020.csv')

oct19 <- slice_sample(oct19.og, prop = 0.03)
apr20 <- apr20.txt
oct20 <- slice_sample(oct20.og, prop = 0.03)

#transmute and separate date and time
oct19.df <- oct19 %>% 
  transmute(created) %>% 
  mutate(time = format(as.POSIXct(oct19.df$created), format = "%H:%M:%S"), 
         date = as.POSIXct(oct19.df$created,format='%Y-%m-%d')) 

apr20.df <- apr20 %>% 
  transmute(created)%>% 
  mutate(time = format(as.POSIXct(apr20.df$created), format = "%H:%M:%S"), 
         date = as.POSIXct(apr20.df$created,format='%Y-%m-%d')) 

oct20.df <- oct20 %>% 
  transmute(created)%>% 
  mutate(time = format(as.POSIXct(oct20.df$created), format = "%H:%M:%S"), 
         date = as.POSIXct(oct20.df$created,format='%Y-%m-%d'))

##Removing "created" column which showed date and time together
oct19.df <- oct19.df[,-1]
apr20.df <- apr20.df[,-1]
oct20.df <- oct20.df[,-1]

## Convert to days of week
oct19.df <- oct19.df %>% 
  mutate(daysofweek = format(as.Date(date), "%A"))
oct19.df$daysofweek <-factor(oct19.df$daysofweek, 
                             levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                        "Saturday", "Sunday"))

apr20.df <- apr20.df %>% 
  mutate(daysofweek = format(as.Date(date), "%A"))
apr20.df$daysofweek <-factor(apr20.df$daysofweek, 
                             levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                        "Saturday", "Sunday"))


oct20.df <- oct20.df %>% 
  mutate(daysofweek = format(as.Date(date), "%A"))
oct20.df$daysofweek <-factor(oct20.df$daysofweek, 
                             levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                        "Saturday", "Sunday"))


##Bar Chart Plot
ggplot(oct19.df, aes(x = daysofweek))+
  geom_bar(width = 0.6, fill = "#517693")+
  ggtitle('Oct 2019')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(apr20.df, aes(x = daysofweek))+
  geom_bar(width = 0.6, fill ="#4F94CD")+
  ggtitle('Apr 2020')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(oct20.df, aes(x = daysofweek))+
  geom_bar(width = 0.6, fill = "#17408B")+
  ggtitle('Oct 2020')+
  theme(plot.title = element_text(hjust = 0.5))

########################## TRIED BUT FAILED #############################
##Comparison cloud -------------------------------------------------------
## Vector omit the data
oct19.str <- oct19.df %>% 
  group_by()

oct19.cl <- VCorpus(VectorSource(oct19.str$daysofweek))
apr20.cl <- VCorpus(VectorSource(apr20.df$daysofweek))
oct20.cl <- VCorpus(VectorSource(oct20.df$daysofweek))

##Colapse in one corpus
oct19.cl <- paste(oct19.cl, collapse = ' ')
apr20.cl <- paste(apr20.cl, collapse = ' ')
oct20.cl <- paste(oct20.cl, collapse = ' ')

##Combine subjects into a corpus of 3 documents
allmonths.cl <- c(oct19.cl, apr20.cl, oct20.cl)
allmonths.cl <- VCorpus((VectorSource(allmonths.cl)))

#make TDM
ctrl <- list(weighting = weightTfIdf)
allmonthTDM <- TermDocumentMatrix(allmonths.cl, control = ctrl)
allmonthTDMm <- as.matrix(allmonthTDM)

comparison.cloud(allmonthTDMm, 
                 max.words=40, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(allmonthTDMm),"Dark2"),
                 scale=c(3,0.1))

