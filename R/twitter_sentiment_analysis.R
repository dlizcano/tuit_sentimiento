
# Guide videos here http://txcdk.unt.edu/iralab/how
#############################################
# Chunk - 1 - Authenticate with twitter API
#############################################

library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
library(tm)

## Windows users need to get this file
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "XXXXXXXXXXXXXXXXXXX" 
consumerSecret = "XXXXXXXXXXXXXXXXXXXXX"
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL, 
                         authURL=authURL)
Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl") )
6235667
save(Cred, file="twitter authentication.Rdata")
registerTwitterOAuth(Cred)

## Future use

load("twitter authentication.Rdata")
registerTwitterOAuth(Cred)

############################################################
# Chunk  - 2 - Twitter Scrape  #Catatumbo #SanTurban #Cucuta  
############################################################

Catatumbo.list <- searchTwitter('Catatumbo', n=1000, cainfo="cacert.pem")  
Catatumbo.df = twListToDF(Catatumbo.list)  
write.csv(Catatumbo.df, file='C:/temp/CatatumboTweets.csv', row.names=F)

SanTurban.list <- searchTwitter('Santurban', n=1000, cainfo="cacert.pem", since='2010-03-01')  
SanTurban.df = twListToDF(SanTurban.list)  
write.csv(SanTurban.df, file='C:/temp/SanTurbanTweets.csv', row.names=F)

Cucuta.list <- searchTwitter('Cucuta', n=1000, cainfo="cacert.pem")  
Cucuta.df = twListToDF(Cucuta.list)  
write.csv(Cucuta.df, file='C:/temp/CucutaTweets.csv', row.names=F)


###############################
#Chunk -3- Sentiment Function     
###############################

library (plyr)
library (stringr)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')  
{  
  require(plyr)  
  require(stringr)         
  # we got a vector of sentences. plyr will handle a list  
  # or a vector as an "l" for us  
  # we want a simple array ("a") of scores back, so we use   
  # "l" + "a" + "ply" = "laply":  
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {     
    # clean up sentences with R's regex-driven global substitute, gsub():     
    sentence = gsub('[[:punct:]]', '', sentence)     
    sentence = gsub('[[:cntrl:]]', '', sentence)     
    sentence = gsub('\\d+', '', sentence)     
    # and convert to lower case:     
    sentence = tolower(sentence)     
    # split into words. str_split is in the stringr package     
    word.list = str_split(sentence, '\\s+')     
    # sometimes a list() is one level of hierarchy too much     
    words = unlist(word.list)     
    # compare our words to the dictionaries of positive & negative terms     
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)     
    # match() returns the position of the matched term or NA  
    # we just want a TRUE/FALSE:     
    pos.matches = !is.na(pos.matches)     
    neg.matches = !is.na(neg.matches)      
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():    
    score = sum(pos.matches) - sum(neg.matches)  
    return(score)  
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
} 


############################################
#Chunk - 4 - Scoring Tweets & Adding a column      
############################################

# =======================
#   4. Citation Info for lexicon en espaniol
# 
# If you use these lexicons please cite:
#   
#   @InProceedings{Perez12,
#                  author =       {Veronica Perez Rosas , Carmen Banea, Rada Mihalcea},
#                  title =            {Learning Sentiment Lexicons in Spanish},
#                  booktitle =    {Proceedings of the international conference on Language
#                                  Resources and Evaluation (LREC)},
#                  address =      {Istanbul, Turkey},
#                  year =            {2012}
#   }

# load lexicom
sentimentcomb<-read.csv("data\\lexicon\\subjectivity.csv",header=FALSE,sep=",")
positive<-t(subset(sentimentcomb,V3=="positive",select=V1))

negative<-t(subset(sentimentcomb,V3=="negative",select=V1))

# Add colombian words to list if you want more
pos.words = c(positive, 'upgrade',"bueno","bien","chereve","amplia","ampliacion")
neg.words = c(negative, 'wtf', 'wait','waiting', 'epicfail', 'mechanical',"mal","malo","mierda",
              "prohíbe", "polariza", "no", "irresponsable", "prohibirá", "cagada",
              "lucha","olvido","polarizó","incertidumbre","inconformes","inconforme",
              "temen","pisando","pisar","prohibira","faltan","desplazados",
              "desplazado")

#Import 3 csv
DatasetCatatumbo <- read.csv("C:/temp/CatatumboTweets.csv")
DatasetCatatumbo$text<-as.factor(DatasetCatatumbo$text)

DatasetSanTurban <- read.csv("C:/temp/SanTurbanTweets.csv")
DatasetSanTurban$text<-as.factor(DatasetSanTurban$text)

DatasetCucuta <- read.csv("C:/temp/CucutaTweets.csv")
DatasetCucuta$text<-as.factor(DatasetCucuta$text)

###########################
#Score all tweets 
#############################
Catatumbo.scores = score.sentiment(DatasetCatatumbo$text, pos.words,neg.words, .progress='text')
SanTurban.scores = score.sentiment(DatasetSanTurban$text, pos.words,neg.words, .progress='text')
Cucuta.scores = score.sentiment(DatasetCucuta$text, pos.words,neg.words, .progress='text')

path<-"C:/temp/"
write.csv(Catatumbo.scores,file=paste(path,"CatatumboScores.csv",sep=""),row.names=TRUE)
write.csv(SanTurban.scores,file=paste(path," SanTurbanScores.csv",sep=""),row.names=TRUE)
write.csv(Cucuta.scores,file=paste(path,"CucutaScores.csv",sep=""),row.names=TRUE)

Catatumbo.scores$Team = 'Catatumbo'
SanTurban.scores$Team = 'SanTurban'
Cucuta.scores$Team = 'Cucuta'

############################# 
#Chunk -5.5- loading  sentiment functions       
#############################
# source("sentiment.R")
# t1<-sentiment(text=c("I hate my apple", "I love my apple"))
source("classify_polarity.R")
source("classify_emotion.R")
source("create_matrix.R")


############################# 
#Chunk -5- simple Visualization        
#############################

hist(Catatumbo.scores$score)
qplot(Catatumbo.scores$score)

hist(SanTurban.scores$score)
qplot(SanTurban.scores$score)

hist(Cucuta.scores$score)
qplot(Cucuta.scores$score)

#################################
#Chunk -6- Comparing 3 data sets	              
#################################

all.scores = rbind(Catatumbo.scores, SanTurban.scores, Cucuta.scores)
ggplot(data=all.scores) + # ggplot works on data.frames, always
  geom_bar(mapping=aes(x=score, fill=Team), binwidth=1) +
  facet_grid(Team~.) + # make a separate plot for each hashtag
  theme_bw() + scale_fill_brewer() # plain display, nicer colors

####################################################
#Chunk -7- Classification by emotions and polarity                
####################################################

library(twitteR)
#library(sentiment) not suported under R ver 3.0 Used as funcion
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# Get the text
SanTurban_txt = sapply(SanTurban.list, function(x) x$getText())

# Prepare text for the analysis
SanTurban_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", SanTurban_txt)
SanTurban_txt = gsub("@\\w+", "", SanTurban_txt)
SanTurban_txt = gsub("[[:punct:]]", "", SanTurban_txt)
SanTurban_txt = gsub("[[:digit:]]", "", SanTurban_txt)
SanTurban_txt = gsub("http\\w+", "", SanTurban_txt)
SanTurban_txt = gsub("[ \t]{2,}", "", SanTurban_txt)
SanTurban_txt = gsub("^\\s+|\\s+$", "", SanTurban_txt)

try.error = function(x)
  
{  
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply 
SanTurban_txt = sapply(SanTurban_txt, try.error)

# remove NAs in SanTurban_txt
SanTurban_txt = SanTurban_txt[!is.na(SanTurban_txt)]
names(SanTurban_txt) = NULL

#classify emotion
class_emo = classify_emotion(SanTurban_txt, algorithm="bayes", prior=1.0)
#get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(SanTurban_txt, algorithm="bayes")

# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=SanTurban_txt, emotion=emotion, 
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of tweets", 
       title = "Sentiment Analysis of Tweets about SanTurban\n(classification by emotion)",
       plot.title = element_text(size=12))

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets",
       title = "Sentiment Analysis of Tweets about SanTurban\n(classification by polarity)",
       plot.title = element_text(size=12))





