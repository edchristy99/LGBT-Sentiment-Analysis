library(tm)
library(wordcloud2)
library(twitteR)
library(rtweet)

api_key<- "hC4qUgtGr0z19YhJi62BuIqeM"
api_secret<- "fNTamT0PvxCLopIaelIJjObsuIYjU3vxVs3IbneW3waCgHQ1wk"
access_token<- "475848898-DttjhEjUBhiGjYqtgb5E8DdA2ygkgLUOwQe5kQhY"
access_token_secret<- "QxuP7l2cNtSCpFG2EaQeFNMh0GVAC7fgK966vwITz5Vv2"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

tw = searchTwitter('#TerimakasihBPJS', 
                   n = 10000,
                   retryOnRateLimit = 10e5)
saveRDS(tw,file = 'tweet-mentah.rds')


tw <- readRDS('tweet-mentah.rds')
d = twListToDF(tw)
komen <- d$text
komenc <- Corpus(VectorSource(komen))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
twitclean <- tm_map(komenc, removeURL)
removeNL <- function(y) gsub("\n", " ", y)
twitclean <- tm_map(twitclean, removeNL)
replacecomma <- function(y) gsub(",", "", y)
twitclean <- tm_map(twitclean, replacecomma)
removeRT <- function(y) gsub("RT ", "", y)
twitclean <- tm_map(twitclean, removeRT)
removetitik2 <- function(y) gsub(":", "", y)
twitclean <- tm_map(twitclean, removetitik2)
removetitikkoma <- function(y) gsub(";", " ", y)
twitclean <- tm_map(twitclean, removetitikkoma)
removetitik3 <- function(y) gsub("p.", "", y)
twitclean <- tm_map(twitclean, removetitik3)
removeamp <- function(y) gsub("&amp;", "", y)
twitclean <- tm_map(twitclean, removeamp)
removeUN <- function(z) gsub("@\\w+", "", z)
twitclean <- tm_map(twitclean, removeUN)
remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
twitclean <- tm_map(twitclean,remove.all)
twitclean <- tm_map(twitclean, removePunctuation)
twitclean <- tm_map(twitclean, tolower)
myStopwords = readLines("stopwords.txt")
twitclean <- tm_map(twitclean,removeWords,myStopwords)
twitclean <- tm_map(twitclean , removeWords, 
                    c('kalo','gak','org',''))

dataframe<-data.frame(text=unlist(sapply(twitclean, `[`)), stringsAsFactors=F)
View(dataframe)
write.csv(dataframe,file = 'tweetclean-tidy-fix.csv')