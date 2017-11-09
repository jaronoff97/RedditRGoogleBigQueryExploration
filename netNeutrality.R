---
title: "Net Neutrality"
author: "Jacob Aronoff"
date: "11/8/2017"
output: html_document
---

# install.packages('devtools') 
# devtools::install_github("rstats-db/bigrquery")
library("bigrquery")
library("ggplot2")
library("dplyr")
library("wordcloud")
library("urltools")


if(exists("wordsNotToUse", inherits = T)) {
  # Pass
} else {
  wordsNotToUse = scan("stopWords.txt", what="", sep="\n")
}

## Initial setup


theme_set(theme_bw()) # Change the theme to my preference
# Use your project ID here
project <- "redditcollaborativefiltering" # put your project ID here

# Example query - select copies of files with content containing "TODO"
postQuery <- function(x)
{
  return(paste("SELECT
               title, score, subreddit, created_utc, num_comments, author, url
               FROM
               TABLE_QUERY([fh-bigquery:reddit_posts], \"REGEXP_MATCH(table_id, '^201._..$')\")
               WHERE
               title CONTAINS \"", x, "\"
               and
               not(lower(subreddit) contains \"trump\")
               and
               not(lower(subreddit) contains \"donald\")
               GROUP BY
               subreddit, score, title, created_utc, num_comments, author, url
               ORDER BY
               score DESC
               LIMIT 300;", sep = ""))
}

commentQuery <- function(x)
{
  return(paste("SELECT
               body, score, subreddit, created_utc, author
               FROM
               TABLE_QUERY([fh-bigquery:reddit_comments], \"REGEXP_MATCH(table_id, '^201._..$')\")
               WHERE
               body CONTAINS \"", x, "\"
               GROUP BY
               body, score, subreddit, created_utc, author
               ORDER BY
               score DESC
               LIMIT 5000;", sep = ""))
}

commentFreqQuery <- function(x)
{
  return(paste("SELECT
               score, created_utc
               FROM
               TABLE_QUERY([fh-bigquery:reddit_comments], \"REGEXP_MATCH(table_id, '^201._..$')\")
               WHERE
               body CONTAINS \"", x, "\"
               ORDER BY
               created_utc DESC;", sep = ""))
}

## Load the data

loadPostData <- function(debug = F) { # Optional debugging
  # If post.data exists
  if(exists("post.data", inherits = T)) {
    if(debug) {
      # let the user know
      print("Exists")
    }
  } else {
    if(debug) {
      # let the user know
      print("Doesn't Exist")
    }
    # load the data
    post.data <- query_exec(postQuery(queryParam), project = project, useLegacySql = FALSE)
  }
  # return the senators
  class(post.data$created_utc) <- class(Sys.time())
  return(post.data)
}
loadCommentData <- function(debug = F) { # Optional debugging
  # If post.data exists
  if(exists("comment.data", inherits = T)) {
    if(debug) {
      # let the user know
      print("Exists")
    }
  } else {
    if(debug) {
      # let the user know
      print("Doesn't Exist")
    }
    # load the data
    comment.data <- query_exec(commentQuery(queryParam), project = project, useLegacySql = FALSE)
  }
  # return the senators
  class(comment.data$created_utc) <- class(Sys.time())
  return(comment.data)
}
loadCommentFreqData <- function(debug = F) { # Optional debugging
  # If post.data exists
  if(exists("comment.freq.data", inherits = T)) {
    if(debug) {
      # let the user know
      print("Exists")
    }
  } else {
    if(debug) {
      # let the user know
      print("Doesn't Exist")
    }
    # load the data
    comment.freq.data <- query_exec(commentFreqQuery(queryParam), project = project, useLegacySql = FALSE)
  }
  # return the senators
  class(comment.freq.data$created_utc) <- class(Sys.time())
  return(comment.freq.data)
}

## Compile graph data for Puerto Rico Posts

compileDataPerDay <- function(df = loadPostData())
{
  toReturn = data.frame(time=as.POSIXct(character()), score=numeric(0), numComments=numeric(0))
  for(day in sort(unique(format(as.Date(df$created_utc,format="%Y-%m-%d"), "%d"))))
  {
    rowsForDay = df[format(as.Date(df$created_utc,format="%Y-%m-%d"), "%d")==day,]
    r = list(time=as.POSIXct(rowsForDay$created_utc[1]), score=sum(rowsForDay$score), numComments=sum(rowsForDay$num_comments))
    # print(r)
    toReturn[nrow(toReturn) + 1,] = r
  }
  return(toReturn)
}

countForSubreddit <- function(df = loadPostData())
{
  toReturn = data.frame(subreddit=character(0), cnt=numeric(0), stringsAsFactors=FALSE)
  for(sub in unique(df$subreddit))
  {
    toReturn[nrow(toReturn) + 1,] = list(subreddit=sub, cnt=nrow(df[df$subreddit==sub,]))
  }
  toReturn$subreddit <- factor(toReturn$subreddit, levels = toReturn$subreddit[order(toReturn$cnt)])
  toReturn <- toReturn[toReturn$cnt>1,]
  return(toReturn)
}
getLinksForPosts <- function(df = loadPostData())
{
  toReturn = df$url %>% lapply(domain) %>% unlist() %>% table() %>% data.frame()
  names(toReturn) <- c("Domain", "Count")
  toReturn <- toReturn[toReturn$Domain != "i.imgur.com",]
  toReturn <- toReturn[toReturn$Domain != "imgur.com",]
  toReturn <- toReturn[toReturn$Domain != "www.reddit.com",]
  toReturn$Domain <- factor(toReturn$Domain, levels = toReturn$Domain[order(toReturn$Count)])
  toReturn <- toReturn[toReturn$Count>1,]
  return(toReturn)
}


## Compile graph data for Puerto Rico Comments


getMostUsedWords <- function(df = loadCommentData())
{
  words <- df$body %>% tolower() %>% as.character() %>% strsplit(" ") %>% lapply(function(x){ gsub("[^[:alnum:]['-]", " ", x)}) %>% unlist() %>% table() %>% data.frame()
  names(words) <- c("Word", "Count")
  for(word in wordsNotToUse)
  {
    words <- words[!grepl(word, words$Word),]  
  }
  words <- words[order(-words$Count, words$Word),]
  return(words)
}

getCommentFreqGraph <- function(df = loadCommentFreqData())
{
  return(df)
}


queryParam <- "Net Neutrality"
net.neutrality.t.compileDataPerDay <- compileDataPerDay()
net.neutrality.t.countForSubreddit <- countForSubreddit()
net.neutrality.t.getLinksForPosts <- getLinksForPosts()
net.neutrality.t.getMostUsedWords <- getMostUsedWords()
net.neutrality.t.getCommentFreqGraph <- getCommentFreqGraph()



ggplot(aes(x = net.neutrality.t.compileDataPerDay$time, y = net.neutrality.t.compileDataPerDay$score), data = net.neutrality.t.compileDataPerDay) + geom_line() + ggtitle("Orlando")
ggplot(aes(x = net.neutrality.t.compileDataPerDay$time, y = net.neutrality.t.compileDataPerDay$numComments), data = net.neutrality.t.compileDataPerDay) + geom_line() + ggtitle("Orlando")
# - - - - - - - - - - - - - - - - - - -
ggplot(net.neutrality.t.countForSubreddit, aes(x = subreddit, y = cnt)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Orlando")
# - - - - - - - - - - - - - - - - - - -
ggplot(net.neutrality.t.getLinksForPosts, aes(x = Domain, y = Count)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Orlando")
# - - - - - - - - - - - - - - - - - - -
wordcloud(words = net.neutrality.t.getMostUsedWords$Word, freq = net.neutrality.t.getMostUsedWords$Count, min.freq = 50,
          max.words=200, random.order=FALSE, rot.per=0.20, scale=c(4,.9),
          colors=brewer.pal(9,"BuGn")[-(1:4)])
# - - - - - - - - - - - - - - - - - - -
ggplot(aes(x = net.neutrality.t.getCommentFreqGraph$created_utc, y = net.neutrality.t.getCommentFreqGraph$score), data = net.neutrality.t.getCommentFreqGraph) + geom_line() + ggtitle("Orlando")


