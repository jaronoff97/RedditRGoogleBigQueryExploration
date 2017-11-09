# install.packages('devtools') 
# devtools::install_github("rstats-db/bigrquery")

library("bigrquery")
library("ggplot2")

theme_set(theme_bw()) # Change the theme to my preference
# Use your project ID here
project <- "redditcollaborativefiltering" # put your project ID here

# Example query - select copies of files with content containing "TODO"
sql <- "SELECT
      title, score, subreddit, created_utc, num_comments, author
        FROM
      [fh-bigquery:reddit_posts.2017_09]
        WHERE
      title CONTAINS \"Puerto Rico\"
        and
      not(lower(subreddit) contains \"trump\")
        and
      not(lower(subreddit) contains \"donald\")
        GROUP BY
      subreddit, score, title, created_utc, num_comments, author
        ORDER BY
      score DESC
        LIMIT 100;"

# Execute the query and store the result
loadData <- function(debug = F) { # Optional debugging
  # If puerto.rico.post.data exists
  if(exists("puerto.rico.post.data", inherits = T)) {
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
    puerto.rico.post.data <- query_exec(sql, project = project, useLegacySql = FALSE)
  }
  # return the senators
  class(puerto.rico.post.data$created_utc) <- class(Sys.time())
  puerto.rico.post.data
}
# Load the data once
puerto.rico.post.data <- loadData()

compileDataPerDay <- function(df = loadData())
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

data.per.day <- compileDataPerDay()
ggplot(aes(x = data.per.day$time, y = data.per.day$score), data = data.per.day) + geom_line()
ggplot(aes(x = data.per.day$time, y = data.per.day$numComments), data = data.per.day) + geom_line()







