library(rvest) #html_nodes, html_text
library(purrr) #map_df
library(tm) #stopwords
library(ggplot2) #barplot
library(sentimentr) #replace_emoticon
library(wordcloud) #wordcloud
#library(dplyr)
#library(syuzhet)

base.url <- "https://www.walmart.com/reviews/product/27608624?limit=20&page="

# Total reviews :5580
map_df(1:279, function(i) {
  
  page <- read_html(sprintf(base.url, i))
  
  data.frame(review=html_text(html_nodes(page, ".js-customer-review-text")),
             date=as.numeric(format(as.Date(html_text(html_nodes(page, ".js-review-date")),"%m/%d/%Y"),'%Y')),
             stringsAsFactors=FALSE)
  
}) -> reviews

reviews.txt <- unlist(reviews$review) #convert list into character 
reviews.txt

# DATA CLEANING
reviews.txt <- tolower(reviews.txt) 
reviews.txt <- replace_emoticon(reviews.txt) 
reviews.txt <- gsub("[[:punct:]]", "", reviews.txt)
reviews.txt = gsub("[[:digit:]]", "", reviews.txt) 
reviews.txt <- gsub("[ |\n]{2,}", " ", reviews.txt) 
remove.words <- stopwords("en")

ps <- read.delim("positive_words.txt")
ps <- paste(ps$Name, collapse = " ")
ps <- tokenizers::tokenize_word_stems(ps, stopwords = remove.words)
pos.words = unlist(ps)

ns <- read.delim("negative_words.txt")
ns <- paste(ns$Name, collapse = " ")
ns <- tokenizers::tokenize_word_stems(ns, stopwords = remove.words)
neg.words = unlist(ns)

review.count <- length(reviews.txt)
pos.count <- pos.percent <- neg.count <- neg.percent <- numeric(review.count)

for (i in 1:review.count){
  
  review <- reviews.txt[i]
  
  
  tt <- tokenizers::tokenize_word_stems(review, stopwords = remove.words)
  words = unlist(tt)
  
  pos.count[i] = sum(!is.na(match(words, pos.words)))
  
  neg.count[i] = sum(!is.na(match(words, neg.words)))
  
  pos.percent[i] <- pos.count[i] * 100 / (pos.count[i]+neg.count[i])
  neg.percent[i] <- neg.count[i] * 100 / (pos.count[i]+neg.count[i])
  
}

df <- data.frame(pos.count, neg.count, pos.percent, neg.percent, reviews$date)
df

df1 <-c(mean(na.omit(df$pos.percent)), mean(na.omit(df$neg.percent)))
df2 <- c("Positive Sentiments", "Negative Sentiments")
barplot(df1, names.arg= df2, xlab = "Sentiments", ylab = "Percentage", col = "blue", main="Overall Product Sentiments", border = "red")


readreviewnumber <- function()
{ 
  n <- readline(prompt="Enter review number: ")
  n <- as.integer(n)
  if (is.na(n)){
    n <- readinteger()
  }
  return(n)
}

n <- readreviewnumber()


df[n,]
df3 <- c(df[n,]$pos.percent, df[n,]$neg.percent)
barplot(df3, names.arg= df2, ylab = "Percentage", col = "green", main="Product Sentiments by Review", border = "black")

col=brewer.pal(6,"Dark2")
wordcloud(reviews.txt, min.freq=25, scale=c(3,0.5), 
          random.color=T, max.word=200, random.order=F,colors=col)


df4 <- df
df4 <- aggregate(.~reviews.date, data=df4, mean)
df5 <- c(df4$pos.percent, df4$neg.percent)
df6 <- c("2013","2014","2016")
barplot(df5, names.arg= df6, xlab = "Sentiments", ylab = "Percentage", col = "blue", main="Overall Product Sentiments", border = "red", beside = TRUE)