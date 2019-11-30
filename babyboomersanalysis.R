install.packages('rvest')
library(rvest)
#web scraping
url <- 'https://www.americanmobile.com/nursezone/nursing-news/the-baby-boomers-massive-impact-on-health-care/'
webpage <- read_html(url)
data <- html_nodes(webpage,'p:nth-child(23) , p:nth-child(22) , p:nth-child(21) , p:nth-child(20) , p:nth-child(19) , p:nth-child(18) , p:nth-child(17) , p:nth-child(16) , p:nth-child(15) , p:nth-child(14) , p:nth-child(13) , p:nth-child(12)')
main_data<- html_text(data)
head(main_data)
class(main_data)


#web scraping for website 2
url <- 'https://www.encorepbc.org/blog/baby-boomers-how-they-impact-american-society/'
webpage1<- read_html(url)
data1 <- html_nodes(webpage1,'p')
main_data1<- html_text(data1)
head(main_data1)
class(main_data1)

#web scraping
url <- 'https://www.mckinsey.com/featured-insights/employment-and-growth/talkin-bout-my-generation'
webpage2<- read_html(url)
data2 <- html_nodes(webpage2,'p , .deck-content-wrapper')
main_data2<- html_text(data2)
head(main_data2)
class(main_data2)

#web scraping
url <- 'https://www.governing.com/topics/politics/gov-baby-boomer-impact-on-elections.html'
webpage3<- read_html(url)
data3 <- html_nodes(webpage3,'.body p')
main_data3<- html_text(data3)
head(main_data3)
class(main_data3)

#web scraping
url <- 'https://www.forbes.com/sites/forbestechcouncil/2019/06/28/healthcare-and-baby-boomers-tech-adoption-is-the-key-to-better-outcomes/#1e1eb5007ad4'
webpage4<- read_html(url)
data4 <- html_nodes(webpage4,'p')
main_data4<- html_text(data4)
head(main_data4)
class(main_data4)

#web scraping
url <- 'https://www.thefiscaltimes.com/2019/02/20/Baby-Boomers-Will-Drive-Health-Care-Spending-Nearly-6-Trillion-Year-2027'
webpage5<- read_html(url)
data5 <- html_nodes(webpage5,'p')
main_data5<- html_text(data5)
head(main_data5)
class(main_data5)

##merge the character arrays
merge<- c(main_data,main_data1,main_data2,main_data3,main_data4,main_data5)
view(merge)
#data cleaning and pre-processing
library(tm)
merge <- gsub(",.*","",merge)
merge<- removeNumbers(merge)
merge <- removePunctuation(merge) #just to be sure of the punctuations
#check the data for clarification
View(merge)
##converting to tidy text format
library(dplyr)
text_df <- tibble(line=1:102,text=merge)
text_df
library(tidytext)
text_df <- text_df %>% unnest_tokens(word,text)
View(text_df)

##now we have the cleansed data
#performing sentiment analysis
#testing out various sentiment lexicons present in the package tidytext
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#let us choose the bing lexicon for simplicity of a broader number of negative words.
bing_negative <- get_sentiments("bing") %>% 
  filter(sentiment=="negative")
nrc_negative <- get_sentiments("nrc") %>% 
  filter(sentiment=="negative")
View(nrc_negative)
main_bing<- text_df%>% 
  inner_join(bing_negative) %>% 
  count(word,sort=TRUE)
View(main_bing)
main_nrc<- text_df%>% 
  inner_join(nrc_negative) %>% 
  count(word,sort=TRUE)
View(main_nrc)
##constructing a word cloud to find the most important words
library(wordcloud)
text_df %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word,n,max.words=200))
##constructing a comparison cloud
library(reshape2)
text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
library(tidyr)

baby_boomers_sentiment<- text_df%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(baby_boomers_sentiment, aes(index, sentiment, fill = word)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~word, ncol = 2, scales = "free_x")



#traditional sentiment analysis
library(SentimentAnalysis)
sentiment <- analyzeSentiment(text_df)
convertToBinaryResponse(sentiment)$SentimentQDAP
sentiment$SentimentQDAP
a <- convertToDirection(sentiment$SentimentQDAP)
View(a)
