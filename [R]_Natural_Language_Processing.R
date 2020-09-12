library(reshape2) # Melting and Casting functions
library(dplyr) # Data manipulation library  
library(tidytext) # Supporting data sets to allow text conversion from tidy formats
library(tidyverse) # important data science package
library(textreadr) #to Read text
library(stringr) # focusses on the most important and commonly used string manipulation
library(igraph) # graphing library function 
library(ggraph) # ggplot2 API tailored to graph 
library(wordcloud) # word cloud 
library(dplyr) # set of tools for efficiently manipulating datasets in R
library(textreadr) # text reader 
library(stringr) # use string read
library(tidytext) #tidytext 
library(reshape2) #reshape function
library(tidyr) # make the text tidy, very useful library

#Assining 
Answers <- read_document(file="Team 1 Survey-2.docx")
class_combo <- c(Answers)

a <- 30 #how many observations to you have
b <- 4 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- class_combo[i*b+z-b]
  }#closing z loop
}#closing i loop


names(my_df)[1] <- "How do you see the world in the next 10 years?"
names(my_df)[2] <- "How do you describe yourself?"
names(my_df)[3] <- "What plans do you have post-graduation?"
names(my_df)[4] <- "3 reasons for choosing a school?"
#my_df

my_txt1 <- my_df$"How do you see the world in the next 10 years?"
my_txt1 <- substr(my_txt1, start=11 , stop = 10000)
mydf1 <- data_frame(line=1:a, text=my_txt1)
data(stop_words)

custom_stop_words <- data_frame(word=c('3rd','10','holt',
                                       'scribe','ob','na',
                                       'um','uh','4','yep',
                                       'yeah','2030','2','18',
                                       'ic','lee','6','520',
                                       'pounds','foot','ck',
                                       '1st','wow','en','ing',
                                       '3','reasons','2015',
                                       '30s','afe','ly','deoti',
                                       '80'),
                                lexicon=rep('cust', each = 33))

custom_stop_words <- rbind(stop_words, custom_stop_words)

mydf1 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>% 
  count(word, sort=TRUE)

print(mydf1)

nrc_sent <- get_sentiments("nrc")

nrc_counts <- mydf1 %>%
    inner_join(nrc_sent) 

mydf1 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(0.7,0.7),
                   fixed.asp = TRUE,
                   title.size = 1
                   )
mydf1 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(0.5,0.5),
                   fixed.asp = TRUE,
                   title.size = 1
  )



my_txt2 <- my_df$"How do you describe yourself?"
my_txt2 <- substr(my_txt2, start=11 , stop = 10000)
mydf2 <- data_frame(line=1:a, text=my_txt2)

data(stop_words)
mydf2 <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>% 
  count(word, sort=TRUE)
nrc_sent <- get_sentiments("nrc")

nrc_counts <- mydf2 %>%
  inner_join(nrc_sent) 

mydf2 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(0.7,0.7),
                   fixed.asp = TRUE,
                   title.size = 1
  )

mydf2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(1,1),
                   fixed.asp = TRUE,
                   title.size = 1
  )



my_txt3 <- my_df$"What plans do you have post-graduation?"
my_txt3 <- substr(my_txt3, start=11 , stop = 10000)
mydf3 <- data_frame(line=1:a, text=my_txt3)

data(stop_words)
mydf3 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>% 
  count(word, sort=TRUE)

print(mydf3)

nrc_sent <- get_sentiments("nrc")

nrc_counts <- mydf3 %>%
  inner_join(nrc_sent) 

mydf3 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(0.7,0.7),
                   fixed.asp = TRUE,
                   title.size = 0.8
  )

mydf3 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(1,1),
                   fixed.asp = TRUE,
                   title.size = 0.3
  )




my_txt4 <- my_df$"3 reasons for choosing a school?"
my_txt4 <- substr(my_txt4, start=11 , stop = 10000)
mydf4 <- data_frame(line=1:a, text=my_txt4)

data(stop_words)
mydf4 <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>% 
  count(word, sort=TRUE)

print(mydf4)

nrc_sent <- get_sentiments("nrc")

nrc_counts <- mydf4 %>%
  inner_join(nrc_sent) 

mydf4 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "grey80"),
                   max.words=100,
                   scale = c(1,1),
                   fixed.asp = TRUE,
                   title.size = 0.9
  )

mydf4 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale = c(1,1),
                   fixed.asp = TRUE,
                   title.size = 1
  )





my_combined <- bind_rows(
  mutate(mydf1,location = 'Q-One'),
  mutate(mydf2,location = 'Q-Two'),
  mutate(mydf3,location = 'Q-Three'),
  mutate(mydf4,location = 'Q-Four')
)

my_combined <-my_combined %>%
  bind_tf_idf(word,location,n)

my_combined %>%
  arrange(desc(tf_idf))


my_combined %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(location) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=location))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~location, ncol=2, scales="free")+
  coord_flip()



##############################################################
##############################################################
### Bi - Grams

##########QUESTION 1###############

my_vec1 <- my_df$"How do you see the world in the next 10 years?"
my_vec1 <- substr(my_vec1, start=11 , stop = 10000)
myvecdf1 <- data_frame(line=1:a, text=my_vec1)

myvecdf1 <- myvecdf1 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

myvecdf1 #We want to see the bigrams (words that appear together, "pairs")

myvecdf1 %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated <- myvecdf1 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts

negation_tokens <- c("no", "never","not","don't", "dont", "avoid","doesnt", "doesn't",
                     "nope")#what negation tokens do you want to use?
afinn_data <- get_sentiments("afinn")
negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(afinn_data, by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

negated_words

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
#library(igraph)

bigram_graph <- bigram_counts %>%
  filter(n>0.5) %>%
  graph_from_data_frame()

bigram_graph

#install.packages("ggraph")
#library(ggraph)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

##########QUESTION 2###############

my_vec2 <- my_df$"How do you describe yourself?"
my_vec2 <- substr(my_vec2, start=11 , stop = 10000)
myvecdf2 <- data_frame(line=1:a, text=my_vec2)

myvecdf2 <- myvecdf2 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

myvecdf2 #We want to see the bigrams (words that appear together, "pairs")

myvecdf2 %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
bigrams_separated2 <- myvecdf2 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered2 <- bigrams_separated2 %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts2 <- bigrams_filtered2 %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts2

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
#library(igraph)

bigram_graph2 <- bigram_counts2 %>%
  filter(n>0.5) %>%
  graph_from_data_frame()

bigram_graph2

#install.packages("ggraph")
#library(ggraph)

ggraph(bigram_graph2, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


########## QUESTION 3 ###############

my_vec3 <- my_df$"What plans do you have post-graduation?"
my_vec3 <- substr(my_vec3, start=11 , stop = 10000)
myvecdf3 <- data_frame(line=1:a, text=my_vec3)

myvecdf3 <- myvecdf3 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

myvecdf3 #We want to see the bigrams (words that appear together, "pairs")

myvecdf3 %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
bigrams_separated3 <- myvecdf3 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered3 <- bigrams_separated3 %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts3 <- bigrams_filtered3 %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts3

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
#library(igraph)

bigram_graph3 <- bigram_counts3 %>%
  filter(n>0.5) %>%
  graph_from_data_frame()

bigram_graph3

#install.packages("ggraph")
#library(ggraph)

ggraph(bigram_graph3, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



########## QUESTION 4 ###############

my_vec4 <- my_df$"3 reasons for choosing a school?"
my_vec4 <- substr(my_vec4, start=11 , stop = 10000)
myvecdf4 <- data_frame(line=1:a, text=my_vec4)

myvecdf4 <- myvecdf4 %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

myvecdf4 #We want to see the bigrams (words that appear together, "pairs")

myvecdf4 %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
bigrams_separated4 <- myvecdf4 %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered4 <- bigrams_separated4 %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts4 <- bigrams_filtered4 %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts4

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#install.packages("igraph")
#library(igraph)

bigram_graph4 <- bigram_counts4 %>%
  filter(n>0.5) %>%
  graph_from_data_frame()

bigram_graph4

#install.packages("ggraph")
#library(ggraph)

ggraph(bigram_graph4, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)



#############################################################

