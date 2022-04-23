library(tidyverse)
library(tidytext)
library(textstem)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggeasy)
speeches=read.csv("presidential_speeches_sample.csv")
speeches$content <- recode(speeches$content,"health care"="healthcare")
tokenized = unnest_tokens(speeches, word, content)
custom_stop_words = bind_rows(
  stop_words, tibble(word=c("president","jennings","people","lot","question","bush","minister"
  ))
)
tokenized = anti_join(tokenized, custom_stop_words, by="word")
tokenized = mutate(tokenized, lemma=lemmatize_words(word))
word_counts = group_by(tokenized, document, lemma) %>% summarize(count=n())
word_matrix = cast_dtm(word_counts, document, lemma, count)
#this line will take you like 30 minutes to run
speech_topic_model = LDA(word_matrix, 24, control=list(seed=18))
beta_matrix = tidy(speech_topic_model,matrix="beta")
topics=group_by(beta_matrix, topic) %>% slice_max(beta, n=10)
ggplot(topics, aes(y=reorder_within(term, beta, topic), x=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()
speech_topics = tidy(speech_topic_model,matrix="gamma")
speech_topics$year = as.numeric(c(str_extract(speech_topics$document,"(?<=\\()[:digit:]+(?=-)")))
topic4_analysis = group_by(filter(speech_topics, topic==4),year) %>% summarise(avg_gamma=mean(gamma))
topic6_analysis = group_by(filter(speech_topics, topic==6),year) %>% summarise(avg_gamma=mean(gamma))
topic7_analysis = group_by(filter(speech_topics, topic==7),year) %>% summarise(avg_gamma=mean(gamma))
topic8_analysis = group_by(filter(speech_topics, topic==8),year) %>% summarise(avg_gamma=mean(gamma))
grid.arrange(
  ggplot(topic4_analysis,aes(x=year,y=avg_gamma)) + geom_line(color='brown') + ggtitle("Middle East") + easy_center_title(),
  ggplot(topic6_analysis,aes(x=year,y=avg_gamma)) + geom_line(color='blue') + ggtitle("Sports") + easy_center_title(),
  ggplot(topic7_analysis,aes(x=year,y=avg_gamma)) + geom_line(color='red') + ggtitle("Healthcare") + easy_center_title(),
  ggplot(topic8_analysis,aes(x=year,y=avg_gamma)) + geom_line(color='green') + ggtitle("Trade") + easy_center_title(),
  nrow=2
)
speech_topics$party <- c(str_extract(speech_topics$document,"^.+(?=:)"))
#need this line because line 40 kept resulting in two entries of "Jimmy Carter: Italy" that I had no way to fix
speech_topics <- mutate(speech_topics,party=str_replace(party,": Italy",""))
speech_topics$party <- recode(speech_topics$party,"Jimmy Carter"="D","Ronald Reagan"="R","George Bush"="R","William J. Clinton"="D","George W. Bush"="R","Barack Obama"="D")
topics_by_party = speech_topics %>% group_by(topic,party) %>% summarise(avg_gamma=mean(gamma))
ggplot(topics_by_party,aes(x=topic,y=avg_gamma,fill=party)) + geom_bar(stat="identity",position="dodge")
