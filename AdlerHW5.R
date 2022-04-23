library(tidyverse)
library(tidytext)
library(textstem)
library(topicmodels)
library(reshape2)
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
speech_topic_model = LDA(word_matrix, 24, control=list(seed=18))
beta_matrix = tidy(speech_topic_model,matrix="beta")
topics=group_by(beta_matrix, topic) %>% slice_max(beta, n=10)
ggplot(topics, aes(y=reorder_within(term, beta, topic), x=beta)) +
  geom_col() +
  facet_wrap(~topic, scales="free") +
  scale_y_reordered()
##the correct line is one of the following two, but they both produce an even number of rows per topic, making this
##analysis entirely useless?
#speech_topics = tidy(speech_topic_model,matrix="gamma")
#speech_topics = tidy(speech_topic_model,matrix="gamma") %>% group_by(topic) %>% slice_max(gamma, n=10)
speech_topics$year = as.numeric(c(str_extract(speech_topics$document,"(?<=\\()[:digit:]+(?=-)")))
speeches_by_year = speech_topics %>% group_by(year) %>% summarize(count=n())
topic4_analysis = group_by(filter(speech_topics, topic==4),year) %>% summarise(avg=(speeches_by_year/n()))
topic6_analysis = group_by(filter(speech_topics, topic==6),year) %>% summarise(count=mean(n()))
topic7_analysis = group_by(filter(speech_topics, topic==7),year) %>% summarise(count=mean(n()))
topic8_analysis = group_by(filter(speech_topics, topic==8),year) %>% summarise(count=mean(n()))