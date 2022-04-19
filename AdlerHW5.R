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