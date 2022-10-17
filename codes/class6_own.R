library(tidyverse)

mixer <- read_csv("datasets/Philips_Viva.csv")
mixer

table(is.na(mixer))

mixer_complete <- na.omit(mixer)

mixer_complete$View1[11]
str(mixer_complete)

library(quanteda)
sessionInfo()

mixer_corpus <- corpus(paste(mixer_complete))
mixer_corpus
mixer_tokens <- tokens(mixer_corpus, what = "word")
mixer_tokens

# dfm
mixer_dfm <- dfm(mixer_tokens)
mixer_dfm

sentence <- "Learning to use R is so much fun!
I will need a break after this though. I will go for coffee."

ntoken(sentence)
ntype(sentence)

?ntype

topfeatures(mixer_dfm, 10)
topfeatures(mixer_dfm, 50)

v1_corpus <- corpus(mixer_complete$View1)
v1_corpus %>% tokens() %>% dfm() %>% topfeatures()

mixer_complete$View1 %>% tokens() %>% dfm() %>% topfeatures()
topfeatures(dfm(tokens(mixer_complete$View1)))

mixer_complete$View1[5]
writeLines(mixer_complete$View1[5]) #can read like .txt file
# df$variable<- gsub(pattern = "\n", replacement = " ", x = df$vairiable) #this is how you change the specific features within data

mixer_tokens <- tokens(mixer_corpus, what = "word",
                       remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove_url = FALSE,
                       remove_separators = TRUE,
                       remove_symbols = TRUE)
mixer_tokens

mixer_tokens_lower <- tokens_tolower(mixer_tokens)
mixer_tokens_lower

stopwords('english')[1:20]
stopwords('english')[1:200]
stopwords('korean')[1:200]
stopwords('spanish')[1:200]
stopwords('chinese')[1:200]
stopwords('zh', source='misc')[1:200]

?stopwords_getlanguages

mixer_tokens_lower_stop <- tokens_remove(mixer_tokens_lower, stopwords('english'))
mixer_tokens_lower_stop

mixer_tokens_lower_stop_stem <- tokens_wordstem(mixer_tokens_lower_stop)
mixer_tokens_lower_stop_stem

# remove too rare or too much frequent word
mixer_dfm<-dfm(mixer_tokens)
mixer_dfm

mixer_dfm_proc<-dfm(mixer_tokens_lower_stop_stem)
mixer_dfm_rare_freq <- dfm_trim(mixer_dfm_proc, 
                                min_docfreq = 0.005, 
                                max_docfreq = 0.99, 
                                docfreq_type = "prop")

mixer_dfm_rare_freq %>% topfeatures()


# practice
library(quanteda)
library(magrittr)
library(dplyr)
view_complete_tasks <- mixer_complete$View1 %>% tokens(what = "word",
                                remove_punct = TRUE,
                                remove_numbers = TRUE,
                                remove_separators = TRUE,
                                remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(stopwords('english')) %>% 
  tokens_wordstem() %>% 
  dfm() %>% 
  dfm_trim(min_docfreq = 0.1,
           max_docfreq = 0.95,
           docfreq_type = 'prop')

ndoc(mixer_dfm_rare_freq)
nfeat(mixer_dfm_rare_freq)
mixer_features<-as.data.frame(featfreq((mixer_dfm_rare_freq)))
colnames (mixer_features)

names(mixer_features)[names(mixer_features) == 'featfreq((mixer_dfm_rare_freq))'] <- 'freq'

mixer_features<-tibble::rownames_to_column(mixer_features, "feature")

mixer_ordered<-mixer_features %>% arrange(-(freq))
head(mixer_ordered)

#frequent features
topfeatures(mixer_dfm_rare_freq, 20)

#10 least frequent features
topfeatures(mixer_dfm_rare_freq, decreasing=F, 20)

library(quanteda.textplots)
textplot_wordcloud(mixer_dfm_rare_freq, max_words=50, color="grey10")

textplot_wordcloud(view_complete_tasks, max_words=50, color='grey10')

# co-currence
mixer_fcm <- fcm(mixer_dfm_rare_freq)
mixer_fcm

mixer_fcm_plotdata <- fcm_select(mixer_fcm,
                                 pattern = c("tanmay", "ankita", "yoda", "sangeeta"),
                                 selection = "keep")
textplot_network(mixer_fcm_plotdata)

df <- convert(mixer_fcm, to = "data.frame")

