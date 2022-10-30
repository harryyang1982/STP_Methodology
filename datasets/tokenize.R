library(tidyverse)
library(quanteda)

mixer<-read.csv("datasets/Homework_Mixerdata.csv", header=T, na.strings=c("","NA")) #If blank, make it NA
table(is.na(mixer))
mixer_complete<-na.omit(mixer) #let's just use the complete cases only 


# practice
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

ndoc(view_complete_tasks)
nfeat(view_complete_tasks)
mixer_features<-as.data.frame(featfreq((view_complete_tasks)))
colnames(view_complete_tasks)

names(mixer_features)[names(mixer_features) == 'featfreq((view_complete_tasks))'] <- 'freq'

mixer_features<-tibble::rownames_to_column(mixer_features, "feature")

mixer_ordered<-mixer_features %>% arrange(-(freq))
head(mixer_ordered)

#frequent features
topfeatures(view_complete_tasks, 20)

#10 least frequent features
topfeatures(view_complete_tasks, decreasing=F, 20)

library(quanteda.textplots)
textplot_wordcloud(view_complete_tasks, max_words=50, color="grey10")

# co-currence
mixer_fcm <- fcm(view_complete_tasks)
mixer_fcm

mixer_fcm_plotdata <- fcm_select(mixer_fcm,
                                 pattern = c("bosch", "lid", "jar", "time"),
                                 selection = "keep")
textplot_network(mixer_fcm_plotdata)

df <- convert(mixer_fcm, to = "data.frame")

df
