
#############################################################
#slide 16

setwd("/Users/dasomlee/Desktop/Data/STP702")
mixer<-read.csv("datasets/Philips_Viva.csv", header=T, na.strings=c("","NA")) #If blank, make it NA
table(is.na(mixer))
mixer_complete<-na.omit(mixer) #let's just use the complete cases only 
#very difficult to use missing data analysis for qualitative data

#if you want to just look at the 11th row
mixer_complete$View1[15]
str(mixer_complete)

#############################################################
#slide 21
#install.packages("quanteda") #this may take a whole
#type "yes" when it asks you a question
library(quanteda)
sessionInfo()

mixer_corpus<-corpus(mixer_complete)
mixer_corpus<-corpus(paste(mixer_complete))
#here the paste function concatenate vectors

mixer_corpus
#you will be able to see that each variable (e.g., name) has become a single document
#because we had 7 variables, now we have 7 text documents 

#if you want to just use one variable from the mixer data, you can also do this. 
head(mixer_complete$View1)
mixer_corpus<-corpus(mixer_complete$View1)
mixer_corpus

#############################################################
#slide 23
mixer_corpus<-corpus(paste(mixer_complete))
mixer_tokens<-tokens(mixer_corpus, what="word") #you can also use "sentence" instead of "word"
mixer_tokens

#let's use a little more understandable example
sentence<-"Learning to use R is so much fun! I will need a break after this though. I will go for coffee."
ntoken(sentence) #we have 24 tokens
ntype(sentence) #we have 21 types because "I", "will", and "." occur more than once

#############################################################
#slide 24
mixer_dfm<-dfm(mixer_tokens)
print(mixer_dfm)

#It says we have 7 documents
#We have 1,507 features occurring across all 7 documents - these features denote types (not the same ones are repeated)
#Cells describe how many times something occurs - for example "c" occurs once in all texts, "(" occurs once in text 1, twice in text 2 etc.

topfeatures(mixer_dfm, 10)
topfeatures(mixer_dfm, 50)

#############################################################
#slide 28

mixer_complete$View1[5]
writeLines(mixer_complete$View1[5]) #can read like .txt file
df$variable<- gsub(pattern = "\n", replacement = " ", x = df$vairiable) #this is how you change the specific features within data

#############################################################
#slide 31

#we need to use quanteda package here
library(quanteda)
mixer_tokens<-tokens(mixer_corpus, what="word",
                     remove_punct=T,
                     remove_numbers=F,
                     remove_url=F,
                     remove_separators=T,
                     remove_symbols=T,)
mixer_tokens

#############################################################
#slide 33
mixer_tokens_lower<-tokens_tolower(mixer_tokens)
mixer_tokens
mixer_tokens_lower

#############################################################
#slide 35

#we need to use quanteda package here
library(quanteda)
stopwords("english")[1:20]
stopwords("english")[1:200]
stopwords("korean")[1:200]
stopwords("spanish")[1:200]
stopwords("chinese")[1:200]
stopwords("zh", source="misc")[1:200]

?stopwords_getlanguages

mixer_tokens_lower_stop<-tokens_remove(mixer_tokens_lower, stopwords("english"))
mixer_tokens_lower_stop

#you can also remove individual words
mixer_tokens_lower_stop <- tokens_remove(mixer_tokens_lower_stop, c("c", "stars"))
mixer_tokens_lower_stop

#############################################################
#slide 37
library(quanteda)

mixer_tokens_lower_stop_stem<-tokens_wordstem(mixer_tokens_lower_stop)
mixer_tokens_lower_stop_stem


#############################################################
#slide 39
library(quanteda)

mixer_dfm<-dfm(mixer_tokens)
mixer_dfm

mixer_dfm_proc<-dfm(mixer_tokens_lower_stop_stem)
mixer_dfm_rare_freq <- dfm_trim(mixer_dfm_proc, 
                min_docfreq = 0.005, 
                max_docfreq = 0.99, 
                docfreq_type = "prop")

mixer_dfm #1493 features
mixer_dfm_rare_freq #1204 features

df<-convert(mixer_dfm_rare_freq, to="data.frame") #can convert it as a data frame


#############################################################
#slide 41

#to find out how many documents the corpus contains
ndoc(mixer_dfm_rare_freq)

#how many feartures the corpus contains
nfeat(mixer_dfm_rare_freq)

#which features occur how many times
mixer_features<-as.data.frame(featfreq((mixer_dfm_rare_freq)))
colnames (mixer_features)
#just cleaning up the column name because it is messay
names(mixer_features)[names(mixer_features) == 'featfreq((mixer_dfm_rare_freq))'] <- 'freq'
#to change row names to the actual variable
#install.packages("tibble")
library(tibble)
mixer_features<-tibble::rownames_to_column(mixer_features, "feature")
#install.packages("dplyr")
library(dplyr)
mixer_ordered<-mixer_features %>% arrange(desc(freq))
head(mixer_ordered)

#frequent features
topfeatures(mixer_dfm_rare_freq, 20)

#10 least frequent features
topfeatures(mixer_dfm_rare_freq, decreasing=F, 20)

#############################################################
#Slide 43
#creating a wordcloud
#install.packages("quanteda.textplots")
library(quanteda.textplots)
textplot_wordcloud(mixer_dfm_rare_freq, max_words=50, color="grey10")
?textplot_wordcloud #if you want to do a more sophisticated word cloud, read this

#############################################################

#Slide 45
mixer_fcm<-fcm(mixer_dfm_rare_freq)
mixer_fcm
#Not very useful for our data - but it may be useful for other types of data

#You can also draw plots
#it wouldn't work for our data, but here is the code
mixer_fcm_plotdata <- fcm_select(mixer_fcm, 
                  pattern = c("tanmay", 
                              "ankita", "yoda", 
                              "sangeeta"),
                  selection = "keep")
textplot_network(mixer_fcm_plotdata)

#You can also convert it into a data frame to use for your analysis
df<-convert(mixer_fcm_plotdata, to="data.frame")













