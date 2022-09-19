
#importing data
setwd("/Users/dasomlee/Desktop/Data/STP702") #macbook
getwd()
df<-read.csv("immprotestdata.csv", header=T, na.strings=c("","NA")) #If blank, make it NA
#if you don't know your working directory
    #mac users: open "terminal" and get your working directory from there
    #window users: your file location should be available from my computer
#DATA FILES SHOULD ALWAYS BE CSV FILES, NOT EXCEL FILES

#####################################################################
#UNDERSTAND YOUR DATA
#Slide 22

head(df) #first six observations
tail(df) #last six observations
colnames(df) #lists column names # how have the variable names changed?
is.na(df) #are there NAs --> too complicated to read 
table(is.na(df)) #are there NAs --> much easier to read
table(df$Duration.Mean.) #understand each variable
mean(df$Duration.Mean.)
median(df$Duration.Mean.)
min(df$Duration.Mean.)
max(df$Duration.Mean.)
sd(df$Duration.Mean.) #what is standard deviation? #why is understanding standard deviation important? 
str(df)
str(df$Duration.Mean.)
summary(df$Duration.Mean.)

#subsetting data
df_IWFRonly<-subset(df, df$ImmigrantWorkersFreedomRide>0) #new dataset created

#PRACTICE
#Find the mean, median, min, max, sd of the NativityForeign variable
#Write one sentence explaining what each of the values mean

#####################################################################
#UNDERSTAND YOUR DATA
#Slide 28
num_data<-c(3,7,2)
num_data

class(num_data)

#numeric series with decimals
num_data_decimals<-c(3.4, 7.1, 2.9)
num_data_decimals

str(num_data)
str(num_data_decimals)

#creating a data frame
num_data_decimals<-data.frame(3.4, 7.1, 2.9)
num_data_decimals

#####################################################################
#UNDERSTAND YOUR DATA

#Slide 30
num_data<-c(3,7,2)
num_data

class(num_data)

num_data_int<-as.integer(num_data)

class(num_data_int)

num_data_decimals<-c(3.4, 7.1, 2.9)
num_data_decimals

num_data_decimals<-c(3.4, 7.1, 2.9)
num_data_decimals

class(num_data_decimals)

num_data_decimals_int<-as.integer(num_data_decimals)
num_data_decimals_int
class(num_data_decimals_int)

#####################################################################
#UNDERSTAND YOUR DATA

#Slide 32
char<-"some text"
char
class(char)

num_data<-c(3,7,2)
num_data
class(num_data)

num_data_char<-as.character(num_data)
num_data_char
is.numeric(num_data_char)

#####################################################################
#UNDERSTAND YOUR DATA

#Slide 34
gender <- factor(c("female", "female", "male", "female", "male"))
gender

levels(gender) #to find out different levels

#levels are sorted alphabetically. If you want to change the levels

gender <- factor(gender, levels = c("male", "female"))
levels(gender)

#practice
text <- c("test1", "test2", "test1", "test1") # create a character vector
class(text) # to know the class

#####################################################################
#UNDERSTAND YOUR DATA
#slide 36

#we want to create a new variable 
#we have data on how many times Knightley ate in one day
#here is data on how many times Knightley ate in one day
Knightley_foodfrequency<-c(4,3,3,4,3,3,3,2,2,3,4,4,2,3,4,5,2,2,4)
#we can create new variables by using the dollar sign 

#now that we have new dataset with the new variable added, 
#we want to export the data
write.csv(file="Cat", "cat.csv")

#####################################################################
#UNDERSTAND YOUR DATA
#slide 45

fruits <- c("Apple", "Banana", "Orange", "Lemon")
price <- c(0.1,0.55,0.78,0.4)
color <- c("red", "yellow", "orange", "yellow")

data_fruits <- data.frame("fruits"= fruits,
                          "price" = price,
                          "color" = color)
data_fruits

#####################################################################
#UNDERSTAND YOUR DATA
#slide 46

list_cat_fruit<-list(data_fruits,Cat)
#the first element encompasses the data frame fruits
#the second element encompasses the data frame Cat

#####################################################################
#UNDERSTAND YOUR DATA
#slide 48

word <- "hello"
word

word <- "bye"
word

numbers <- c(1:20)
numbers

numbers <- numbers + 1 #add 1 to all observations
numbers

numbers[2] <- 100 #replace the second observation with 100
numbers 

