#Importing libraries
library('tidyverse')

#Enter the path and name of your corpus from Twitter between the quotation marks.
corpus_fn <- "~/need_passives.csv"
#Enter the path where you've stored the folder 'normsFiles'
norms_path <- "~/Dissertation"
#Enter the path and name for the file with norms assigned.
destination_fn <- "~/normsFiles"


#Reading the corpus
txtf <- read_csv(corpus_fn) #importing the dataset
txtf <- txtf %>% filter(txtf$voice == 'passive') %>% select("text", "screenName", "City", "form", "variety")
txtf <- txtf%>%add_column(tweetID = 1:NROW(txtf), .before = 'text')


######################----Data Cleaning -----###################################

#clearing tweets of alphanumeric
txtf$text <- gsub("[A-Za-z]+[0-9]+|[0-9]+[A-Za-z]+","",  txtf$text)
#clearing tweets of numbers
txtf$text <- gsub("[0-9]+","",  txtf$text)
#clearing tweets of tweet handles
txtf$text <- gsub("[A-Z]*[a-z]+[A-Z]+[a-z]+[A-Z]*|[a-z]+[A-Z]+|[A-Z]+[a-z]+","",  txtf$text)
#clearing tweets consisting of words more than 12
txtf$text <- gsub("[a-zA-Z]{11,20}","",  txtf$text)

#reading the semantic norm ratings from files 
cnorms <- as_tibble(read.csv(paste(c(norms_path, "normsFiles", "ConcretenessNorms.csv"), collapse="/")))
aoanorms <- as_tibble(read.csv(paste(c(norms_path, "normsFiles", "aoaNorms.csv"), collapse="/")))
aoanorms <- rename(aoanorms, aoa_rating = Rating.Mean)
vadnorms <- as_tibble(read.csv(paste(c(norms_path, "normsFiles", "vadNorms.csv"), collapse="/")))


#############-------Tokenisation and lemmatisation-----#########################

fullText <- txtf %>%  # this tokenises text into a 'word' column , preserving other columns
  unnest_tokens(word, text)

# Performing lemmatisation on the tokens
fullText <- fullText %>%
  mutate(word = textstem::lemmatize_words(word))

# adding the norm ratings to the corpus
fullText1 <- right_join(fullText, cnorms, by = "word" )
fullText2 <- left_join(fullText1, aoanorms, by = "word")
fullText3 <- left_join(fullText2, vadnorms, by = "word")
wordByRow <- fullText3 %>% drop_na(form)


####################-------Stop word remove----#################################
# Remove stopwords (tidytext version which combines three lists, n = 1149)
data(stop_words)
#adding custom stop words
add_stopw <- as.data.frame(c("need", "needed", "needs", "needing"), stringsAsFactors = FALSE)
colnames(add_stopw) <- "word"
#Removing stop words
wordByRow <- wordByRow %>%
  anti_join(stop_words, by = "word")%>%
  anti_join(add_stopw, by = "word")

wordByRow <- tibble::rowid_to_column(wordByRow, "index")

#Performing undersampling through stratified sampling
wbr_strat <- wordByRow 

#############-----Taking care of missing values using MICE-------##############£

summary(wordByRow )
#Finding % of missing data
p <- function(x) {sum(is.na(x))/length(x)*100}
apply(wordByRow , 2, p)
#Visualize the missing data
md.pattern(wordByRow)
#Graphically seeing the missing values
mice_plot <- aggr(wordByRow [7:11], col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(wordByRow [7:11]), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

temp <- mice(wordByRow [,8 : 12], m=3, maxit=50,meth='pmm',seed=500)
completedData <- complete(temp,1)

#Imputed dataset
wordByRow [8 : 12] <- as.data.frame(completedData)
wbr <- wordByRow  
