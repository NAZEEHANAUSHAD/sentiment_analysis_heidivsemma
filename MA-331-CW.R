#loading libraries
library(stringr)
library(dplyr)
library(tidytext)
library(textdata)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(word)
library(ggthemes)

#load and view data , child and adult

child_data <- read.csv("Heidi.csv")

adult_data <- read.csv("Emma.csv")

#view data
head(child_data , 50)
#literature doesnt start till row 50
#trial and error with indexing to find start of literature
child_data[1:100,]
#chapter 1 starts from row 78
#chceking if last chapter is at the end of the data 
tail(child_data,50)
#it is
#checking size of data
dim(child_data)
#possible cleanings
#empty rows

#start data from row 78





head(adult_data , 30)
#literature starts from line 8 
dim(adult_data)
#16235 rows
#possible cleaning 
#empty rows
#till rows 8 , from where the actual literature starts


#initial observations
#the size of the literature , a very obvious feature
#possible cleaning common :- Chapter names ? (all_caps) str_detect any string with character (chapter)
#headings have the character "CHAPTER" , detect and drop
child_data[500:600,]
#looking for other anomalies
#common cleaning
#punctuations and qoutation marks , lowercase etc , see(unnest_tokens)


#convert empty strings to convert them into NA's and drop them
#tried using str_detect(data , "" ) , but then it would return every character vector
child_data <- child_data %>%
  mutate(text = replace(text , text == "" , NA))

adult_data <- adult_data %>%
  mutate(text = replace(text , text == "" , NA))



#saving literature in seperate variable
child_lit <- child_data[-c(1:77) , ]
#dropping the NA's
child_lit <- na.omit(child_lit)
#check length
dim(child_lit)
#8404

adult_lit <- adult_data[-c(1:7) , ]
adult_lit <- na.omit(adult_lit)
dim(adult_lit)
#13681




#child data 
#detect headings , save as seperate and drop from literature 
#every heading has the character "CHAPTER" in it 
sum(str_detect(child_lit$text , "CHAPTER"))


#chceking the pattern of headings
head(child_lit)

#checking if 23 chapters
str_which(child_lit$text , "CHAPTER XXIII")
#returns nothing , roman numerals have a full stop after them
str_which(child_lit$text , "CHAPTER XXIII.")
#row 7686
#checking manually
child_lit[7800:7900]
# it is the last chapter
str_which(child_lit$text , "CHAPTER")

#storing headings seperatly
child_heads <- child_lit[c(1 ,455 , 715, 1165, 1615, 1963, 2224, 2695, 3042, 3233, 3545, 3702,
                         4068, 4597, 5026, 5241, 5529, 5804, 6155, 6405, 6935, 7208, 7686) , ]

#removing headings from the literature

final_child <- child_lit[-c(1 ,455 , 715, 1165, 1615, 1963, 2224, 2695, 3042, 3233, 3545, 3702,
                           4068, 4597, 5026, 5241, 5529, 5804, 6155, 6405, 6935, 7208, 7686) , ]
#now this only has the text of the actual story 
  
#there are unnecessary backlashes during quotations in some of the texts , but we won't worry about that now as it 
#will not effect our analysis , punctuation will be removed during tokenization

#adult data
head(adult_lit)

#headings 
sum(str_detect(adult_lit$text , "CHAPTER"))
# 38 CHAPTERS
#checking indexes of chapter headings
str_which(adult_lit$text , "CHAPTER")

#storing headings
adult_heads <- adult_lit[c( 2 ,  279  , 425 ,  585  , 879,  1046 , 1282 , 1491 , 1851 , 2277  ,2501 , 2681 , 2962 , 3222 , 3426 ,
                            3707 , 3870 , 3975 , 4190 , 4434,  4640 , 4966 , 5109 , 5398  ,5657,  5850  ,6409,  6676  ,6856 , 7139,  7332,
                            7496  ,7824,  8081,  8352,  8537,  8779 , 8885,  9265 , 9409  ,9590,  9847, 10288, 10576, 10803, 11014, 11294,
                            11663, 11914 ,12203, 12501, 12706 ,12992 ,13253, 13575) , ]

#volume titles
sum(str_detect(adult_lit$text , "VOLUME"))
str_which(adult_lit$text , "VOLUME")

#dropping headings and volumes from the literature text
final_adult <- adult_lit[-c( 1, 2 ,  279  , 425 ,  585  , 879,  1046 , 1282 , 1491 , 1851 , 2277  ,2501 , 2681 , 2962 , 3222 , 3426 ,
                             3707 , 3870 , 3975 , 4190 , 4434,  4640 , 4966 , 5109 , 5398  ,5657,  5850  ,6409,  6676  ,6856 , 7139,  7332,
                             7496  ,7824,  8081,  8352,  8537,  8779 , 8885,  9265 , 9409  ,9590,  9847, 10288, 10576, 10803, 11014, 11294,
                             11663, 11914 ,12203, 12501, 12706 ,12992 ,13253, 13575 , 4189 , 8778) , ]
#we wont worry about the wierd punctuation marks as these will be removed during tokenization
#now there is only literature text 


####COMPARITIVE ANALYSIS



#defining a tidy data frames and also removing stopwords using anti-join
tidy_child <- final_child %>%
  unnest_tokens(word , text) %>%
  anti_join(stop_words) 
dim(tidy_child)
#viewing the words that occur most frequently
tidy_child %>%
  count(word , sort = TRUE)%>% #counting  the words and sorting it high to low
  head(10)


#making a tidy dataframe for adults as well , and removing stop words
tidy_adult <- final_adult %>%
  unnest_tokens(word , text) %>%
  anti_join(stop_words)
dim(tidy_adult)
#viewing most frequent words
tidy_adult %>%
  count(word , sort = TRUE) %>% 
  arrange(desc(n)) %>%
  head(10)

#binding together the tidy data , and forming a new variable to indicate the book type using mutate() function
tidy_data <- bind_rows(mutate(tidy_child , book = "Child"),
                       mutate(tidy_adult , book = "Adult")) %>%
  mutate(word = str_extract(word , "[a-z']+"))   #extracting only words

#checking dimension
dim(tidy_data)

#viewing top 10 of most frequent words
tidy_data %>%
  count(book , word) %>%
  arrange(desc(n)) %>%
  head(n=10)


#getting nrc lexicon for sentiment analysis 

nrc <- get_sentiments("nrc") %>%
  select(word , sentiment)  #selecting the two variables nrc and sentiment from the nrc data frame
head(nrc , 10)

tidy_data %>%
  inner_join(nrc , by = "word") %>%    #joining the nrc words using inner_join() , matching by words that existing in both
  select(book , word , sentiment) %>%  #viewing books word and sentiment 
  sample_n(100)                       # a sample of 100 words

#saving sentiment counts
sentiment_counts <- tidy_data %>%
  left_join(nrc , by = "word") %>%
  count(book , sentiment) %>%
  spread(book, n) %>%
  mutate(sentiment = replace_na(sentiment , replace = "none"))    #replacing words with no sentiments as "none"
sentiment_counts


#plotting sentiments occuring in EMMA 
adult_sentplot <- sentiment_counts %>%
  filter(sentiment != "none") %>%    #Filtering out the "none"s as they make the graph highly skewed and are not useful for the analysis
  ggplot(aes(x = sentiment , y= Adult  , color = sentiment)) + geom_col() +
  labs(title = "Column graph showing sentiments in Emma",
       x = "Sentiment" , 
       y = "Frequency",
       color = "Sentiments") +
  theme_dark()+
  geom_text(aes(label = Adult))
  
  
adult_sentplot

#PLOTTING SENTIMENTS IN HEIDI
child_sentplot <- sentiment_counts %>%
  filter(sentiment != "none") %>%    #filtering out none's
  ggplot(aes(x = sentiment , y = Child , color = sentiment)) + geom_col() +
  labs(title = "Column graph showing sentiments in Heidi",
       x = "Sentiment",
       y= "Frequency",
       color = "Sentiments") +
  theme_dark() +
  geom_text(aes(label = Child))
child_sentplot
grid.arrange(adult_sentplot , child_sentplot , ncol = 2)


#these numbers speak to the kind of writer Jane Austen was.
#Her books were filled , adventorous , exciting events for a witty heroin 
#we can take a look at the words used for joy , positive , anticipation etc , in both the works and see a difference 


#Getting only the joy sentiment from nrc lexicon
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

child_joy <- tidy_data %>%
  filter(book == "Child") %>%  #filtering only the children's book
  inner_join(nrc_joy) %>%
  count(word , sort = TRUE) %>%
  head(10) %>%
  ggplot(aes(x = reorder(word,n) , y = n)) + geom_col(fill = "#523e5e")+      #plotting column graph
  labs(title = "Heidi") +
  coord_flip()+
  theme_fivethirtyeight()
child_joy


adult_joy <- tidy_data %>%
  filter(book == "Adult") %>%      #filtering adult's boook
  inner_join(nrc_joy) %>%
  count(word , sort = TRUE) %>%
  head(10) %>%
  ggplot(aes(x= reorder(word ,n) , y = n)) + geom_col(fill = "#917946")+
  labs(title = "Emma") +
  coord_flip() +
  theme_fivethirtyeight()
adult_joy
#words refer to the central theme of the story os healing and restoration
#the theme of the word seems more appropriate for audience children 
# like mountain , sun , happy , delight , mother ,  these words do remind us of a jouyous child 
#up in the mountains , which prettu much is the story heidi
#words to story , her characters have extravagant balls and is often centered around finding 
#logical love
#pretty , hope , ready , 


##looking at fear
#getting words with sentiment fear from nrc lexicon
nrc_fear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")

child_fear <- tidy_data %>%
  filter(book == "Child") %>%
  inner_join(nrc_fear) %>%
  count(word , sort=TRUE) %>%
  head(10) %>%
  ggplot(aes(reorder(word,n)  , n)) + geom_col(fill = "#523e5e") +
  labs(title = "Heidi") +
  coord_flip() +
  theme_fivethirtyeight()
child_fear

adult_fear <- tidy_data %>%
  filter(book == "Adult") %>%
  inner_join(nrc_fear) %>%
  count(word , sort=TRUE) %>%
  head(10) %>%
  ggplot(aes(reorder(word,n)  , n)) + geom_col(fill = "#917946")+
  labs(title = "Emma") +
  coord_flip() +
  theme_fivethirtyeight()
adult_fear

#analysis took into account 
#audience intended
#type of writers
#writing styles 

###### BING ANALYSIS TO COMPARE RATIOS OF POSITIVE TO NEGETIVES
#Joining "bing" words to our "text" words 
tidy_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(book ,sentiment) #counting


##### to plot
#reference link
#http://www.j-asc.com/gallery/66-november-1233.pdf 
#code found during background research 
#complete credit for the source , will be citing in the report as well , no credit claimed. 
tidy_data %>%
     group_by(book) %>%
     mutate(word_count = 1:n(),
            index = word_count %/% 500 + 1) %>%      #index - the words are chosen at an interval of 500 words , that is 
     inner_join(get_sentiments("bing")) %>%         #approximately every 2 pages
     count(book, index = index , sentiment) %>%
     ungroup() %>%
     spread(sentiment, n, fill = 0) %>%             #spreading a key value between variables
     mutate(sentiment = positive - negative) %>%    #calculating net sentiment in each index
     ggplot(aes(index, sentiment , fill = book)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free_x")

### no clear differences to be seen , however positives are more



