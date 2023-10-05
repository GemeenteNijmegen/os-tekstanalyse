
#-----------------------------------------------------------------------------------------------

#Lexicons

#-----------------------------------------------------------------------------------------------

#load lexicons
list_lexicons=list.files(lexicon.dir,recursive=TRUE)
search_lexicons="*.rda"
present_lexicons=grepl(search_lexicons,list_lexicons)

lexicons_missing <- all(present_lexicons == FALSE)

#lexicons_missing <- TRUE #force update of lexicons (last update May 10th 2021)

if(lexicons_missing==TRUE){
  #sentometrics: An Integrated Framework for Textual Sentiment Time Series Aggregation and Prediction
  load(file("https://github.com/SentometricsResearch/sentometrics/raw/master/data-raw/valence-raw/valShifters.rda"))
  save(valShifters,file=paste0(lexicon.dir,"valShifters.Rda"))
  
  load(url("https://github.com/SentometricsResearch/sentometrics/raw/master/data-raw/FEEL_nl_tr.rda"))
  save(FEEL_nl_tr,file=paste0(lexicon.dir,"FEEL_nl_tr.Rda"))
  
  #Pattern NLP: web mining module for Python
  sentiment_patternnl <- read_xml(
    "https://raw.githubusercontent.com/clips/pattern/master/pattern/text/nl/nl-sentiment.xml"
  ) %>% 
    as_list() %>% 
    .[[1]] %>% 
    map_df(function(x) {
      tibble::enframe(attributes(x))
    }) %>% 
    dplyr::mutate(id = cumsum(str_detect("form", name)))  %>% 
    unnest(value) %>% 
    pivot_wider(id_cols = id) %>% 
    dplyr::mutate(form = tolower(form),
           polarity = as.numeric(polarity),
           subjectivity = as.numeric(subjectivity),
           intensity = as.numeric(intensity),
           confidence = as.numeric(confidence))
  save(sentiment_patternnl,file=paste0(lexicon.dir,"sentiment_patternnl.Rda"))
} else {
  load(paste0(lexicon.dir,"valShifters.Rda"))
  load(paste0(lexicon.dir,"FEEL_nl_tr.Rda"))
  load(paste0(lexicon.dir,"sentiment_patternnl.Rda"))
} 

#-----------------------------------------------------------------------------------------------
#Compile lexicon

#Subjectivity

#Nederlandse Taal Unie
#import (enriched) lexicon of word subjectivity
#covers most important and pronounced words (10k+ words)
src_subjectivity<-paste0(data.dir,"LEXICON/", "subjectivity.xlsx")

subjectivity_srce <- 
  read_excel(src_subjectivity) %>%
  filter(!is.na(subjectivity)) %>%
  as_tibble() %>% 
  dplyr::mutate(subjectivity = as.numeric(subjectivity),
         word = tolower(word)
  ) %>%
  select(-upos)

#------------------------------------

#Syuzhet package
#add additional words from nrc lexicon 
nrc_ditionary <- get_sentiment_dictionary(dictionary = "nrc", language = "dutch") %>%
  filter(!is.na(word)) %>%
  select(word) %>%
  distinct(word)

words_nrc <- nrc_ditionary[!nrc_ditionary$word%in%subjectivity_srce$word,]
words_nrc$word <- tolower(words_nrc$word)

#get sentiment of these additional words and use it as proxy for subjectivity 
nrc_word_vector <- get_sentiment(words_nrc$word, method="nrc", language = "dutch")
words_nrc$sentiment<-nrc_word_vector

#single words only
words_nrc$nwords <-str_count(words_nrc$word, pattern = " ")

words_nrc <- words_nrc %>%
  filter(nwords==0) %>%
  select(-nwords) %>%
  dplyr::mutate(subjectivity=sentiment*3.33,subjectivity=round(subjectivity,0)) %>%
  select(-sentiment)

subjectivity_raw_df <- rbind(subjectivity_srce,words_nrc)
rm(subjectivity_srce)

#-----------------------------------------------------------------------------------------------
#Polarity 

#Sentometrics
#Polarity from Sentometrics lexicon
words_sm <- FEEL_nl_tr

#single words only
words_sm$nwords <-str_count(words_sm $x, pattern = " ")
words_sm$x <- stringi::stri_trans_general(words_sm$x, 'latin-ascii')

words_sm <- words_sm %>%
  filter(nwords==0) %>%
  select(-nwords) %>%
  rename (word=x, polarity=y)

subjectivity_raw_df <- full_join(subjectivity_raw_df,words_sm,by="word")
subjectivity_raw_df$sentiment <- NA

#-----------------------------------------------------------------------------------------------
#Pattern
#Polarity and subjectivity from pattern nlp lexicon (Python library)

words_pt <- sentiment_patternnl
words_pt$word <- stringi::stri_trans_general(words_pt$form, 'latin-ascii')

words_pt <- words_pt %>%
  rename (polarity_cont=polarity, subjectivity_cont=subjectivity)

#polarity dichotomous
words_pt$polarity <- 0
words_pt$polarity[words_pt$polarity_cont>0] <- 1
words_pt$polarity[words_pt$polarity_cont<0] <- -1

#subjectivity (adjust range and loading)
#words_pt$subjectivity <- 0

words_pt <- words_pt %>%
  dplyr::mutate(subjectivity = subjectivity_cont*10*polarity, sentiment=polarity_cont*10) %>%
  filter(subjectivity!=0) %>%
  group_by(word) %>%
  summarise(subjectivity_mn=round(mean(subjectivity),0), polarity_first=first(polarity), sentiment_mn=round(mean(sentiment),0)) %>%
  rename(subjectivity=subjectivity_mn,sentiment=sentiment_mn, polarity=polarity_first)

subjectivity_raw_df <- rbind(subjectivity_raw_df,words_pt)

subjectivity_raw_df$polarity[is.na(subjectivity_raw_df$polarity) & subjectivity_raw_df$subjectivity>-1] <- 1
subjectivity_raw_df$polarity[is.na(subjectivity_raw_df$polarity) & subjectivity_raw_df$subjectivity<0] <- -1

subjectivity_df<- subjectivity_raw_df %>%
  group_by(word) %>%
  summarise(subjectivity_mn=round(mean(subjectivity),0), sentiment_mn=round(mean(sentiment),0),  polarity_first=first(polarity)) %>%
  rename(subjectivity=subjectivity_mn,sentiment=sentiment_mn, polarity=polarity_first)

#get sentiment of these additional words 
nrc_word_vector <- get_sentiment(subjectivity_df$word, method="nrc", language = "dutch")
subjectivity_df$sentiment_nrc<-nrc_word_vector
subjectivity_df$sentiment_nrc<-subjectivity_df$sentiment_nrc*3.33

subjectivity_df$sentiment <- ifelse(is.na(subjectivity_df$sentiment) & subjectivity_df$sentiment_nrc!=0, subjectivity_df$sentiment_nrc, subjectivity_df$sentiment)
subjectivity_df$sentiment <- ifelse(subjectivity_df$sentiment > 9.99, 10, subjectivity_df$sentiment)
subjectivity_df$sentiment <- ifelse(subjectivity_df$sentiment < -9.99, -10, subjectivity_df$sentiment)
save(subjectivity_df,file=paste0(lexicon.dir,"subjectivity_df.Rda"))

#unload objects from memory
objects_to_remove <- c("nrc_ditionary","sentiment_patternnl","subjectivity_raw_df","words_nrc", "words_sm", "words_pt")
remove_objects(objects_to_remove)