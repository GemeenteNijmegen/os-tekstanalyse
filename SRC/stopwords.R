
#------------------------------------------------------------------------------

#Stopwords

#------------------------------------------------------------------------------

#custom stopwords (extension)
stopwords.loc<-paste0(data.dir, "LEXICON/", "stopwords_custom.txt")

#@stopwords_cus<- readr::read_tsv(stopwords.loc,col_names = TRUE)

stopwords_cus_df<-readr::read_tsv(stopwords.loc,col_names = FALSE) 
colnames(stopwords_cus_df)[1]<-'word'

stopwords_manual_df<-stopwords_manual %>% 
              as.data.frame() %>% 
              `colnames<-`(c("word"))

#compile stopwords list
#base - stopwords_remove + stopswords_manual
stopwords_df<-unique(rbind(
  #stopwords_tm, 
  stopwords_cus_df, stopwords_manual_df)) %>%
  filter(!word %in% stopwords_remove)

save(stopwords_df,file=paste0(data.dir,"stopwords_df.Rda"))
#rm(stopwords_cus)