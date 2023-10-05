# Import word and sentence vectors, word weights and stop words from section I (if not available)

if(!exists("wv_df") & scope!='BERT'){
#load word vectors 'wv_df' from:
src_wv<-paste0(data.loc,scope.prefix,"_","wv_df.Rda")
load(file = src_wv)
}

if(!exists("sv_wv_df")){
#load (weighted) document vectors 'sv_wei_df' from:
src_sv<-paste0(data.loc,scope.prefix,"_","sv_wei_df.Rda")
load(file = src_sv)
}

if(!exists("tfidf_wrd")){
#load word tfidf 'tfidf_wrd' from:
src_tfidf<-paste0(data.dir,"tfidf_wrd.Rda")
load(file = src_tfidf)
}

if(!exists("pos_df")){
#load POS of unique words' from:
src_pos<-paste0(data.dir,"pos_df.Rda")
pos_df <- readRDS(file = src_pos)
} 

if(!exists("perception_doc")){
#load subjectivity doc 'perception_doc' from:
src_perception<-paste0(data.dir,"perception_doc.Rda")
load(file = src_perception)
}

if(!exists("subjectivity_df")){
#load subjectivity words 'subjectivity_df' from:
src_subjectivity<-paste0(data.dir,"subjectivity_df.Rda")
load(file = src_subjectivity)
}

if(!exists("stopwords_df")){
#load stop words 'stopwords_df' from:
src_stopwords<-paste0(data.dir,"stopwords_df.Rda")
load(file = src_stopwords)
}

#-----------------------------------------------------------------------------------------------

#subjectivity unique words 
#temporary solution 

subjectivity_wrd <- subjectivity_df %>% 
  group_by(word) %>% 
  summarise(subjectivity=mean(as.numeric(subjectivity)))

if(scope!='BERT') {
#preparing for dimension reduction 
#remove low-incidence, unimportant words first
train_dr_df <- wv_df %>%
  #word weight (tfidf)
  left_join(tfidf_wrd, by = "word") %>%
  #word subjectivity
  left_join(subjectivity_wrd) %>%
  #pos
  merge(pos_df, by.x = "word", by.y="token",all.x = TRUE,all.y = FALSE) %>%
  filter(upos %in% pos_list) %>%
  #pruning
  #min frequency / incidence (highest 98%)
  filter(total_sum > quantile(total_sum, 0.02,na.rm = TRUE)) %>%
  #min importance (highest 95%)
  filter(tfidf_mn > quantile(tfidf_mn, 0.05,na.rm = TRUE)) 

#get vector dimension names (for filtering data frames)
#dimensions of a vector start with V
cols<-colnames(train_dr_df[ , grepl( "V" , names(train_dr_df) ) ])

#n-dimensional vector space
(vec_dim <- length(cols))
} else {
 
  #BERT sentence embeddings
  
  #dimension of features
  vec_dim <- ncol(sv_wei_df)
  
  #get vector dimension names (for filtering data frames)
  #dimensions of a vector start with V
  cols<-colnames(sv_wei_df[ , grepl( "V" , names(sv_wei_df) ) ])
  
  #by-pass dimension reduction of words
  lvl_start <- 2
  lvl_nme <- 'sentence'
}