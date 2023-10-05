
#-----------------------------------------------------------------------------------------------

#Sentence embedding (weighted) 
#actually doc embedding

#-----------------------------------------------------------------------------------------------

#merge tf-idf weights with word vectors
wv_tfidf_pre<-merge(x=tfidf_df,y=wv_df,by="word",all.x=TRUE)
#merge with Part of Speech tagging (POS)
wv_tfidf_pos<-merge(x=wv_tfidf_pre,y=pos_df,by.x="word",by.y="token",all=TRUE)

#pruning: filter out meaningless and unimportant words (noise reduction)
wv_tfidf_df<-wv_tfidf_pos %>% 
  #POS filtering in 'tokenisation annotation.R'
  #filter(!upos %in% c("PUNCT", "SYM", "DET", "ADP", "NUM", "AUX")) %>%
  #min importance
  #filter(tfidf < quantile(tfidf, 0.95,na.rm=TRUE)) %>%  
  #min frequency
  filter(total > quantile(total, 0.05,na.rm=TRUE)) 

#garbage collection
rm(wv_tfidf_pre)
#Note: we have multiple instances of the same word belonging to different 
#text segments as records

#create indices (as vector)
#weights
wei_vec<-as.vector(wv_tfidf_df$tfidf)
#words
word_vec<-as.vector(wv_tfidf_df$word)

#doc id
doc_vec<-as.vector(wv_tfidf_df$doc_id)

# select all columns starting with name V% (aka vector dimensions)
wv_tfidf_dim<-wv_tfidf_df %>%
  dplyr:: select(starts_with("V"))  

cols<-colnames(wv_tfidf_dim)

# apply tf-idf weight to word vectors
wv_wei_df<-wv_tfidf_dim * rep(wei_vec, rep.int(nrow(wv_tfidf_df), length(wei_vec)))

#alternative method
#wv_wei_df<-mapply("*",as.data.frame(wv_tfidf_dim),wei_vec)

#re-assign (append) words and ids 
wv_wei_df$word<-word_vec
wv_wei_df$doc_id<-as.numeric(doc_vec)

#equation for sentence embedding:
#((word-embedding woord1 * tf.idf gewicht woord1) + (word-embedding woord2 * tf.idf gewicht woord2)) 
#/ (tf.idf gewicht woord1 + tf.idf gewicht woord2)= tf.idf gewogen wordembedding op zinsniveau.  

#upper part of equation 
counter_df<-wv_wei_df %>%
  group_by(doc_id) %>%   
  summarise_at(vars(all_of(cols)),              
               list(name=sum), na.rm=TRUE) 

names(counter_df)<-gsub("[_]name","",names(counter_df))  #remove "_name" suffix 

#lower part of equation 
denom_prt1_df<-tfidf_df %>%
  group_by(doc_id, word) %>%   
  summarise(mean_tfidf=mean(tfidf))

denom_df<-denom_prt1_df %>%
  group_by(doc_id) %>% 
  summarise(sum_tfidf=sum(mean_tfidf))

st<-merge(counter_df,denom_df,by="doc_id")

#garbage collection
rm(counter_df,denom_prt1_df,denom_df)

#(weighted) sentence embeddings  !!!       
sv_wei_df<-(st[,cols] / st[,"sum_tfidf"] )

sv_wei_df$doc_id<-as.numeric(st$doc_id)

sv_wei_df<-column_to_rownames(sv_wei_df, var="doc_id")

sv_wei_df$doc_id<-as.numeric(rownames(sv_wei_df))

sv_wei_df<-sv_wei_df %>%
#document contains information 'value'
subset(doc_id %in% vec_relevant_docs)
  
head(sv_wei_df,5)         

#save sentence vectors
save(sv_wei_df,file=paste0(data.loc,scope.prefix,"_","sv_wei_df.Rda"))

#garbage collection
rm(pos_df)