
#-----------------------------------------------------------------------------------------------

# T2 WORD and DOCUMENT EMBEDDING -> Word2Vec

#-----------------------------------------------------------------------------------------------


# Google's Word2Vec
# Builds a Word2Vec model and word vectors

# Word2Vec takes texts as training data for a neural network. 
# The resulting embedding captures whether words appear in similar contexts. 

# Word2Vec is a predictive model

#-----------------------------------------------------------------------------------------------

#vectorize all words except stopwords and low-incidence words, but keep unimportant words for now

if(w2v_pretrained == TRUE) {

message("Applying pretrained Word2Vec model...")
model_w2v <- word2vec::read.word2vec(path_model_pretrained) 

} else {

  message("Computing embeddings based on Word2Vec algorithm...")
  # use window of 5 for context words  
  model_w2v <- word2vec::word2vec(x = text_normalised$text, dim = vec_dim, iter = iterations_n, type = prediction_method, 
                                  window = context_window, min_count = word_freq
                                  ,stopwords = stopwords_df$word 
                                  #splitting regex:  first element indicates how to split words and the second element 
                                  #indicates how to split sentences in x
                                  ,split = c(" \n,.-!?:;/\"#$%&'()*+<=>@[]\\^_`{|}~\t\v\f\r", ".\n?!")    
                                  ,threads=thread)
  
  #save model 
  word2vec::write.word2vec(model_w2v, file = path_model) 
   
}

#get vocabulary
vocab <- summary(model_w2v, type = "vocabulary")

# word-vector matrix
emb <- as.matrix(model_w2v)
head(emb,5)

#----------------------------------------------------------------------------------------------

wv_df <- as.data.frame(emb)
wv_df$word<-row.names(wv_df)

#save word vectors data frame
save(wv_df,file=paste0(data.loc,scope.prefix,"_","wv_df.Rda"))

#garbage collection
gc()