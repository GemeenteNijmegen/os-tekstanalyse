
#-----------------------------------------------------------------------------------------------

# Word similarity and -association

#-----------------------------------------------------------------------------------------------

message("Word similarity and -association...")

if(scope=="WORD2VEC") {
#Word2Vec
  
vectors<-emb[predict_words, ]
vectors<-rbind(vectors, avg = colMeans(vectors))
cw<-(predict(model_w2v, vectors, type = "nearest", top_n = 12)) 
print(cw)
} else if (scope=="GLOVE") {
#GLOVE
  
for (term in predict_words) {
  
sim_word = word_vectors[term, , drop = F]
cos_sim_word = sim2(x = word_vectors, y = sim_word, method = "cosine", norm = "l2")
iw<-head(sort(cos_sim_word[,1], decreasing = T), 12)
print(iw)

#get_similar_words(term, word_vectors)

} else { 
#BERT
#todo
}
  
}

#word_associate( text_df$text, match.string = c('concert'), 
#                stopwords = stopwords_df, 
#                network.plot = T, 
#                cloud.colors = c('gray85','darkred')) 