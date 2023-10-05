
#-----------------------------------------------------------------------------------------------

# T2 WORD and DOCUMENT EMBEDDING -> GloVe

#-----------------------------------------------------------------------------------------------


# Stanford's GloVe https://nlp.stanford.edu/projects/glove/
# Builds a GloVe model using text2vec package

# GloVe focuses on words co-occurrences over the whole corpus. 
# Its embeddings relate to the probabilities that two words appear together.
# With Glove, you build a co-occurrence matrix for the entire corpus first,
# then factorize it to yield matrices for word vectors and context vectors.

# GloVe, is a new global log-bilinear regression model for the unsupervised  
# learning of word representations that outperforms other models on word   
# analogy, word  similarity,and named entity recognition tasks

#-----------------------------------------------------------------------------------------------

#Word vectors based on the GloVe algorithm

#fit the model
message("Computing embeddings based on GloVe algorithm...")

#factorize tcm matrix using the GloVe algorithm
#text2vec uses a parallel stochastic gradient descent algorithm
glove = GlobalVectors$new(rank = vec_dim, x_max = x_max)

# keep in mind fit, fit_transform methods modify models!
wv_main = glove$fit_transform(tcm, n_iter = iterations_n, convergence_tol = convergence_tolerance, n_threads = thread, seed = seed)

#wv_main$phi <- t(Reduce("+", wv_main$word_vectors) / 2)
#colnames(wv_main$phi) <- colnames(tcm)

# obtain word vector
wv_context = glove$components

# either word-vectors matrices could work, but the sum/mean may work better
word_vectors = wv_main + t(wv_context)

wv_df <- as.data.frame(word_vectors)
wv_df$word<-row.names(wv_df)

#save word vectors data frame
save(wv_df,file=paste0(data.loc,scope.prefix,"_","wv_df.Rda"))

#garbage collection
objects_to_remove <- c("wv_main","wv_context")
remove_objects(objects_to_remove)

#garbage collection
gc()