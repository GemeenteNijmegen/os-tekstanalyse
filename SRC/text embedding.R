
#----------------------------------------------------------------------------------------------

#Text embedding / - data matrices

#----------------------------------------------------------------------------------------------

#DocumentTermMatrix (DTM), Term co-occurence matrix (TCM), 
#Term frequency-inverse document frequency (TF-IDF)

#A DTM is a matrix whose rows index documents and whose columns index linguistic features of these documents. 
#These linguistic features called terms, though they may be single words, groups of words called n-grams, stems, lemmas, or other tokens. 

#A TCM is a square, but not necessarily symmetric, matrix whose rows and columns both index terms. The $(i, j)$ entries of a TCM 
#represent a relationship between term $i$ and term $j$, 

message("Text embedding / - data matrices...")

#Udpipe version
tokens_df <- annotate_core_df

if(build_te_doc == TRUE) {
  if(build_te_lemma == TRUE) {
  tokens_te <- tokens_df %>%
    select(doc_id, lemma)
  } else {
  tokens_te <- tokens_df %>%
    select(doc_id, token)   
  }
} else {
  if(build_te_lemma == TRUE) {
  tokens_te <- tokens_df %>%
    select(textrank_id, lemma) 
  } else {
  tokens_te <- tokens_df %>%
    select(textrank_id, token) 
  }
}

message("Document term frequencies (DTF)...")
#document term frequencies (DTF) 
#term frequency and weighting (TF-IDF)
dtf <- document_term_frequencies(tokens_te)
dtf_tfidf <- document_term_frequencies_statistics(dtf)
dtf_tfidf$word <- dtf_tfidf$term
dtf_tfidf$tfidf <- dtf_tfidf$tf_idf

save(dtf,file=paste0(data.dir,"dtf.Rda"))
save(dtf_tfidf,file=paste0(data.dir,"dtf_tfidf.Rda"))

message("Document term matrix (DTM)...")
#document term matrix (DTM)
#each row is a sentence from a single document
dtm <- document_term_matrix(dtf, weight = dtm_weight)

#remove terms with low-differentiation power
dtm <- dtm_remove_terms(dtm, terms = terms_2b_ignored)

#remove terms with low frequency
dtm <- dtm_remove_lowfreq(dtm, minfreq = word_freq)

#remove very-unimportant terms
#keep only terms in dtm where the tfidf is above tfidf_threshold
dtm <- dtm_remove_tfidf(dtm, cutoff=tfidf_threshold)

save(dtm,file=paste0(data.dir,"dtm.Rda"))

cat(paste0('DTM dimensions: Documents (',dim(dtm)[1],') x Tokens (',dim(dtm)[2],')',
           ' (average token frequency: ',round(sum(dtm)/sum(dtm!=0),2),')'),"\n")
rm(tokens_te)

#-----------------------------------------------------------------------------------------------

#all word occurrences (incidence per doc)
wrd_cnt <- tokens_df %>%
  count(doc_id,token, sort = TRUE) %>% 
  group_by(token,.drop=FALSE) %>% 
  dplyr::summarize(total = sum(n)) %>%
  rename(word = token)
save(wrd_cnt,file=paste0(data.dir,"wrd_cnt.Rda"))

tfidf_df <- dtf_tfidf %>%
  left_join(wrd_cnt,by="word") %>%
  dplyr::mutate(doc_id=as.numeric(doc_id)) %>%
  filter(total>word_freq)
save(tfidf_df,file=paste0(data.dir,"tfidf_df.Rda"))

#unique words
tfidf_wrd <- tfidf_df %>%
  group_by(word) %>%
  summarise(total_sum=mean(total),
            tfidf_mn=mean(tfidf)) %>%
  arrange(desc(tfidf_mn)) 
#save tf-idf unique words data frame 
save(tfidf_wrd,file=paste0(data.dir,"tfidf_wrd.Rda"))

sub_title<-'TF-IDF'
tf <-ggplot(head(tfidf_wrd,20), aes(reorder(word,tfidf_mn),tfidf_mn,fill=tfidf_mn)) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_minimal() + 
  scale_fill_viridis(option = "viridis", begin = 0, end = 1, alpha = 1, direction = -1, name=sub_title, 
                     na.value = "transparent") +
  xlab("Term") + ylab("TF-IDF") +
  ggtitle("Most important words")
tf
plot.nme = paste0('important_words.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi)

rm(dtf,dtf_tfidf)

#-----------------------------------------------------------------------------------------------

#Term cooccurrence matrix (TCM) (for GloVe Word-embedding)

#Rows and columns are unigrams
#Frequency measure is the number of times term $j$ appears within 'context_window' (set in T2 00 Init Preparation) 
#places of term $i$
#The TCM is de-composed later by a method called GloVe (see T2 01 Wordembedding GloVe)

message("Term cooccurrence matrix (TCM)...")

#Create iterator
it <- text_df %>%
  with(text2vec::word_tokenizer(text)) %>%
  text2vec::itoken() 

#Create vocabulary
vocab <- it %>%
  text2vec::create_vocabulary()

#tokens <- text_df$text
#it = text2vec::itoken(tokens, tokenizer=word_tokenizer)
#vocab = text2vec::create_vocabulary(it)

#prune vocabulary 
pruned_vocab = text2vec::prune_vocabulary(vocab 
                                #prune the vocabulary of low-frequent words
                                , term_count_min = word_freq 
                                #maximum proportion of documents which should contain term
                                , doc_proportion_max = doc_prop_max
                                #minimum proportion of documents which should contain term
                                , doc_proportion_min = doc_prop_min
                                #or, at least appear in x documents
                                #, doc_count_min = 2
)

rm(vocab)

#remove stopwords and character fragments from vocabulary
pruned_vocab <- pruned_vocab %>%
  #remove stopwords
  anti_join(stopwords_df, by= c("term" = "word")) %>%
  #respect minimum string length
  filter(str_length(term) >= word_len)
  
if(remove_numbers==TRUE) {
  pruned_vocab <- pruned_vocab %>%
    #alphanumeric only
    filter(stringr::str_detect(term, "[:alpha:]"))
}

# maps words to indices
vectorizer = text2vec::vocab_vectorizer(pruned_vocab)

# maximum number of co-occurrences to use in the weighting function, 
# we choose the entire token set divided by 100
x_max <- length(pruned_vocab$doc_count)/100

#rm(pruned_vocab)

#Term-cooccurrence matrix (TCM)
tcm = text2vec::create_tcm(it, vectorizer, skip_grams_window = context_window, skip_grams_window_context = skipgram_context)
save(tcm,file=paste0(data.dir,"tcm.Rda"))

#tokens_unique<-create_vocabulary(it, ngram = c(ngram_min = 1L, ngram_max = 1L),stopwords = stopwords_df, sep_ngram = "_")