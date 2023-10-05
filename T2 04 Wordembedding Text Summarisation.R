
#-----------------------------------------------------------------------------------------------

# T2 Text summarisation : TextRank

# Best-fit text sentences per (document) cluster

#-----------------------------------------------------------------------------------------------


#Text summarisation (TextRank algorithm)

#TextRank – is a graph-based ranking model for text processing which can be used in
#order to find the most relevant sentences in text 

message("Text summarisation...")

#-----------------------------------------------------------------------------------------------

#set pointer, loop through doc clusters and report top x most-representative sentences 
k<-ks
cl<-1

#initiate list of extractions
extractlist = list()
for (cl in 1:k) {
  sent_cl<-NULL
  sent_cl<-sent_txt %>% filter(best_cl==cl) 
  
  #vector of doc ids (within clusters)
  docids_cl<-sent_cl$doc_id
  
  sentences<-unique(sent_cl[, c("textrank_id", "txt_condensed")])  
  #rm(sent_cl)

terminology<-annotate_core_df %>%
  subset(doc_id %in% docids_cl) %>%
  select(textrank_id,lemma) 

## Textrank for finding the most relevant sentences


## Limit the number of candidates with the minhash algorithm
minhash<-textreuse::minhash_generator(n = 1000, seed = seed)

candidates<- textrank_candidates_lsh(x = terminology$lemma, 
                                      sentence_id = terminology$textrank_id,
                                      minhashFUN = minhash, 
                                      bands = 500)

#candidates <- textrank_candidates_all(unique(terminology$textrank_id))
  
dim(candidates)

tr<-textrank_sentences(data = sentences, terminology = terminology
                                 , textrank_dist = textrank_jaccard
                                 , textrank_candidates = candidates
                                 )


text_summary<-summary(tr, n =2)
ts_df<-as.data.frame(text_summary)
ts.file<-paste0(report.loc,"textsummarisation-",scope,"-cluster",cl,".csv")
write_csv(ts_df,ts.file)

} 