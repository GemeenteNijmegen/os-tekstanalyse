
#-----------------------------------------------------------------------------------------------

# T2 Extractive text summarisation : LexRank

#-----------------------------------------------------------------------------------------------


#Extractive text summarisation (LexRank algorithm)

# Best-fit text segments per (document) cluster

# Graph-based Lexical Centrality as Salience in Text Summarisation
# The goal of LexRank is to find the most representative sentences of a corpus (segments/doc).

#-----------------------------------------------------------------------------------------------

message("Extractive text summarisation...")

#set pointer, loop through clusters and report top x most-representative sentences 
k <- ks
cl <- 1

#initiate list of extractions
extractlist = list()
for (cl in 1:k) {
  
  sent_cl <- NULL
  sent_cl <- sent_txt %>% filter(best_cl==cl) 
  
  lr = lexRankr::lexRank(sent_cl$txt_condensed,
                         #docId = sent_cl$doc_id,
                         docId = sent_cl$textrank_id,
                         #use page rank algorithm
                         usePageRank = pagerank,
                         #return top x sentences
                         n = extract_text_n,
                         continuous = pagerank_continuous, 
                         threshold=.1,
                         #cleaning was done in 'normalisation cleaning.R' 
                         #stemming (in Dutch) in 'tokenisation annotation.R' (SRC dir)
                         stemWords=FALSE,
                         toLower=FALSE,
                         rmStopWords=FALSE,
                         removeNum=FALSE,
                         removePunc=FALSE)
  
  top_lr <- lr %>% mutate(textrank_id=as.numeric(docId),cluster=cl)
  
  extractlist[[cl]] <- top_lr # appending extracts
  
  order_of_appearance = order(as.integer(gsub("_","",top_lr$sentenceId)))
  #extract sentences in order of appearance
  ordered_top_n = top_lr[order_of_appearance, "sentence"]
  cat("Cluster: ", cl,sep = "\n")
  print(ordered_top_n)
  
  #consensus about cluster label
  ws<-stringi::stri_join_list(stri_extract_all_words(ordered_top_n), sep=" ", collapse=" ")

  cat("Label for cluster ", cl,sep = " ")
  wl <- tail(sort(table(strsplit(ws, "[[:space:]]+")[[1]])), 2)
  print(wl)
  
  #coocurrences within sentence
  cooc_neigh <- annotate_clusters %>%
    filter(upos %in% c("NOUN", "ADJ") &  best_cl==cl) %>%
    cooccurrence(x = .,term = "lemma",group = c("textrank_id"))
  
  #coocuurence network 
  wordnetwork_neigh <- head(cooc_neigh, 30)
  wordnetwork_neigh <- graph_from_data_frame(wordnetwork_neigh)
  ggraph(wordnetwork_neigh, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "red") +
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    theme(legend.position = "none",plot.title = element_text(size=8),plot.subtitle = element_text(size=6)) +
    labs(title = paste0("Neighbouring cooccurrences within sentence in cluster ", cl), subtitle = "Nouns & Adjective")
  plot.store <- paste0(report.loc,scope.prefix,'_',best_method_dr,'_',best_method_cl,"_cooccurrences_direct_within_sentence_cluster_",cl,".png")
  ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)
  
  ## Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
  cooc_vic <- annotate_clusters %>%
    filter(upos %in% c("NOUN", "ADJ") &  best_cl==cl) %>%
    cooccurrence(x = .,term = "lemma",group = c("doc_id") , skipgram = 3)
  
  #coocuurence network 
  wordnetwork_vic <- head(cooc_vic, 30)
  wordnetwork_vic <- graph_from_data_frame(wordnetwork_vic)
  ggraph(wordnetwork_vic, layout = "fr") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "red") +
    geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
    theme_graph(base_family = "Arial Narrow") +
    theme(legend.position = "none",plot.title = element_text(size=8),plot.subtitle = element_text(size=6)) +
    labs(title = paste0("Cooccurrences vicinity within document in cluster ", cl), subtitle = "Nouns & Adjective, skipgram=3")
  plot.store <- paste0(report.loc,scope.prefix,'_',best_method_dr,'_',best_method_cl,'_cooccurrences_vicinity_within_document_cluster_',cl,'.png')
  ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)
}

cluster_extractions <- do.call(rbind, extractlist) %>%
  left_join(sent_txt, by="textrank_id") %>%
  select(-c("docId","sentenceId","sentence", "text")) %>%
  select(cluster, textrank_id,doc_id,value,txt_condensed,subjectivity_score,sentiment_score,everything()) %>%
  left_join(condensed_doc, by="doc_id") %>%
  #relocate(txt_condensed, .before = doc_condensed)
  select(cluster,txt_condensed,doc_condensed,text,doc_id,textrank_id, everything())

clu_ex <- paste0(report.loc,'cluster-bestextractions-',scope.prefix,'-',best_method_dr,'-',best_method_cl,'-k',k,'.csv')
write.csv(cluster_extractions, file=clu_ex,row.names=FALSE)

message("For best text extractions per cluster see directory REPORT > ", scope)