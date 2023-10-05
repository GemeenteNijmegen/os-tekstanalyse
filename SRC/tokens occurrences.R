
#------------------------------------------------------------------------------

#Tokens (co)occurrences

#------------------------------------------------------------------------------

message("Tokens (co)occurrences...")

#Visualisation of tokens (occurrences and cooccurrences)

if(tok_oc == TRUE) {
#Upos report
stats <- txt_freq(annotate_df$upos)
stats$key <- factor(stats$key, levels=rev(stats$key))

sub_title<-'UPOS'
rake<-ggplot(head(subset(stats), 25), aes(x=key, y=freq,fill=freq)) +
  geom_bar(stat="identity") +
  theme_minimal() + 
  scale_fill_viridis(option="viridis", begin=0, end=1, alpha=1, direction=-1,name=sub_title, 
                     na.value="transparent") +
  labs(title="Universal Parts of Speech (UPOS)")
rake
plot.nme='Occurrence_UPOS.png'
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

# wordcloud of adjectives and nouns
stats <- subset(annotate_df, upos %in% c('ADJ',"NOUN")) 

if(keyword_report=="token") {
stats <- txt_freq(stats$token)  
} else {
stats <- txt_freq(stats$lemma)  
}  

stats$key <- factor(stats$key, levels=rev(stats$key))

stats <- stats %>% top_n(80, freq)
plot.store<-paste0(report.loc,'Occurence_ADJ_NOUN_wordcloud_',keyword_report,'.png')
png(plot.store, width=12, height=8, units="in", res=300)
par(mar=rep(0, 4))
textrank_wc <- wordcloud(words=stats$key, freq=stats$freq,
                         random.order=FALSE, 
                         rot.per=0.35, 
                         scale=c(6.5, 2.5),
                         use.r.layout=FALSE,
                         colors=color_scheme_dark)
(textrank_wc)
dev.off()

#-----------------------------------------------------------------------------------------------
#Occurences of keywords (nouns)

# Nouns
stats <- subset(annotate_df, upos %in% c("NOUN")) 

if(keyword_report=="token") {
stats <- txt_freq(stats$token)
} else {
stats <- txt_freq(stats$lemma)  
}

stats$key <- factor(stats$key, levels=rev(stats$key))
sub_title<-'freq'
nouns<-ggplot(head(stats, 25), aes(x=freq, y=key,fill=freq)) +
  geom_bar(stat="identity") +
  theme_minimal() + 
  #scale_fill_viridis_d() +
  scale_fill_viridis(option="viridis", begin=0, end=1, alpha=1, direction=-1,name=sub_title, 
                     na.value="transparent") +
  labs(title="Most occurring nouns")
nouns
plot.nme=paste0('Occurrence_nouns_',keyword_report,'.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

## Adjectives
stats <- subset(annotate_df, upos %in% c("ADJ")) 

if(keyword_report=="token") {
  stats <- txt_freq(stats$token)
} else {
  stats <- txt_freq(stats$lemma)  
}
stats$key <- factor(stats$key, levels=rev(stats$key))
sub_title<-'freq'
nouns<-ggplot(head(stats, 25), aes(x=freq, y=key,fill=freq)) +
  geom_bar(stat="identity") +
  theme_minimal() + 
  #scale_fill_viridis_d() +
  scale_fill_viridis(option="viridis", begin=0, end=1, alpha=1, direction=-1,name=sub_title, 
                     na.value="transparent") +
  labs(title="Most occurring adjectives")
nouns
plot.nme=paste0('Occurrence_adjectives_',keyword_report,'.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)
}

#-----------------------------------------------------------------------------------------------
#Cooccurrences

#Keyword combinations
#3 methods

if(tok_cooc == TRUE) {
#1. RAKE (Rapid Automatic Keyword Extraction)
# contiguous sequence of words which do not contain irrelevant words  
stats <- keywords_rake(x=annotate_df, term=keyword_report, group=keyword_level,
                       relevant=annotate_df$upos %in% c("NOUN", "ADJ"))

stats$key <- factor(stats$keyword, levels=rev(stats$keyword))

sub_title<-paste0('based on ', keyword_report)
rake<-ggplot(head(subset(stats, freq > 3), 25), aes(x=rake, y=key,fill=rake)) +
  geom_bar(stat="identity") +
  theme_minimal() + 
  scale_fill_viridis(option="viridis", begin=0, end=1, alpha=1, direction=-1, name="rake", 
                     na.value="transparent") +
  labs(title="Cooccurrences : Rapid Automatic Keyword Extraction (RAKE)", subtitle=sub_title)
rake
plot.nme=paste0('Cooccurrences_rake_keywords_',keyword_report,'_',keyword_level,'.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

#2. Collocation ordering using Pointwise Mutual Information (PMI)
# results in meaningful word combinations
stats <- keywords_collocation(x=annotate_df, term=keyword_report, group=keyword_level)
stats$key <- factor(stats$keyword, levels=rev(stats$keyword))

sub_title<-paste0('based on ', keyword_report)
pmi<-ggplot(head(subset(stats, freq > word_freq), 25), aes(x=pmi, y=key,fill=pmi)) +
  geom_bar(stat="identity") +
  theme_minimal() + 
  scale_fill_viridis(option="viridis", begin=0, end=1, alpha=1, direction=-1, name="pmi", 
                     na.value="transparent") +
  labs(title="Cooccurrences : PMI Collocation", subtitle=sub_title)
pmi
plot.nme=paste0('Cooccurrences_pmi_keywords_meaningful_',keyword_report,'_',keyword_level,'.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

#3. Parts of Speech phrase sequence detection
annotate_df$phrase_tag <- as_phrasemachine(annotate_df$upos, type="upos")

if(keyword_report=="token") {
  stats <- keywords_phrases(x=annotate_df$phrase_tag, term=tolower(annotate_df$token), 
                            pattern="(A|N)*N(P+D*(A|N)*N)*", 
                            is_regex=TRUE, ngram_max=4, detailed=FALSE)
} else {
  stats <- keywords_phrases(x=annotate_df$phrase_tag, term=tolower(annotate_df$lemma), 
                            pattern="(A|N)*N(P+D*(A|N)*N)*", 
                            is_regex=TRUE, ngram_max=4, detailed=FALSE)
}

stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels=rev(stats$keyword))
sub_title<-'freq'
sim<-ggplot(head(stats, 50), aes(x=freq, y=key,fill=freq)) +
  geom_bar(stat="identity") +
  theme_minimal() + 
  scale_fill_viridis(option="viridis", begin=0, end=1, alpha=1, direction=-1, name=sub_title, 
                     na.value="transparent") +
  labs(title="Cooccurrences : Parts of Speech phrase sequences", subtitle=sub_title)
sim
plot.nme=paste0('Cooccurrences_part_of_speech_sequences_',keyword_report,'.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height=graph_height, width=graph_height * aspect_ratio, dpi=dpi)

#word network ordered by Google Pagerank
if(keyword_report=="token") {
  stats <- textrank::textrank_keywords(annotate_df$token, 
                             relevant=annotate_df$upos %in% pos_list, 
                             ngram_max=context_window, sep=" ",
                             p=1/2)  
} else {
  stats <- textrank::textrank_keywords(annotate_df$lemma, 
                             relevant=annotate_df$upos %in% pos_list, 
                             ngram_max=context_window, sep=" ",
                             p=1/2)
}  

stats <- subset(stats$keywords, ngram > 1) %>% top_n(80, freq)

plot.store<-paste0(report.loc,'Cooccurrences_googlepageranked_wordcloud_',keyword_report,'.png')
png(plot.store, width=12, height=8, units="in", res=300)
par(mar=rep(0, 4))
textrank_wc <- wordcloud(words=stats$keyword, freq=stats$freq,
                         random.order=FALSE,
                         rot.per=0.35, 
                         scale=c(6.5, 2.5),
                         use.r.layout=FALSE,
                         colors=color_scheme_dark)
(textrank_wc)
dev.off()

#dependency parsing: nominal subject and the adjective 
stats <- subset(annotate_core_parenting, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))

if(keyword_report=="token") {
stats$term <- paste(stats$token_parent, stats$token, sep=" ")
} else {
stats$term <- paste(stats$lemma_parent, stats$lemma, sep=" ")  
}  

stats <- txt_freq(stats$term) %>% top_n(80, freq)

plot.store<-paste0(report.loc,'Cooccurrences_noun_adj_dependency_wordcloud_',keyword_report,'.png')
png(plot.store, width=12, height=8, units="in", res=300)
par(mar=rep(0, 4))
dep_term <- wordcloud(words=stats$key, freq=stats$freq, min.freq=word_freq,
          random.order=FALSE, 
          rot.per=0.35, 
          scale=c(6.5, 2.5),
          use.r.layout=FALSE,
          colors=color_scheme_dark)
(dep_term)
dev.off()
}

c<-1
for(c in 1:2) {
  #coocurrences within sentence
  if(c==1) {
    #words following ech other (direct vicinity)
    cooc <- cooccurrence(x=subset(annotate_df, upos %in% c("NOUN", "ADJ")), 
                         term=keyword_report, 
                         group=c("textrank_id"))
    cooc_ttl <- paste0("Cooccurrences within sentence ", keyword_report)
  } else {
    # Co-occurrences: How frequent do words follow one another even if we would skip 2 words in between
    
    if(keyword_report=="token") {
      cooc <- cooccurrence(x=annotate_df$token, 
                           relevant=annotate_df$upos %in% c("NOUN", "ADJ"), skipgram=2) 
    } else {
      cooc <- cooccurrence(x=annotate_df$lemma, 
                           relevant=annotate_df$upos %in% c("NOUN", "ADJ"), skipgram=2)  
    }  
    
    cooc_ttl <- paste0("Cooccurrences within sentence max 2 words apart ", keyword_report)
  }
  
  #head(cooc)
  
  #coocurrence network 
  wordnetwork <- head(cooc, 100)
  wordnetwork <- igraph::graph_from_data_frame(wordnetwork, directed=FALSE)
  
  sub_title<-paste0("Nouns & Adjective (",keyword_report, ")")
  
  gg_cooc <- paste0(report.loc,cooc_ttl,".png")
  ggraph(wordnetwork, layout="fr") +
    geom_edge_link(aes(width=cooc, edge_alpha=cooc), edge_colour="red") +
    geom_node_text(aes(label=name), col="darkgreen", size=4) +
    theme_graph(base_family="Arial Narrow") +
    theme(legend.position="none",plot.title=element_text(size=8),plot.subtitle=element_text(size=6)) +
    labs(title=cooc_ttl, subtitle=sub_title)
  ggsave(gg_cooc, height=graph_height, width=graph_height * aspect_ratio, dpi=dpi)

  if(c==1) {
  #community detection
  
  # Community structure detection based on edge betweenness (http://igraph.org/r/doc/cluster_edge_betweenness.html)
  #igraph::cluster_edge_betweenness(wordnetwork, weights=E(wordnetwork)$cooc)
  
  #Community detection via random walks (http://igraph.org/r/doc/cluster_walktrap.html)
  #https://perso.uclouvain.be/michel.verleysen/papers/esann05ly.pdf
  comm_trap<-igraph::cluster_walktrap(wordnetwork, weights=E(wordnetwork)$cooc, steps=2)
  plot.store<-paste0(report.loc,'Cooccurences_communities_of_words_random_walk.png')
  png(plot.store, width=16, height=8, units="in", res=300)
  plot_dendrogram(comm_trap, main="Community structure dendrogram based on random walks")
  dev.off()
  
  # Community detection via optimization of modularity score
  # This works for undirected graphs only
  wordnetwork2 <- igraph::as.undirected(wordnetwork) # an undirected graph
  igraph::cluster_fast_greedy(wordnetwork2, weights=E(wordnetwork2)$cooc)
  
  comm_greed <- igraph::cluster_fast_greedy(wordnetwork2, weights=E(wordnetwork2)$cooc)
  
  plot.store<-paste0(report.loc,'Cooccurences_communities_of_words_greedy.png')
  png(plot.store, width=16, height=8, units="in", res=300)
  plot_dendrogram(comm_greed, main="Community structure dendrogram based on reedy optimization of modularity")
  dev.off()
  }
}

#unload objects from memory
objects_to_remove <- c("wordnetwork","stats","nouns", "rake", "pmi")
remove_objects(objects_to_remove)
