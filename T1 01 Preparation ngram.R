#----------------------------------------------------------------------------------------------

#Ngrams

#----------------------------------------------------------------------------------------------

#legend label: frequency (n)
sub_title <- "n"

for (tg in 2:3) {
  #bigram, trigram
  ngram <- tg

if(ngram ==2){

#-----------------------------------------------------------------------------------------------  

#Bigram

#-----------------------------------------------------------------------------------------------
  
ngram_df <- condensed_sent %>% 
  unnest_tokens(bigram, txt_condensed, token="ngrams", n=tg) %>% 
  #filter(!is.na(bigram)) %>% 
  separate(bigram, c("w1", "w2"), sep = " ") %>% 
  # filter short words
  filter(str_length(w1) > word_len & str_length(w2)>word_len) %>% 
  unite(bigram, w1, w2, sep = " ", remove = FALSE) 

## frequencies 
bigram_freq <- ngram_df %>% 
  group_by(bigram) %>% 
  dplyr::summarize(aantal=n(), bigram=unique(bigram), word1=first(w1), word2=first(w2), n=aantal) %>%
  arrange(desc(aantal)) 

write.csv(bigram_freq, paste0(report.loc, 'Cooccurrences_ngram_bigram_freq.csv'))

big <-ggplot(head(bigram_freq,15), aes(reorder(bigram,aantal), aantal,fill=aantal)) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_minimal() + 
  scale_fill_viridis(option = "viridis", begin=0, end=1, alpha=1, direction=-1, name=sub_title, 
                     na.value = "transparent") +
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most occurring bigrams")
big
plot.nme = paste0('Cooccurrences_ngram_bigram.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi)

bigram_freq <- bigram_freq %>% 
 select(-bigram, -aantal) %>%
 top_n(50) %>%
 graph_from_data_frame()

#Markov chain
a <- grid::arrow(type="closed", length=unit(.15, "inches"))

plot.store<-paste0(report.loc,'Cooccurrence_bigram_network_markov_chain.png')
#png(plot.store, width = 720, height = 480, units = "px")
bigram_network <- ggraph(bigram_freq, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
bigram_network
ggsave(plot.store, height = graph_height, width = graph_height * aspect_ratio, dpi=dpi)

#words preceded by negative charge
bigram_context_negative <- ngram_df %>%
  filter(w1 %in% words_context_negative) %>%
  group_by(w1,w2) %>%
  dplyr::summarize(bigram=unique(bigram), n=n())

rm(bigram_freq,big,bigram_network)
} else{ 

#-----------------------------------------------------------------------------------------------  
  
#Trigram
  
#-----------------------------------------------------------------------------------------------
  
ngram_df <- condensed_sent %>% 
  unnest_tokens(trigram, txt_condensed, token="ngrams", n=tg) %>% 
  #filter(!is.na(trigram)) %>% 
  separate(trigram, c("w1", "w2", "w3"), sep=" ") %>% 
  #filter short words
  filter(str_length(w1) > word_len & str_length(w2) > word_len & str_length(w3) > word_len) %>% 
  unite(trigram, w1, w2, w3, sep = " ", remove = FALSE)

## frequencies 
trigram_freq <- ngram_df %>% 
  group_by(trigram) %>% 
  dplyr::summarize(aantal=n(), trigram=unique(trigram), w1=first(w1), w2=first(w2), w3=first(w3), n=aantal) %>%
  arrange(desc(aantal)) 
write.csv(trigram_freq, paste0(report.loc, 'Cooccurrences_ngram_trigram_freq.csv'))

trg <- ggplot(head(trigram_freq,15), aes(reorder(trigram,aantal), aantal,fill=aantal)) +
  geom_bar(stat = "identity") + coord_flip() +
  theme_minimal() + 
  scale_fill_viridis(option = "viridis", begin=0, end=1, alpha=1, direction= -1,name=sub_title, 
                     na.value = "transparent") +
  xlab("Trigrams") + ylab("Frequency") +
  ggtitle("Most occurring trigrams")
trg
plot.nme = paste0('Cooccurrences_ngram_trigram.png')
plot.store <-paste0(report.loc,plot.nme)
ggsave(plot.store, height = graph_height , width = graph_height * aspect_ratio, dpi=dpi)

rm(trigram_freq,trg)
}

}

#garbage collection
rm(ngram_df)