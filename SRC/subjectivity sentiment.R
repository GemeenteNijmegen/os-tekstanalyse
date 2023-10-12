
#------------------------------------------------------------------------------

#Subjectivity, sentiment, polarity and emotions

#------------------------------------------------------------------------------

#method: lexicon based, hybrid

# subjectivity: objective vs. subjective 
# sentiment: attitude, thought, or judgment prompted by feeling
# polarity: negative vs. positive sentiment   
# intensity: modifies next word? 
# emotion: a strong feeling deriving from one's circumstances, mood, or relationships with others
# valence : criterion for emotion

message("Subjectivity, sentiment, polarity and emotions...")

src_lx<-paste0(lexicon.dir,"subjectivity_df.Rda")
range_labs<-c("low-end", "mid-range", "high-end")

if(file.exists(src_lx)){
  #load pre-assembled lexicon
  load(file=src_lx)
  load(file=paste0(lexicon.dir,"FEEL_nl_tr.rda"))
  load(file=paste0(lexicon.dir,"valShifters.rda"))
} else {
  source('SRC/lexicon.R')  
}


#------------------------------------------------------------------------------
#Subjectivity and sentiment

annotate_sentiment_df <-annotate_core_df %>%
  left_join(subjectivity_df, by=c("token"="word"), keep=TRUE ) %>%
  select(-word)

neg.use <- (annotate_sentiment_df$token_previous %in% words_context_negative)
neg.use <- as.numeric(neg.use)*-1
neg.use <- replace(neg.use, neg.use==0, 1)

fort.use <- (annotate_sentiment_df$token_previous %in% words_fortify)
fort.use <- as.numeric(fort.use)*1.5
fort.use <- replace(fort.use, fort.use==0, 1)

#adjust sentiment and sentiment score based on previous word
annotate_sentiment_df$sentiment_cor<-annotate_sentiment_df$sentiment*neg.use*fort.use
annotate_sentiment_df$subjectivity_cor<-annotate_sentiment_df$subjectivity*neg.use*fort.use


#------------------------------------------------------------------------------

#(avg) subjectivity and (weighted) sentiment of document
subjectivity_doc<-annotate_sentiment_df %>% 
  dplyr::mutate(doc_id=as.numeric(doc_id)) %>%
  left_join(tfidf_wrd, by=c("token"="word"), keep=TRUE) %>%
  select(-word) %>%
  filter(!is.na(doc_id)) %>%
  group_by(doc_id) %>%
  summarise(subjectivity_score=round(mean(subjectivity_cor,na.rm=TRUE), digits=1),
            #weighted sentiment score
            sentiment_score=round(weighted.mean(sentiment_cor,tfidf_mn,na.rm=TRUE), digits=1)) 


subjectivity_doc$sentiment_score<-ifelse(is.nan(subjectivity_doc$sentiment_score), subjectivity_doc$subjectivity_score, subjectivity_doc$sentiment_score)

subjectivity_doc<-subjectivity_doc %>%
  dplyr::mutate(
    subjectivity_bin3_labs=cut(subjectivity_score,breaks=3,labels=range_labs),
    subjectivity_bin3=cut(subjectivity_score,breaks=3),
    sentiment_bin3_labs=cut(sentiment_score,breaks=3,labels=range_labs),
    sentiment_bin3=cut(sentiment_score,breaks=3)
  ) %>%
  arrange(doc_id)


#------------------------------------------------------------------------------

#combine sentiment and subjectivity
perception_doc<-text_df %>%
  left_join(subjectivity_doc, by="doc_id")
#perception_doc$doc_id<-as.numeric(perception_doc$id)

save(perception_doc,file=paste0(data.dir,"perception_doc.Rda"))


#plot sentiment by subjectivity 
sen_plot<-ggplot(perception_doc, aes(x=subjectivity_bin3_labs,y=sentiment_score))  + 
  geom_jitter(aes(color='blue'),alpha=0.2) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3) + 
  labs(x='subjectivity', y='sentiment') +
  guides(color=FALSE) +
  theme_minimal() 
sen_plot

plot.nme=paste0('sentiment_subjectivity_boxplot.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

#plot sentiment by subjectivity (via dutch-english translation)
#nrc_plot<-ggplot(perception_doc, aes(x=subjectivity_bin3_labs,y=sentiment_nrc_score))  + 
#  geom_jitter(aes(color='blue'),alpha=0.2) +
#  geom_boxplot(fill="bisque",color="black",alpha=0.3) + 
#  labs(x='subjectivity', y='sentiment (nrc)') +
#  guides(color=FALSE) +
#  theme_minimal() 
#nrc_plot

#plot sentiment by subjectivity 
reg_plot<-ggplot(perception_doc, aes(x=subjectivity_score, y=sentiment_score)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme_minimal() 
reg_plot
plot.nme=paste0('sentiment_subjectivity_regression.png')
plot.store <-paste0(plots.loc,plot.nme)
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

#sent_plot<-ggplot(data=perception_doc, mapping=aes(x=subjectivity_bin3_labs,y=sentiment_score))  + 
#  geom_jitter(aes(color='blue'),alpha=0.2) +
#  geom_boxplot(fill="bisque",color="black",alpha=0.3) + 
#  labs(x='subjectivity', y='sentiment') +
#  guides(color=FALSE) +
#  theme_minimal() 
#sent_plot
#plot.nme=paste0('sentiment_subjectivity_boxplot.png')
#plot.store <-paste0(plots.loc,plot.nme)
#ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

#------------------------------------------------------------------------------

#Emotions: NRC Word-Emotion Association Lexicon 
#The NRC Emotion Lexicon is a list of words and their associations with eight basic emotions
#(anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive)
#please note that accuracy is fairly low for dutch due to dutch-english translation

if(emotion_report == TRUE) {
# sentiment per row
sentiment<-get_nrc_sentiment(text_df$text, language="dutch")

# Emotion
sentiment_sum<-gather(sentiment[1:8], key="emotions") %>%
  group_by(emotie) %>%
  dplyr::summarize(aantal=sum(value, na.rm=TRUE))

sent.multiple.plot<-ggplot(sentiment_sum, aes(x=reorder(emotie, -aantal), y=aantal, fill=emotie)) +
  geom_col() + labs(title="Emotions", subtitle ="NRC", x="", y= "n") +
  theme_minimal() 
sent.multiple.plot
#save
plot.store <-paste0(report.loc,'emotions.png')
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

sent_pos_neg_sum<-gather(sentiment[c(9,10)], key="emotions") %>%
  group_by(emotie) %>%
  dplyr::summarize(aantal=sum(value, na.rm=TRUE))

sent.plot<-ggplot(sent_pos_neg_sum, aes(x=emotie, y=aantal, fill=emotie)) +
  geom_col(width=0.3) + labs(title="Emotions: positive/negative", subtitle ="NRC",  x="", y= "n") +
  theme_minimal() 
sent.plot
#save
plot.store <-paste0(report.loc,'emotions.png')
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

rm(sent_plot, sentiment_sum)
}

#------------------------------------------------------------------------------
#Reasons behind negativity

if(up_dep_parse=='default') {
  ## negators, amplifiers and deamplifiers
  
  polarity_terms<-subjectivity_df %>%
    select(word, polarity) %>%
    rename(term=word)
  polar.file<-paste0(data.dir,"polarity_terms.Rda")
  saveRDS(polarity_terms,file=polar.file)
  
  polarity_negators<-subset(valShifters$valence_nl, t == 1)$x
  polarity_amplifiers<-subset(valShifters$valence_nl, t == 2)$x
  polarity_deamplifiers<-subset(valShifters$valence_nl, t == 3)$x
  
  sentiments<-txt_sentiment(annotate_core_df, term="lemma", 
                              polarity_terms=polarity_terms,
                              polarity_negators=polarity_negators, 
                              polarity_amplifiers=polarity_amplifiers,
                              polarity_deamplifiers=polarity_deamplifiers)
  sentiments<-sentiments$data
  
  ## Use cbind_dependencies to add the parent token to which the keyword is linked
  reasons<-sentiments %>% 
    cbind_dependencies() %>%
    select(doc_id, lemma, token, upos, sentiment_polarity, token_parent, lemma_parent, upos_parent, dep_rel) %>%
    #negative polarity
    filter(sentiment_polarity < 0)
  head(reasons)
  
  reasons<-filter(reasons, dep_rel %in% "amod")

    word_cooccurences<-reasons %>% 
    group_by(lemma, lemma_parent) %>%
    summarise(cooc=n()) %>%
    filter(!is.na(lemma_parent)) %>%
    arrange(-cooc)
    
  vertices<-bind_rows(
    tibble(key=unique(reasons$lemma)) %>% dplyr::mutate(in_dictionary=if_else(key %in% polarity_terms$term, "in_dictionary", "linked-to")),
    tibble(key=unique(setdiff(reasons$lemma_parent, reasons$lemma))) %>% dplyr::mutate(in_dictionary="linked-to"))
  
  cooc<-head(word_cooccurences, 50)
  
  neg_reasons<-cooc %>%  
    graph_from_data_frame(vertices=filter(vertices, key %in% c(cooc$lemma, cooc$lemma_parent))) %>%
    ggraph(layout="fr") +
    geom_edge_link0(aes(edge_alpha=cooc, edge_width=cooc)) +
    geom_node_point(aes(colour=in_dictionary), size=5) +
    geom_node_text(aes(label=name), vjust=1.8, col="darkgreen") +
    #ggtitle("Words linked to negative terms") +
    theme_void()
  
  neg_reasons 
  
  plot.nme=paste0('coocurrence_words_linked_to_negative_terms.png')
  plot.store <-paste0(report.loc,plot.nme)
  ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)
  
  #garbage collection
  objects_to_remove <- c("reasons","sentiments", "FEEL_nl_tr", "polarity_terms", "neg_reasons", "cooc", "udmodel_dutch")
  remove_objects(objects_to_remove)
}