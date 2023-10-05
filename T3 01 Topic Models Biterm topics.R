
#-----------------------------------------------------------------------------------------------

#Biterm Topic Modeling (BTM) for Short Text 

#-----------------------------------------------------------------------------------------------

if(btm==TRUE) {
  
#BTM models word-word co-occurrences patterns
message("Biterm Topic Model...")

#prepare biterms
ac_btm<-annotate_core_df %>% #pos_list filtered version of annotated list
  select(doc_id, sentence_id, token_id, token, lemma, upos) %>%
  #ignore terms with low-differentiation power
  filter(!token %in% terms_2b_ignored) 

#datatable version
ac_btm_dt<-as.data.table(ac_btm)
#biterms_df<-as.data.table(ac_btm)

if(build_te_lemma == TRUE) {
#cooccurrence based on lemma 
biterms_df<-ac_btm_dt[, cooccurrence(x=lemma, skipgram=2),by=doc_id]
#head(biterms_df)

ac_btm_te<-ac_btm %>%
  select(doc_id,lemma)
} else {
#cooccurrence based on token  
biterms_df<-ac_btm_dt[, cooccurrence(x=token, skipgram=2),by=doc_id]  

ac_btm_te<-ac_btm %>%
  select(doc_id,token)  
}

rm(ac_btm)

#-----------------------------------------------------------------------------------------------
#Constructing the BTM-model

model_biterms<-BTM(ac_btm_te, k=k_btm, beta=0.01, iter=700, alpha=1, 
                     #window size for biterm extraction (default:15)
                     window=10, 
                     background=denoise, biterms=biterms_df, trace=50, detailed=TRUE)

# Inspect the model - topic frequency + conditional term probabilities
model_biterms

model_biterms$theta

#log-likelihood of the BTM model
#measure of how plausible model parameters are given the data
m1<-logLik(model_biterms)
m1$ll

btm_topic_terms<-stats::terms(model_biterms, top_n=topic_terms_n)
btm_topic_terms

#de-list topics and save to csv
lapply(btm_topic_terms, function(x) write.table( data.frame(x), paste0(report.loc,'Topic_Modells_Biterm_Topic_k',k_btm,'_probabilities.csv')  , append= T, sep=',' ))

bitermset<-stats::terms(model_biterms, "biterms")
head(bitermset$biterms, 20)

bitermset$n
sum(biterms_df$cooc)

#-----------------------------------------------------------------------------------------------

#document topic distribution
btm_topic_scores<-as.data.frame(predict(model_biterms,ac_btm_te))
colnames(btm_topic_scores)=gsub("V", "topic", colnames(btm_topic_scores))
btm_topic_scores$doc_id<-as.numeric(row.names(btm_topic_scores))
write.csv(btm_topic_scores,file=paste(report.loc,"Topic_Models_BTM_k",k_btm,"_document_topic_distribution.csv"))

#-----------------------------------------------------------------------------------------------

#plot biterm topic clusters
plot_biterms<-plot(model_biterms, title="Biterm Topic Model", top_n=topic_terms_n)
plot_biterms 
plot.store <-paste0(report.loc,'Topic_Models_Biterm.png')
ggsave(plot.store, height=graph_height, width=graph_height * aspect_ratio, dpi=dpi)

cluster_terminology<-stats::terms(model_biterms, type="tokens",top_n = 15)
cluster_biterms<-stats::terms(model_biterms, type="biterm",top_n = 15)

#topic proportions
btm_tprop<-model_biterms$theta
btm_tprop<-as.data.frame(btm_tprop)
colnames(btm_tprop)<-"topic_proportions"

#topic number
btm_tprop$topic<-as.numeric(rownames(btm_tprop))

#convert nested list to datafarem
btm_terms<-data.table::rbindlist(cluster_terminology, idcol=TRUE)
colnames(btm_terms)<-c("topic","term","beta")

#join terms and topics
btm_topic_terms <-left_join(btm_terms,btm_tprop, by=c("topic"="topic"))
btm_topic_terms$full_label<-btm_topic_terms$topic 

btm_ttl<-"BTM topics in order of probability (asc), labels indicate topic number"
plot_btm_dist<-btm_topic_terms %>%
  mutate(term=reorder_within(term, beta, topic)) %>% #reorder_within sorteert binnen categorieen facet wrap (te sorteren variabelen, variabele waarop moet worden gesorteerd, variabele waarbinnen dit moet gebeuren)
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(round(topic_proportions,3) ~ full_label, scales="free") +
  coord_flip() +
  scale_x_reordered() +
  guides(fill=FALSE) +
  theme_bw() + theme(strip.background =element_blank(),
                     panel.grid.major=element_line(colour="grey80"),
                     panel.border=element_blank(),
                     axis.ticks=element_line(size=0),
                     panel.grid.minor.y=element_blank(),
                     panel.grid.major.y=element_blank(),
                     plot.title=element_text(size=7)) +
  theme(legend.position="bottom") +
  labs(title=btm_ttl) 
plot_btm_dist
plot.store <-paste0(report.loc,'Topic_Models_BTM_k',k_btm,'_topics_terms_probabilities.png')  
ggsave(plot.store, height=graph_height, width=graph_height * aspect_ratio, dpi=dpi)

#call custom function btm_cooc from SRC > 'functions.R'

#plot cooccurrence terms within topics
cl<-1

for (cl in 1:k_btm) {
btm_cooc(model_biterms, topic_nr=cl, top_n=wc_words_max)
plot.store<-paste0(report.loc,'Topic_Models_BTM_cooccurrences_within_topic_',cl,'.png')
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)
}

#--------------------------------------------

if(lda_vis==TRUE & lda_vis_mode=='btm') {
#Interactive presentation in shiny (LDAvis)
docsize<-table(ac_btm_te$doc_id)
cluster_scores<-predict(model_biterms, ac_btm_te)
cluster_scores<-cluster_scores[names(docsize), ]
json<-createJSON(
  phi=t(model_biterms$phi), 
  theta=cluster_scores, 
  doc.length=as.integer(docsize),
  vocab=model_biterms$vocabulary$token, 
  term.frequency=model_biterms$vocabulary$freq)

serVis(json,open.browser=TRUE)

message("Interactive visualization of BTM topics at local server http://127.0.0.1 see port number above ...")

#terminate server
#servr::daemon_stop(1)

#cluster_scores<-as.data.frame(cluster_scores)
#cluster_scores$doc_id<-as.numeric(cluster_scores$doc_id)
#x_cluster<-merge(annotate_core_df, cluster_scores, by.x="doc_id", by.y="doc_id")
}

#--------------------------------------------

#Child-parent dependency relationships
if(btm_dep_rel==TRUE) {
  
annotate_core_parenting$relevant<-annotate_core_parenting$dep_rel %in% c("obj") & !is.na(annotate_core_parenting$lemma_parent)

biterms_parenting<-subset(annotate_core_parenting, relevant == TRUE)

biterms_parenting<-data.frame(doc_id=biterms_parenting$doc_id, 
                      term1=biterms_parenting$lemma, 
                      term2=biterms_parenting$lemma_parent,
                      cooc=1, 
                      stringsAsFactors=FALSE)
biterms_parenting<-subset(biterms_parenting, !term1 %in% stopwords_df & !term2 %in% stopwords_df)

annotate_core_parenting<-annotate_core_parenting %>%
  filter(!is.na(head_token_id)) 

## Put in x only terms whch were used in the biterms object such that frequency stats of terms can be computed in BTM

anno<-annotate_core_parenting[, keep := relevant | (token_id %in% head_token_id[relevant == TRUE]), by=list(doc_id, paragraph_id, sentence_id)]

x<-subset(anno, keep == TRUE, select=c("doc_id", "lemma"))
x<-subset(x, !lemma %in% stopwords_df)

## Build the topic model
model_biterms_parenting<-BTM(x, k=20, beta=0.01, iter=300, background=TRUE, 
                     biterms=biterms_parenting, trace=50, detailed=TRUE)

btm_topic_terms_parenting<-stats::terms(model_biterms_parenting, top_n=topic_terms_n)
btm_topic_terms_parenting

#de-list topics and save to csv
lapply(btm_topic_terms_parenting, function(x) write.table(data.frame(x), paste0(report.loc,'LDA_Biterm_Topic_probabilities_parent_child.csv')  , append= T, sep=',' ))

# Plot biterm topic clusters
plot_biterms_pc<-plot(model_biterms_parenting, top_n=topic_terms_n)
plot_biterms_pc 
plot.store <-paste0(report.loc,'Topic_Models_Biterm_parent_child.png')
ggsave(plot.store, height=graph_height, width=graph_height * aspect_ratio, dpi=dpi)

rm(btm_topic_terms_parenting)
rm(model_biterms_parenting)
rm(plot_biterms_pc)
}

#-----------------------------------------------------------------------------------------------

#garbage collecting
objects_to_remove <- c("x","bitermset","biterms_df","ac_btm_dt","ac_btm_te",
                       "btm_terms","btm_topic_terms","btm_topic_terms","btm_topic_scores","btm_tprop",
                       "model_biterms","m1", "plot_biterms")
remove_objects(objects_to_remove)


}