
#----------------------------------------------------------------------------------------------

#Build LDA Topic Models

#----------------------------------------------------------------------------------------------

#Latent Dirichlet Allocation (LDA)
#The calculation of topic models aims to determine the proportionate 
#composition of a fixed number of topics in the documents of a collection.

#----------------------------------------------------------------------------------------------

message("Construct LDA Topic Models...")

#Run Latent Dirichlet Allocation (LDA) using Gibbs Sampling
#adjust values (when needed) in SRC > 'tuning LDA.R'
control_Gibbs_topicmodels<-list(
  iter=niter
  ,burnin=burnin
  ,keep=keep
  ,delta=deltaprior
  ,alpha=alphaprior
  ,nstart=nstart
  ,best=best
  ,seed=seed_lst
  ,thin=thin
)

#------------------------------------------

if(LDA_guided==TRUE) {
#guided LDA

#When Gibbs sampling is used for fitting the model, seed words with their 
#additional weights for the prior parameters can be specified in order to be able 
#to fit seeded topic models.
  
#fit 'k_ldas' topics.
#specify 5 seed words for k_ldas-1 topics, the last topic has no seed words.
k_ldas.min1<-k_lda-1

i<-rep(1:k_ldas.min1, each=5)
topics.p<-k_ldas.min1*5
j<-sample(1:ncol(dtm), topics.p)
seed_weight<-500 - 0.1
delta_s<-simple_triplet_matrix(i, j, v=rep(seed_weight, topics.p),
                                nrow=k_lda, ncol=ncol(dtm))

#Constructor for LDA model 
#dmodel_lda was created in 'text embedding' section (directory 'SRC')

model_lda<-LDA(dtm, k=k_lda, method="Gibbs", seedwords=delta_s, 
            control=control_Gibbs_topicmodels
            #use first topic as filter (denoising)
            , background=denoise
            )

apply(delta_s, 1, function(x) which(x == seed_weight))
apply(posterior(model_lda)$terms, 1, function(x) order(x, decreasing=TRUE)[1:k_ldas.min1])
} else {

#UNguided LDA

#Constructor for LDA model 
#dtm was created in 'text embedding.R' (directory 'SRC')

model_lda<-topicmodels::LDA(dtm, k=k_lda, method="Gibbs"
              ,control=control_Gibbs_topicmodels
              #use first topic as filter (denoising)
              , background=denoise
              )
}

#----------------------------------------------------------------------------------------------
#Diagnostics and metrics

#diag_df<-topic_diagnostics(model_lda, dtm)
#diag_df

topic_diagnostics(model_lda, dtm) %>% 
  gather(diagnostic, value,  -topic_num) %>%
  ggplot(aes(x=topic_num, y=value,
             )) +
  geom_bar(stat="identity") +
  facet_wrap(~diagnostic, scales="free") +
  guides(fill=FALSE) +
  theme(strip.background =element_blank(),
                     panel.grid.major=element_line(colour="grey80"),
                     panel.border=element_blank(),
                     axis.ticks=element_line(size=0),
                     panel.grid.minor.y=element_blank(),
                     panel.grid.major.y=element_blank(),
                     plot.title=element_text(size=7)) +
  theme(legend.position="bottom") +
  labs(x="Topic Number", y="Diagnostic value",
       fill="Topic Label", title="All Topic Model Diagnostics") 
plot.store <-paste0(report.loc,'Topic_Models_LDA_Gibbs_k',k_lda,'_model_diagnostics.png')
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

#metrics description
#topic size: Total (weighted) number of tokens per topic
#mean token: Average number of characters for the top tokens per topic
#distance from corpus distribution:	Distance of a topic’s token distribution from the overall corpus token distribution
#distance between token and document frequencies:	Distance between a topic’s token and document distributions
#document prominence:	Number of unique documents where a topic appears
#topic coherence:	Measure of how often the top tokens in each topic appear together in the same document
#topic exclusivity:	Measure of how unique the top tokens in each topic are compared to the other topics

#Get posterior probabilities from the model
postprob_lda<-posterior(model_lda)
# format of the resulting object
#attributes(postprob_lda)
 
# topics are probability distributions over the entire vocabulary
# phi (topic - token distribution matrix) -  topics in rows, tokens in columns
phi<-postprob_lda$terms %>% as.matrix   # get beta from results
dim(phi) # topic distributions over ncol(DTM) terms

cat(paste0('Dimensions of phi (topic-token-matrix): ',paste(dim(phi),collapse=' x '),'\n'))

#probability distribution per document of its contained topics
# theta (document - topic distribution matrix) -  documents in rows, topic probs in columns
theta<-postprob_lda$topics %>% as.matrix
dim(theta) # nDocs(DTM) distributions over number of topics

cat(paste0('Dimensions of theta (document-topic-matrix): ',paste(dim(theta),collapse=' x '),'\n'))

beta<-postprob_lda$terms   # get beta from results
dim(beta)  

term_freq<-slam::col_sums(dtm)

# vocabulary: unique tokens
vocab<-colnames(phi)

# alpha
#high alpha value : each document contains most of the topics
#lower alpha value : documents are likely to contain a fewer number of topic
attr(model_lda, "alpha") 
#rerun model with lower alpha value? 
#see 'control_Gibbs_topicmodels' above

#log-likelihood of the LDA model
#measure of how plausible model parameters are given the data
logLik(model_lda)

#perplexity(model_lda)

#terms(model_lda, topic_terms_n)

#----------------------------------------------------------------------------------------------

if(lda_vis==TRUE & lda_vis_mode=='lda') {
#Interactive visualisation (LDAVis)

# Convert to json
#cl<-makeCluster(thread)
json_lda<-LDAvis::createJSON(phi=phi, theta=theta,
                               vocab=vocab,
                               doc.length=as.vector(table(dtm@i)),
                               term.frequency=term_freq
                               #,cluster=cl
                               )

message("Interactive visualization of LDA topics at local server http://127.0.0.1 see port number below ...")

serVis(json_lda)

message("Adjust lambda slider. A lower lambda gives more importance to topic exclusivity. .4 is a fairly safe choice")
}

#----------------------------------------------------------------------------------------------
#topic labels

#concatenate the five most likely terms of each topic 
#to a string that represents a pseudo-name for each topic
topic_terms_top5<-topicmodels::terms(model_lda, 5)
topic_names<-apply(topic_terms_top5, 2, paste, collapse=" ")
topic_terminology<-predict(model_lda, type="terms", min_posterior=0.005, min_terms=word_freq)
topic_scores<-predict(model_lda, newdata=dtm, type="topics")
topic_scores$doc_id<-as.numeric(topic_scores$doc_id)
x_topics<-merge(annotate_core_df, topic_scores, by.x="doc_id", by.y="doc_id")

#----------------------------------------------------------------------------------------------
#topic ranking

# re-rank top topic terms for topic names
topic_names<-apply(lda::top.topic.words(beta, 5, by.score=T), 2, paste, collapse=" ")
topic_names_df<-as.data.frame(topic_names)
topic_names_df$topic_id<-as.numeric(row.names(topic_names_df))

# Most probable topics in the entire collection
topic_proportions<-round(colSums(theta) / nrow(dtm),6)  # mean probablities over all text segments
topic_names_df$topic_proportions<-topic_proportions
topic_names_df<-topic_names_df %>% arrange(desc(topic_proportions))

names(topic_proportions)<-topic_names     # assign the topic names 
sort(topic_proportions, decreasing=TRUE) # show summed proportions in decreased order

#----------------------------------------------------------------------------------------------
#Topic-word probabilities
tm_topics<-tidytext::tidy(model_lda, matrix="beta") 
write.csv(tm_topics,file=paste(report.loc,"Topic_Models_LDA_Gibbs_k",k_lda,"_topic_word_probabilites.csv"))

#Document-topic probabilities

#dominant topic
tm_documents<-tidytext::tidy(model_lda, matrix="gamma")
write.csv(tm_documents,file=paste(report.loc,"Topic_Models_LDA_Gibbs_k",k_lda,"_document_dominant_topic.csv"))

#topic distribution per document
gamma_df<-as.data.frame(model_lda@gamma) 
names(gamma_df)<-c(paste0("topic",1:k_lda))
gamma_df$doc_id<-as.numeric(model_lda@documents)
write.csv(gamma_df,file=paste(report.loc,"Topic_Models_LDA_Gibbs_k",k_lda,"_document_topic_distribution.csv"))

# topics per document
top_df<-as.data.frame(topicmodels::topics(model_lda))

top_df$doc_id<-as.integer(row.names(top_df))
colnames(top_df)<- c('topic', 'doc_id')

#----------------------------------------------------------------------------------------------
#Composition of topics

#per topic top 10 terms
tm_top_terms<-tm_topics %>%
  group_by(topic) %>%
  top_n(topic_terms_n, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

tm_top_terms<-tm_top_terms %>%
   left_join(topic_names_df,by=c("topic"="topic_id"),copy=TRUE) %>%
  arrange(desc(topic_proportions)) %>% 
  mutate(full_label=paste(topic, topic_names)) 
  
lda_ttl<-"LDA Topics in order of probabilty (asc), labels indicate topic number"
plot_lda<-tm_top_terms %>%
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
  labs(title=lda_ttl) 

plot_lda
if(LDA_guided==TRUE) {
plot.store <-paste0(report.loc,'Topic_Models_LDA_Gibbs_Guided_k',k_lda,'_topics_terms_probabilities.png')  
} else {
plot.store <-paste0(report.loc,'Topic_Models_LDA_Gibbs_k',k_lda,'_topics_terms_probabilities.png')  
}

ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

#----------------------------------------------------------------------------------------------


if(lvl_start==1 & topic_embed==TRUE) {
#sentiment
# combine reduced word space (TSNE), subjectivity and topics
pd_sent_topic<-tsne_words %>%
  left_join(tm_topics, by=c("word"="term")) %>%
  left_join(wrd_cnt, by=c("word"="word")) %>%
  filter(beta > .005) 

plot_sent_top<-ggplot() +
  geom_point(data=pd_sent_topic, aes(X1, X2,size=total,alpha=.01, color=subjectivity)) +
  geom_text(data=pd_sent_topic, aes(X1, X2, label=word), size=2) +
  scale_colour_gradient2(low=muted("red"), mid="white",
                         high=muted("blue"), midpoint=0) +
  scale_size(range=c(1, 4)) +
  xlab("") + ylab("") +
  ggtitle(paste0("Term mapping per topic")) +
  labs(subtitle=paste0(scope," TSNE, LDA"))+
  guides(color=guide_legend(title="Subjectivity"), size=guide_legend(title="Frequency"), alpha=NULL) +
  scale_alpha(range=c(1, 1), guide="none") +
  theme_minimal() +
  scale_fill_viridis_d() +
  facet_wrap(~topic, scales=  "free")
(plot_sent_top)
plot.store <-paste0(report.loc,'Topic_Models_LDA_Gibbs_k',k_lda,'_TSNE_words_subjectivity_topic.png')
ggsave(plot.store, height=graph_height , width=graph_height * aspect_ratio, dpi=dpi)

rm(tnse_words)
}

rm(tm_top_terms)

#----------------------------------------------------------------------------------------------

# wordcloud for every topic

#join topics with annotation output 
lda_token<-left_join(tokens_df, gamma_df, by=c("doc_id"="doc_id"))
lda_token<-left_join(lda_token, top_df, by=c("doc_id"="doc_id"))

#future use?
#lda_text<-left_join(text_df, gamma_df, by=c("doc_id"="doc_id"))
#lda_text<-left_join(lda_text, top_df, by=c("doc_id"="doc_id"))

#lda_words<-left_join(tokens_df, gamma_df, by=c("doc_id"="doc_id"))
#lda_words<-left_join(lda_words, top_df, by=c("doc_id"="doc_id"))

#----------------------------------------------------------------------------------------------

for (i in 1:k_lda) { 
  
if(plot_topic_wc == TRUE) { 
  df_lda_topic <-lda_token[lda_token$topic == i, ]
  dmodel_lda_topic<-cast_dtm(df_lda_topic, doc_id, token, sentence_id)
  m<-as.matrix(dmodel_lda_topic)
  v<-sort(colSums(m),decreasing=TRUE)
  d<-data.frame(word=names(v),freq=v)
  
#create clouds
plot.store<-paste0(report.loc,'Topic_Models_LDA_Gibbs_wordcloud_topic_',i,'.png')
wc.title<-paste0("Wordcloud LDA topic ",i)
png(plot.store, width=8, height=6, units="in", res=300)

layout(matrix(c(1, 2), nrow=2), heights=c(1, 10))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, paste0("LDA topic ", i))

wordcloud(words=d$word,
          # Create word cloud by raw frequency
          freq=d$freq, 
          # Or by adjusted importance
          #freq=d$adjusted_importance,
          #c(8,.3),
          min.freq=wc_n_min,
          max.words=wc_words_max, 
          random.order=FALSE, 
          rot.per=0.35, 
          scale=c(6.5, 2.5),
          use.r.layout=FALSE,
          colors=color_scheme_dark
          )

dev.off()
}

if(plot_topic_cor == TRUE) {   
#correlations among the terms driving the topic 
#lemma based to reduce number of unique (similar) terms
plot.store<-paste0(report.loc,'Topic_Models_LDA_Gibbs_correlation_lemma_topic_',i,'.png')
wc.title<-paste0("Term correlation within LDA topic ",i, " (lemma based)")
png(plot.store, width=12, height=8, units="in", res=300)
termcorrs<-subset(x_topics, topic %in% i & lemma %in% topic_terminology[[i]]$term)
termcorrs<-document_term_frequencies(termcorrs, document="textrank_id", term="lemma")
termcorrs<-document_term_matrix(termcorrs)
termcorrs<-dtm_cor(termcorrs)
termcorrs[lower.tri(termcorrs)]<-NA
diag(termcorrs)<-NA
qgraph::qgraph(termcorrs, layout="spring", labels=colnames(termcorrs), directed=FALSE,
       borders=FALSE, label.scale=FALSE, label.cex=1, node.width=0.5,title=wc.title, minimum=0.1)
dev.off()
}

}

objects_to_remove <- c("lda_token","gamma_df","model_lda","dmodel_lda_topic")
remove_objects(objects_to_remove)