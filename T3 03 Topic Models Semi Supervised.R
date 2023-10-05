
#----------------------------------------------------------------------------------------------

#Semi-supervised Topic Modeling (seeded LDA)

#----------------------------------------------------------------------------------------------

if(LDA_seeded==TRUE) {
  
#please define your topics in MODELS>topics.yml
topics_defined<-here::here(model.dir,'topics.yml')

dict <- quanteda::dictionary(file=topics_defined)
print(dict)

#create corpus format from tokens df
tc = corpustools::tokens_to_tcorpus(annotate_core_df, doc_col = 'doc_id',
                       sentence_col = 'sentence_id', token_id_col = 'token_id')
#tc

tc$preprocess(column = 'token', new_column = 'feature',
              remove_stopwords=F, use_stemming=F)

dfm = corpustools::get_dfm(tc, 'feature')
#dfm

slda <- seededlda::textmodel_seededlda(dfm, dict, residual = TRUE)

#topic terms
slda_terms<-terms(slda, 30)
write.csv(slda_terms,file=paste(report.loc,"Topic_Models_SLDA_topic_terms.csv"))

#topic incidence
seededlda::topics(slda) %>%
  table() %>%
  write.csv(file=paste(report.loc,"Topic_Models_SLDA_topic_distribution.csv"))

}
