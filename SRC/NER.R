
#------------------------------------------------------------------------------

#Named-entity recognition

#------------------------------------------------------------------------------

#Named entities are phrases that contain the names of persons, organizations, locations, times and quantities. 
#Under construction

#------------------------------------------------------------------------------

x <- ner_download_modeldata("conll2002-nl")
x <- crf_cbind_attributes(x, 
                          terms = c("token", "pos"), by = c("doc_id", "sentence_id"), 
                          from = -2, to = 2, ngram_max = 3, sep = "-")

## Split in train/test set
crf_train <- subset(x, data == "ned.train")
crf_test <- subset(x, data == "testa")

## Build the crf model
attributes <- grep("token|pos", colnames(x), value=TRUE)
ner_model <- crfsuite::crf(y = crf_train$label, 
             x = crf_train[, attributes], 
             group = crf_train$doc_id, 
             #training method : L-BFGS with L1/L2 regularization (lbfgs)
             method = "lbfgs", options = list(max_iterations = 25, feature.minfreq = 5, c1 = 0, c2 = 1)) 
ner_model

## Use the model to score on existing tokenised data
ner_df <- predict(ner_model, newdata = annotate_core_df[,"token"], group = annotate_core_df$doc_id)

table(ner_df$label)

#add \NER-label tokens annotation
annotate_core_df$namedentity<-ner_df$label

#  category labels: O, B-ORG, B-MISC, B-PER, I-PER, B-LOC, I-MISC, I-ORG, I-LOC

#filter-put persons
#annotate_core_df <- annotate_core_df %>% 
#  filter(!namedentity %in%  c("I-PER", "B-PER"))


