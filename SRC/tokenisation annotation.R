
#------------------------------------------------------------------------------

#Tokenization and annotation
#Parts of Speech tagging with Udpipe

#------------------------------------------------------------------------------

#Udpipe by BNOSAC (https://bnosac.github.io/udpipe/en)
#paper: https://www.aclweb.org/anthology/K17-3009/
#accuracy: https://github.com/jwijffels/udpipe.models.ud.2.5/blob/master/inst/udpipe-ud-2.5-191206/README
#UDPipe provides language-agnostic tokenization, tagging, lemmatization and 
#dependency parsing of raw text

message("Tokenization and annotation...")

#store location tokens
anno.file<-paste0(data.dir,"annotate_df.Rda")

if (read_saved_tokens==FALSE | !exists("annotate_df_df")) {
#is Udpipe model available already?
list_models=list.files(model.dir,recursive=TRUE)
search_udpipe="*.udpipe"
present_udpipe=grepl(search_udpipe,list_models)

udpipe_missing<-all(present_udpipe == FALSE)

if(udpipe_missing==TRUE){
  #download Udpipe (annotation model Alpino (Dutch))
  #https://github.com/jwijffels/udpipe.models.ud.2.5
  ud_dutch<-udpipe_download_model(language="dutch-alpino", model_dir=model.dir 
                                    #1GB version (initiate udpipe package in 'SRC/packages.R' first, see end of script) 
                                    #,udpipe_model_repo="udpipe.models.ud.2.5"
                                    )
  if(!ud_dutch$download_failed){
  udmodel_dutch<-udpipe_load_model(file=ud_dutch$file_model)
  }
} else {
  #Udpipe is available, load Udpipe
  udpipe.file<-list.files(path=model.dir,pattern="udpipe")   
  ud_loc<-paste0(model.dir,udpipe.file)
  udmodel_dutch<-udpipe_load_model(file=ud_loc)
}

#------------------------------------------------------------------------------
## Tokenization and annotation

if(word_correct_passed==TRUE || word_correct==FALSE) { 
  #Part-of-speech tagging (POS) and lemmatisation 
  up_tagger<-"default" # "none" or "default" 
  #dependency parsing 
  up_dep_parse<-"default" 
} else {
  #Tokenisation only
  up_tagger<-"none" 
  up_dep_parse<-"none" 
}

#Part-of-speech tagging (POS) and lemmatisation (=requirement)
up_tagger<-"default" # "none" or "default" 

#It is important that the x argument to udpipe_annotate is in UTF-8 encoding 

if(size_tc>40000 & par_processing==TRUE) {
#large text collections (40k< records)
#split text collections and use as many cores as possible
  
#splitted annotation function
  annotate_splits<-function(x, file) {
    udmodel_dutch<-udpipe_load_model(file=ud_loc)
    x<-as.data.table(udpipe_annotate(udmodel_dutch, 
                                       x=text_df$text,
                                       doc_id=text_df$doc_id,
                                       tokenizer="tokenizer", 
                                       tagger=up_tagger, 
                                       parser=up_dep_parse))
    return(x)
  }  
    
## Split and run process over multiple cores
plan(multiprocess, workers=thread)
corpus_splitted<-split(text_df, seq(1, size_tc, by=500))
anno<-future_lapply(corpus_splitted,annotate_splits,file=udmodel_dutch$file_model) 
anno<-rbindlist(anno)
annotate_df<-anno
rm(anno)
} else {
#'large' text collections
if(size_tc>10000) {  
#use eight cores instead of one
n.cor.up<-8 #keeps overhead rel. low
} else {
#'small' text collections  
#dont bother, setting up parallel backend takes too much time for this effort
n.cor.up<-1 
}

annotate_df<-as.data.frame(udpipe_annotate(udmodel_dutch,x=text_df$text, doc_id=text_df$doc_id
                         ,parallel.cores=n.cor.up
                         #Tokenizer, POS tagging, lemmatization and dependency parsing
                         ,tokenizer="tokenizer", tagger=up_tagger, parser=up_dep_parse)) 
}

#Udpipe Output  | Beschrijving
#-------------- | ---------------
#LEMMA          | Lemma of stem van woord
#UPOS           | Universele Part of speech label 
#XPOS           | Taal specifieke POS label 
#FEATS          | Extra morphologische attributen
#HEAD           | ID van HEAD van het woord (= 0 als woord root is)
#DEPREL         | Dependency relatie met HEAD 
#DEPS           | Enhanced dependency graph van HEAD-DEPREL paren
#MISC           | Verdere annotatie


#------------------------------------------------------------------------------
#extended pruning and cleaning

annotate_df<-annotate_df %>% 
  #stopwords
  anti_join(stopwords_df, by = c("token"="word")) %>% 
  #alphanumeric
  filter(str_detect(token, "[:alpha:]")) %>% 
  #word length
  filter(str_length(lemma) > word_len) 

#(unique) sentence identifier 'textrank': a combination of a document, paragraph and sentence identifier
annotate_df$textrank_id<-as.numeric(unique_identifier(annotate_df, c("doc_id", "paragraph_id", "sentence_id")))

#set to class numeric for numeric variables
cols.num<-c("doc_id","paragraph_id","sentence_id","token_id","head_token_id")
annotate_df[cols.num]<-sapply(annotate_df[cols.num],as.numeric)
sapply(annotate_df, class)

#store annotation (all tokens)
saveRDS(annotate_df,file=anno.file)
} else {
#read previous (stored) version of tokens
  load(file=anno.file)  
}

if(word_correct_passed==TRUE || word_correct==FALSE ) {
#tokens in our pos_list (aka relevant tokens)
annotate_core_df<-annotate_df %>% 
  data.table() %>% 
  filter(upos %in% pos_list) 

#add previous and next token
annotate_core_df <- annotate_core_df[, token_previous := data.table::shift(token, n = 1, type = "lag"), by = list(doc_id)]
annotate_core_df <- annotate_core_df[, upos_previous := data.table::shift(upos, n = 1, type = "lag"), by = list(doc_id)]

if(ner_anno==TRUE) {
#named entity recognition (under development)
source(here::here('SRC/NER.R'))  
}

anno_core.file<-paste0(data.dir,"annotate_core_df.Rda")
#store annotation (relevant tokens)
saveRDS(annotate_core_df,file=anno_core.file)

#annotate_core_df <- annotate_core_df[, token_next := data.table::shift(token, n = 1, type = "lead"), by = list(doc_id)]
#annotate_core_df <- annotate_core_df[, upos_next := data.table::shift(upos, n = 1, type = "lead"), by = list(doc_id)]

#Coerce data frame to data table 
annotate_core_dt<-setDT(annotate_core_df)
annotate_core_parenting<-merge(annotate_core_dt, annotate_core_dt, 
                                 by.x=c("doc_id", "paragraph_id", "sentence_id", "head_token_id"), 
                                 by.y=c("doc_id", "paragraph_id", "sentence_id", "token_id"), 
                                 all.x=TRUE, all.y=FALSE, suffixes=c("", "_parent"), sort=FALSE)
anno_core_parent.file<-paste0(data.dir,"annotate_core_parenting.Rda")
#store annotation (relevant tokens)
saveRDS(annotate_core_parenting,file=anno_core_parent.file)

#subsetting : POS version of UNIQUE relevant tokens
pos_df<-annotate_core_df %>%
  select(token, lemma, upos, xpos) %>%
  distinct(token, .keep_all=TRUE)
  # %>% count(token)
pos.file<-paste0(data.dir,"pos_df.Rda")
saveRDS(pos_df,file=pos.file)

#------------------------------------------------------------------------------

#create condensed version of sentences
#by stitching relevant POS-words (aka noise reduction)

#condensed sentences
condensed_sent<-annotate_core_df %>%
  group_by(textrank_id) %>% 
  dplyr::mutate(txt_condensed=paste0(token, collapse=" ")) %>% 
  #dplyr::mutate(txt_condensed=paste0(lemma, collapse=" ")) %>% 
  distinct(doc_id,textrank_id,txt_condensed, .keep_all=FALSE) 
condensed_sent.file<-paste0(data.dir,"condensed_sent.Rda")
saveRDS(condensed_sent,file=condensed_sent.file)

#condensed doc
condensed_doc<-annotate_core_df %>%
  group_by(doc_id) %>% 
  dplyr::mutate(doc_condensed=paste0(token, collapse=" ")) %>%
  #dplyr::mutate(doc_condensed=paste0(lemma, collapse=" ")) %>%
  distinct(doc_id,doc_condensed, .keep_all=TRUE) %>%
  #merge original doc text
  right_join(text_df, by="doc_id") %>% 
  select(doc_id, doc_condensed,text)
condensed_doc.file<-paste0(data.dir,"condensed_doc.Rda")
saveRDS(condensed_doc,file=condensed_doc.file)

#vector containing doc_ids of docs with information value
vec_relevant_docs<-condensed_doc$doc_id

#unload from memory
objects_to_remove <- c("udmodel_dutch","annotate_core_dt")
remove_objects(objects_to_remove)

}