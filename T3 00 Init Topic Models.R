
#-----------------------------------------------------------------------------------------------

#Analyzing open-ended survey responses unsupervised

#Pipeline III : Topic modeling

#-----------------------------------------------------------------------------------------------

#Probabilistic topic modeling
#automatically classifying sets of responses/documents into topics or themes

# Biterm Topic Models (BTM) T3 > 
# Latent Dirichlet Allocation (LDA) T4

#-----------------------------------------------------------------------------------------------

#### Initialization ####

#Please review the text embedding settings (DocumentTermMatrix) in 'T1 00 Init Preparation.R' 
#1. UPOS filtering
#2. 'doc_id' vs 'textrank_id' (reference level)
#3. 'lemma' vs 'tokens' (normalisation of terms)

#-----------------------------------------------------------------------------------------------

#pipeline identifier
scope<-"TOPICMODEL"
scope.prefix<-"TM"

#global settings
source(here::here('SRC/globals.R'))

#-----------------------------------------------------------------------------------------------
#settings

#general settings

#set first topic as garbage bin for background topic distribution (aka noise)
#This can be used to filter out common words which build the majority of topics (BTM,LDA). 
denoise<-TRUE #default (F) 

#number of terms to show per topic (BTM,LDA)
topic_terms_n<-15

#plot wordclouds (LDA) 
wc_n_min<-5 #minimal frequency occurrence for display in wordcloud
wc_words_max<-30 #maximum number of words to show in wordcloud

#show interactive (shiny) visualisation (BTM,LDA)
lda_vis<-FALSE #default (T)
#either for btm or lda
lda_vis_mode<-"lda" # 'lda' (default), 'btm'


#---------------------------------

#LDA (default)

#range of topics
#minimum number of topics (LDA)
lda_topic_min<-5
#maximum number of topics (LDA)
lda_topic_max<-20

#determine optimal number of topics within the defined range (LDA)
#advice: do this once (set: TRUE) with a lda_topic_max value of 30 
#run again with K_lda set to this value and lda_optimal_k to FALSE

#for manual interpretation of 'optimal' value see PLOTS>
#Topic_Models_LDA_Gibbs_number_of_topics.png
#Best situation:
#minimization: Arun2010, CaoJuan2009
#maximization: Deveaud2014, Griffiths2004 
lda_optimal_k<-FALSE #default (T)

#set number of topics manually (in case lda_optimal_k==FALSE ) (LDA)
k_lda<-15

#combine topics and word embedding
topic_embed<-FALSE

#plot combination of reduced word space (TSNE), subjectivity and topics (LDA)
lvl_start<-1 #(0=no, 1=yes)

#plot topic word clouds (LDA)
plot_topic_wc<-TRUE

#plot top term correlations (LDA)
plot_topic_cor<-TRUE


#-----------------------------------------------------------------------------------------------

#pipeline timer
start_time<-Sys.time()

#-----------------------------------------------------------------------------------------------


#-----------------------------------------------------------------------------------------------

#Biterm Topic Models (BTM) (T3 01)

#-----------------------------------------------------------------------------------------------

#BTM is a word co-occurrence based topic model that learns topics by modeling 
#word-word co-occurrences patterns (e.g., biterms) and is designed for clustering short text

#generate BTM
btm<-TRUE #default (F)

#number of BTM topics
k_btm<-15

#plot parent-child dependency relationship (besides direct word-word cooccurrences)
#sometimes this gives better insight than the standard word-word cooccurrences in the standard BTM
btm_dep_rel<-FALSE #default (F)

source(here::here('T3 01 Topic Models Biterm topics.R'))

#terminate server (LDAvis interactive presentation BTM)
#servr::daemon_stop(1)

#-----------------------------------------------------------------------------------------------

# Latent Dirichlet Allocation (LDA) (T3 02)

#-----------------------------------------------------------------------------------------------
#LDA is for clustering long(er) text 
#LDA clusters word-document occurrences

#documents are considered as random mixtures of various topics and 
#topics are considered a mixture of different words.

#guided LDA (to be depreciated), please use Semi-supervised Topic Modeling instead
LDA_guided<-FALSE #default (F)
#if FALSE, continue with regular Gibbs LDA...

#tuning LDA Topic Models
source(here::here('SRC/tuning LDA.R'))

# detect 'optimal' number of topics:
source(here::here('SRC/topicnumber.R'))

source(here::here('T3 02 Topic Models LDA topics.R'))

#terminate server LDAvis interactive presentation LDA)
#servr::daemon_stop(1)


#-----------------------------------------------------------------------------------------------

#Semi-supervised Topic Modeling (SLDA) (T3 03)

#-----------------------------------------------------------------------------------------------

#Seeded LDA
LDA_seeded<-FALSE #default (F)

##please define your topics (seeds) first in MODELS>topics.yml
#use max 5 key words per topics

source(here::here('T3 03 Topic Models Semi Supervised.R'))

#----------------------------------------------------------------------------------------------

#debugging

#----------------------------------------------------------------------------------------------

rlang::last_error()
rlang::last_trace()

end_time<-Sys.time()
end_time - start_time