
#-----------------------------------------------------------------------------------------------

#Analyzing open-ended survey responses unsupervised

#Pipeline II: Word embedding

#-----------------------------------------------------------------------------------------------

# Pipeline T2: 
# Word and (weighted) Document Embedding (EM) (T2 01) > 
# Dimensionality Reduction (DR) (T2 02) > 
# Clustering (CL) (T2 03) >
# (Extractive Text) Summarisation (TS) (T2 04) >
# Reporting (RP) (T2 05) >
# Word Similarity (WS) (T2 06)

# This procedure evaluates and outputs different routes (methods) towards text summarisation 
# All output is written to the PLOTS and DATA directory, see subdirectory WORD2VEC, GLOVE or BERT 
# depending on the word embedding algorithm initialized below

# objective: capturing semantic similarities. find groups of coherent documents and summarize and describe them

#-----------------------------------------------------------------------------------------------

#Embedding algorithms

#-----------------------------------------------------------------------------------------------

#choose between: Word2Vec, Glove and BERT


#------------------------------

# Word2Vec
# Word2Vec takes texts as training data for a neural network. 
# The resulting embedding captures whether words appear in similar contexts. 
# Word2Vec is a predictive model
# scope<-"WORD2VEC" #default 
# scope.prefix<-"W2V"

#use pretrained Word2Vec-model Open Raadsinformatie
#why/when? if you need a policy perspective 
w2v_pretrained<-FALSE #(FALSE (default), TRUE (much slower!))

#------------------------------

#context independent methods (GloVe, Word2Vec) :

# Global Vectors (GloVe)
# GloVe focuses on words co-occurrences over the whole corpus. 
# Its embeddings relate to the probabilities that two words appear together.
scope<-"GLOVE"
scope.prefix<-"GLV"

#------------------------------

#context dependent method (BERT):

#Bidirectional Representation for Transformers (BERT)
#BERT can generate different word embeddings for a word that captures the context
#of a word - that is its position in a sentence.

# !!! BERT requires Python environment, read and follow instructions 
# in 'T2 01 Wordembedding BERT BRT.R' first!!!  
#scope<-"BERT"
#scope.prefix<-"BRT"

#-----------------------------------------------------------------------------------------------

#Settings and tuning parameters

#-----------------------------------------------------------------------------------------------

#(reload) global settings
source(here::here("SRC/globals.R"))

#Hyperparameters word embedding (T2 01)
#n-dimensional vector space (GloVe, Word2Vec)
#dimensionality : total number of features encoded during embedding process
vec_dim<-150

#type of algorithm: either ’cbow’or ’skip-gram’ (Word2Vec)
#skipgram is slower, but better for infrequent words, cbow is faster

#cbow: predict a word from its context 
#skip-gram: predict the context words
prediction_method<-"cbow"

#context window: skip length between words (embedding) (see T1 00)
#smaller contexts → syntactic properties
#larger contexts → semantic/topical properties
#context_window<-5 3, 5 or 7 (default:5, 7 or 9 for skipgram) 

#context window assumption
skipgram_context<-"symmetric" #(Word2Vec)

#iteration convergence tolerance
convergence_tolerance<-0.004 #(GloVe, Word2Vec)

#number of training iterations
iterations_n<-30 #(GloVe, Word2Vec)

#----------------------------------------------------
#Hyper-parameters dimension reduction (T2 02)

#TSNE

#perplexity : balance between local and global aspects of the text collection
#higher value means more preservation of the global structure
#anywhere between 30 and 50 is usually fine 
#https://distill.pub/2016/misread-tsne/

perplex<-40

#theta : default 0.5, decrease for more accuracy 
th<-0.3

#----------------------------------------------------
#extractive text summarisation

#number of (representative) text extractions per cluster (T2 04)
extract_text_n<-10

#-----------------------------------------------------------------------------------------------

#create fresh W2V model (w2v_pretrained==0)
path_model<-paste0(model.loc,scope.prefix,"_model.bin")
#apply trained model 'open raadsinformatie' (w2v_pretrained==1)
path_model_pretrained<-paste0(model.loc,"word2vec_ori.model")
#more pretrained models : http://vectors.nlpl.eu/repository

#-----------------------------------------------------------------------------------------------

#pipeline timer
start_time<-Sys.time()

#load normalised text (from preparation pipeline T1)
text_normalised<-readRDS(file=norm.file)

#----------------------------------------------------------------------------------------------

# Word embedding

#----------------------------------------------------------------------------------------------

#Word embedding is capable of capturing the context of a word in a document, semantic and 
#syntactic similarity, relation with other words, etc.

#----------------------------------------------------------------------------------------------

# Document embedding

#----------------------------------------------------------------------------------------------

#(weighted) document vectors
source(here::here("SRC/document embedding.R"))

#----------------------------------------------------------------------------------------------

# Dimensionality reduction
# two methods: TSNE and UMAP

#----------------------------------------------------------------------------------------------

lvl_start<-1 # (1=words, documents, 2=documents only)
#choose 1 for additional output: word clouds, a mix of topic modeling and word embedding i.c.w. subjectivity

# (re)load (stored) word and document vectors, word weights and stop words 
source(here::here("SRC/prepare dimension reduction.R"))

source(here::here("T2 02 Wordembedding Dimensionality Reduction.R"))

#----------------------------------------------------------------------------------------------

# Clustering
# four methods: K-mediods (PAM), Kmeans, Spherical Kmeans, Kmeans++ 

#----------------------------------------------------------------------------------------------

#optimal number of clusters for both words and documents
#set number 

#number of word clusters ('kw') 
kw<-15 #not very important

#'optimal' number of document clusters ('ks') is detected automatically, within the range set below 

#minimum number of document clusters ('k_min')
k_min<-5

#maximum number of document clusters ('k_max')
k_max<-15

#determine optimal number of document clusters. If FALSE set number of desired
#document clusters 'ks' manually below
k_optimal<-FALSE #default (T)

#!!!IMPORTANT!!!: you might want to choose a larger number of document clusters (ks) for more differentiation
#see XXX_n_clusters_documents.png in de PLOTS directory for alternative value

#'ks' must reside within 'k_min' - 'k_max' range (see above)
ks<-10 #when k_optimal is set to FALSE

source(here::here("SRC/clusternumber.R"))

#clustering methods
source(here::here("T2 03 Wordembedding Clustering.R"))

#----------------------------------------------------------------------------------------------

# Word similarity (Word2Vec, GLOVE)

#----------------------------------------------------------------------------------------------

#find neighbouring words for specific terms:
#predict_words<-c("wachtrij", "duur")

#source(here::here("T2 06 Wordembedding Wordsimilarity.R"))

#----------------------------------------------------------------------------------------------

#Extractive text summarisation
#method: LexRank algoritm for text extraction

#----------------------------------------------------------------------------------------------

#the objective is to provide sentences per cluster that are most representative

#use page rank algorithm for ranking sentences
#if false a sentences unweighted centrality will be used to rank (faster)
pagerank<-TRUE #default (T)

#compute lexrank using a weighted graph representation of the sentences (slower)
pagerank_continuous<-TRUE #default (T)

source(here::here("SRC/prepare summarisation.R"))

source(here::here("T2 04 Wordembedding Extractive Text Summarisation.R"))

#----------------------------------------------------------------------------------------------

#Text summarisation
#method: Textrank algoritm for sentence ranking

#----------------------------------------------------------------------------------------------

#source(here::here("T2 04 Wordembedding Text Summarisation.R"))

#----------------------------------------------------------------------------------------------

#Reporting 

#----------------------------------------------------------------------------------------------

source(here::here("T2 05 Wordembedding Reporting.R"))

#----------------------------------------------------------------------------------------------

#debugging

#----------------------------------------------------------------------------------------------

rlang::last_error()
rlang::last_trace()

end_time<-Sys.time()
end_time - start_time