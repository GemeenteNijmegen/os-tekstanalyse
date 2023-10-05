
#-----------------------------------------------------------------------------------------------

# T2 WORD and DOCUMENT EMBEDDING -> BERT

#-----------------------------------------------------------------------------------------------


#Contextualised Embeddings and Language Modelling using BERT

#BERT stands for Bidirectional Encoder Representations from Transformers

#Word2vec and Glove word embeddings are context independent- these models output 
#just one vector (embedding) for each word, combining all the different senses of the word 
#into one vector.

#BERT can generate different word embeddings for a word that captures the context
#of a word - that is its position in a sentence.

#-----------------------------------------------------------------------------------------------

#setup requirements:

#needed for installation from GitHub
#install.packages("devtools")
#comes with Rtools
#install.packages("Rcpp")
#incorporate Python in R
#install.packages("reticulate")

packages <- c('devtools','Rcpp','reticulate')

#install packages which are not available on the computing setup
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE
       #,quietly = TRUE
)

#--------------------------------------------------------------------------------------------
#requirements

#got Python?
Sys.which("python")

#Python modules transformers and tensorflow present?
transformer_mod <- reticulate::py_module_available('transformers')
tensorflow_mod <- reticulate::py_module_available('tensorflow')

#--------------------------------------------------------------------------------------------

if(transformer_mod == FALSE) {
  #if not, install

  #Python Conda environment
  #https://docs.conda.io/
  conda_install(
    envname = 'r-reticulate',
    c('torch', 'transformers==4.3.2','tensorflow'), 
    pip = TRUE
    )
  
  #in case of problems make sure PyTorch is installed first
  #install PyTorch
  virtualenv_install("r-reticulate", "torch")
  
  #or manually via (Anaconda) powershell, as administrator
  #pip install transformers[tf-cpu]
  
  #or regular Python environment
  #reticulate::py_install('transformers', pip = TRUE)
} else {
  conda_create("r-reticulate")  
}

#--------------------------------------------------------------------------------------------
#review settings

#review paths
py_config()
#reticulate::py_discover_config()

#Alternatively you can select the Python environment using the reticulate package:
  
#reticulate::use_python("~/anaconda3/bin/python", required = TRUE)  
#reticulate::use_python(python = "/usr/local/bin/python3.9")

#list condaenv
conda_list(conda = "auto")

# indicate that we want to use Conda environment r-reticulate
reticulate::use_condaenv("r-reticulate")
# or Python environment (change environment name to environment with transformers installed)
#reticulate::use_virtualenv(virtualenv = "myenv")



#--------------------------------------------------------------------------------------------

#import modules
transformer = reticulate::import('transformers')
tf = reticulate::import('tensorflow')
#builtins <- import_builtins() #built in python methods

Sys.setenv(TF_KERAS=1) 

#--------------------------------------------------------------------------------------------

#Golgotha : Contextualised Embeddings and Language Modelling using BERT
#https://github.com/bnosac/golgotha

#This package requires transformers and torch to be installed. 
#Normally R package reticulate automatically gets this done for you.

if("golgotha" %in% rownames(installed.packages()) == FALSE) {
  
  devtools::install_github("bnosac/golgotha", 
                           #INSTALL_opts = "--no-multiarch", 
                           force=TRUE)
  }

library(golgotha)

#----------------------------------------------------------------------------------------------

#Model list (Dutch)
#https://huggingface.co/models?filter=nl

#Dutch model BERTje
#BERTje is a Dutch pre-trained BERT model developed at the University of Groningen

bert.model<-paste0('GroNLP/bert-base-dutch-cased')
bert.loc <- paste0(model.loc,bert.model)
#DistilBERT is smaller and faster than BERT, which was pretrained on the same corpus in a 
#self-supervised fashion, using the BERT base model as a teacher
#distilbert.loc <- paste0(model.loc,'pdelobelle/robbert-v2-dutch-base')

#is Bert model available already?
list_bert=list.files(bert.loc,recursive=FALSE)
search_bert="*.bin"

present_bert=grepl(search_bert,list_bert)

bert_missing <- all(present_bert == FALSE)
#bert_missing <- TRUE #(for distilbert testing)

if(bert_missing == TRUE) {
#it is not, therefore download  
  
#transformer_download_model(model_name="pdelobelle/robbert-v2-dutch-base", architecture = "DistilBERT", path=model.loc)  
transformer_download_model(model_name=bert.model, architecture="BERT", path=model.loc)
}

#bert_model <- transformer(model_name ="pdelobelle/robbert-v2-dutch-base", architecture = "DistilBERT", path=distilbert.loc)
bert_model <- transformer(model_name=bert.model, architecture="BERT", path=bert.loc)

text_df_brt <- text_df %>%
select(text, doc_id) 

#text_df_brt <- setNames(text_df_brt, c("text","doc_id"))

#sentence embedding
sv_wei_df <- predict(bert_model, text_df_brt, type = "embed-sentence")
sv_wei_df <- as.data.frame(sv_wei_df)

sv_wei_df$doc_id <- as.numeric(text_df_brt$doc_id)
sv_wei_df <- column_to_rownames(sv_wei_df, var = "doc_id")
sv_wei_df$doc_id <- as.numeric(rownames(sv_wei_df))

#save sentence vectors
save(sv_wei_df,file=paste0(data.loc,scope.prefix,"_","sv_wei_df.Rda"))

#embedding <- predict(bert_model, text_df_brt, type = "embed-token")
#tokens    <- predict(bert_model, text_df_brt, type = "tokenise")

rm(text_df_brt)

#garbage collection
gc()