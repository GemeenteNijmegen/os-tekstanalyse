
#-----------------------------------------------------------------------------------------------

#TextMining: Analyzing open-ended survey responses unsupervised

#Pipeline I: Preparation and exploration

#-----------------------------------------------------------------------------------------------

# Expertise Groep Data Science VNG
# Contact : Mark Henry Gremmen mark.gremmen@vng.nl
# Licence : Creative Commons BY-NC-SA 4.0
# GitLab repository
# https://gitlab.com/datasciencehub/text-analysis-of-short-texts
# report issues, bugs and wishes via GitLab

# last update : 2023-06-16


#-----------------------------------------------------------------------------------------------

# Scope pipeline T1: 
# packages > 
# globals > 
# import text collection > 
# cleaning and normalisation >
# word correction and synonym substitution>
# tokenisation and annotation >
# textembedding >
# ngram (T1 01) >
# sentiment 

#-----------------------------------------------------------------------------------------------

# primary output is located in directory 'REPORT', secondary output in 
# directories 'PLOTS' and 'DATA'. Each pipeline (Tx) reports in it's own subdirectory e.g. 'EXPLORATION'

# IMPORTANT: every run of this procedure -in new Rstudio session- deletes the content of these 
# directories, except for 'DATA'-directory where only *.Rda files will be deleted

#-----------------------------------------------------------------------------------------------

# Setup environment

#-----------------------------------------------------------------------------------------------

#show warnings
debug_mode<-FALSE #default (F)

#Run in isolated project environment (to avoid package conflicts)
#FALSE : use current version of packages, add new packages (recommended)
#TRUE : fresh install of all packages in isolated environment

proj_env<-FALSE #default (F)

#more information on updating packages
#https://www.linkedin.com/pulse/3-methods-update-r-rstudio-windows-mac-woratana-ngarmtrakulchol/

#language setting of computing setup (english or dutch)
lc_language<-"english" #default(s) to english

#set working directory explicitly
#setwd("D:/DATASCIENCE/text-analysis-of-short-texts") #for example

#setup and packages
source('SRC/packages.R')

#please review 'session_info.txt' if you encounter any problems up-to this point
#persistent problems may be resolved by setting proj_env to TRUE (run in isolated environment)
#conflicts()

#-----------------------------------------------------------------------------------------------

# Initialization

#-----------------------------------------------------------------------------------------------

#full-parallel processing (for very large text collections)
par_processing<-FALSE #default (F)

#pipeline identifier
scope<-"EXPLORATION" #directory name
scope.prefix<-"XPL"

#global settings
source(here::here('SRC/globals.R'))

#functions
source(here::here('SRC/functions.R'))

#pipeline timer
start_time<-Sys.time()

#-----------------------------------------------------------------------------------------------

# Import text collection

#-----------------------------------------------------------------------------------------------

#assumes file(s) with text collection in 'DATA' directory

#IMPORT OPTIONS: 1. dataset(s) containing entities per record, 2. (multiple) document(s) or 3. Twitter feed (API)
import_mode<-"records" #"records", "documents", or "tweets"

#adjust settings below for your specific import mode

#------------------------------------------------------------------

#1. RECORDS
#define extension of text collection file(s) containing entities like respondents
#assumes (uniform) file(s) with a text collection (respondents per row)
#file extension: 'txt',tsv' (tab-separated values),'csv','sav','xlsx'

#"records" setting:
#search in DATA directory for record file(s) of type
file_type<-"xlsx"

#read column names
column_name<-TRUE

#index of sheet (tab) containing text
text_sheet<-1 #integer

#position of column with text segments
text_col<-1 #integer

#position of column containing id of document/source, the column must contain integer id's 
docid_col<-99 #integer (or 99 if not available)

#------------------------------------------------------------------

#2. DOCUMENTS
#in case you want to read multiple files (not containing respondent records or similar entities)
#please specify document or feed type (pdf,rtf,docx,txt,xlsx,json,xml,html)
#note: native pdf only (otherwise OCR first, see 'SRC/OCR.R'), full-width text lay-out within documents

#"documents" setting:

#search in DATA directory for document(s) of type
doc_type<-"txt"

#------------------------------------------------------------------

#3. TWEETS
#extract Twitter timeline 
#set API keys in 'SRC/Twitter.R'

#tweet language
tw_language<-"nl"

#query string
tw_qry<-"Nijmegen"

#start date
#tw_dte<-"2023-04-23"

#number of tweets
tw_n<-10000

#include retweets
tw_rt<-FALSE

#-----------------------------------------------------------------------------------------------

#split text in sentences per row (in case there are no entities like respondents)
sentence_row<-FALSE #default (F)

source(here::here('SRC/import textcollection.R'))

#-----------------------------------------------------------------------------------------------

# Preprocessing: pruning, normalisation and cleaning of text collection

#-----------------------------------------------------------------------------------------------

#remove numbers from textcollection
remove_numbers<-FALSE #default (F) (FALSE: GDPR-related only)

#remove records containing one of the following (wildcard) text segments
dontknows<-c("9999", "nvt", "n.v.t.", "geen mening", "geen idee", "geen suggesties", "geen commentaar", "geen opmerkingen",  
               "niet specifiek", "weet niet", "N/A", "IDK")

#remove short and 'empty' records
#minimum length of text segments (>=x characters)
txt_len<-8 #char length

#cleaning of the text-collection (bulk actions)
source(here::here('SRC/normalisation cleaning.R'), encoding="UTF-8", local=TRUE)

#create unique fingerprint of (cleaned) source text 
#text_df$md5_pre<-sapply(text_df$text, digest)

#normalisation and synonyms
syns<-list(
  list(term="gunsnroses", syns=c("guns and roses", "guns 'n rose", "guns 'n roses", "guns en roses", "guns n roses", "guns n' roses", "guns roses", "gnr"))
  #,list(term="lhbti", syns=c('lhbtiqa+','lhbtiqa','lhbti+', 'lhbtiq+','lhbtiq'))
  #,list(term="goed", syns=c('toppie', 'puik', 'tof', 'juist', 'passend', 'correct', 'best', 'prima'))
  #,list(term="slecht", syns=c('niet goed', 'ondeuglijk', 'beroerd', 'belabberd', 'ondermaats', 'erbarmelijk', 'waardeloos', 'onvoldoende'))
  #,list(term="onduidelijk", syns=c('niet duidelijk', 'niet helder', 'wazig', 'warrig', 'twijfelachtig'))
  #,list(term="moeilijk", syns=c('niet makkelijk', 'weinig makkelijk', 'lastig', 'ingewikkeld', 'gecompliceerd','problematisch'))
  #,list(term="onbelangrijk", syns=c('niet belangrijk', 'onbetekend', 'van ondergeschikt belang','futiel','weinigzeggend', 'onbeduidend'))
  #,list(term="keren", syns=c('malen'))
  #,list(term="gevaarlijk", syns=c('levens gevaarlijk', 'levensgevaarlijk', 'riskant'))
  )

#search and replace
source(here::here('SRC/search replace.R'))

#-----------------------------------------------------------------------------------------------

# Stopwords

#-----------------------------------------------------------------------------------------------

#add stop words to default stopwords list (remove words from text collection)
stopwords_manual<-c("graag", "ja", "nee", "wel", "echt", "fan"
                    #,"suggestie", "vraag", "antwoord","reactie", "zie", "eergenoemde", "correspondentie", "tip"
                    #,"brief", "email", "telefoon", "gesprek", "gesproken", "voorgelegd"
                    ,"vriendelijke", "hartelijke" #afsluiting
                    #,"komende", "jaren", "jaar", "zoveel", "mogelijk", "beoogd", "mogelijkheden", "intenties" #intenties
                    #,"vandaag", "morgen", "periode", "termijn", "gaan", "rode", "draad" #intenties
                    #,"nu","gelijk","meteen","weldra","spoedig","snel","gauw", "inmiddels" #tijdsaspect
                    #,"wacht", "vooralsnog"
                    #,"fout", "goed", "slecht", "onjuist", "foutief", "incorrect",
                    #,"hoog", "laag"
                    #,"teveel", "teweinig", "genoeg", "tekort", "zonder", "afwezig", "frequent"
                    )

#remove stop words from default stopwords list (keep words in text collection)
stopwords_remove<-c( "geen", "niet", "niets", "niks", "nooit", "nimmer", "ooit", "meestal"
                     ,"erg", "minder", "meer", "evenveel", "veel", "weinig", "beetje", "enigszins", "zonder"
                     ,"redelijk", "tamelijk", "vrij", "behoorlijk"
                     ,"vaak", "dikwijls"
                     ,"zonodig","eveneens","voorts"
                     #,"vooral", "bovenal", "voornamelijk"
                     #,"wanneer", "waar", "hoe", "wat", "waarom", "waarvoor", "welke", "zodra", "ergens"  
                     ,"vooraf", "achteraf", "tijdens", "gedurende" , "daarna", "zolang"
                     #,"waardoor", "daardoor", "wegens", "vervolgens", "nadat", "zodat"
                     #,"voorheen", "voordien", "sindsdien", "sinds", "sedert", "wederom", "weer"
                     ,"vanaf", "vanuit", "tussen", "over", "op", "boven", "beneden"
                     ,"voorbij", "tegenover", "vandaan", "omstreeks", "verderop", "tussen"
                     ,"binnen", "buiten"
                     #,"alweer", "nogmaals", "wederom","opnieuw", "altijd", "steeds", "erbij" 
                     #,"verschillende", "verscheidene"
                     #,"wellicht", "misschien"
                     #,"dank"
                    )

#compile extended stop-word list 
source(here::here('SRC/stopwords.R'))

#-----------------------------------------------------------------------------------------------

# Conditional arguments for tokens (pruning)

#-----------------------------------------------------------------------------------------------

#general conditions for terms to be included in the token annotation and text embedding process 

#minimum length of word in characters (word_len >=x)
word_len<-2 #char length

#minimum absolute occurrence of word in the text collection (word_freq >=x)
word_freq<-5 #min. word occurrence

#reduce vector cloud (text-embedding)
#maximum proportion of documents which should contain word 
doc_prop_max<-0.95 #keep above 0.9

#minimum proportion of documents which should contain word
doc_prop_min<-0.0008 #factor related to number of records

#minimum importance of word in the text collection (tf-idf)
tfidf_threshold<-0.05 #keep under 0.2

#create unique fingerprint of cleaned text 
#text_df$md5_post<-sapply(text_df$text, digest)

#-----------------------------------------------------------------------------------------------

# Tokenisation and annotation

#-----------------------------------------------------------------------------------------------

#by-pass tokenisation and annotation and read saved tokens (after initial run within current session)
read_saved_tokens<-FALSE #default (F)

#POS-tag filter (relevant term types)
#NOUN: noun, VERB: verb, ADJ: adjective, ADP: adposition, ADV: adverb, X: other
pos_list<-c("NOUN", "VERB", "ADJ", "ADV")

#named entity recognition (NER)
#add NER- qualification in annotation process to annotate_core_df data frame
#under construction 
ner_anno<-FALSE #default (F)

#correct spelling
#use when necessary (e.g. respondents)
word_correct<-FALSE #default (T)

#tokenisation and annotation 
source(here::here('SRC/tokenisation annotation.R'))

#-----------------------------------------------------------------------------------------------

# Spelling correction

#-----------------------------------------------------------------------------------------------

#correct misspelling and alternative notation
source(here::here('SRC/wordcorrection.R'))

#-----------------------------------------------------------------------------------------------

# Token co-location and co-occurrences

#-----------------------------------------------------------------------------------------------

#token isolation method (function)
#tokenizer<-word_tokenizer # word_tokenizer or space_tokenizer

#context window: skip length between words (token embedding) 
#smaller contexts → syntactic properties
#larger contexts → semantic/topical properties
context_window<-5 # 3, 5 or 7 (default:5)

#context window assumption
skipgram_context<-"symmetric"

#plot token occurrences (oc)
tok_oc<-TRUE
#plot token co-occurrences (cooc)
tok_cooc<-TRUE

#report keywords in the exploration pipeline by token (original word) or lemma (canonical form)
keyword_report<-"token" # default "token"
#...within document (response) or sentence 
keyword_level<-"doc_id" # "doc_id" or "textrank_id" 

source(here::here('SRC/tokens occurrences.R'))

#-----------------------------------------------------------------------------------------------

#Text embedding / data matrices

#-----------------------------------------------------------------------------------------------

#DocumentTermMatrix (DTM), Term co-occurence matrix (TCM), 
#Term frequency-inverse document frequency (TF-IDF)

#text embedding represents a 'document'/answer (TRUE) or sentence (FALSE)
build_te_doc<-TRUE #default (T)

#build text embedding (dtm) on lemma (TRUE) or token (FALSE) (impact on : T3 Topic Modeling: BTM, (S)LDA)
build_te_lemma<-TRUE #default (T)

#apply weight when constructing dtm,  impact on : T3 Topic Modeling: LDA)
dtm_weight<-"freq" #freq (default) or tf_idf

#ignore terms with low differentiation power (impact on T3 Topic Modeling)
#especially, consider removing some 'useless' high-frequency terms 
#see plot in directory REPORT > EXPLORATION 'Occurrence_nouns'
terms_2b_ignored<-c("gemeente", "inwoner"
                   #,"aanpak", "uitdaging", "belangrijk", "rol" #urgentie termen
                    )

source(here::here('SRC/text embedding.R'))

#-----------------------------------------------------------------------------------------------

#Token cooccurence 

#-----------------------------------------------------------------------------------------------

#negation
#words that negate (make negative or nullify) the meaning of following word  (sentiment)
words_context_negative<-c("niet", "geen", "slecht", "slechte", "moeilijk", "moeilijke", "onvoldoende", "gebrekkig", "gebrekkige",
                          "nauwelijks", "afwezige", "geenszins", "niks", "niets", "nooit", "waardeloos", "waardeloze",
                          "nimmer","nergens", "niemand", "rommelig", "moeizaam", "moeizame", "mager")

#fortifiers
#words that reinforce the meaning of the following word (sentiment)
words_fortify<-c("zeer", "heel", "extreem", "lang", "super", "top", "echt", "perfect", "erg", "prima","gaaf","fantastisch","fantastische",
                 "absoluut","flink", "flinke", "groot", "grote", "ontzettend", "ontzettende", "beste","perfecte","mega","te","extra",
                 "onwijs", "onwijze", "geweldig", "geweldige", "enorm", "enorme", "fijn", "fijne", "dramatisch", "verschrikkelijk")

#contiguous sequences of tokens in text collection (cooccurrences)
source(here::here('T1 01 Preparation Ngram.R'))

#----------------------------------------------------------------------------------------------

#Subjectivity, sentiment, polarity and emotions

#-----------------------------------------------------------------------------------------------

#report emotions
emotion_report<-FALSE  #default (F)

source(here::here("SRC/subjectivity sentiment.R"))

#----------------------------------------------------------------------------------------------

#Debugging

#----------------------------------------------------------------------------------------------

rlang::last_error()
rlang::last_trace()

end_time<-Sys.time()
end_time - start_time