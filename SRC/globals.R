
#-----------------------------------------------------------------------------------------------

#Globals and constants

#-----------------------------------------------------------------------------------------------

#OS
system <- Sys.info()['sysname']

# encoding
Sys.getlocale()

#default character set
charset<-"UTF-8"

options(encoding = charset)      
#see if things were changed
#getOption("encoding") 

loc <- function(os, language = "english") {
  switch(language,
         english = ifelse(os == "Windows", "English_United States.1252", "en_US.UTF-8"),
         dutch = ifelse(os == "Windows", "Dutch_Netherlands.1252", "nl_NL.utf-8"))
}

##set locale
Sys.setlocale(category = "LC_ALL", loc(system, lc_language))

#shutup dplyr's summarise notifications
options(dplyr.summarise.inform=FALSE)

#m tidyverse
options(tidyverse.quiet = TRUE)

#shutup readr
options(readr.show_col_types = FALSE)

#prevent exponential / scientific notation.
options(scipen=999) 

#turn off R’s automatic conversion of strings into factors
options(stringsAsFactors=FALSE)

#turn off warnings
if(debug_mode==FALSE) {
  options(warn = -1)
} else {
  options(warn=0)  
} 

#https://irkernel.github.io/installation/
#IRkernel::installspec(user = FALSE)

#-----------------------------------------------------------------------------------------------
#CPU and RAM

#number of CPU cores to use (defaults to 1)
thread<-parallel::detectCores()
#for system stability leave one core available
thread<-thread-1

if(is.numeric(thread)==FALSE) {
  thread<-8  
}

if(par_processing==TRUE) {
  #register cores
  cl <- NULL
  if (system == 'Windows') {
    cl <- parallel::makeCluster(getOption('cl.cores', thread)
                                ,setup_strategy = "sequential"
                                ,setup_timeout = 0.5,
                                type = "PSOCK"
    )
    
    doParallel::registerDoParallel(cl)
    #registerDoSEQ()
    on.exit(parallel::stopCluster(cl))
  } else {
    options('mc.cores' = thread)
    doParallel::registerDoParallel(thread)
  }
}

#windows uses parLapply(cl, <code>)
#Mac / Linux relies on forked processing and uses mclapply(<code>)
#... or use foreach instead

#explicitly set number of threads
RcppParallel::setThreadOptions(numThreads=thread)

ram <- memory.limit() #check memory limit

memory.limit(size=ram)  #use entire available memory

#-----------------------------------------------------------------------------------------------

#set pipeline identifier
if (!exists("scope")) {
  #set initial pipeline
  scope<-"EXPLORATION" #default
  scope.prefix<-"EXP" #default
}

#directories
#create directories on-the-fly if not exist

#location data
data.dir <- here("DATA",'/')
data.loc <- here("DATA",scope,'/')

#location lexicons
lexicon.dir <- here("DATA","LEXICON",'/')

#location graphs (throughput)
plots.dir <- here("PLOTS",'/')
plots.loc <- here("PLOTS",scope,'/')

#location report (output)
report.dir <- here("REPORT",'/')
report.loc <- here("REPORT",scope,'/')

#location models
model.dir <- here("MODELS",'/')
model.loc <- here("MODELS",scope,'/')

#(re)create locations if not exist
locations <- c(report.dir, plots.dir, data.dir, model.dir, lexicon.dir, 
               report.loc, plots.loc, data.loc, model.loc)

lapply(locations, function(x) {
  if (!dir.exists(x)) {dir.create(x)}
})

#clear output directories each new session
if(!exists("text_df") ) {
  #clear graphs and reports folder (output)
  clear_locations <- c(plots.dir,report.dir)
  
  # get all files in the directories, recursively
  f <- list.files(clear_locations, include.dirs=F, full.names=T, recursive=T)
  # remove the files
  file.remove(f)
  
# Remove .Rda files from DATA directory (temporary data files), first level only
data.hist <- list.files(data.dir,pattern = ".Rda",full.names=TRUE, include.dirs=TRUE,recursive=FALSE)
#except for annotated tokens from previous run
data.hist[lapply(data.hist,function(x) length(grep("annotate_df",x,value=FALSE))) == 0]

unlink(data.hist, recursive=FALSE)
}

#-----------------------------------------------------------------------------------------------
#Models

#path fresh W2V model (w2v_pretrained==0)
path_model <- paste0(model.loc,scope.prefix,"_model.bin")
#path trained model 'open raadsinformatie' (when w2v_pretrained==1)
path_model_pretrained <- paste0(model.loc,"word2vec_ori.model")

#seed for reproducibility
set.seed(90210)  
seed<-90210

#-----------------------------------------------------------------------------------------------
#Graphs

#dimension and quality plots
graph_height <- 9
png_height <- 600
aspect_ratio <- 2
dpi <- 320 #retina
sub_title<-''
bgcolor<- "transparent"
#word cloud color.s
color_scheme_dark <- brewer.pal(10, "Paired") #Paired, Dark2, Set1

#-----------------------------------------------------------------------------------------------

#filtering
clean_full<-TRUE

#word correction passed (set pointer)
word_correct_passed<-FALSE

#in case of Rprofile issues
#file.edit(file.path("~", ".Rprofile"))