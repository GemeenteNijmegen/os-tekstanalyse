
#------------------------------------------------------------------------------

#Setup and packages

#------------------------------------------------------------------------------


#Functions

#unload all packages
unload_all_packages <- function() {
  # Get a vector of all currently loaded package names
  loaded_pkgs <- loadedNamespaces()
  
  # Loop through the vector and unload each package
  for (pkg in loaded_pkgs) {
    detach(package:pkg, unload = TRUE)
  }
}


# check if a specific package exists, install it if it doesn't exist, and load it
check_and_load_package <- function(pkg){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}


#management of renv
manage_renv <- function(required_packages = NULL) {
  # Check if renv is installed
  if (!requireNamespace("renv", quietly = TRUE)) {
    message("renv is not installed. Installing now...")
    install.packages("renv")
  }
  
  # Check if renv is initialized for this project
  renv_status <- renv::status()
  if (length(renv_status) == 0 || (length(renv_status) > 0 && renv_status$`renv version` == "uninitialized")) {
    message("Initializing renv for this project...")
    renv::init()
  }
  
  # Activate the project's renv environment
  renv::activate(force = TRUE)
  
  # Populate the renv cache with package sources
  message("Populating the renv cache with package sources...")
  renv::populate_cache()
  
  # Install required packages if they are not already installed
  if (!is.null(required_packages)) {
    missing_packages <- setdiff(required_packages, rownames(installed.packages()))
    if (length(missing_packages) > 0) {
      message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
      install.packages(missing_packages)
    }
  }
  
  # Restore the project's packages
  renv::restore()
}



#------------------------------------------------------------------------------

#clear console
cat("\014")  

#garbage collection
gc(verbose = FALSE, full = TRUE)

#CPU cores (defaults to 1, we use 8)
getOption("Ncpus", 1L)
options(Ncpus = 8)

#detach packages
#packages_unload <- c('dplyr','tidyverse')
#detach_package(packages_unload, TRUE)

#detach all packages
#unload_all_packages()

#CRAN packages
packages <- c(
  #set up directories
  'here',
  #environment variables
  'usethis',
  'tools','utils','fs',
  #external packages (not-being from cran-repo or similar)
  #'devtools',
  #essentials
  'tidyverse',
  #or elements of tidyverse:
  #'stringr','tibble','ggplot2',
  'janitor','data.table',
  #'slam',
  #'htmlTable',
  #color schemes
  'RColorBrewer','viridis','scales',
  #documents
  'readtext',
  #excel  
  #'openxlsx',
  'readxl',
  # SPSS 
  'haven',
  # txt 
  'readr',
  #Read and Write Rectangular Text Data Quickly
  'vroom',
  #read and convert pdf
  #pdf
  'pdftools',
  #ocr
  #'tesseract',
  #tweets
  'rtweet',
  #xml
  'xml2',
  #text manipulation, general text mining functions
  'tidytext',
  #'corpus',
  'stringi','fuzzyjoin',
  #scraping
  'rvest',
  #annotation
  'udpipe',
  #tools for anayzing text corpora
  'corpustools',
  #auto-correct
  #'hunspell',
  #'nametagger',
  #plots and graphs
  'ggraph','qgraph','igraph','wordcloud','lattice','ggrepel','textplot','esquisse',
  #word-embedding
  'text2vec','word2vec',
  #wrapper for StarSpace C++ library
  #'ruimtehol',
  #wrapper for CRFsuite C/C++ library (NER)
  'crfsuite',
  #sentiment
  'syuzhet',
  #dimensionality reduction
  'Rtsne','umap','dimRed','coRanking',
  #clustering
  'cluster','fpc','skmeans',
  #Light Cone Reconstruction of States : kmeanspp
  #'LICORS', 
  #Validation of Clustering Results
  #'clValid',
  #visualize
  'factoextra',
  #text summarisation
  'lexRankr','textrank','textreuse',
  #'textreuse',
  #parallel computing
  'RcppParallel', 'future.apply', 'doParallel','parallel',
  #polygon
  'concaveman',
  # topic models
  'BTM', 'topicmodels', 'ldatuning','lda', 'topicdoc','seededlda',
  #interactive interface
  'LDAvis', 'servr',
  #communication with Jupyter
  #'IRkernel', 
  #Compact Hash Digests of R Objects
  'digest')

if(proj_env == TRUE) {
  manage_renv(required_packages = packages)
  
} else {
  #install packages which are not available on the computing setup
  has_available   <- packages %in% rownames(installed.packages())
  if(any(!has_available)) install.packages(packages[!has_available],Ncpus=4)
  
  invisible(lapply(packages,library,character.only=TRUE,quietly=TRUE))
}

#1GB version of Udpipe model (requires devtools package)
#install.packages("udpipe.models.ud.2.5", repos = "http://www.datatailor.be/rcube", type = "source")

#review packages loaded
utils::sessionInfo() %>% capture.output(file="session_info.txt")