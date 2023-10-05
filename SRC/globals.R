
#------------------------------------------------------------------------------

#Globals and constants

#------------------------------------------------------------------------------

system <- Sys.info()['sysname']

#number of CPU threads to use. defaults to 1.
thread<-parallel::detectCores()


# voor parallel computing
# #register cores
# cl <- NULL
# if (system == 'Windows') {
#   cl <- parallel::makeCluster(getOption('cl.cores', thread))
#   registerDoParallel(cl)
#   registerDoSEQ()
#   on.exit(stopCluster(cl))
# } else {
#   options('mc.cores' = thread)
#   registerDoParallel(thread)
# }


#explicitly set number of threads
RcppParallel::setThreadOptions(numThreads = thread)

ram <- memory.limit() #check memory limit
memory.limit(size=ram)  #use entire available memory

#prevent exponential / scientific notation.
options(scipen = 999) 

#turn on  R’s automatic conversion of strings into factors
options(stringsAsFactors = TRUE)

#turn off warnings
options(warn=0) #0 is on, -1 is off


#directories
#create directories on-the-fly if not exist

# location SRC
src.dir <- here('SRC')

#location data
scope_data <- c('')
data.dir <- here("DATA",'/')
data.loc <- here("DATA",scope_data,'/')
data.loc.raw <- here("DATA/RAW",'/')
data.loc.tussentijds <- here("DATA/TUSSENTIJDS",'/')

#location graphs (throughput)
scope_categorie_plots <- c() # bijvoorbeeld 'exploratief', 'final'
plots.dir <- here("PLOTS",'/')
plots.loc <- here("PLOTS",scope_categorie_plots,'/')

#location report (output)
scope_categorie_karel <- c('PLOTS', 'TABEL', 'RAPPORTAGE')
report.dir <- karelstad_report_loc
report.loc <-paste0(karelstad_report_loc, scope_categorie_karel,'/')
report.loc.plots <- paste0(karelstad_report_loc, '/PLOTS/')
report.loc.tabel <- paste0(karelstad_report_loc, '/TABEL/')
report.loc.rapportage <- paste0(karelstad_report_loc, '/RAPPORTAGE/')

#location models
scope_categorie_models <- c('')
model.dir <- here("MODELS",'/')
model.loc <- here("MODELS",scope_categorie_models ,'/')

#(re)create locations if not exist
locations <- c(report.dir, plots.dir, data.dir, model.dir, report.loc, plots.loc, data.loc, model.loc, data.loc.raw, data.loc.tussentijds,
               report.loc.plots, report.loc.tabel, report.loc.rapportage)


lapply(locations, function(x) {
  if (!dir.exists(x)) {dir.create(x)}
})

#clear output directories each new session

if(remove == TRUE){
  
  
  #clear graphs and reports folder (output)
  clear_locations <- c(plots.dir, report.loc.plots, report.loc.rapportage, report.loc.tabel)
  
  # get all files in the directories, recursively
  f <- list.files(clear_locations, include.dirs = F, full.names = T, recursive = T)
  # remove the files
  file.remove(f)
  
  # Remove .Rda files from DATA directory (temporary data files)
  
  data.hist <- list.files(data.dir,pattern = ".Rda",full.names = TRUE, include.dirs = TRUE,recursive = TRUE)
  
  
  unlink(data.hist, recursive = FALSE)
}

#seed for reproducibility
set.seed(90210)  

#dimension and quality plots
graph_height <- 9
png_height <- 600
aspect_ratio <- 2
dpi <- 320 #retina
sub_title<-''

# Nijmegen kleuren handmatig
nijmegen_kleuren <- c(
  'rood1' = "#a80a2d",
  'rood2' = "#c76667", 
  'rood3' = "#dca09b", 
  'blauw1' = "#03A9F4",
  'blauw2' = "#b0d0ef",
  'blauwgroen1' = "#61B5A5", 
  'blauwgroen2' = "#b8dad4", 
  'oranje1' = "#F57C00",
  'geelgroen1' = "#9E9D24", 
  'geelgroen2' = "#d0c97d", 
  'grijs1' = "#878787", 
  'grijs2' = "#b5b4b4")

# Nijmegen kleuren als palette voor bv ggplot
nijmegen_palette <- 
c("#a80a2d",
"#c76667", 
"#dca09b", 
"#03A9F4",
"#b0d0ef",
"#61B5A5", 
"#b8dad4", 
"#F57C00",
"#9E9D24", 
"#d0c97d", 
"#878787", 
"#b5b4b4")


# Sla Readme op op karelstad report locatie
# Specify the current location of the README file
current_readme_loc <- here("README.md")

# Specify the new location where you want to save the README file
new_readme_loc <- report.dir

# Copy the README file to the new location
file.copy(from = current_readme_loc, to = new_readme_loc, overwrite = TRUE)

# Confirm that the file has been copied
cat("README file has been copied to", new_readme_loc, "\n")

