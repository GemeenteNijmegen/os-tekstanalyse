
#------------------------------------------------------------------------------

#Setup and packages

#------------------------------------------------------------------------------

#clear console
cat("\014")  

#g arbage collection
# Dit script voert een garbage collection uit in R, wat het proces is van het terughalen van geheugen dat niet meer door het R-programma wordt gebruikt.
gc(verbose = FALSE, full = TRUE)

#CPU cores (default=1) for package installation (defaults to 1, we use 4)
# De eerste regel haalt de standaardwaarde op voor het aantal CPU-cores (Ncpus) dat wordt gebruikt tijdens de installatie van R-pakketten, als deze waarde nog niet expliciet is ingesteld.
# De tweede regel stelt het aantal CPU-cores in op 4 met de options() functie. Dit betekent dat bij het installeren van R-pakketten, R maximaal 4 CPU-cores zal gebruiken om de installatie te versnellen.
getOption("Ncpus", 1L)
options(Ncpus = 4)

# RENV
# Als proj_env TRUE is en renv nog niet is geïnstalleerd, wordt het package geïnstalleerd met de install.packages() functie. 
# Vervolgens wordt de renv::init() functie gebruikt om een project-specifieke renv-omgeving op te zetten en te activeren. 
# Dit zorgt ervoor dat de packages die voor dit project worden gebruikt, worden opgeslagen in een aparte omgeving, 
# zodat je er zeker van kunt zijn dat de packages altijd beschikbaar zijn en de juiste versies hebben wanneer je het project later opnieuw uitvoert.

if(proj_env_init == TRUE) {
  if (!require("renv")) install.packages("renv")
  library("renv")
  renv::init()
}

if(proj_env_snapshot == TRUE) {
  if (!require("renv")) install.packages("renv")
  library("renv")
  renv::snapshot()
}

if(proj_env_restore == TRUE) {
  if (!require("renv")) install.packages("renv")
  library("renv")
  renv::restore()
}

#CRAN packages
packages <- c(
  # Connectie DHW
  'DBI', 'odbc', 'rstudioapi',
  #set up directories
  'here',
  # data cleaning
  'dplyr', 
  # Parallel computing
  'RcppParallel')

#install packages which are not available on the computing setup
has_available   <- packages %in% rownames(installed.packages())
if(any(!has_available)) install.packages(packages[!has_available])

lapply(packages,library,character.only = TRUE
       #,quietly = TRUE
)

#review packages loaded
sessionInfo() %>% capture.output(file="session_info.txt")
