#-----------------------------------------------------------------------------------------------

# Titel project

# Beschrijving 

#-----------------------------------------------------------------------------------------------

# ontwikkelaars van het script, Gemeente Nijmegen

#-----------------------------------------------------------------------------------------------

# last update 
# datum

# De hoofdresultaten worden weggeschreven naar de Karelstad omgeving (report.loc) in een van de 3 mappen: PLOTS, TABEL, RAPPORTAGE
# De data blijft zoveel mogelijk op de 'originele' locatie staan. 

#-----------------------------------------------------------------------------------------------
#### Initialization ####

# Gebruik van RENV voor package beheer.

# init voor het starten met packagebeheer, snapshot om packages toe te voegen, restore om packages te laden vanuit de renv library
# alle 3 op false dan heb je een verse installatie van alle packages op je lokale account (dit is niet best practice)

# Zet 1 van de onderstaande 3 parameters op TRUE
proj_env_init <-  FALSE #default
proj_env_snapshot <- FALSE
proj_env_restore <- FALSE


# remove previous runs
# dit verwijderd alle bestanden in: plots.dir,report.loc.plots, report.loc.rapportage, report.loc.tabel)
remove <- FALSE # default = FALSE

# map toevoegen van de karelstad omgeving waar de resultaten naar weg geschreven worden
# file.choose() en dan /mp om het path op te halen
# let er op om altijd te eindigen met / 
karelstad_report_loc <- "/mp/kennisvelden/20_Kennisvelden/90_Overig/50_Projecten/test git/" 

#setup and packages
source('SRC/setup_packages.R')

#please review 'session_info.txt' if you encounter any problems up to this point

#-----------------------------------------------------------------------------------------------


#global settings
source(here('SRC/globals.R'))

#-----------------------------------------------------------------------------------------------

#pipeline timer
start_time <- Sys.time()



#-----------------------------------------------------------------------------------------------
# hieronder laten we de scripts draaien die we hebben geschreven
# T1 is voor het importeren van data
# T2 is voor data cleaning en een analyse set klaarzetten
# T3 is voor analyse onderdeel x
# Tx is voor analyse onderdeel x
# TX is voor rapportage

#import data collection

# uit het DWH van Nijmegen
#source(here('T1 00 import interne data.R'))


# einde van pipeline timer
end_time <- Sys.time()
start_time - end_time