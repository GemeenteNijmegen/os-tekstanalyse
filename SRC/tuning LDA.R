
#-----------------------------------------------------------------------------------------------

# Preset LDA Topic Models

#-----------------------------------------------------------------------------------------------

message("LDA Topic Model presets...")

#Config Topic Models 
alphaprior<-0.1
deltaprior<-0.001
#set iterations
niter<-1000
#convergence tolerance
convtol<-0.001
#use random integers as seed 
seed_lst<-list(254672,109,122887,145629037,2)
#thin the spaces between samples
thin<-500
# return the highest probability as the result
best<-TRUE
#set burn in
burnin<-1000
keep<-50
#set random starts at 5
nstart<-5

#load DocumentTermMatrix
if(!exists("dtm")){
  src_dtm<-paste0(data.dir,"dtm.Rda")
  load(file = src_dtm)
}
