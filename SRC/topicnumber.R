
#------------------------------------------------------------------------------

#Number of topics

#------------------------------------------------------------------------------


if(lda_optimal_k == TRUE | !exists("k_lda")) {

message("LDA optimal number of topics...")
  
tune_lda<-FindTopicsNumber(
  dtm,
  topics = seq(from = lda_topic_min, to = lda_topic_max, by = 1),
  metrics = c("CaoJuan2009", 
              "Arun2010",
              "Griffiths2004", 
              "Deveaud2014"
              ), 
  method = "Gibbs",
  #mc.cores = thread, #mac /linux, does not work in Windows
  #mc.cores = NA,
  control = list(
    #nstart = nstart,
    #seed = seed,
    #best = best,
    #burnin = burnin,
    #iter = niter,
    #keep = keep,
    #thin = thin,
    seed = 77),
  verbose = TRUE 
)

#Arun2010 and CaoJuan2009 needs to be minimized
#Griffiths2004 and Deveaud2014 needs to be maximized

#k_lda <- tune_lda$topics[min(which.min(tune_lda$CaoJuan2009)
#                               ,which.min(tune_lda$Arun2010)
#                               ,which.max(tune_lda$Griffiths2004))]

k_lda.auron<-tune_lda$topics[which.min(tune_lda$Arun2010)]
k_lda.caojuan<-tune_lda$topics[which.min(tune_lda$CaoJuan2009)]

k_lda<-ifelse(k_lda.caojuan<k_lda.auron, k_lda.caojuan,k_lda.auron)

print(paste("The optimal number of topics for the text collection is ",k_lda))

ftn.loc<-paste0(plots.loc,"Topic_Models_LDA_Gibbs_number_of_topics.png")
png(file=ftn.loc)
ftn<-FindTopicsNumber_plot(tune_lda)
dev.off()

#Topic_Models_LDA_Gibbs_number_of_topics.png
#Best situation:
#minimization: Arun2010, CaoJuan2009
#maximization: Deveaud2014, Griffiths2004 

}