
#------------------------------------------------------------------------------

#Synonym substitution

#------------------------------------------------------------------------------

message("Synonyms substitution...")

regex_batch<-sapply(syns, function(syn) syn$term)  
names(regex_batch)<-sapply(syns, function(x) paste(x$syns, collapse = "|"))

if(length(syns)>0) {
text_df$text<-stringr::str_replace_all(text_df$text, regex_batch)
}
