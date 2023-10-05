
#------------------------------------------------------------------------------

#Extract misspellings from Woorden.org

#------------------------------------------------------------------------------

wordcorrection_df = data.frame()

for (i in 1:19) {
  
correctwords_url <- paste0("https://www.woorden.org/foute-spelling/",i)

correctwords <- read_html(correctwords_url)
pag <- html_node(correctwords, css = "table") %>%
  html_table(header = TRUE) 
pag

wordcorrection_df <- rbind(wordcorrection_df,pag)
}

wc.loc <- paste0(lexicon.dir,"wordcorrection.xlsx")
write.xlsx(df_total,wc.loc, row.names=FALSE)
