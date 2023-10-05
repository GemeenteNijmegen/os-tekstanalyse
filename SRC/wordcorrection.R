
#------------------------------------------------------------------------------

#Word correction

#------------------------------------------------------------------------------

if(word_correct==TRUE && read_saved_tokens==FALSE) {
  
message("Spelling correction...")

wc.loc <- paste0(lexicon.dir,"wordcorrection.xlsx")

misspellings <- load_dataset("xlsx",TRUE,"none",wc.loc) %>%
  filter(woordlengte>3 & woordlengte<16) %>%
  select(-doc_id)

#fuzzyjoin token and misspelling data frames (Levenshtein distance)
gb_joined <- misspellings %>%
  fuzzyjoin::stringdist_inner_join(annotate_df, by = c(Fout = "token"), 
                        max_dist = 1, 
                        ) %>%
  count(Fout,Goed)

which_correct <- gb_joined %>%
  group_by(Fout, Goed) %>%
  summarize(guesses = n(), one_correct = any(Goed == token))

which_correct

# percentage of guesses getting at least one right
mean(which_correct$one_correct)

# number uniquely correct
sum(which_correct$guesses == 1 & which_correct$one_correct)

spelling_incorrect <- which_correct$Fout
spelling_correct <- which_correct$Goed

text_df2<-as.data.frame(text_df$text)
for(i in 1:nrow(text_df2)) {
  row <- text_df2[i,]
  #print(as.character(row))
  row <- stringi::stri_replace_all_fixed(row, spelling_incorrect, spelling_correct, vectorize_all=FALSE)
  #row <- stri_replace_all_regex(as.character(row), "\\b" %s+% patterns %s+% "\\b", replacements, vectorize_all=FALSE)
  #print(row)
  text_df2[i,] <- row
}

colnames(text_df2)[1] <- "text"

text_df$text <- clean_txt(text_df2$text)

rm(misspellings,gb_joined,text_df2)

word_correct_passed<-TRUE

#rerun normalisation, tokenisation, and this time thorough annotation
source(here('SRC/normalisation cleaning.R'))
source(here('SRC/tokenisation annotation.R'))
}

#------------------------------------------------------------------------------
#save normalised text

text_normalised <- text_df
#save normalised text
norm.file <- paste0(data.dir,"text_normalised_df.Rda")
saveRDS(text_normalised,file=norm.file)
rm(text_normalised)

#size cleaned text collection
size_tc <- nrow(text_df)
#vector of ids (valid records)
ids <- text_df$id