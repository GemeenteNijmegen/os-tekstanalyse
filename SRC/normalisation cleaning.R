
#------------------------------------------------------------------------------

#Normalisation and cleaning of text collection

#------------------------------------------------------------------------------

#normalization of noisy text data

message("Normalisation and cleaning...")

#preprocessing : normalisation

#charset; converts between character string encodings
text_df$text <- tolower(stringi::stri_trans_general(text_df$text, 'latin-ascii'))

if(clean_full==TRUE) {
#remove 'empty' records
text_df <- text_df %>% 
  filter(!is.na(text),
         !text %like% dontknows, 
         nchar(text)>=txt_len)
}

#batch cleaning of text_df
clean_txt <- function(tc, remove_numbers=FALSE){
  #replace (unicode) control characters
  tc <- gsub("[[:cntrl:]]", " ", tc) 
  
  #remove html
  tc <- gsub("<[^>]+>", "", tc)
  
  #remove hyperlinks
  #tc <- gsub("https?://[^[:space:]]+", " ***URL*** ", tc)
  tc <- gsub("http.+ |http.+$", " ***URL*** ", tc)
  
  #remove email addresses
  #tc <- gsub("(\\s+|^)(\\S+@\\S+).*", " ***EMAIL*** ", tc)
  tc <- gsub("[a-z0-9]+@[a-z]+\\.[a-z]{2,3}", " ***EMAIL*** ", tc)
  
  #translate emoticons (see SRC>functions.R)
  tc <- convert_emoticons_dutch(tc)
  
  #remove remaining ascii emoticon characters
  tc <- gsub("[:;][-~]?[)D]", "", tc)
  
  if(remove_numbers==TRUE) {
    #remove all (independent) numbers
    #tc <- gsub("[[:digit:]]", "", tc)
    tc <- gsub("\\b\\d+\\b", "", tc) 
  } else {
    #privacy related numbers and codes
    #social-security number BSN (dutch)
    tc <- gsub("[0][1-9][0-9]{7}|([1-9][0-9]{8})", " ***BSN*** ", tc, perl=TRUE)
    
    #remove zipcode (dutch)
    #tc <- gsub("[1-9][0-9]{3}\\s*[A-Za-z]{2}", " ***ZIPCODE*** ", tc)
    tc <- gsub("[[:space:]][1-9][0-9]{3}[[:space:]]{0,1}(?!sa|sd|ss)[a-z]{2}[[:space:]]", " ***ZIPCODE*** ", tc, perl=TRUE) 
    
    #mobile phone numbers
    tc <- gsub("\\b06[ -]\\d{8}\\b", " ***MOBILE-PHONE*** ", tc)
    
    #land-line phone numbers 
    #tc <- gsub("(\\+31|0)[0-9]{9}", " ***PHONE*** ", tc)
    #tc <- gsub("\\s*(?:\\d{8,10})\\s*", " ***LANDLINE-PHONE*** ", tc, perl=TRUE)
    tc <- gsub("\\b0\\d{2}[ -]\\d{6,7}\\b", " ***LANDLINE-PHONE*** ", tc)
  }
  

  #remove #hashtags and @mentions
  #tc <- gsub("(?:#|@)[a-zA-Z0-9_]+ ?", "", tc)
  
  #(more) Twitter specific
  tc <- gsub("&amp;|&lt;|&gt;|RT", "", tc)
  
  #remove forward slash near alphanumeric characters
  tc <- gsub('(?<![0-9])/|/(?![0-9])', ' ', tc, perl=TRUE)
  
  #remove punctuation characters, except dot, comma, hyphen, question mark and asteriks
  tc <- gsub("(?![.,-?*'])[[:punct:]]", " ", tc, perl=TRUE)
  
  #remove multiple consecutive exclamation, - question marks and similar - special characters (but keep one)
  tc <- gsub("([[:punct:]])\\1+", "\\1", tc)
  
  #convert exclamation mark to dot
  tc <- gsub("\\!", ".", tc, perl=TRUE)

  #remove pseudo-math equations
  tc <- gsub("[-|=|\\+]"," ",tc)
  
  #remove parentheses and brackets but keep string inside
  tc <- gsub("[\\(\\)\\[\\]]", "", tc)
  
  #remove semi-colon
  #tc <- gsub("^;+|;+$|(;)+", " ", tc)
  
  #remove colon and semi-colon
  tc <- gsub("[:;]", " ", tc)
  
  #remove bullet lists characters
  #tc <- gsub("\\-\\s", " ", tc, perl=TRUE)
  tc <- gsub("^-\\s+", "", tc, perl = TRUE)
  tc <- gsub("\\n-\\s+", " ", tc, perl = TRUE)
  
  #tc <- gsub("/^\s*(?:[\dA-Z]+\.|[a-z]\)|•)\s+/gm", " ", tc) #https://regexr.com/2u326
  tc <- gsub("\\•\\s", " ", tc, perl=TRUE)
  tc <- gsub("\\·\\s", " ", tc, perl=TRUE) 
 
  #remove apostrophe
  tc <- gsub("‘", " ", tc) 
  #tc <- gsub("'", " ", tc)
  tc <- gsub("’", " ", tc) 
  tc <- gsub('"', ' ', tc)
  tc <- gsub("´", " ", tc) 
  #tc <- gsub("'", " ", tc)  
 
  #misc normalisation
  #strip multiple consecutive alphanumeric characters (more than 2) (word elongation)
  #tc <- gsub("(\\w)\\1+", "\\1", tc)
  tc <- gsub("(.)\\1{3,}", "\\1", tc)

  #add space after punctuation (except for comma),if not there
  #tc <- gsub("([[:punct:]])([^[:space:]])", "\\1 \\2", tc)
  #tc <- gsub("([!.?])[^\\s,]+", "\\1 ", tc)
  
  #remove space before punctuation
  #dot
  tc <- gsub(" \\.", ".", tc, perl=TRUE)
  #comma
  tc <- gsub(" ,", ",", tc, perl=TRUE)
  #substitute comma at the end with dot
  tc <- sub(",$", ".", tc)
  
  #lay-out and white space
  #remove tabs and line feeds
  #tc <- gsub("\r?\n|\r", " ", tc)
  tc <- gsub("[\r\n\t ]+", " ", tc)
  
  #remove space characters: tab, newline, vertical tab, form feed, carriage return, space
  #tc <- sub("[[:space:]]+$", "", tc)
  #trim excess white space
  tc <- gsub("\\s+"," ", tc)
  #trim leading or trailing white space
  #tc <- gsub("^\\s+|\\s+$", "", tc)
  tc <- trimws(tc)
  
  return(tc)
}

text_df$text <- clean_txt(text_df$text,remove_numbers)

#extract everything in the sentence that is not a dot in the middle of the string
#for(r in 1:nrow(text_df)) {
#text_df$text[r] <- paste0(str_extract_all(text_df$text[r], "[^\\.]|(\\.$)|(\\. )")[[1]], collapse = "")
#}

#x <- text_df$text

#colnames(text_df)[1] <- "text"

#size cleaned text collection
size_tc <- nrow(text_df)

#------------------------------------------------------------------------------
#save clean textcollection
cleantext.df<-paste0(data.dir,"/text_clean.RData")
save(text_df, file = cleantext.df)

#note: cleaning of tokens in 'tokenisation and annotation.R'