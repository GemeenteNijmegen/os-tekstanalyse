
#-----------------------------------------------------------------------------------------------

#Text collection

#-----------------------------------------------------------------------------------------------

message("Text collection...")

#-----------------------------------------------------------------------------------------------

# import functions

#-----------------------------------------------------------------------------------------------

filename <- ' '

# function records file
load_dataset <- function(file_type,column_name,doc_id,filename,docid_col=99){
  # opens file
  #
  # file_type - file extension: 'txt' , 'tsv', csv', 'xlsx', 'sav', 
  #
  # return: tibble
  
  #extract file location
  
  qry<-paste0("*",file_type)
  files <- fs::dir_ls(glob = qry, path = data.dir)
  
  if(nchar(filename)>5) {
    files<-filename
  }
  
  message("importing ", file_type, " files")
  
  #read .xlsx file(s) 
  if (file_type == 'xlsx') { # tibble 
    textfile <- map_df(set_names(files), function(file) {
      file %>% 
        map_df(
          ~ readxl::read_xlsx(path = file, col_names = column_name, sheet = text_sheet, trim_ws = TRUE, guess_max = 20))
    })
  }
  
  #read .csv file(s) 
  else if (file_type == 'csv' | file_type == 'tsv') { # tibble 
    textfile <- map_df(set_names(files), function(file) {
      file %>% 
        map_df(
          ~ vroom::vroom(files, col_names = column_name) 
        )
    })
  }
  
  #read .sav file
  else if (file_type == 'sav') {
    textfile <- map_df(set_names(files), function(file) {
      file %>% 
        map_df(
          ~ haven::read_sav(file) %>% as_tibble()
        ) 
    })
    
  }
  
  #read .txt file
  else if (file_type == 'txt') { # tibble 
    textfile <- readr::read_tsv(files, col_names = column_name) 
  }
  
  else 
  {
    print('No valid file_type') 
    return() 
  }
  
  if(docid_col==99) {
    textfile <- textfile %>%
      dplyr::mutate(doc_id = row_number())
  } else {
    textfile <- textfile %>%
      rename(doc_id = names(.)[[docid_col]])
  }
  
  return(textfile)
}

#-----------------------------------------------------------------------------------------------

# function multiple files
#read DATA directory for docs like .doc(x), .pdf etc.
load_docs <- function(doc_type){
  
  clean_full<-FALSE
  #pdf.loc <- list.files(path=data.dir,pattern = "pdf$")
  #pdf <-lapply(paste0(data.dir,pdf.loc), pdf_text)
  #textfile <- as.data.frame(lapply(pdf, unlist))
  
  text_df <- readtext::readtext(paste0(data.dir, "*.",doc_type), 
                                docvarsfrom = "filenames"
  )
  #, 
  #sep = "_")
  
  source(here('SRC/normalisation cleaning.R'), encoding = charset, local = TRUE)
  
  #set ids
  textfile <- text_df %>%
    dplyr::mutate(source_id=row_number()) %>%
    select(-doc_id) %>%
    separate_rows(.,text,sep = "\\.+") %>%
    rownames_to_column(.,"doc_id") %>%
    dplyr::mutate(doc_id=as.numeric(doc_id))
  
  clean_full<-TRUE
  
  return(textfile)
}  

#-----------------------------------------------------------------------------------------------

# function tweets
load_tweets <- function(){
  #Twitter credentials must be set in SRC>Twitter.R
  twitter_token <- create_token(
    app = appname,
    consumer_key = consumer_key,
    consumer_secret = consumer_secret,
    access_token = access_token,
    access_secret = access_secret,
    set_renv = FALSE)
  
  tweet <- search_tweets(tw_qry, 
                         lang = tw_language,
                         n = tw_n, 
                         retryonratelimit = TRUE,
                         token = twitter_token, 
                         include_rts = tw_rt)
  
  write_as_csv(tweet,paste0(data.dir,"tweets.csv"))
  
  textfile <- tweet %>%
    #filter(created_at > tw_dte) %>% 
    select(text,user_id)
}

#-----------------------------------------------------------------------------------------------

# import text data

#-----------------------------------------------------------------------------------------------


if(import_mode=="tweets") { #tweets mode
  
  #credentials for accessing Twitter API    
  source(here('SRC/Twitter.R'))  
  
  text_df <- 
    load_tweets() %>%  #tweets
    as_tibble() %>%
    #split text in sentences per row (if you lack entities like respondent, customer etc)
    #separate_rows(.,text_col,sep = "\\.+") %>% 
    #rename(text = names(.)[[text_col]]) %>%   
    filter(!is.na(text) & !is_empty(text) & text!='') %>%
    dplyr::mutate(id=row_number(), doc_id=as.numeric(user_id)) %>% #use row number as id
    select(text,id,doc_id
           ,id #and other columns you want to keep
    )
  
} else if (import_mode=="documents") { #documents mode
  
  text_df <- 
    load_docs(doc_type) %>%  #multiple docs 
    #as_tibble() %>%
    filter(!is.na(text) & !is_empty(text) & text!='') %>%
    dplyr::mutate(id = row_number()) %>% #use row number as id
    select(text,doc_id,source_id, docvar1)  
  
} else { #records mode
  
  text_df <- 
    load_dataset(file_type,column_name, doc_id, filename) %>% 
    #split text in sentences per row (if you lack entities like respondent, customer etc)
    #separate_rows(.,text_col,sep = "\\.+") %>% 
    rename(text = names(.)[[text_col]]) %>%   
    filter(!is.na(text) & !is_empty(text) & text!='') %>%
    dplyr::mutate(id = row_number()) #%>% #use row number as id
    #select(text,id,doc_id
           #,id #and other columns you want to keep
    #)    
  
}

dplyr::glimpse(text_df)