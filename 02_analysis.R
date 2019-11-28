################
# Example of topic model for the
# attendeens of the NLP-Workshop of the "GDCP" in Wien
# using conference abstracts 2012 to 2018
# @David Buschhüter and Peter Wulff
# 27.11.2019
#################

library(slam)
library(tidyverse)
library(tidytext)
library(tm)
library(topicmodels)
library(stringi)
library(SnowballC)
library(tidytext)
library(clue)
library(textcat)

# setting seed for reproducibility
SEED <- 42

#***************
# A. Functions ####
#***************
# outputs mean "tf-idf" (mean term frequency inverse document frequency) for a 
# given document term matrix 
# tf–idf =  term frequency–inverse  document frequency, 
# is meant to reflect how meaningful a word 
# is to a document in a given corpus
mean_tfidf_func <- function(DTM){
  tapply(DTM$v/row_sums(DTM)[DTM$i], DTM$j, mean) *
    log2(nDocs(DTM)/col_sums(DTM > 0))
}

# outputs mean of "mean term frequency" for a given document term matrix
# 1. for each document with index j:
#    take the value v of each cell in document-term-frequency (dtm) an divide it 
#    by the times the word 
#    appears in all documents
# 2. calculate the mean of that value over all documents j via tapply()
mtf_func <- function(DTM){tapply(DTM$v/row_sums(DTM)[DTM$i], DTM$j,  mean)}

# outputs "document frequency" for a given document term matrix =
# proportion of documents a term appears in
df_func <- function(DTM){col_sums(DTM > 0)/nDocs(DTM)}

# function for cleaning and data preparation
# it prepares data so than it can be input to functions from the topicmodels 
# package as well as some other packages
# Values: 
# 1. a Document Term Matrix
# 2. a Corpus
# 3. a Character Vector
clean_func <- function(doc_frame = train_data, #input train data
                      words_rm = c("werde", "werden"), #terms to remove
                      str_rm = "z. B.", # one string to remove additionally
                      min_mean_tfidf = 0.01, # minimum mean tf-idf
                      min_df_abs = 2, # minimum number of docs for a word
                      to_lower = TRUE, # lower casing
                      stem_terms = TRUE # stemming
){
  # making a corpus in the right class
  docs_df <- data.frame(doc_id = doc_frame$doc_id,
                        text = doc_frame$text, 
                        stringsAsFactors = FALSE)
  my_docs <- DataframeSource(docs_df)
  this_corpus <- Corpus(my_docs)
  
  # getting predefined transformations (mappings) which can be used with tm_map
  getTransformations()
  
  # executing the transformations on the corpora. the results are corpora again
  this_corpus <- tm_map(this_corpus, stri_trans_nfc) %>% #normalize characters
    tm_map(removeNumbers) %>% # removing numbers 
    tm_map(removeWords, words_rm) %>% # words in words_rm argument
    tm_map(function(x)gsub(str_rm, " ", x)) %>% # string in str_rm argument
    tm_map(function(x)gsub("/innen", " ", x)) %>%
    tm_map(function(x)gsub("-", " ", x)) %>% #  "-"
    tm_map(removePunctuation) %>% # punctuation
    tm_map(function(x)gsub("[^[:alnum:]]", " ", x)) %>% # special characters
    tm_map(function(x)gsub("\u2028", " ", x) ) %>% # line seprator
    tm_map(removeWords, c(stopwords("german"), "dass", "Dass", # stopwords
                          tools::toTitleCase(stopwords("german")))) %>% 
    tm_map(function(x)gsub('\\b\\w{1,2}\\s','',x)) %>% # very short terms 
    tm_map(stripWhitespace) # whitespaces
  
  # transform to lower case if desired
  if(to_lower){this_corpus <- tm_map(this_corpus, tolower)}
  # stem if desired
  if(stem_terms){this_corpus <- tm_map(this_corpus, stemDocument, lang = "ger")}
  
  # cast document term matrix
  dtm = DocumentTermMatrix(this_corpus, control=list(tolower=FALSE, 
                                                     stemming = FALSE))
  
  # use mean tf-idf and include only terms with 
  # at least a tf-idf value of min_mean_tfidf (function argument), 
  # use mean_tfidf (functions defined above)
  term_mtfidf <- mean_tfidf_func(DTM = dtm)
  term_tf <- mtf_func(DTM = dtm)
  term_df_abs <- col_sums(dtm > 0)
  term_df <- df_func(DTM = dtm)
  
  # exclude low information terms
  mtfidfframe <- data.frame(NAME = dtm$dimnames$Terms, 
                            term_mtfidf, term_sum = col_sums(dtm),
                            term_tf, term_df, term_df_abs,
                            stringsAsFactors = FALSE)
  low_info_id <- mtfidfframe$term_mtfidf<min_mean_tfidf | 
    mtfidfframe$term_df_abs<min_df_abs
  low_info_words <- mtfidfframe[low_info_id,"NAME"]
  
  # update the corpus without the terms
  this_corpus2 <- this_corpus
  if(length(low_info_words) > 0){
    low_info_words_spl <- split(low_info_words, 
                                ceiling(seq_along(low_info_words)/800))
    # because tm_map cannot remove many words at the same time
    # a loop is needed
    for(i in 1:length(low_info_words_spl)){
      this_corpus2 <- tm_map(this_corpus2,removeWords, low_info_words_spl[[i]])
    }
    this_corpus2 <- tm_map(this_corpus2, 
                           function(x)str_trim(x, side = c("both")))%>%
      tm_map(stripWhitespace)
  }
  
  #make new document term matrix to check that corpus aligns with the dtm
  dtm_new <- dtm[,term_mtfidf >= min_mean_tfidf & term_df_abs >= min_df_abs]
  dtm2 <- DocumentTermMatrix(this_corpus2, control=list(tolower=FALSE, 
                                                        stemming = FALSE))
  
  #write output if dtm is in line with corpus
  if(identical(dtm_new, dtm2)){
    # exclude documents without any words from the dtm
    doc_length = apply(dtm_new, MARGIN = 1, sum)
    
    #give warning if that was the case
    if(sum(doc_length == 0) > 0){warning(
      "some documents removed from document term matrix because they were empty"
    )}
    
    dtm_new = dtm_new[doc_length > 0,]
    list(dtm_new, this_corpus2, unlist(lapply(this_corpus2, 
                                              FUN = function(x){unlist(x)})))
  }else{stop("document term matrices not identical: Please adjust clean_func")}
}

#***************************
# B. Text preprocessing ####
#***************************
# loading paths
machine_path <- getwd()
text_path <- paste(machine_path, "/text_data_raw/", sep ="")
out_path <-  paste(machine_path, "/output/", sep ="")
other_path <- paste(machine_path, "/other_data/", sep ="")

# loading texts #### need to be webscraped first
docs_raw <- read.csv(paste0(text_path, "all_abs_utf-8.csv"), 
                     stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# loading names of cities and bundesländer because this might to problems with
# confunding
places <- read.csv(paste0(other_path, "cities.csv"), 
                   stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# clean names from special characters etc.
places <- unique(c(places$Name, places$Bundesland)) %>%
  removeNumbers() %>% vapply(FUN = function(x){gsub("\\s*\\([^\\)]+\\)","",x)}, 
                             FUN.VALUE = "asdasda")%>%
  unique()
places <- append(places,c("Österreich", "Schweiz", "Deutschland"))

# excluding english abstracts
lang_vec <- textcat(docs_raw$abs_text)
docs_raw <- docs_raw[!is.na(lang_vec),]
lang_vec <- lang_vec[!is.na(lang_vec)]
docs_raw <- docs_raw[lang_vec == "german",]

# adding title to text of document because they are meaningful
docs_raw[, "abs_text"] <- paste(docs_raw$abs_title, docs_raw$abs_text, 
                                sep = " ")

# giving more general names for important coloumns
colnames(docs_raw)[c(1,5)] <- c("doc_id","text")

#****************************************************************
# C. Splitting between train and test data (hold out data)   ####
#****************************************************************
# in this example we are not testing yet if the model is generalizable 
# to new documents (but we should still not look at the test data because 
# we still might want to do this analysis as well and we might be biased)
n <- nrow(docs_raw)
set.seed(SEED)
splitter <- sample(1:n, round(n * 0.75))
train_data <- docs_raw[splitter, ]
test_data <- docs_raw[-splitter, ] 

# remove some invalid documents
train_data <- train_data[nchar(train_data[,"abs_author"]) > 10,] #from train set
docs_raw <- docs_raw[nchar(docs_raw[,"abs_author"]) > 10,] #from all docs

# cleaning documents
train_clean_dtm_corp_vec <- clean_func(doc_frame = train_data, 
                                      words_rm = c("werde", "SuS", "Lehrer",
                                                   "Lehrerinnen", 
                                                   "Lehrpersonen",
                                                   "wurde", "wurden",
                                                   "wurd", "sowie", 
                                                   "Ergebnis"
                                                   ,"Ergebnisse", 
                                                   "Ergebnissen",
                                                   # the following is the only 
                                                   # content related word 
                                                   # removed
                                                   "Elektronengasmodell", 
                                                   places
                                      ), 
                                      str_rm = "-innen", 
                                      min_mean_tfidf = 0.1, 
                                      min_df_abs = 4, 
                                      to_lower = TRUE, 
                                      stem_terms = TRUE) 

# choosing the input format of package topicmodels (a document term matrix)
dtm_train <- train_clean_dtm_corp_vec[[1]]

#******************************
#  D. Topic modeling LDA     #####
#******************************
# we found a model with 37 topics to have statisfactory chorence
# (even if not all of them are yet ).
# finding a model can be done by just trying out different numbers of topics
# or other methods. For further reading and other methods:
# https://www.r-bloggers.com/cross-validation-of-topic-modelling/
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
# https://papers.nips.cc/paper/3700-reading-tea-leaves-how-humans-interpret-topic-models.pdf
# https://arxiv.org/abs/1404.4606
K = 37 #number of topics

# LDA with K topics
set.seed(42)
mod_LDA <- LDA(dtm_train, k = K,
               control = list(seed = SEED))

#*********************************************
#  E. Plausibility inspection  of model   ####
#*********************************************
# 1. Plot maximum topic proprtion for each document ####

# this means for each document the proportion of the topic appearing 
# the most frequent in this document
# the plot shows that there documents which consit mainly of one topic and those
# that constist of several topics

columns=as.character(c(1:K))
max_prob <- tidy(mod_LDA, "gamma")%>%
  mutate(document = as.numeric(document))%>%
  pivot_wider(names_from = topic, values_from = gamma)%>%
  left_join(as_tibble(train_data), by = c("document" = "doc_id")) %>%
  pivot_longer(cols= columns, names_to = "topic", values_to = "gamma")%>%
  group_by(document) %>%
  summarise(max = max(gamma))
ggplot(max_prob,aes(x=max))+
  geom_histogram(bins =100)+
  xlab("Maximum topic proportion")


# 2. Creating a table to be inspected for coherence and plausibility of the ####
#    classification of documents

# summarizing topic proportions X_1 to X_K and joining with other topic
# informations
abs <- tidy(mod_LDA, "gamma") %>%
  mutate(document = as.numeric(document))%>%
  left_join(as_tibble(train_data), by = c("document" = "doc_id"))%>%
  pivot_wider(names_from = topic, values_from = gamma)%>%
  data.frame()%>%
  as_tibble()

# Creating the summary table to output as a csv #
# for every topic it contains 
# a. most prevalent words and 
# b. the titles of the documents with the highest proportion of this topic and
#    the topic proportion probability
# c. document titles are not shown if they do not exceed a probability of the 
#    value prop regarding the topic

prop = 0.5 
mat <- data.frame(terms(mod_LDA, 10))
if(exists("df_summary")){rm(df_summary)}
i=0
for(x in 1:ncol(mat)){
  i = i+1
  name_x <- paste0("X", i)
  # create a dataframe ordered by the probability of the topic i for the 
  # 10 abstracts with the highest prevalence for this topics
  abs_x <- abs[order(abs[,name_x, drop = TRUE], decreasing = TRUE), 
               c("abs_title",name_x)]%>%
    slice(1:10)
  
  # erase titles and probabilities if needed
  abs_x[abs_x[,2] < prop, "abs_title"] <- "-"
  abs_x[abs_x[,2] < prop, name_x] <- "-"
  
  #create df
  df_x <- data.frame(mat[,i], abs_x[,1, drop = TRUE], abs_x[,2, drop = TRUE],
                     stringsAsFactors = FALSE)
  
  #giv column names
  colnames(df_x) <- paste(c("terms", "title","prop"), name_x , sep ="")
  
  # ceate new data.frame in the first run
  # bind by columns df_summary and the new df_x  in the next runs
  if(base::exists("df_summary")){
    df_summary <- cbind(df_summary, df_x)
  }else{
    df_summary <- df_x
  }
}

# creating a path to save the file in
dir.create(paste(out_path), showWarnings = FALSE)
out_path_x <- paste0(out_path, "K", K, "/")
dir.create(paste(out_path_x), showWarnings = FALSE)

# save the file
write.csv(df_summary,paste0(out_path_x,"summary.csv"),fileEncoding =  "UTF-8")

# 3. Outputting all topics (word distributions ) for analysis  ####
# inspect consistency of topics via plots in out_path
top_terms <- tidy(mod_LDA, matrix = 'beta')%>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# make six plots per page
x_vec <- seq(1,mod_LDA@k, by =6)
j=0
for(i in x_vec){
  topic_plot <-top_terms %>%
    mutate(term = reorder_within(term, beta, topic)) %>%
    filter(topic >= i & topic <= i+5)%>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered()+
    ylim(c(0,0.5))
  ggsave(filename = paste0(out_path_x, "topic_",i, ".png"),
         plot = topic_plot, 
         width = 8*1.2, height = 4*1.2)
}