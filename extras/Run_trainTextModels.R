# Connection #
connectionDetails <- createConnectionDetails(
  dbms = "",
  server = "",
  user ="",
  password = "",
  port = 0)
connection<-DatabaseConnector::connect(connectionDetails)

# Cohort #
cdmDatabaseSchema <- ""
cohortDatabaseSchema <- ""
cohortTable <- ""
cohortId <- 1
rowIdField<-"row_id"

# Triton CovariateSettings #
custom_preprocessing<-function(string){
  res<-tolower(string) # everything to lower case
  res<-stringr::str_replace_all(res,"\\\\n", " ") # remove new lines
  res<-stringr::str_replace_all(res,"[0-9]+", " ") # replace every number (with a space)
  res<-stringr::str_replace_all(res,"_+", " ") # replace all under scores (with a space)
  return(res)
}
custom_tokenizer <- function(strings) {
  res<-stringi::stri_split_regex(strings, "[\\p{Z}\\p{P}\\p{C}\\p{S}]+")
  return(res)
}
cs <- Triton::createTritonCovariateSettings(
  useTextData = T,
  startDay = -10,
  endDay = -1,
  preprocessor_function = custom_preprocessing,
  tokenizer_function = custom_tokenizer,
  stopwords = NULL,
  custom_pruning_regex = NULL,
  ngrams = 1,
  term_count_min = 50,
  doc_count_min = 50,
  doc_proportion_max = 0.4,
  doc_proportion_min = 0.001,
  parallel = TRUE
)

# settings #
doPar<-cs$parallel
parCores<-parallel::detectCores()
quanteda::quanteda_options(threads=parallel::detectCores())

#### preprocess notes ####
# import notes #
notes <- Triton:::importNotesFromCohort(connection,
                                        cdmDatabaseSchema,
                                        cohortTable,
                                        cohortId,
                                        rowIdField,
                                        cs$startDay,
                                        cs$endDay,
                                        cs$customWhere)
# Preprocessing the notes #
notes <- Triton:::preprocessNotes(notes,cs,doPar,parCores)
# Tokenization of the notes #
notes_tokens <- Triton:::tokenizeNotes(notes,cs,doPar,parCores)
# filter tokens
notes_tokens <- Triton:::filterTokens(notes_tokens,cs)


#### Word embeddings ####
## create word embeddings ##
t2v<-Triton:::trainGloVe(notes_tokens, d = 50, filename = "output/textmodels/gloveModel.rds",parCores = parCores,verbose = T)


#### Topic Models ####
# create dfm
notes_dfm <- Triton:::createDFM(notes_tokens)
# trim dfm
notes_dfm_trimmed <- Triton:::trimDFM(notes_dfm, cs)

## create lda topic model ##
lda<-Triton:::trainLDA(notes_dfm_trimmed,k = 10,filename = "output/textmodels/ldaModel.rds",verbose = T)
ldaCov<-data.frame(lda$theta)
# predict on new data
new<-predict(lda, notes_dfm_trimmed)

## create stm topic model ##
stm<-Triton:::trainSTM(notes_dfm_trimmed,k = 10,filename = "output/textmodels/stmModel.rds",verbose = T)
stmCov<-tidytext::tidy(stm, matrix = "gamma")
# predict on new data
stmNew<-stm::fitNewDocuments(stm, documents= notes_dfm_trimmed@docvars$docname_,newData = notes_dfm_trimmed)

## create lsa topic model ##
lsa<-Triton:::trainLSA(notes_dfm_trimmed,k = 10,filename = "output/textmodels/lsaModel.rds",tfidf = T)
lsaCov<-as.data.frame(lsa$docs)
# predict on new data
lsaNew<-predict(lsa,notes_dfm_trimmed)
lsaRes <- data.frame(as.matrix(lsaNew$docs_newspace))
