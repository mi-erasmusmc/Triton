#' getTextRepCovariateData
#'
#' This covariate builder creates text representation covariates from a cohort in the OMOP cdm
#'
#' @importFrom magrittr "%>%"
#' @param connection A connection to the server containing the schema as created using the connect function in the DatabaseConnector package.
#' @param oracleTempSchema A schema where temp tables can be created in Oracle.
#' @param cdmDatabaseSchema The name of the database schema that contains the OMOP CDM instance. Requires read permissions to this database. On SQL Server, this should specifiy both the database and the schema, so for example 'cdm_instance.dbo'.
#' @param cohortTable Name of the table holding the cohort for which we want to construct covariates. If it is a temp table, the name should have a hash prefix, e.g. '#temp_table'. If it is a non-temp table, it should include the database schema, e.g. 'cdm_database.cohort'.
#' @param cohortId For which cohort ID should covariates be constructed? If set to -1, covariates will be constructed for all cohorts in the specified cohort table.
#' @param cdmVersion The version of the Common Data Model used. Currently only cdmVersion = "5" is supported.
#' @param rowIdField The name of the field in the cohort temp table that is to be used as the row_id field in the output table. This can be especially usefull if there is more than one period per person.
#' @param covariateSettings An object of type covariateSettings as created using the createTextRepCovariateSettings function.
#' @param aggregated Should aggregate statistics be computed instead of covariates per cohort entry?
#' @return a covariateData object that can be used my other cdm framework functions.
#' @export

getTextRepCovariateData <- function(connection,
                                    oracleTempSchema = NULL,
                                    cdmDatabaseSchema,
                                    cohortTable,
                                    cohortId,
                                    cdmVersion = "5",
                                    rowIdField = "subject_id",
                                    covariateSettings,
                                    aggregated = FALSE) {

  #========= 1. SETUP =========#
  ### check if the covariates need to be constructed ###
  if (covariateSettings$useTextData == FALSE) {
    return(NULL)
  }

  t1 <- proc.time() #Start timer
  quanteda::quanteda_options(threads=2) #Must be set as an option
  cs<-covariateSettings #shorten name

  ### Check what covariate types need to be constructed ###
  repOpts<-c("tf","tfidf","text")
  textrep<-tolower(cs$representation)
  if (!all(textrep %in% repOpts)){
    wrong<-paste(textrep[which(!textrep %in% repOpts)], collapse = ", ")
    stop(paste0("'",wrong,"' is/are not a valid text representation(s), choose from '",paste(repOpts,collapse = ", "),"'."))
    return(NULL)
  }

  ### Check valid tokenizer options ###
  tokOpts<-c("word","fasterword","fastestword","sentence","character")
  tokfunc<-cs$tokenizer_function
  if (!is.function(tokfunc) && !tokfunc %in% tokOpts){
    stop(paste0("'",tokfunc,"' is not a valid quanteda tokenizer, choose from '",paste(repOpts,collapse = ", "),"', or provide your own tokenizer as R function."))
    return(NULL)
  }

  ### The observation period window ###
  startDay<-cs$startDay
  endDay<-cs$endDay

  #========= 2. Run Feature Contruction =========#

  writeLines(paste0("Constructing '",paste(textrep,collapse = ", "),"' covariates, during day '",startDay,
                   "' through '",endDay,"' days relative to index."))

  ### 2.1 Import the cohort's notes from OMOP CDM ###
  # SQl to get the Notes
  sqlquery<-Triton:::buildNoteSQLquery(connection,
                              cdmDatabaseSchema,
                              cohortTable,
                              cohortId,
                              rowIdField,
                              startDay,
                              endDay)
  notes<-Triton:::ImportNotes(connection, sqlquery, rowIdField)
  colnames(notes)<-c("rowId","noteText")

  ### 2.2 Preprocessing the notes ###
  writeLines("\tPreprocessing notes")
  notes_processed <- notes %>%
    dplyr::mutate(noteText = cs$preprocessor_function(noteText))

  ### 2.3 Tokenization of the notes ###
  writeLines("\tTokenizing notes")
  tokenizer<-cs$tokenizer
  if(is.function(tokenizer)){
    # Custom Tokenizer function
    tokens<-tokenizer(notes_processed$noteText)
    names(tokens)<-notes_processed$rowId
    notes_tokenized <- quanteda::tokens(tokens)
  } else if(tokenizer == "spacy"){
    #library(spacyr) (POS, lemmatization, dependency)
    # notes_corpus<-quanteda::corpus(notes_processed, docid_field="rowId", text_field="noteText")
    # spacyr::spacy_initialize(model = "en_core_web_sm", condaenv="spacy_condaenv",entity=F)
    # parsedtxt <- spacyr::spacy_parse(notes_corpus, pos=F, tag=F, lemma=T, entity=F, additional_attributes = c("is_punct"), multithread=T)
    # spacyr::spacy_finalize()
  } else if (tokenizer == "udpipe"){
    #library(udpipe) (POS, lemmatization, dependency)

  } else if (tokenizer == "bpe"){
    #library(sentencepiece) (Character combinations based on observed frequency)

  } else if (tokenizer == "sentencepiece"){
    #library(sentencepiece) (Character combinations based on observed frequency)

  } else {
    # Quanteda tokenizer function
    notes_corpus<-quanteda::corpus(notes_processed, docid_field="rowId", text_field="noteText")
    notes_tokenized <- quanteda::tokens(notes_corpus, what = tokenizer, remove_punct=T, remove_symbols=T)
  }
  writeLines(paste0("\t\tTokens: ",sum(quanteda::ntoken(notes_tokenized)),", Memory: ",format(utils::object.size(notes_tokenized), units = "auto")))

  ## 2.3.1 Remove stopwords and regex patterns from tokens ##
  if(!is.null(cs$stopwords)){
    writeLines("\tRemoving stop words")
    notes_tokenized <- quanteda::tokens_remove(notes_tokenized, pattern = cs$stopwords)
    writeLines(paste0("\t\tTokens no stopwords: ",sum(quanteda::ntoken(notes_tokenized)),", Memory: ",format(utils::object.size(notes_tokenized), units = "auto")))
  }
  if(!is.null(cs$custom_pruning_regex)){
    writeLines("\tRemoving additional regex patterns")
    notes_tokenized <- quanteda::tokens_remove(notes_tokenized, pattern = cs$custom_pruning_regex, valuetype="regex")
    writeLines(paste0("\t\tTokens: ",sum(quanteda::ntoken(notes_tokenized)),", Memory: ",format(utils::object.size(notes_tokenized), units = "auto")))
  }

  ## 2.3.2 create ngrams if requested ##
  if (max(cs$ngrams)>1){
    writeLines("\tCalculating word ngrams")
    notes_tokenized <- quanteda::tokens_ngrams(notes_tokenized, n = cs$ngrams)
    writeLines(paste0("\t\tTokens ngrams: ",sum(quanteda::ntoken(notes_tokenized)),", Memory: ",format(utils::object.size(notes_tokenized), units = "auto")))
  }

  ### 2.4 Create DFM(DTM) ###
  writeLines("\tCreating document term matrix (DTM)")
  notes_dfm<-quanteda::dfm(notes_tokenized)
  writeLines(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm)," rowIds x ",quanteda::nfeat(notes_dfm)," words, Memory: ",format(utils::object.size(notes_dfm), units = "auto")))

  ## 2.4.1 trim dfm on term and doc count ##
  writeLines("\t\tTrimming DTM based on word count")
  notes_dfm_trimmed <- quanteda::dfm_trim(notes_dfm,
    min_termfreq = cs$term_count_min, max_termfreq = cs$term_count_max, termfreq_type = 'count',
    min_docfreq = cs$doc_count_min, max_docfreq=cs$doc_count_max, docfreq_type='count')
  writeLines(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm_trimmed)," rowIds x ",quanteda::nfeat(notes_dfm_trimmed)," words, Memory: ",format(utils::object.size(notes_dfm_trimmed), units = "auto")))
  ## 2.4.2 trim dfm on doc proportion ##
  writeLines("\t\tTrimming DTM based on word document proportion")
  notes_dfm_trimmed <- quanteda::dfm_trim(notes_dfm_trimmed,
    min_docfreq = cs$doc_proportion_min, max_docfreq=cs$doc_proportion_max, docfreq_type='prop')
  writeLines(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm_trimmed)," rowIds x ",quanteda::nfeat(notes_dfm_trimmed)," words, Memory: ",format(utils::object.size(notes_dfm_trimmed), units = "auto")))
  if(!is.infinite(cs$vocab_term_max)){
    writeLines("\t\tTrimming DTM based on max number of terms")
    notes_dfm_trimmed <- quanteda::dfm_trim(notes_dfm_trimmed,
                                            max_termfreq = cs$vocab_term_max, docfreq_type='rank')
    writeLines(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm_trimmed)," rowIds x ",quanteda::nfeat(notes_dfm_trimmed)," words, Memory: ",format(utils::object.size(notes_dfm_trimmed), units = "auto")))
  }

  ## 2.4.3 save the vocabulary if requested ##
  if(!is.null(cs$vocabFile)){
    vocab<-quanteda::textstat_frequency(notes_dfm_trimmed)
    saveRDS(vocab, file = cs$vocabFile)
  }

  #========= 3. Build the covariates =========#

  covariates<-NULL # start with empty set
  idstaken<-NULL # start with no ids taken, for assigning unique random ids

  ### 3.1  Build the TF covariates (Using DTM) ###
  if("tf" %in% textrep){
    writeLines("\tCreating TF covariates")
    DTM_TF <- tidytext::tidy(quanteda::convert(notes_dfm_trimmed, to = "tm")) %>% dplyr::rename(rowId=document, word=term, tf=count)
    tempCovariates <- toCovariateData(DTM_TF,"tf", startDay,endDay,idstaken,sqlquery)
    covariates <- appendCovariateData(tempCovariates,covariates)
    idstaken <- c(idstaken,as.data.frame(covariates$covariateRef)$covariateId)
  }

  ### 3.2 Build the TFIDF covariates (Using DTM) ###
  if("tfidf" %in% textrep){
    writeLines("\tCreating TFIDF covariates")
    DTM_TFIDF <- quanteda::dfm_tfidf(notes_dfm_trimmed, scheme_tf = "count", base=10)
    DTM_TFIDF <- suppressWarnings(tidytext::tidy(quanteda::convert(DTM_TFIDF, to = "tm")) %>% dplyr::rename(rowId=document, word=term, tfidf=count))
    tempCovariates <- toCovariateData(DTM_TFIDF, "tfidf", startDay,endDay,idstaken,sqlquery)
    covariates <- appendCovariateData(tempCovariates,covariates)
    idstaken <- c(idstaken,as.data.frame(covariates$covariateRef)$covariateId)
  }

  writeLines(paste0("Done, total time: ",(proc.time() - t1)[3]," secs"))

  #========= 4. return a covariateData object of all the constructed covariates =========#
  return(covariates)
}
