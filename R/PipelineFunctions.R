importNotesFromCohort<- function(connection,
                                 cdmDatabaseSchema,
                                 note_table,
                                 cohortTable,
                                 cohortId,
                                 rowIdField,
                                 startDay,
                                 endDay,
                                 customWhereString=''){
  ##==## Imports notes from database given sql query ##==##
  # build the sql query #
  sqlquery<-SqlRender::loadRenderTranslateSql(sqlFilename = "importNotes.sql",
                                              packageName = "Triton",
                                              dbms = attr(connection, "dbms"),
                                              cohort_table = cohortTable,
                                              cohort_id = cohortId,
                                              row_id_field = rowIdField,
                                              note_database_schema = cdmDatabaseSchema,
                                              note_table = note_table,
                                              startDay = startDay,
                                              endDay = endDay,
                                              customWhere = customWhereString)

  # get the notes #
  ParallelLogger::logInfo(paste0("\tImporting notes"))
  t0<-Sys.time()
  notes<-DatabaseConnector::querySql(connection, sql = sqlquery, snakeCaseToCamelCase=TRUE)
  notes.shape<-dim(notes)
  ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0,units = 'min'),2)," min"))
  ParallelLogger::logInfo(paste0("\t\tNumber of notes: ",notes.shape[1],", Memory size: ",format(utils::object.size(notes), units = "auto")))
  ParallelLogger::logInfo(paste0("\tGrouping by '",rowIdField,"'"))
  t0<-Sys.time()

  # Process the notes #
  nrows<-length(unique(notes[,SqlRender::snakeCaseToCamelCase(rowIdField)]))
  notes<-notes%>%
    dplyr::filter(!is.na(noteText))%>%
    dplyr::group_by_at(SqlRender::snakeCaseToCamelCase(rowIdField)) %>%
    dplyr::summarise(noteText = paste0(noteText, collapse = " nxtnt "))
  notes.shape<-dim(notes)
  ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0,units = 'min'),2)," min"))
  ParallelLogger::logInfo(paste0("\t\tNumber of grouped notes: ",notes.shape[1],", Memory size: ",format(utils::object.size(notes), units = "auto")))
  ParallelLogger::logInfo(paste0("\t\t'",rowIdField,"'s without notes: ", nrows-nrow(notes)))
  colnames(notes)<-c("rowId","noteText")
  return(notes)
}

preprocessNotes<-function(notes,cs,doPar=F,parCores){
  ##==## Preprocess notes ##==##
  ParallelLogger::logInfo("\tPreprocessing notes")
  t0<-Sys.time()
  if(doPar) notes$noteText <- unlist(parallel::mclapply(notes$noteText, cs$pipe_preprocess_function,mc.cores = parCores))
  if(!doPar) notes$noteText <- unlist(pbapply::pblapply(notes$noteText, cs$pipe_preprocess_function))
  ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))
  return(notes)
}

tokenizeNotes<-function(notes,cs,doPar=F,parCores){
  ##==## Tokenize notes ##==##
  ParallelLogger::logInfo("\tTokenizing notes")
  tokenizer<-cs$pipe_tokenizer_function
  t0<-Sys.time()
  if(is.function(tokenizer)){
    ParallelLogger::logInfo("\t\tCustom tokenizer")
    # Custom Tokenizer function
    if(doPar) tokens<-unlist(parallel::mclapply(notes$noteText, tokenizer,mc.cores = parCores),recursive = F)
    if(!doPar) tokens<-unlist(pbapply::pblapply(notes$noteText, tokenizer),recursive = F)
    names(tokens)<-notes$rowId
    notes_tokens <- quanteda::tokens(tokens)
  } else {
    ParallelLogger::logInfo("\t\tQuanteda tokenizer")
    # Quanteda tokenizer function
    ParallelLogger::logInfo(paste0("\t\tUsing quanteda tokenizer: ", tokenizer))
    notes_corpus <- quanteda::corpus(notes, docid_field="rowId", text_field="noteText")
    notes_tokens <- quanteda::tokens(notes_corpus, what = tokenizer, remove_punct=T, remove_symbols=T)
  }
  ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))
  ParallelLogger::logInfo(paste0("\t\tTokens: ",sum(quanteda::ntoken(notes_tokens)),", Memory: ",format(utils::object.size(notes_tokens), units = "auto")))
  return(notes_tokens)
}

filterTokens<-function(notes_tokens,cs){
  ##==## Filter tokens ##==##
  if(!is.null(cs$filter_stopwords)){
    ParallelLogger::logInfo("\tRemoving stop words")
    notes_tokens <- quanteda::tokens_remove(notes_tokens, pattern = cs$filter_stopwords)
    ParallelLogger::logInfo(paste0("\t\tTokens no stopwords: ",sum(quanteda::ntoken(notes_tokens)),", Memory: ",format(utils::object.size(notes_tokens), units = "auto")))
  }
  if(!is.null(cs$filter_custom_regex)){
    ParallelLogger::logInfo("\tRemoving additional regex patterns")
    notes_tokens <- quanteda::tokens_remove(notes_tokens, pattern = cs$filter_custom_regex, valuetype="regex")
    ParallelLogger::logInfo(paste0("\t\tTokens: ",sum(quanteda::ntoken(notes_tokens)),", Memory: ",format(utils::object.size(notes_tokens), units = "auto")))
  }
  return(notes_tokens)
}

createNgrams<-function(notes_tokens, cs){
  ##==## Calculate ngrams in tokens ##==##
  if (max(cs$pipe_ngrams)>1){
    ParallelLogger::logInfo("\tCalculating word ngrams")
    notes_tokens <- quanteda::tokens_remove(notes_tokens, pattern = "nxtnt") #remove the not seperators
    notes_tokens <- quanteda::tokens_ngrams(notes_tokens, n = cs$pipe_ngrams)
    ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0,units = 'min'),2), "min"))
    ParallelLogger::logInfo(paste0("\t\tTokens ngrams: ",sum(quanteda::ntoken(notes_tokens)),", Memory: ",format(utils::object.size(notes_tokens), units = "auto")))
  }
}

getTextStats<-function(tokens){
  ##==## creates general text features using tokens ##==##
  textstats_df<-data.frame(rowId=names(tokens))
  #the number of tokens in total
  textstats_df$nToken <- quanteda::ntoken(tokens)
  #the number of characters in total
  textstats_df$nChar <- unlist(lapply(tokens,function(x) sum(nchar(unlist(x)))))
  #the number of characters per token(or document)
  textstats_df$Avg.nChar <- unlist(lapply(tokens,function(x) mean(nchar(unlist(x)))))
  #the number of notes
  textstats_df$nNotes <- unlist(lapply(tokens,function(x) max(sum(unlist(x)%in%"nextnote"),1) ))
  #the avg number of tokens per note
  textstats_df$Avg.nToken.Note<-textstats_df$nToken/textstats_df$nNotes
  #the avg number of tokens per note
  textstats_df$Avg.nChar.Note<-textstats_df$nChar/textstats_df$nNotes

  dtm<-textstats_df %>%
    tidyr::gather("word", "textstats", 2:7)

  return(dtm)
}

createDFM<-function(notes_tokens){
  ##==## Create DFM from tokens ##==##
  ParallelLogger::logInfo("\tCreating document term matrix (DTM)")
  notes_dfm<-quanteda::dfm(notes_tokens)
  ParallelLogger::logInfo(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm)," rowIds x ",quanteda::nfeat(notes_dfm)," words, Memory: ",format(utils::object.size(notes_dfm), units = "auto")))
  return(notes_dfm)
}

trimDFM<-function(notes_dfm, cs){
  ##==## Trim a dfm ##==##
  ## trim dfm on term and doc count ##
  ParallelLogger::logInfo("\t\tTrimming DTM based on word count")
  notes_dfm_trimmed <- quanteda::dfm_trim(notes_dfm,
                                          min_termfreq = cs$filter_term_count_min, max_termfreq = cs$filter_term_count_max, termfreq_type = 'count',
                                          min_docfreq = cs$filter_doc_count_min, max_docfreq=cs$filter_doc_count_max, docfreq_type='count')
  ParallelLogger::logInfo(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm_trimmed)," rowIds x ",quanteda::nfeat(notes_dfm_trimmed)," words, Memory: ",format(utils::object.size(notes_dfm_trimmed), units = "auto")))
  ## trim dfm on doc proportion ##
  ParallelLogger::logInfo("\t\tTrimming DTM based on word document proportion")
  notes_dfm_trimmed <- quanteda::dfm_trim(notes_dfm_trimmed,
                                          min_docfreq = cs$filter_doc_proportion_min, max_docfreq=cs$filter_doc_proportion_max, docfreq_type='prop')
  ParallelLogger::logInfo(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm_trimmed)," rowIds x ",quanteda::nfeat(notes_dfm_trimmed)," words, Memory: ",format(utils::object.size(notes_dfm_trimmed), units = "auto")))
  if(!is.null(cs$filter_vocab_term_max)){
    ParallelLogger::logInfo("\t\tTrimming DTM based on max number of terms")
    notes_dfm_trimmed <- quanteda::dfm_trim(notes_dfm_trimmed,
                                            max_termfreq = cs$filter_vocab_term_max, docfreq_type='rank')
    ParallelLogger::logInfo(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm_trimmed)," rowIds x ",quanteda::nfeat(notes_dfm_trimmed)," words, Memory: ",format(utils::object.size(notes_dfm_trimmed), units = "auto")))
  }
  return(notes_dfm_trimmed)
}

toCovariateData<- function(dtm, repName, cs, idstaken, valCovariateSet=NULL){
  ##==## Converts sparse long format DTM to FeatureExtraction covariate object ##==##
  strWd<-paste0(" (",cs$startDay," to ",cs$endDay," days)")
  covariates_allCols<-dtm %>%
    dplyr::select(rowId, word, all_of(repName)) %>%
    dplyr::mutate(covariate_name=paste0(repName,strWd," : ",word))%>%
    dplyr::rename(row_id=rowId, covariate_value=repName)%>%
    dplyr::mutate(row_id=as.numeric(row_id),
                  covariate_name=as.factor(covariate_name))

  # Give every covariate a random unique id
  if(is.null(valCovariateSet)){
    covariates_allCols$covariate_id<-as.numeric(Triton:::getUniqueId(covariates_allCols$covariate_name, idstaken, cs$idrange, cs$analysisId))
    # Or give same id as model (validation)
  } else {
    valCovariateSet<-valCovariateSet %>%
      dplyr::filter(covRep==repName) # filter on this text representation
    covariates_allCols<-covariates_allCols %>% # assign the same ids by joining on term
      left_join(valCovariateSet, by = c("word" = "covTerm")) %>%
      mutate(covariate_id=covariateId)
  }

  # construct FeatureExtraction covariates
  covariates<-covariates_allCols %>%
    dplyr::select(row_id,covariate_id,covariate_value)
  colnames(covariates) <- SqlRender::snakeCaseToCamelCase(colnames(covariates))

  # construct FeatureExtraction covariate reference
  covariateRef<-covariates_allCols%>%
    dplyr::select(covariate_id,covariate_name) %>%
    dplyr::distinct(covariate_id,covariate_name) %>%
    dplyr::mutate(analysis_id = cs$analysisId,concept_id=0)
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))

  # construct FeatureExtraction analysis reference:
  analysisname<-paste0(repName, " covariates, during ",cs$startDay,
                       " through ",cs$endDay," days relative to index.")
  analysisRef <- data.frame(analysisId = cs$analysisId,
                            analysisName = analysisname,
                            domainId = "Note",
                            startDay = cs$startDay,
                            endDay = cs$endDay,
                            isBinary = "N",
                            missingMeansZero = "Y",
                            stringsAsFactors=TRUE)

  # Combine all constructs:
  metaData <- list(call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                                 covariatesContinuous=NULL,
                                 covariateRef = covariateRef,
                                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"

  # return FeatreExtraction covariateData object
  return(result)
}

getValidationCovariateSet<-function(cs){
  ##==## Get list of covariates that are in the trained model (varImp) ##==##

  valCovariateSet<-cs$BoW_validationVarImpTable

  # If covariatevalue is given, only select non-zero covariates
  if("covariateValue" %in% colnames(valCovariateSet)){
    valCovariateSet<-valCovariateSet[valCovariateSet$covariateValue!=0,]
  }

  # filter covariates on these covariateSettings and extract the term names and ids.
  valCovariateSet<-valCovariateSet %>%
    dplyr::filter(stringr::str_split_fixed(valCovariateSet$covariateName," ", n=2)[,1] %in% cs$representation) %>%
    tidyr::separate(covariateName,c("covInfo","covTerm"),": ") %>%
    tidyr::separate(covInfo,c("covRep","covInfo")," \\(") %>%
    dplyr::mutate(covInfoStart=as.numeric(stringr::str_extract_all(covInfo,"-[0-9]+|[0-9]+", simplify = T)[,1]),
                  covInfoEnd=as.numeric(stringr::str_extract_all(covInfo,"-[0-9]+|[0-9]+", simplify = T)[,2])) %>%
    dplyr::filter(covInfoStart==startDay, covInfoEnd==endDay) %>%
    dplyr::select(covariateId,covRep,covTerm)
  return(valCovariateSet)
}
