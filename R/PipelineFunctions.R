
importNotesFromCohort<- function(connection,
                                 cdmDatabaseSchema,
                                 cohortTable,
                                 cohortId,
                                 rowIdField,
                                 startDay,
                                 endDay,
                                 customWhereString=''){
  ##==## Imports notes from database given sql query ##==##
  # build the sql query #
  sqlquery<-Triton:::buildNoteSQLquery(connection,
                                       cdmDatabaseSchema,
                                       cohortTable,
                                       cohortId,
                                       rowIdField,
                                       startDay,
                                       endDay,
                                       customWhereString)

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
  if(doPar) notes$noteText <- unlist(parallel::mclapply(notes$noteText, cs$preprocessor_function,mc.cores = parCores))
  if(!doPar) notes$noteText <- unlist(pbapply::pblapply(notes$noteText, cs$preprocessor_function))
  ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))
  return(notes)
}

tokenizeNotes<-function(notes,cs,doPar=F,parCores){
  ##==## Tokenize notes ##==##
  ParallelLogger::logInfo("\tTokenizing notes")
  tokenizer<-cs$tokenizer
  t0<-Sys.time()
  if(is.function(tokenizer)){
    ParallelLogger::logInfo("\t\tCustom tokenizer")
    # Custom Tokenizer function
    if(doPar) tokens<-unlist(parallel::mclapply(notes$noteText, tokenizer,mc.cores = parCores),recursive = F)
    if(!doPar) tokens<-unlist(pbapply::pblapply(notes$noteText, tokenizer),recursive = F)
    names(tokens)<-notes$rowId
    notes_tokens <- quanteda::tokens(tokens)
  } else if (is.character(tokenizer) && tokenizer=="udpipe") {
    ParallelLogger::logInfo("\t\tUdpipe tokenizer and parser")
    # udmodel must be running
    if(!exists("udmodel")){stop("No udmodel running, stopping..")}
    udp_result <- udpipe::udpipe(setNames(notes$noteText,notes$rowId), udmodel, parallel.cores=parCores)
    udp_result <- udp_result %>%
      select(doc_id, lemma, upos) %>%
      group_by(doc_id)%>%
      summarize(tokens=list(lemma))
    # ParallelLogger::logInfo("\t\tCompounding words")
    # udp_result_names <- unique(udp_result$doc_id)
    # udp_result_com <- parallel::mclapply(udp_result_names,
    #                                      FUN= function(x) Triton:::getPOScompounds(udp_result[udp_result$doc_id==x,]),
    #                                      mc.cores = parallel::detectCores())
    # names(udp_result_com)<-udp_result_names
    # notes_tokens <- quanteda::tokens(udp_result_com)
    notes_tokens <- quanteda::tokens(udp_result)
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
  if(!is.null(cs$stopwords)){
    ParallelLogger::logInfo("\tRemoving stop words")
    notes_tokens <- quanteda::tokens_remove(notes_tokens, pattern = cs$stopwords)
    ParallelLogger::logInfo(paste0("\t\tTokens no stopwords: ",sum(quanteda::ntoken(notes_tokens)),", Memory: ",format(utils::object.size(notes_tokens), units = "auto")))
  }
  if(!is.null(cs$custom_pruning_regex)){
    ParallelLogger::logInfo("\tRemoving additional regex patterns")
    notes_tokens <- quanteda::tokens_remove(notes_tokens, pattern = cs$custom_pruning_regex, valuetype="regex")
    ParallelLogger::logInfo(paste0("\t\tTokens: ",sum(quanteda::ntoken(notes_tokens)),", Memory: ",format(utils::object.size(notes_tokens), units = "auto")))
  }
  return(notes_tokens)
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

createNgrams<-function(notes_tokens, cs){
  ##==## Calculate ngrams in tokens ##==##
  if (max(cs$ngrams)>1){
    ParallelLogger::logInfo("\tCalculating word ngrams")
    notes_tokens <- quanteda::tokens_remove(notes_tokens, pattern = "nxtnt") #remove the not seperators
    notes_tokens <- quanteda::tokens_ngrams(notes_tokens, n = cs$ngrams)
    ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0,units = 'min'),2), "min"))
    ParallelLogger::logInfo(paste0("\t\tTokens ngrams: ",sum(quanteda::ntoken(notes_tokens)),", Memory: ",format(utils::object.size(notes_tokens), units = "auto")))
  }
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
                                          min_termfreq = cs$term_count_min, max_termfreq = cs$term_count_max, termfreq_type = 'count',
                                          min_docfreq = cs$doc_count_min, max_docfreq=cs$doc_count_max, docfreq_type='count')
  ParallelLogger::logInfo(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm_trimmed)," rowIds x ",quanteda::nfeat(notes_dfm_trimmed)," words, Memory: ",format(utils::object.size(notes_dfm_trimmed), units = "auto")))
  ## trim dfm on doc proportion ##
  ParallelLogger::logInfo("\t\tTrimming DTM based on word document proportion")
  notes_dfm_trimmed <- quanteda::dfm_trim(notes_dfm_trimmed,
                                          min_docfreq = cs$doc_proportion_min, max_docfreq=cs$doc_proportion_max, docfreq_type='prop')
  ParallelLogger::logInfo(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm_trimmed)," rowIds x ",quanteda::nfeat(notes_dfm_trimmed)," words, Memory: ",format(utils::object.size(notes_dfm_trimmed), units = "auto")))
  if(!is.infinite(cs$vocab_term_max)){
    ParallelLogger::logInfo("\t\tTrimming DTM based on max number of terms")
    notes_dfm_trimmed <- quanteda::dfm_trim(notes_dfm_trimmed,
                                            max_termfreq = cs$vocab_term_max, docfreq_type='rank')
    ParallelLogger::logInfo(paste0("\t\tDTM: ",quanteda::ndoc(notes_dfm_trimmed)," rowIds x ",quanteda::nfeat(notes_dfm_trimmed)," words, Memory: ",format(utils::object.size(notes_dfm_trimmed), units = "auto")))
  }
  return(notes_dfm_trimmed)
}

toCovariateData<- function(dtm, repName, startDay, endDay, idstaken, valCovariateSet=NULL, idrange){
  ##==## Converts sparse long format DTM to FeatureExtraction covariate object ##==##
  strWd<-paste0(" (",startDay," to ",endDay," days)")
  covariates_allCols<-dtm %>%
    dplyr::select(rowId, word, all_of(repName)) %>%
    dplyr::mutate(covariate_name=paste0(repName,strWd," : ",word))%>%
    dplyr::rename(row_id=rowId, covariate_value=repName)%>%
    dplyr::mutate(row_id=as.numeric(row_id),
                  covariate_name=as.factor(covariate_name))

  # Give every covariate a random unique id
  if(is.null(valCovariateSet)){
    covariates_allCols$covariate_id<-as.numeric(Triton:::getUniqueId(covariates_allCols$covariate_name, idstaken, idrange))
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
    dplyr::mutate(analysis_id = 999,concept_id=0)
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))

  # construct FeatureExtraction analysis reference:
  analysisname<-paste0(repName, " covariates, during ",startDay,
                       " through ",endDay," days relative to index.")
  analysisRef <- data.frame(analysisId = 999,
                            analysisName = analysisname,
                            domainId = "Note",
                            startDay = startDay,
                            endDay = endDay,
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

  valCovariateSet<-cs$validationVarImpTable

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
