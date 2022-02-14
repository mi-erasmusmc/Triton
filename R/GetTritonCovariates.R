#' getTritonCovariateData
#'
#' This covariate builder creates text representation (Triton) covariates from a cohort in the OMOP cdm
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

getTritonCovariateData <- function(connection,
                                   oracleTempSchema = NULL,
                                   cdmDatabaseSchema,
                                   cohortTable,
                                   cohortId,
                                   cdmVersion = "5",
                                   rowIdField,
                                   covariateSettings,
                                   aggregated = FALSE) {

  #========= 1. SETUP =========#
  ### check if the covariates need to be constructed ###
  if (covariateSettings$useNoteData == FALSE) {
    ParallelLogger::logInfo("Triton: useNoteData is False. No covariates created")
    return(NULL)
  } else {
    ParallelLogger::logInfo("Starting Triton covariate builder")
    t1 <- Sys.time() #Start timer
  }
  cs<-covariateSettings #shorten name

  ### check if saved covariateData can be loaded ###
  if(cs$covariateDataLoad!=""){
    ParallelLogger::logInfo(paste("\tLoading covariateData from",cs$covariateDataLoad))
    covariates<-FeatureExtraction::loadCovariateData(file = cs$covariateDataLoad)
    ParallelLogger::logInfo(paste0("Done, total time: ",round(difftime(Sys.time(),t1, units = 'min'),2)," min"))
    return(covariates) # return covariates and skip creation
  }

  ## setup parallelization
  quanteda::quanteda_options(threads=max(2,parallel::detectCores()))
  doPar<-FALSE
  if(is.logical(cs$parallel) && cs$parallel){
    #TODO also check OS windows/unix
    doPar<-TRUE
    parCores<-parallel::detectCores()
  } else if (is.numeric(cs$parallel)){
    doPar<-TRUE
    parCores<-cs$parallel
  }

  ### Check what covariate types need to be constructed ###
  textrep<-cs$representations

  ### check whether to create training or validation covariates ###
  # For validation provide the model's variance importance table or a dataframe with the columns covariateId and covariateName
  if(is.null(cs$BoW_validationVarImpTable) | all(cs$representations %in% c("WordEmb_avg","WordEmb_sum","TopicModel_lsa","TopicModel_lda"))){
    validation <- FALSE
    valCovariateSet <- NULL
  } else {
    validation <- TRUE
    valCovariateSet <- Triton:::getValidationCovariateSet(cs)
  }

  ### setup return variable ###
  covariates<-NULL # start with empty covariate set
  idstaken<-NULL # start with no ids taken, for assigning unique random ids

  #========= 2. Run Feature Contruction =========#
  ParallelLogger::logInfo(paste0("Constructing '",paste(textrep,collapse = ", "),"' covariates, during day '",cs$startDay,
                                 "' through '",cs$endDay,"' days relative to index."))

  ### 2.1 Import the cohort's notes from OMOP CDM ###
  # SQl to get the Notes
  notes <- Triton:::importNotesFromCohort(connection,
                                          if(is.null(cs$note_databaseschema))cdmDatabaseSchema else cs$note_databaseschema,
                                          cs$note_table,
                                          cohortTable,
                                          cohortId,
                                          rowIdField,
                                          cs$startDay,
                                          cs$endDay,
                                          cs$note_customWhere)

  ### 2.2 Preprocessing the notes ###
  notes <- Triton:::preprocessNotes(notes,cs,doPar,parCores)

  ### 2.3 Tokenization of the notes ###
  notes_tokens <- Triton:::tokenizeNotes(notes,cs,doPar,parCores)

  if (length(textrep[!textrep %in% c("TextStats","WordEmb_avg","WordEmb_sum")])>0){

    ## 2.3.1 Remove stopwords and regex patterns from tokens ##
    notes_tokens <- Triton:::filterTokens(notes_tokens, cs)

    ## 2.3.2 create ngrams if requested ##
    if (max(cs$pipe_ngrams)>1){
      notes_tokens <- Triton:::createNgrams(notes_tokens, cs)
    }

    ## 2.3.4 (VALIDTION) only select the terms that are in the model
    if(validation){
      ParallelLogger::logInfo("\tVALIDATION: only selecting the terms that are in the model")
      notes_tokens <- quanteda::tokens_keep(notes_tokens, valCovariateSet$covTerm)
      ParallelLogger::logInfo(paste0("\t\tTokens: ",sum(quanteda::ntoken(notes_tokens)),", Memory: ",format(utils::object.size(notes_tokens), units = "auto")))
    }

    ### 2.4 Create DFM ###
    notes_dfm <- Triton:::createDFM(notes_tokens)

    # Trim if not validation covariate set
    if(!validation){
      notes_dfm_trimmed <- Triton:::trimDFM(notes_dfm, cs)
    } else {
      notes_dfm_trimmed <- notes_dfm
    }

    ## 2.4.3 save the vocabulary if requested ##
    if(cs$pipe_saveVocab){
      vocab<-quanteda::textstat_frequency(notes_dfm_trimmed)
      saveRDS(vocab, file = paste0(cs$pipe_outputFolder,"/vocabfile.rds"))
    }
  }

  #========= 3. Build the covariates =========#
  ### Build general text statistics
  if("TextStats" %in% textrep){
    ParallelLogger::logInfo("\tCreating descriptive statistic covariates")
    DTM_STATS <- Triton:::getTextStats(notes_tokens)
    tempCovariates <- Triton:::toCovariateData(DTM_STATS,"textstats",cs,idstaken,valCovariateSet)
    covariates <- Triton:::appendCovariateData(tempCovariates,covariates)
    idstaken <- c(idstaken,as.data.frame(covariates$covariateRef)$covariateId)
  }

  ### Build the TB covariates (Using DTM) ###
  if("BoW_bin" %in% textrep){
    ParallelLogger::logInfo("\tCreating TB covariates")
    DTM_TB <- tidytext::tidy(quanteda::convert(notes_dfm_trimmed, to = "tm")) %>%
      dplyr::rename(rowId=document, word=term, tb=count)%>%
      dplyr::mutate(tb=as.numeric(tb>0)) # make binary from frequency
    tempCovariates <- Triton:::toCovariateData(DTM_TB,"tb",cs,idstaken,valCovariateSet)
    covariates <- Triton:::appendCovariateData(tempCovariates,covariates)
    idstaken <- c(idstaken,as.data.frame(covariates$covariateRef)$covariateId)
  }

  ### Build the TF covariates (Using DTM) ###
  if("BoW_freq" %in% textrep){
    ParallelLogger::logInfo("\tCreating TF covariates")
    DTM_TF <- tidytext::tidy(quanteda::convert(notes_dfm_trimmed, to = "tm")) %>%
      dplyr::rename(rowId=document, word=term, tf=count)
    tempCovariates <- Triton:::toCovariateData(DTM_TF,"tf",cs,idstaken,valCovariateSet)
    covariates <- Triton:::appendCovariateData(tempCovariates,covariates)
    idstaken <- c(idstaken,as.data.frame(covariates$covariateRef)$covariateId)
  }

  ### Build the TFIDF covariates (Using DTM) ###
  if("BoW_tfidf" %in% textrep){
    ParallelLogger::logInfo("\tCreating TFIDF covariates")
    DTM_TFIDF <- quanteda::dfm_tfidf(notes_dfm_trimmed, scheme_tf = "count", base=10)
    DTM_TFIDF <- suppressWarnings(tidytext::tidy(quanteda::convert(DTM_TFIDF, to = "tm")) %>%
                                    dplyr::rename(rowId=document, word=term, tfidf=count))
    tempCovariates <- Triton:::toCovariateData(DTM_TFIDF, "tfidf",cs,idstaken,valCovariateSet)
    covariates <- Triton:::appendCovariateData(tempCovariates,covariates)
    idstaken <- c(idstaken,as.data.frame(covariates$covariateRef)$covariateId)
  }

  ### Apply the LDA topic model covariates (Using DTM) ###
  # if("TopicModel_lda" %in% textrep){
  #   ParallelLogger::logInfo("\tCreating LDA topic model covariates")
  #   lda<-get(cs$TopicModel_model)
  #   ## predicting on new data
  #   #TODO
  #   # newdfm <- quanteda::dfm_match(notes_dfm_trimmed,)
  #   # ldaNew <- predict(lda,newdfm)
  #   # DM_LDA <- ldaRes %>%
  #   #   tibble::rownames_to_column("rowId") %>%
  #   #   tidyr::gather("word","lda",-rowId) %>%
  #   #   dplyr::mutate(word=paste0("lda",word))
  #   tempCovariates <- Triton:::toCovariateData(DM_LDA, "lda",cs,idstaken,valCovariateSet)
  #   covariates <- Triton:::appendCovariateData(tempCovariates,covariates)
  #   idstaken <- c(idstaken,as.data.frame(covariates$covariateRef)$covariateId)
  # }

  ### Apply the LSA model covariates (Using DTM) ###
  if("TopicModel_lsa" %in% textrep){
    ParallelLogger::logInfo("\tCreating LSA topic model covariates")
    if(!file.exists(cs$TopicModel_file)){
      ParallelLogger::logInfo("\t\tTraining a new topic model")
      ## Training a new lsa model
      lsa<-trainLSA(notes_dfm_trimmed, k=10, tfidf=T, filename=cs$TopicModel_file)
      lsaRes<-as.data.frame(lsa$docs)
    } else{
      ## Predicting on new data using existing model
      ParallelLogger::logInfo("\t\tUsing existing topic model")
      lsa<-readRDS(cs$TopicModel_file)
      ParallelLogger::logInfo("\t\tLimit tokens to topicmodel terms")
      newdfm <- quanteda::dfm_match(notes_dfm_trimmed,lsa$data@Dimnames$features)
      ParallelLogger::logInfo("\t\tPredicting new topics")
      lsaRes <- predictLSA(lsa,newdfm)
    }
    DM_LSA <- lsaRes %>%
      tibble::rownames_to_column("rowId") %>%
      tidyr::gather("word","lsa",-rowId) %>%
      dplyr::mutate(word=paste0("lsa",word))
    tempCovariates <- Triton:::toCovariateData(DM_LSA, "lsa",cs,idstaken,valCovariateSet)
    covariates <- Triton:::appendCovariateData(tempCovariates,covariates)
    idstaken <- c(idstaken,as.data.frame(covariates$covariateRef)$covariateId)
  }

  ### Build aggregated word embedding covariates (using tokens and word embeddings)
  if(any(c("WordEmb_avg","WordEmb_sum") %in% textrep)){
    ParallelLogger::logInfo("\tCreating aggregated embedding covariates")
    Emb<-get(cs$Word_embeddings, envir = .GlobalEnv)
    wordsInEmb<-rownames(Emb)
    ParallelLogger::logInfo("\t\tLimit tokens to embedding terms")
    t0<-Sys.time()
    if(doPar) notes_tokens_filt<-parallel::mclapply(notes_tokens,Triton:::selectEmbTerms,wordsInEmb=wordsInEmb,mc.cores = parCores)
    if(!doPar) notes_tokens_filt<-lapply(notes_tokens,Triton:::selectEmbTerms,wordsInEmb=wordsInEmb)
    ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))
    textreptmp<-textrep
    while(any(c("WordEmb_avg","WordEmb_sum") %in% textreptmp)){
      if("WordEmb_avg" %in% textreptmp){
        aggfunc<-"avg"
        textreptmp <- textreptmp[textreptmp!="WordEmb_avg"]
      } else if ("WordEmb_sum" %in% textreptmp){
        aggfunc<-"sum"
        textreptmp <- textreptmp[textreptmp!="WordEmb_sum"]
      }
      ParallelLogger::logInfo(paste0("\t\tAggregating word embeddings using ",aggfunc))
      t0<-Sys.time()
      if(doPar) aggWordEmb<-parallel::mclapply(notes_tokens_filt,Triton:::aggrNoteEmbedding,wordEmb=Emb,aggfunc=aggfunc,mc.cores = parCores)
      if(!doPar) aggWordEmb<-lapply(notes_tokens_filt,Triton:::aggrNoteEmbedding,wordEmb=Emb,aggfunc=aggfunc)
      ParallelLogger::logInfo("\t\tDone.")
      ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))
      aggWordEmb<-data.frame(do.call(rbind, aggWordEmb)) ## list to dataframe
      colnames(aggWordEmb)<-paste0("D",c(1:ncol(aggWordEmb)))
      DTM_aggWordEmb<-aggWordEmb %>%
        tibble::rownames_to_column("rowId") %>%
        tidyr::gather("word",!!paste0("WE",aggfunc),2:ncol(.))
      tempCovariates <- Triton:::toCovariateData(DTM_aggWordEmb,paste0("WE",aggfunc),cs,idstaken,valCovariateSet)
      covariates <- Triton:::appendCovariateData(tempCovariates,covariates)
      idstaken <- c(idstaken,as.data.frame(covariates$covariateRef)$covariateId)
    }
  }

  #========= 4. return a covariateData object of all the constructed covariates =========#
  ### 4.0 saving covariateData
  if(cs$covariateDataSave!=""){
    ParallelLogger::logInfo(paste("\tSaving covariateData to",cs$covariateDataSave))
    FeatureExtraction::saveCovariateData(covariates,file = cs$covariateDataSave)
    covariates<-FeatureExtraction::loadCovariateData(file = cs$covariateDataSave)
  }

  ### 4.1 returning covariateData
  ParallelLogger::logInfo(paste0("Done, total time: ",round(difftime(Sys.time(),t1, units = 'min'),2)," min"))
  return(covariates)
}
