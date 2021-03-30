buildNoteSQLquery<-function(connection,
                            cdmDatabaseSchema,
                            cohortTable,
                            cohortId,
                            rowIdField,
                            obsWindowDays,
                            obsWindowStartDay,
                            customWhereString=''){
  ##==## Creates sql query for extracting cohort notes from cdm database ##==##
  sql <- paste("SELECT c.@row_id_field, notes.note_id, notes.note_date, notes.note_text",
               "FROM @cohort_table AS c",
               "LEFT JOIN @cdm_database_schema.note as notes ",
               "ON (c.subject_id=notes.person_id ",
               "AND (c.cohort_start_date + @window_start*interval '1 day') > notes.note_date ",
               "AND ((c.cohort_start_date + @window_start*interval '1 day') + @days_window*interval '1 day') < notes.note_date)",
               "WHERE 1=1 {@customWhere != ''} ? {AND @customWhere} {@cohort_id != -1} ? {AND c.cohort_definition_id = @cohort_id}")
  sql <- SqlRender::render(sql,
                           cohort_table = cohortTable,
                           cohort_id = cohortId,
                           row_id_field = rowIdField,
                           cdm_database_schema = cdmDatabaseSchema,
                           days_window = obsWindowDays,
                           window_start = obsWindowStartDay,
                           customWhere = customWhereString)
  sqlquery <- SqlRender::translate(sql, targetDialect = attr(connection, "dbms"))
}

getPOScompounds<-function(udpipe_result){
  nt<-udpipe_result
  result<-NULL
  for (row in 1:nrow(nt)) {
    if(nt[row,]$upos%in% c("NOUN","ADJ")){
      if(nt[row,]$upos=="NOUN"){
        result<-c(result,nt[row,]$lemma)
      }
      i<-1
      compound<-nt[row,]$lemma
      while (nt[row+i,]$upos %in% c("NOUN","ADJ")) {
        compound<-paste0(compound,"_",nt[row+i,]$lemma)
        result<-c(result,compound)
        i<-i+1
      }
    }
  }
  return(result)
}

hasData <- function(data) {
  ##==## checks if data has data ##==##
  return(!is.null(data) && (data %>% count() %>% pull()) > 0)
}

getUniqueId<-function(Names, idstaken, idrange=NULL){
  ##==## creates a unique(random) id for a covariate ##==##
  if(is.null(idrange)){idrange<-c(1,2147482)}
  ParallelLogger::logInfo(paste0("\t\tCreating unique covariate ids between ",idrange[1], " and ",idrange[2]))
  Names<-as.data.frame(Names)
  colnames(Names)<-"name"
  uniqueNames<-as.data.frame(unique(Names))
  colnames(uniqueNames)<-"name"
  ## generate the ids
  uniqueNames$ids<-as.integer(paste0((as.integer(runif(n=nrow(uniqueNames),min = idrange[1],max = idrange[2]))),999))
  ## change the ids if duplicated or already taken
  while ((length(uniqueNames$ids[duplicated(uniqueNames$ids)])>0) || (!all(is.na(match(uniqueNames$ids,idstaken))))) {
    #print(paste0("duplicated:",length(uniqueNames$ids[duplicated(uniqueNames$ids)]))) #Debug
    #print(paste0("taken:",length(uniqueNames$ids[!is.na(match(uniqueNames$ids,idstaken))]))) #Debug
    dubs<-duplicated(uniqueNames$ids) # duplicated ids
    taken<-!is.na(match(uniqueNames$ids,idstaken)) # ids that are already taken
    both<-dubs | taken # combine both
    uniqueNames$ids[both]<-as.integer(paste0((as.integer(runif(n=length(both[both]),min = idrange[1],max = idrange[2]))),999)) # create new id for those
  }
  Names<-dplyr::left_join(Names,uniqueNames,by = "name")
  return(Names$ids)
}

appendCovariateData<- function(tempCovariateData,covariateData){
  ##==## appends covariate objects ##==##
  if (is.null(covariateData)) {
    covariateData <- tempCovariateData
  } else {
    if (hasData(covariateData$covariates)) {
      if (hasData(tempCovariateData$covariates)) {
        Andromeda::appendToTable(covariateData$covariates, tempCovariateData$covariates)
      }
    } else if (hasData(tempCovariateData$covariates)) {
      covariateData$covariates <- tempCovariateData$covariates
    }
    if (hasData(covariateData$covariatesContinuous)) {
      if (hasData(tempCovariateData$covariatesContinuous)) {
        Andromeda::appendToTable(covariateData$covariatesContinuous, tempCovariateData$covariatesContinuous)
      } else if (hasData(tempCovariateData$covariatesContinuous)) {
        covariateData$covariatesContinuous <- tempCovariateData$covariatesContinuous
      }
    }
    Andromeda::appendToTable(covariateData$covariateRef, tempCovariateData$covariateRef)
    Andromeda::appendToTable(covariateData$analysisRef, tempCovariateData$analysisRef)
    for (name in names(attr(tempCovariateData, "metaData"))) {
      if (is.null(attr(covariateData, "metaData")[name])) {
        attr(covariateData, "metaData")[[name]] <- attr(tempCovariateData, "metaData")[[name]]
      } else {
        attr(covariateData, "metaData")[[name]] <- list(attr(covariateData, "metaData")[[name]],
                                                        attr(tempCovariateData, "metaData")[[name]])
      }
    }
  }
  return(covariateData)
}

selectEmbTerms<-function(toks,wordsInEmb){
  ##==## Select the terms in the tokens that are also in the embedding ##==##
  toks<-toks[toks %in% wordsInEmb]
  return(toks)
}

aggrNoteEmbedding<-function(toks,wordEmb){
  ##==## Aggregate over word embeddings to create a note embedding ##==##
  noteEmb<-wordEmb[toks,]
  noteEmb<-noteEmb[complete.cases(noteEmb),]
  noteEmb<-colSums(noteEmb)/length(toks)
  return(noteEmb)
}
