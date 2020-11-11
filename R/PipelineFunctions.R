
ImportNotes<- function(connection, sqlquery, rowIdField){
  ##==## Imports notes from database given sql query ##==##
  ParallelLogger::logInfo(paste0("\tImporting notes"))
  t0<-Sys.time()
  notes<-DatabaseConnector::querySql(connection, sql = sqlquery, snakeCaseToCamelCase=TRUE)
  notes.shape<-dim(notes)
  ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0,units = 'min'),2)," min"))
  ParallelLogger::logInfo(paste0("\t\tNumber of notes: ",notes.shape[1],", Memory size: ",format(utils::object.size(notes), units = "auto")))
  ParallelLogger::logInfo(paste0("\tGrouping by '",rowIdField,"'"))
  t0<-Sys.time()
  notes<-notes%>%
    dplyr::group_by_at(SqlRender::snakeCaseToCamelCase(rowIdField)) %>%
    dplyr::summarise(noteText = paste0(noteText, collapse = " nextnote ")) %>%
    dplyr::mutate(noteText = stringr::str_replace(noteText, "(^NA$)|((NA )+NA)", "NA")) %>%
    dplyr::na_if("NA")
  notes.shape<-dim(notes)
  #assign("notes", notes, envir = .GlobalEnv) #debug
  ParallelLogger::logInfo(paste0("\t\t",round(difftime(Sys.time(),t0,units = 'min'),2)," min"))
  ParallelLogger::logInfo(paste0("\t\tNumber of grouped notes: ",notes.shape[1],", Memory size: ",format(utils::object.size(notes), units = "auto")))
  cntNa<-sum(is.na(notes$noteText))
  ParallelLogger::logInfo(paste0("\t\t'",rowIdField,"'s without notes: ", cntNa))
  notes<-notes[complete.cases(notes),] #remove all empty notes
  return(notes)
}

getTextStats<-function(tokens){
  ##==## creates general text features using tokens ##==##
  textstats_df<-data.frame(rowId=names(tokens))
  #the number of tokens in total
  textstats_df$ntoken <- quanteda::ntoken(tokens)
  #the number of characters in total
  textstats_df$nchar <- unlist(lapply(tokens,function(x) sum(nchar(unlist(x)))))
  #the number of characters per token(or document)
  textstats_df$avgnchar <- unlist(lapply(tokens,function(x) mean(nchar(unlist(x)))))
  #the number of notes
  textstats_df$nnotes <- unlist(lapply(tokens,function(x) max(sum(unlist(x)%in%"nextnote"),1) ))
  #the avg number of tokens per note
  textstats_df$avgntokennote<-textstats_df$ntoken/textstats_df$nnotes
  #the avg number of tokens per note
  textstats_df$avgncharnote<-textstats_df$nchar/textstats_df$nnotes

}

toCovariateData<- function(dtm, repName, startDay, endDay, idstaken, idrange, sql){
  ##==## Converts sparse long format DTM to FeatureExtraction covariate object ##==##
  strWd<-paste0(" (",startDay," to ",endDay," days)")
  covariates_allCols<-dtm %>%
    dplyr::select(rowId, word, repName) %>%
    dplyr::mutate(covariate_name=paste0(repName,strWd," : ",word))%>%
    dplyr::rename(row_id=rowId, covariate_value=repName)%>%
    dplyr::mutate(row_id=as.numeric(row_id),
           covariate_name=as.factor(covariate_name))

  # Give every covariate a unique id
  covariates_allCols$covariate_id<-as.numeric(getUniqueId(covariates_allCols$covariate_name, idstaken, idrange))

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
  metaData <- list(sql = sql, call = match.call())
  result <- Andromeda::andromeda(covariates = covariates,
                 covariatesContinuous=NULL,
                 covariateRef = covariateRef,
                 analysisRef = analysisRef)
  attr(result, "metaData") <- metaData
  class(result) <- "CovariateData"

  # return FeatreExtraction covariateData object
  return(result)
}

