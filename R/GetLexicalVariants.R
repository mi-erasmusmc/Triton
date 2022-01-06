#' getLexicalVariants
#'
#' Get the lexical variants of covariates(concepts) from the note_nlp table.
#'
#' @export

getLexicalVariants<-function(covariateIds=NULL,
                             conceptIds=NULL,
                             connectionDetails,
                             notenlp_database="cdm",
                             notenlp_table="note_nlp"){

  if(is.null(covariateIds) & is.null(conceptIds)) stop("Provide a vector of covariateIds and/or conceptIds.")

  connection<-DatabaseConnector::connect(connectionDetails)

  conceptIds<-c(conceptIds,(covariateIds-999)/1000)

  sql<-"SELECT note_nlp_concept_id, lexical_variant FROM @notenlp_database.@notenlp_table WHERE note_nlp_concept_id in (@conceptIds)"
  sql<-SqlRender::render(sql,
                         notenlp_database=notenlp_database,
                         notenlp_table=notenlp_table,
                         conceptIds=paste(conceptIds, collapse = ", "))
  sql<-SqlRender::translate(sql,targetDialect = connectionDetails$dbms)

  result<-DatabaseConnector::querySql(connection,sql)
  result2<-result %>%
    count(NOTE_NLP_CONCEPT_ID,LEXICAL_VARIANT)%>%
    arrange(NOTE_NLP_CONCEPT_ID,desc(n))

  DatabaseConnector::disconnect(connection)
  return(result2)
}
