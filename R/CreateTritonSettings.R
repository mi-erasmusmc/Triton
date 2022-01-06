#' createTritonCovariateSettings
#'
#' Create a covariateSettings object for constructing text representation (Triton) covariates from the notes table in the OMOP CDM.
#'
#' @param useTextData logical; option to disable the creation of text representation covariates.
#' @param startDay integer; start day before the index date for with the text representations have to be computed.
#' @param endDay integer; end day before the index date for with the text representations have to be computed.
#' @param idrange (optional) integer vector; specifying the range of integers that can be used to generate the covariateids, max is 2147482. Default is c(1,2147482).
#' @param parallel logical; to indicate whether multi-threading should be used. Default is True.
#' @param note_databaseschema t
#' @param note_tablename t
#' @param note_customWhere (optional) character; with a SQL where statement to filter the note import. Example "WHERE note_source_value='communication'".
#' @param pipe_preprocess_function function; to preprocess the stings before tokenization. Default is \code{\link{tolower}}.
#' @param pipe_tokenizer_function character or function; to tokenize the strings. Default is quanteda tokenizer (\code{\link[quanteda]{tokens}}), with argument "word". Other possible arguments are "fasterword", "fastestword", "sentence", and "character". It is also possible to provide a custom tokenizer function. This function should take the document strings as input and should return a list of character vectors (tokens).
#' @param pipe_ngrams integer vector; specifying the number of elements to be concatenated in each ngram. For example: \code{c(1,2)} creates all unigrams and bigrams; \code{c(1:3)} creats all unigrams, bigrams, and trigrams. Default is 1: no ngrams (unigram).
#' @param pipe_saveVocab logical; option to save the generated vocabulary as rds file in the outputFolder.
#' @param pipe_outputFolder (optional) character; file path and name for saving output files. Default is \code{NULL}.
#' @param filter_stopwords character vector; of list of stopwords that will be removed. Default is NULL. See \code{\link[stopwords]{stopwords}} for generating stopwords.
#' @param filter_custom_regex (optional) character; regular expression (regex) that selects tokens that will be removed. Default is \code{NULL}.
#' @param filter_vocab_term_max integer; maximum number of terms in vocabulary, takes top most frequent terms. Default is \code{Inf}.
#' @param filter_term_count_min integer; minimum number of occurences over all documents.
#' @param filter_term_count_max integer; maximum number of occurences over all documents.Default is \code{Inf}.
#' @param filter_doc_count_min integer; term will be kept when number of documents that contain this term is larger than this value.
#' @param filter_doc_count_max integer; term will be kept when number of documents that contain this term is lower than this value. Default is \code{Inf}.
#' @param filter_doc_proportion_max numeric; maximum proportion (0.-1.) of documents which should contain term.
#' @param filter_doc_proportion_min numeric; minimum proportion (0.-1.) of documents which should contain term.
#' @param representations character vector; of text representations that should be constructed, chose from \code{"BoW"},\code{"TextStats"},\code{"DocEmb"} and \code{"TopicModel"}. Multiple representations can be constructed at once: \code{c("BoW","TextStats")}.
#' @param BoW_type t
#' @param BoW_validationVarImpTable (optional) data.frame; used for validation of a model with bag-of-word covariates. A varImp data.frame with the covariate names and covariate values of a trained model. The varImp data.frame can be found in plpResult$model$varImp or plpModel$varImp.
#' @param DocEmb_word_embeddings (optional) character; of a data.frame loaded in the R environment that contains the word embeddings. First column must contain the word, the other n-1 columns contain the embedding values.
#' @param TopicModel_type t
#' @param TopicModel_model character; name of a topic model object loaded in the R environment.
#' @param covariateDataSave (optional) character; location and file name of where the created covariateData must be stored.
#' @param covariateDataLoad (optional) character; location and file name of where the created covariateData must be loaded from. Anything else is ignored, just the covariateData is loaded and returned.
#' @return covariateSettings object, that can be used by the OHDSI FeatureExtraction package.
#' @export

createTritonCovariateSettings<-function(
  # GENERAL SETTINGS
  useNoteData = TRUE,
  startDay=-30,
  endDay=0,
  idrange=NULL,
  parallel=FALSE,
  analysisId=999,
  # NOTE TABLE SETTINGS
  note_databaseschema="cdm",
  note_tablename="note",
  note_customWhere="",
  # PIPELINE SETTINGS
  pipe_preprocess_function=NULL,
  pipe_tokenizer_function="word",
  pipe_ngrams=1,
  pipe_saveVocab=FALSE,
  pipe_outputFolder=NULL,
  # TERM FILTER SETTINGS
  filter_stopwords=NULL,
  filter_custom_regex=NULL,
  filter_vocab_term_max=Inf,
  filter_term_count_min=50,
  filter_term_count_max=Inf,
  filter_doc_count_min=50,
  filter_doc_count_max=Inf,
  filter_doc_proportion_max=0.5,
  filter_doc_proportion_min=0.001,
  # REPRESENTATION SETTINGS
  representations=c("TextStats"), # c("BoW","DocEmb","TopicModel","TextStats")
  BoW_type=c("binary"), # c("binary","frequency","tfidf")
  BoW_validationVarImpTable=NULL,
  DocEmb_word_embeddings=NULL,
  TopicModel_type=c("lsa"), # c("lsa","lda","stm")
  TopicModel_model=NULL,
  # SAVING AND LOADING
  covariateDataSave="",
  covariateDataLoad="")
{
  textrep<-representations
  reps<- c("BoW","DocEmb","TopicModel","TextStats")
  BoW_types<- c("binary","frequency","tfidf")
  TopicModel_type<- c("lsa","lda","stm")
  if (!all(textrep %in% reps)){
    wrong<-paste(textrep[which(!textrep %in% reps)], collapse = ", ")
    stop(paste0("'",wrong,"' is/are not a valid text representation(s), choose from '",paste(reps,collapse = ", "),"'."))
  }
  if("BoW" %in% textrep){
    if(!all(BoW_type %in% BoW_types)){
      wrong<-paste(BoW_type[which(!BoW_type %in% BoW_types)], collapse = ", ")
      stop(paste0("'",wrong,"' is/are not a BoW_type(s), choose from '",paste(BoW_types,collapse = ", "),"'."))
    }
  }
  if("DocEmb" %in% textrep & is.null(DocEmb_word_embeddings)) stop("Provide the DocEmb_word_embeddings to create document embeddings.")
  if("TopicModel" %in% textrep){
    if(!all(TopicModel_type %in% TopicModel_type)){
      wrong<-paste(TopicModel_type[which(!TopicModel_type %in% TopicModel_type)], collapse = ", ")
      stop(paste0("'",wrong,"' is/are not a TopicModel_type(s), choose from '",paste(TopicModel_type,collapse = ", "),"'."))
    }
    if(is.null(TopicModel_model)){
      stop("Provide a trained TopicModel_model.")
    }
  }

  covariateSettings <- list(useNoteData = useNoteData,
                            startDay=startDay,
                            endDay=endDay,
                            idrange=idrange,
                            parallel=parallel,
                            analysisId=analysisId,
                            note_databaseschema=note_databaseschema,
                            note_tablename=note_tablename,
                            note_customWhere=note_customWhere,
                            pipe_preprocess_function=pipe_preprocess_function,
                            pipe_tokenizer_function=pipe_tokenizer_function,
                            pipe_ngrams=pipe_ngrams,
                            pipe_saveVocab=pipe_saveVocab,
                            pipe_outputFolder=pipe_outputFolder,
                            filter_stopwords=filter_stopwords,
                            filter_custom_regex=filter_custom_regex,
                            filter_vocab_term_max=filter_vocab_term_max,
                            filter_term_count_min=filter_term_count_min,
                            filter_term_count_max=filter_term_count_max,
                            filter_doc_count_min=filter_doc_count_min,
                            filter_doc_count_max=filter_doc_count_max,
                            filter_doc_proportion_max=filter_doc_proportion_max,
                            filter_doc_proportion_min=filter_doc_proportion_min,
                            representations=representations,
                            BoW_type=BoW_type,
                            BoW_validationVarImpTable=BoW_validationVarImpTable,
                            DocEmb_word_embeddings=DocEmb_word_embeddings,
                            TopicModel_type=TopicModel_type,
                            TopicModel_model=TopicModel_model,
                            covariateDataSave=covariateDataSave,
                            covariateDataLoad=covariateDataLoad)
  attr(covariateSettings, "fun") <- "getTritonCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}


