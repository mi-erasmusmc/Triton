#' createTritonCovariateSettings
#'
#' Create a covariateSettings object for constructing text representation (Triton) covariates from the notes table in the OMOP CDM.
#' Possible representations: text statistics(\code{TextStats}), and Bag-of-Words(\code{BoW})(\code{binary},\code{frequency},\code{TFIDF}) and Topic Models(\code{TopicModel}), and averaged or summed word embeddings(\code{AvgWordEmb},\code{SumWordEmb}) using trained models.
#'
#' @param useTextData logical; option to disable the creation of text representation covariates. Default is \code{True}.
#' @param startDay integer; start day before the index date for with the text representations have to be computed. Default is \code{-30}.
#' @param endDay integer; end day before the index date for with the text representations have to be computed. Default is \code{0}.
#' @param idrange (optional) integer vector; specifying the range of integers that can be used to generate the covariateids, max is 2147482. Default is \code{c(1,2147482)}.
#' @param parallel logical; to indicate whether multi-threading should be used (Not on Windows). Default is \code{False}.
#' @param note_databaseschema character; database schema other than the one passed through FeatureExtraction. Default is \code{NULL}.
#' @param note_tablename character; note table name, provide if different than OMOP cdm default. Default is "note".
#' @param note_customWhere (optional) character; with a SQL where statement to filter the note import. Example "WHERE note_source_value='communication'". Default is \code{""}.
#' @param pipe_preprocess_function function; to preprocess the stings before tokenization. Default is \code{\link{tolower}}.
#' @param pipe_tokenizer_function character or function; to tokenize the strings. Default is quanteda tokenizer (\code{\link[quanteda]{tokens}}), with argument "word". Other possible arguments are "fasterword", "fastestword", "sentence", and "character". It is possible to provide a custom tokenizer function. This function should take the document strings as input and should return a list of character vectors (tokens).
#' @param pipe_ngrams integer vector; specifying the number of elements to be concatenated in each ngram. For example: \code{c(1,2)} creates all unigrams and bigrams; \code{c(1:3)} creats all unigrams, bigrams, and trigrams. Default is 1: no ngrams (unigram).
#' @param pipe_saveVocab logical; option to save the generated vocabulary as rds file in the outputFolder. Default is \code{False}.
#' @param pipe_outputFolder (optional) character; file path and name for saving output files. Default is \code{NULL}.
#' @param filter_stopwords character vector; of list of stopwords that will be removed. Default is \code{NULL} See \code{\link[stopwords]{stopwords}} for generating stopwords.
#' @param filter_custom_regex (optional) character; regular expression (regex) that selects tokens that will be removed. Default is \code{NULL}.
#' @param filter_vocab_term_max integer; maximum number of terms in vocabulary, takes top most frequent terms. Default is \code{NULL}.
#' @param filter_term_count_min integer; minimum number of occurences over all documents. Default is \code{NULL}.
#' @param filter_term_count_max integer; maximum number of occurences over all documents. Default is \code{NULL}.
#' @param filter_doc_count_min integer; term will be kept when number of documents that contain this term is larger than this value. Default is \code{NULL}.
#' @param filter_doc_count_max integer; term will be kept when number of documents that contain this term is lower than this value. Default is \code{NULL}.
#' @param filter_doc_proportion_max numeric; maximum proportion (0.-1.) of documents which should contain term. Default is \code{NULL}.
#' @param filter_doc_proportion_min numeric; minimum proportion (0.-1.) of documents which should contain term. Default is \code{NULL}.
#' @param representations character vector; of text representations that should be constructed, chose from \code{"TextStats"}(default), \code{"BoW"}, \code{"TopicModel"}, and \code{"DocEmb"}. Multiple representations can be constructed at once: \code{c("BoW","TextStats")}.
#' @param BoW_validationVarImpTable (optional) data.frame; used for validation of a model with bag-of-word covariates. A varImp data.frame with the covariate names and covariate values of a trained model. The varImp data.frame can be found in plpResult$model$varImp or plpModel$varImp.
#' @param Word_embeddings (optional) character; of a data.frame loaded in the R environment that contains the word embeddings. Row names must contain the word, the n columns contain the embedding values.
#' @param TopicModel_file character; name of the topic model file in the outputfolder for saving and loading.
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
  note_databaseschema=NULL,
  note_tablename="note",
  note_customWhere="",
  # PIPELINE SETTINGS
  pipe_preprocess_function=NULL,
  pipe_tokenizer_function="word",
  pipe_ngrams=1,
  pipe_saveVocab=FALSE,
  pipe_outputFolder="",
  # TERM FILTER SETTINGS
  filter_stopwords=NULL,
  filter_custom_regex=NULL,
  filter_vocab_term_max=NULL,
  filter_term_count_min=NULL,
  filter_term_count_max=NULL,
  filter_doc_count_min=NULL,
  filter_doc_count_max=NULL,
  filter_doc_proportion_max=NULL,
  filter_doc_proportion_min=NULL,
  # REPRESENTATION SETTINGS
  representations=c("TextStats"), # c("BoW_bin","BoW_freq","BoW_tfidf","WordEmb_Avg","WordEmb_Sum","TopicModel_lsa","TopicModel_lda","TextStats")
  BoW_validationVarImpTable=NULL,
  Word_embeddings=NULL,
  TopicModel_file=NULL,
  # SAVING AND LOADING
  covariateDataSave="",
  covariateDataLoad="")
{
  textrep<-representations
  reps<- c("BoW_bin","BoW_freq","BoW_tfidf","WordEmb_avg","WordEmb_sum","TopicModel_lsa","TopicModel_lda","TextStats")

  ### check the input
  if (!all(textrep %in% reps)){
    wrong<-paste(textrep[which(!textrep %in% reps)], collapse = ", ")
    stop(paste0("'",wrong,"' is/are not a valid text representation(s), choose from '",paste(reps,collapse = ", "),"'."))
  }
  if(("WordEmb_avg" %in% textrep | "WordEmb_sum" %in% textrep) & is.null(Word_embeddings)) stop("Provide the Word_embeddings to create aggregated word embeddings.")
  if(("TopicModel_lsa" %in% textrep | "TopicModel_lda" %in% textrep) & is.null(TopicModel_file)) stop("Provide a TopicModel_file (name) for saving or loading a topic model.")
  if(!is.null(TopicModel_file)){
    if(file.exists(TopicModel_file)) print(paste("TopicModel_file exists,",TopicModel_file,"will be loaded."))
    else print(paste("The TopicModel_file does not exist, a new model will be trained and saved as",TopicModel_file))
  }

  ### Check if processing function is a function, if not null ###
  if(!is.null(pipe_preprocess_function) & !is.function(pipe_preprocess_function)){
    stop("The pipe_preprocess_function is not a valid R function.")
  }

  ### Check valid tokenizer options ###
  tokOpts<-c("word","fasterword","fastestword","sentence","character")
  tokfunc<-pipe_tokenizer_function
  if (!is.function(tokfunc) && !tokfunc %in% tokOpts){
    stop(paste0("'",tokfunc,"' is not a valid quanteda tokenizer, choose from '",paste(repOpts,collapse = ", "),"', or provide your own tokenizer as R function."))
    return(NULL)
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
                            BoW_validationVarImpTable=BoW_validationVarImpTable,
                            Word_embeddings=Word_embeddings,
                            TopicModel_file=TopicModel_file,
                            covariateDataSave=covariateDataSave,
                            covariateDataLoad=covariateDataLoad)
  attr(covariateSettings, "fun") <- "getTritonCovariateData"
  class(covariateSettings) <- "covariateSettings"
  return(covariateSettings)
}


