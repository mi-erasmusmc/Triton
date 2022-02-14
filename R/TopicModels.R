
trainLSA<-function(notes_dfm, k=10, tfidf=T, filename="lsaModel.rds"){
  ## training LSA
  t0<-Sys.time()
  ParallelLogger::logInfo(paste0("\t\tTraining LSA with k=",k,"..."))
  if(tfidf){notes_dfm <- quanteda::dfm_tfidf(notes_dfm, scheme_tf = "count", base=10)}
  lsa <- quanteda.textmodels::textmodel_lsa(notes_dfm, nd = k)
  ParallelLogger::logInfo("\t\tTraining done.")
  saveRDS(lsa, file = filename)
  ParallelLogger::logInfo(paste0("\t\tDone ",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))

  # print some stats #TODO
  return(lsa)
}
predictLSA<-function(lsa,newdata){
  lsaPred <- as.data.frame(as.matrix(predict(lsa,newdata)$docs_newspace))
  return(lsaPred)
}

# trainLDA.gensim<-function(notes_dfm, k=10, filename="ldaModel"){
#   t0<-Sys.time()
#   ParallelLogger::logInfo(paste0("Training LDA topic model with k=",k,"..."))
#   # to do in PYTHON using gensim
#   ParallelLogger::logInfo(paste0("Done ",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))
#   # print some stats #TODO
#   return(lda)
# }
# predictLDA.gensim<-function(){
#
# }
