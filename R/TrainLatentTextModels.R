
trainLSA<-function(notes_dfm, k=10, tfidf=T, filename="lsaModel.rds"){
  ## training LSA
  t0<-Sys.time()
  ParallelLogger::logInfo(paste0("Training LSA topic model with k=",k,"..."))
  if(tfidf){notes_dfm <- quanteda::dfm_tfidf(notes_dfm, scheme_tf = "count", base=10)}
  lsa <- quanteda.textmodels::textmodel_lsa(notes_dfm, nd = k)
  ParallelLogger::logInfo("Training done.")
  saveRDS(lsa, file = filename)
  ParallelLogger::logInfo(paste0("Done ",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))

  # print some stats #TODO
  return(lsa)
}

trainSTM<-function(notes_dfm, k=10, filename="stmModel.rds", verbose=T){
  ## training STM
  t0<-Sys.time()
  ParallelLogger::logInfo(paste0("Training STM topic model with k=",k,"..."))
  stm <- stm::stm(notes_dfm, K = k, verbose = verbose, init.type = "Spectral")
  ParallelLogger::logInfo("Training done.")
  saveRDS(stm, file = filename)
  ParallelLogger::logInfo(paste0("Done ",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))

  # print some stats #TODO
  return(stm)
}

trainLDA<-function(notes_dfm, k=10, filename="ldaModel.rds"){
  ## training STM
  t0<-Sys.time()
  ParallelLogger::logInfo(paste0("Training LDA topic model with k=",k,"..."))
  lda <- seededlda::textmodel_lda(notes_dfm_trimmed, k = k)
  ParallelLogger::logInfo("\t\tTraining done.")
  saveRDS(lda, file = "ldaModel.rds")
  ParallelLogger::logInfo(paste0("Done ",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))

  # print some stats #TODO
  return(lda)
}

trainGloVe<-function(notes_tokens, d=50, parCores=2, filename="gloveModel.rds", verbose=T){
  ## training GloVe
  t0<-Sys.time()
  feats<-quanteda::dfm(notes_tokens,verbose=T) %>%
    quanteda::dfm_trim(min_termfreq = 5) %>%
    quanteda::featnames()
  notes_tokens_filt<-quanteda::tokens(parallel::mclapply(notes_tokens,Triton:::selectEmbTerms,wordsInEmb=feats,mc.cores = parCores))

  notes_fcm <- quanteda::fcm(notes_tokens_filt,
                             context="window",
                             count="weighted",
                             window=5,
                             weights=1/(1:5),
                             tri=T)

  glove<- text2vec::GlobalVectors$new(rank=50,x_max=10)
  wv_main<-glove$fit_transform(notes_fcm, n_iter=200, covergence_tol=0.01, n_threads=parCores,verbose=verbose)
  saveRDS(glove, file = filename)
  ParallelLogger::logInfo(paste0("Done ",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))

  wv_context <-glove$components
  word_vectors <- wv_main + t(wv_context)
  return(word_vectors)
}

