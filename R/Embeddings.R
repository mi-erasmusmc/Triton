### Functions for training embeddings
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

# trainW2V.gensim<-function(notes_dfm, d=50, filename="w2vModel"){
#   t0<-Sys.time()
#   ParallelLogger::logInfo(paste0("Training Word2Vec embedding"))
#   # to do in PYTHON using gensim
#   ParallelLogger::logInfo(paste0("Done ",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))
#   # print some stats #TODO
#   return(lda)
# }
#
# trainFT.gensim<-function(notes_dfm, k=50, filename="ftModel"){
#   t0<-Sys.time()
#   ParallelLogger::logInfo(paste0("Training FastText embedding"))
#   # to do in PYTHON using gensim
#   ParallelLogger::logInfo(paste0("Done ",round(difftime(Sys.time(),t0, units = 'min'),2)," min"))
#   # print some stats #TODO
#   return(lda)
# }


### Helper functions for applying the embeddings
selectEmbTerms<-function(toks,wordsInEmb){
  ##==## Select the terms in the tokens that are also in the embedding ##==##
  toks<-toks[toks %in% wordsInEmb]
  return(toks)
}
aggrNoteEmbedding<-function(toks,wordEmb,aggfunc){
  ##==## Aggregate over word embeddings to create a note embedding ##==##
  noteEmb<-wordEmb[toks,] # select the embeddings of the tokens
  noteEmb<-noteEmb[complete.cases(noteEmb),] # remove the tokens that have no embedding
  if(aggfunc=="avg") noteEmb<-colSums(noteEmb)/length(toks) # aggregate (average) word/token embeddings
  else if(aggfunc=="sum") noteEmb<-colSums(noteEmb) # aggregate (sum) word/token embeddings
  else stop("error not the right aggregation function")
  return(noteEmb)
}
