getS3method("ranef", "merMod")
function (object, condVar = FALSE, drop = FALSE, whichel = names(ans), 
          postVar = FALSE, ...) 
{
  if (!missing(postVar) && missing(condVar)) {
    warning(sQuote("postVar"), " is deprecated: please use ", 
            sQuote("condVar"), " instead")
    condVar <- postVar
  }
  ans <- object@pp$b(1)
  if (!is.null(object@flist)) {
    levs <- lapply(fl <- object@flist, levels)
    asgn <- attr(fl, "assign")
    cnms <- object@cnms
    nc <- vapply(cnms, length, 1L)
    nb <- nc * (nl <- vapply(levs, length, 1L)[asgn])
    nbseq <- rep.int(seq_along(nb), nb)
    ml <- split(ans, nbseq)
    for (i in seq_along(ml)) ml[[i]] <- matrix(ml[[i]], ncol = nc[i], 
                                               byrow = TRUE, dimnames = list(NULL, cnms[[i]]))
    ans <- lapply(seq_along(fl), function(i) data.frame(do.call(cbind, 
                                                                ml[asgn == i]), row.names = levs[[i]], check.names = FALSE))
    names(ans) <- names(fl)
    stopifnot(is(whichel, "character"))
    whchL <- names(ans) %in% whichel
    ans <- ans[whchL]
    if (condVar) {
      sigsqr <- sigma(object)^2
      vv <- .Call(merPredDcondVar, object@pp$ptr(), as.environment(rePos$new(object)))
      for (i in names(ans)) attr(ans[[i]], "postVar") <- vv[[i]] * 
        sigsqr
    }
    if (drop) 
      ans <- lapply(ans, function(el) {
        if (ncol(el) > 1) 
          return(el)
        pv <- drop(attr(el, "postVar"))
        el <- drop(as.matrix(el))
        if (!is.null(pv)) 
          attr(el, "postVar") <- pv
        el
      })
    class(ans) <- "ranef.mer"
  }
  ans
}
<environment: namespace:lme4>