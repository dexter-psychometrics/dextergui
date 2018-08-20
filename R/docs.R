

# based on: https://stackoverflow.com/questions/8918753/r-help-page-as-object

Rd2list <- function(Rd){
  names(Rd) <- substring(sapply(Rd, attr, "Rd_tag"),2);
  temp_args <- Rd$arguments;

  Rd$arguments <- NULL;
  myrd <- lapply(Rd, unlist);
  myrd <- lapply(myrd, paste, collapse="");

  temp_args <- temp_args[sapply(temp_args , attr, "Rd_tag") == "\\item"];
  temp_args <- lapply(temp_args, lapply, paste, collapse="");
  temp_args <- lapply(temp_args, "names<-", c("arg", "description"));
  myrd$arguments <- temp_args;

  ## added this part myself for convenience
  argnames = lapply(myrd$arguments, function(x) x$arg )
  myrd$arguments = lapply(myrd$arguments, function(x) x$description )
  names(myrd$arguments) = argnames
  ##
  return(myrd);
}

getHelpList <- function(...){
  thefile <- help(...)
  myrd <- utils.getHelpFile(thefile);
  Rd2list(myrd);
}



# utils:::.getHelpFile
# copied from utils to avoid CRAN warning when doing utils:::.getHelpFile
utils.getHelpFile = function (file) 
{
  path <- dirname(file)
  dirpath <- dirname(path)
  if (!file.exists(dirpath)) 
    stop(gettextf("invalid %s argument", sQuote("file")), 
         domain = NA)
  pkgname <- basename(dirpath)
  RdDB <- file.path(path, pkgname)
  if (!file.exists(paste0(RdDB, ".rdx"))) 
    stop(gettextf("package %s exists but was not installed under R >= 2.10.0 so help cannot be accessed", 
                  sQuote(pkgname)), domain = NA)
  #tools:::fetchRdDB(RdDB, basename(file))
  tools.fetchRdDB(RdDB, basename(file))
}

#tools:::fetchRdDB
tools.fetchRdDB = function (filebase, key = NULL) 
{
  fun <- function(db) {
    vals <- db$vals
    vars <- db$vars
    datafile <- db$datafile
    compressed <- db$compressed
    envhook <- db$envhook
    fetch <- function(key) lazyLoadDBfetch(vals[key][[1L]], 
                                           datafile, compressed, envhook)
    if (length(key)) {
      if (!key %in% vars) 
        stop(gettextf("No help on %s found in RdDB %s", 
                      sQuote(key), sQuote(filebase)), domain = NA)
      fetch(key)
    }
    else {
      res <- lapply(vars, fetch)
      names(res) <- vars
      res
    }
  }
  res <- lazyLoadDBexec(filebase, fun)
  if (length(key)) 
    res
  else invisible(res)
}
