#' -----------------------------------------------------------------------------
#' TUS Converter                                                 {Environment}
#'
#' @file `_get_Additional.R` contains auxiliary functions to improve
#'       the project environment.
#' -----------------------------------------------------------------------------
#' @author g.sousa
#' @keywords household, diary, survey, parser
#' @repository github.com/EnHub-UK/TUS-to-HSEM-converter
#'


fnGetOS <- function(...) {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if ((.Platform$OS.type == "unix") &
             (Sys.info()["sysname"] == "linux-gnu")) {
    "linux"
  } else if ((.Platform$OS.type == "unix") &
             (Sys.info()["nodename"] == "hpclogin")) {
    "hpc"
  } else {
    stop("Mystery Machine")
  }
}

fnGetTUSProjectPath <- function(...){

  pathFinder <- function(type) {
    switch(type,
           win = getwd(),
           hpc = system('pwd', intern = T),
           mac = system('pwd', intern = T),
           linux = system('pwd', intern = T))
  }

  path <- pathFinder(fnGetOS())
  return(path)
}

fnDisplayUser <- function(...){
  os <- fnGetOS()
  if(os=="mac" | os=="linux"){
    user <- system("whoami", wait = T, intern = TRUE, ignore.stderr = TRUE)
  }else{
    user <- "windows user"
  }
  txtOut <- paste("welcome: ",user," ------------------------+-")
  return(txtOut)
}

fnMakeDir <- function(pathNew, myPath=path.TUS, myOS=fnGetOS()){
  if(myOS == "win"){
    pathNew <- paste0(normalizePath(myPath), gsub("/","\\\\",pathNew))
    shell(paste("mkdir ", pathNew, sep=""))
  }else{
    system(paste("mkdir -p ", pathNew, "/", sep=""))
  }
}

fnRemoveDir <- function(pathNew, myPath=path.TUS, myOS=fnGetOS()){
  if(myOS == "win"){
    pathNew <- paste0(normalizePath(myPath), gsub("/","\\\\",pathNew))
    shell(paste("del /S /Q ", pathNew, sep=""))
  }else{
    system(paste("rm -rf ", pathNew, "/", sep=""), intern=F, ignore.stdout=T)
  }
}
