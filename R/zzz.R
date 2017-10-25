# On-Load Functions
.onAttach <- function(libname, pkgname){
  # Loads data
  setTimeFrame()
  # Sends welcome message (if verbose) or interactive
  if(interactive() || getOption("verbose")){
    desc <- utils::packageDescription(pkgname)
    start_msg <- paste0("Package ", pkgname,"(",desc$Version,") was sucessfully loaded.\nFor more info, visit: ",
                        desc$URL)
    packageStartupMessage(start_msg)
  }
}
# .onLoad <- function(libname, pkgname){
#   .onAttach(libname = libname, pkgname = pkgname)
# }
