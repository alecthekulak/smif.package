# On-Load Functions
.onAttach <- function(libname, pkgname){
  # Loads data
  setTimeFrame()
  options("getSymbols.warning4.0" = FALSE)
  # Sends welcome message (if verbose) or interactive
  if (interactive() || getOption("verbose")) {
    desc <- utils::packageDescription(pkgname)
    start_msg <- paste0("Package ", pkgname,"(",desc$Version,") was sucessfully loaded.\nFor more info, visit: ",
                        desc$URL)
    packageStartupMessage(start_msg)
  }
}
# Set up environment
.server.data <- new.env(parent = emptyenv())
.ss <- new.env(parent = emptyenv())
# ^ this is important, leave this in

# .server.data <- new.env(parent = emptyenv())
# .onLoad <- function(libname, pkgname){
#   .onAttach(libname = libname, pkgname = pkgname)
# }
#.onUnload <- function(libname){}
#.onDetach <- function(libname){}
#.Last.lib <- function(libname){}
# On Package Detach
# .onDetach <- function(libname){
#   if(exists(".server.data", mode="environment")){
#     detach(.server.data)
#   }
# }
