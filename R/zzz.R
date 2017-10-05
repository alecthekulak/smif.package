# On-Load Functions
.onAttach <- function(libname, pkgname){
  desc <- utils::packageDescription(pkgname)
  start_msg <- "\nQuestions regarding smif.package should be directed to 'alecthekulak@gmail.com'"
  if(interactive() || getOption("verbose")){
    start_msg <- paste0("\nlibrary(smif.package v",desc$Version,") was recently updated. For more info, view the documentation.",
                        start_msg)
  }
  packageStartupMessage(start_msg)
}
# .onLoad <- function(libname, pkgname){
#   .onAttach(libname = libname, pkgname = pkgname)
# }
