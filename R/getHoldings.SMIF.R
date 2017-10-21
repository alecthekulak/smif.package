#' Loads the current SMIF holdings
#'
#' Fetches the current holdings for the SMIF from the server. Data returned will be sufficient to conduct
#' portfolio optimization on its own. Additional analysis on the information is possible in conjunction with
#' the other \code{smif.package} functions.
#'
#' @details
#' For compliance purposes, in order to connect to the server the connection info is not stored in plaintext.
#' The password is inputted directly by the user at runtime, and the server's IP address is encrypted. Decryption
#' occurs at runtime via answering a question that members are aware of but the general public does not know.
#' Function uses specific error handling and custom error messages.
#'
#' @section Warning:
#' Due to security settings on our SQL server, for certain R sessions the DBI connections are severed improperly.
#' When DBI connections are left open too long they expire and subsequent DBI connections to the server are
#' impossible until a new session is started. For this reason it is reccomended the results be stored in an
#' environment variable as soon as they are downloaded, and the values are drawn from a variable rather then
#' directly from the function call.
#'
#' @param pw Character; string of the SQL server's password
#' @param ipkey Character; string answer to decryption question
#' @param auto.assign Logical; should results be assigned to variable in the environment specified by \code{env}.
#' If results are assigned to environment, variable name will be "smif.holdings". Defaults to \code{TRUE}
#' @param env Environment; specifies the environment to assign the results to. If this is not a valid \code{environment}
#' then auto.assign will evaluate as \code{FALSE}. Defaults to \code{.GlobalEnv}
#'
#' @return a data.frame with the following columns:
#' \item{Ticker}{The ticker of the given holding}
#' \item{Shares}{The number of shares we currently own}
#'
#' @aliases getHoldings.SMIF
#' @author Alec Kulakowski, \email{alecthekulak@gmail.com}
#' @keywords misc data
#' @examples
#' \donttest{
#' getHoldings.SMIF(FALSE, ipkey="The IP Key", pw="The Server Password")
#' }
#' @rdname getHoldings.SMIF
#' @export getHoldings.SMIF
"getHoldings.SMIF" <- function(auto.assign=TRUE,
                               ipkey = readline("What is our favorite bank: "),
                               pw = readline("Enter the server password: "),
                               env=.GlobalEnv){
  if(tolower(gsub("[[:blank:]]+", "", auto.assign)) == "help"){
    stop("For help with this function please contact a SMIF Department Head.")
  }
  ip_encrypt = "+KmVTGBOZEWNHPK3TqpqwTwl+oVLqS8BDeeqfNHO"
  decryption_key = tolower(gsub("[[:blank:]]+", "", ipkey))
  tryCatch({ip_raw = safer::decrypt_string(ip_encrypt, key=decryption_key)},
           error = function(e) stop("Incorrect value for decryption key. Connection cannot be made to SMIF server."))
  #driver <- RMySQL::MySQL()
  tryCatch({con <- DBI::dbConnect(RMySQL::MySQL(),user='root',password=pw, host=ip_raw, port=3306, dbname='smif')},
           error = function(e) stop("Incorrect password for server. Connection cannot be made to SMIF server. "))
  #if(get0("advanced") == TRUE){ # MAKE THIS WORK WITH GLOBAL GETOPTIONS
  if(isTRUE(get0(".advanced")) || .getAdmin()){
    positions <- DBI::dbReadTable(con, "openPositions")
  }else{ # MAKE THIS WORK WITH GLOBAL GETOPTIONS
    positions <- DBI::dbReadTable(con, "openPositions")[,2:3]
    colnames(positions) <- c("Ticker", "Shares")
  }
  DBI::dbDisconnect(con)
  # con=NULL
  # DBI::dbUnloadDriver(driver)
  if(!is.environment(env)){ auto.assign = FALSE }
  # browser()
  if(auto.assign){
    assign(x = "smif.holdings", value = positions, envir = env)
    return("smif.holdings")
  }else{
    return(positions)
  }
}
# enc <- safer::encrypt_string("ip_address", key=ipkey)
# aa <- tryCatch({con <- DBI::dbConnect(driver,user='root',password=pw, host=ip_raw, port=3306, dbname='smif')},
#          error = function(e) {
#            print(e)
#            print("error")
#            return(e)})
