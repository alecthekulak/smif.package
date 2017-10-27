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
#' @keywords misc data
#' @family data retrieval functions
#' @examples
#' \donttest{
#' getHoldings.SMIF(FALSE, ipkey="The IP Key", pw="The Server Password")
#' }
#' @export getHoldings.SMIF
"getHoldings.SMIF" <- function(auto.assign=TRUE,
                               ipkey = readline("What is our favorite bank: "),
                               pw = readline("Enter the server password: "),
                               env=.GlobalEnv){
  #make arg 'pw' -> '.pw' in all such functions
  if(tolower(gsub("[[:blank:]]+", "", auto.assign)) == "help"){
    stop("For help with this function please contact a SMIF Department Head.")
  }
  ip_encrypt = "+KmVTGBOZEWNHPK3TqpqwTwl+oVLqS8BDeeqfNHO"
  decryption_key = tolower(gsub("[[:blank:]]+", "", ipkey))
  tryCatch({ip_raw = safer::decrypt_string(ip_encrypt, key=decryption_key)},
           error = function(e) stop("Incorrect value for decryption key. Connection cannot be made to SMIF server."))
  #driver <- RMySQL::MySQL()
  tryCatch({con <- DBI::dbConnect(RMySQL::MySQL(),user='root',password=pw, host=ip_raw, port=3306, dbname='smif')},
           error = function(e) stop("Connection cannot be made to SMIF server. Verify password/check connection."))
  #if(get0("advanced") == TRUE){ # MAKE THIS WORK WITH GLOBAL GETOPTIONS
  if(.getAdmin()){
    positions <- DBI::dbReadTable(con, "openPositions")[,-1] #<-[,-1] removes position id
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
#' Retrieves data from the SMIF server
#'
#' @param what Character; string indicating what to retrieve. Valid entries
#' include 'holdings', 'cash balance'.
#' @param \dots additional parameters
#' @return
#' Data.frame; current holdings for the SMIF (if \code{what=="holdings"})
#' xts; cash balance history of the SMIF (if \code{what=="cash"})
#' @rdname getServerData
#' @export getServerData
"getServerData" <- function(what, ...){
  # ipkey = readline("What is our favorite bank: "),
  # pw = readline("Enter the server password: ")
  # Ensure environment exists
  if(!exists(".server.data", mode="environment")){
    .showUSER("Server Data environment does not exist. Creating it.")
    loadServerData(...)
  }
  if(grepl("holdings", what, ignore.case = TRUE)){
    return( get("holdings", envir = as.environment(".server.data")) ) #in quotes?
    #maybe .GlobalEnv$.server.data$holdings
  }else if(grepl("cash|balance", what, ignore.case = TRUE)){
    return( get("cash_balance", envir = as.environment(".server.data")) ) #just ".server.data" ?
  }else{
    message("Item ", what, " is not found on the SMIF server. ")
  }
}
# \/ change this from (...) to (ipkey, pw) and then use @inheritParams up ^ there
#' Loads/reloads the data from the SMIF server
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom RMySQL MySQL
#' @keywords internal
#' @rdname getServerData
#' @export loadServerData
"loadServerData" <- function(...){
  # Ensure internet exists ---------------------------------------------------------------------
  if(!canConnect()){
    stop("Cannot get data from server. Computer not connected to the internet. Please try again.")
  }
  # .showUSER("Resetting server data environment")
  # # Ensure environment exists ---------------------------------------------------------------------
  # .server.data <- new.env(parent = globalenv())
  clearServerData()
  # ipkey input validation ---------------------------------------------------------------------
  .showUSER("Validating input variables")
  inputs <- list(...)
  if("ipkey" %in% names(inputs)){ ipkey <- inputs$"ipkey"
  }else{ ipkey <- readline("What is our favorite bank: ")
  }
  # Check connection to SMIF server ---------------------------------------------------------------------
  ip_encrypt <- "+KmVTGBOZEWNHPK3TqpqwTwl+oVLqS8BDeeqfNHO"
  decryption_key <- tolower(gsub("[[:blank:]]+", "", ipkey))
  tryCatch({ip_raw <- safer::decrypt_string(ip_encrypt, key=decryption_key)},
           error = function(e) stop("Incorrect value for IP decryption key."))
  #could make more robust, i.e. if error, ask for it again or ask if quit?
  if(!canConnect(ip_raw)){
    stop("Cannot connect to server. Use VPN or connect to Stevens intranet")
  }
  # pw input validation ----------------------------------------------------------------------------------
  if("pw" %in% names(inputs)){ pw <- inputs$"pw"
  }else{ pw <- readline("Enter the server password: ")
  }
  # Connect to server ----------------------------------------------------------------------------------
  .showUSER("Attempting to connect to server")
  tryCatch({con <- DBI::dbConnect(RMySQL::MySQL(),user='root',password=pw, host=ip_raw, port=3306, dbname='smif')},
           error = function(e) stop("Connection cannot be made to SMIF server. Possible incorrect password."))
  .showUSER("Connected to server. Writing holdings")
  # Processing holdings ----------------------------------------------------------------------------------
  holdings <- DBI::dbReadTable(con, "openPositions")[,-1]
  holdings$initial_purchase <- as.Date(holdings$initial_purchase)
  holdings$sector <- cleanSector(holdings$sector)
  names(holdings) <- c("Ticker", "Shares", "Sector", "Purchase_Date")
  assign(x = "holdings", value = holdings, envir = as.environment(".server.data")) #try inherits = TRUE?
  # Processing cash ----------------------------------------------------------------------------------
  .showUSER("Writing cash_balance")
  cash <- DBI::dbReadTable(con, "cashBalance")
  cash <- xts(cash$balance, order.by = as.Date(cash$date))
  names(cash) <- c("Cash_Balance")
  assign(x = "cash_balance", value = cash, envir = as.environment(".server.data"))
  .showUSER("Server data reset. Closing connection")
  DBI::dbDisconnect(con)
  .showUSER("Connection succesfully closed.")
}
#' Determines if connection is possible
#'
#' Pings the specified URL(s) and/or IP address(es) to determine if connection
#' can be made/internet is up/websites are up.
#'
#' @note
#' To simply test if internet connection is possible, run canConnect() or
#' canConnect
#' To test specific sites: Google A: "8.8.8.8", Google B: "8.8.4.4",
#' Google Search: "www.google.com" or "google.com"
#'
#' @param test.site Character; the site URL or IP address to test connection to.
#' Defaults to "8.8.8.8", Google's DNS-A server. Function
#' @param n Numeric; number of times to try the connection. Defaults to 1
#' @param timeout Numeric; time (in milliseconds) to try to connect before
#' returning \code{FALSE}. Defaults to 1000ms (1s)
#' @param clean Logical; should the input \code{test.site} be parsed automatically?
#' Defaults to \code{TRUE}
#' @return Logical; logical vector corresponding to whether or not a connection
#' could be made to given site(s). Length will be equal to \code{length(test.site)}
#' @examples
#' canConnect()
#' @export canConnect
"canConnect" <- function(test.site = "8.8.8.8", n = 1, timeout = 1000, clean = TRUE){
  # For vector inputs
  if(length(cleanIP(test.site)) > 1){
    if(interactive()){ cat("Success") }
    return( sapply(X = cleanIP(test.site), FUN=canConnect, n = n, timeout = timeout, clean = clean, USE.NAMES=F) )
  }
  # For checking server connection status
  if(grepl("server|smif", test.site, ignore.case = TRUE)){
    ipkey <- readline("What is our favorite bank: ")
    ip_encrypt <- "+KmVTGBOZEWNHPK3TqpqwTwl+oVLqS8BDeeqfNHO"
    decryption_key <- tolower(gsub("[[:blank:]]+", "", ipkey))
    tryCatch({test.site <- safer::decrypt_string(ip_encrypt, key=decryption_key)},
             error = function(e) stop("Incorrect value for IP decryption key."))
  }
  # Tests connection
  if(clean){
    com <- paste("ping -n", n, "-w", timeout, cleanIP(test.site))
  }else{
    com <- paste("ping -n", n, "-w", timeout, test.site)
  }
  #https://www.lifewire.com/ping-command-2618099
  if(interactive()){ cat("Success") }
  return( suppressWarnings(
    !as.logical(system(command = com, show.output.on.console = FALSE))
  ))
}
#' Checks status of specific site
#'
#' Checks overall connection to internet (Google and Yahoo),
#' returns \code{FALSE} if connection cannot be made to site,
#' returns \code{TRUE} if
#' @param site.name Character; name of the site to check
#' @return Logical; can site (or internet) be connected to
# "checkSite" <- function(site.name){
#   #IP Tools? use? dont?
#   if(all(canConnect(c("8.8.8.8","yahoo.com")))){ #maybe choose ones OTHER then google and yahoo?
#     if( canConnect( .siteName(site.name) )){
#       return(TRUE)
#     }else{
#       .showUSER("Internet is up, site (",site.name,") is down")
#       return(FALSE)
#     }
#   }else{
#     .showUSER("Internet is down")
#     return(FALSE)
#   }
# }

# enc <- safer::encrypt_string("ip_address", key=ipkey)
# aa <- tryCatch({con <- DBI::dbConnect(driver,user='root',password=pw, host=ip_raw, port=3306, dbname='smif')},
#          error = function(e) {
#            print(e)
#            print("error")
#            return(e)})

#' Clears the server data stored by replacing the environment
#' @rdname getServerData
#' @keywords internal
#' @export clearServerData
"clearServerData" <- function(){
  .showUSER("Resetting server data environment")
  # Wipe environment exists ---------------------------------------------------------------------
  .server.data <- new.env(parent = globalenv())
  if(interactive()){ cat("Success") }
}
