# Put custom tests in this file.

# Uncommenting the following line of code will disable
# auto-detection of new variables and thus prevent swirl from
# executing every command twice, which can slow things down.

# AUTO_DETECT_NEWVAR <- FALSE

# However, this means that you should detect user-created
# variables when appropriate. The answer test, creates_new_var()
# can be used for for the purpose, but it also re-evaluates the
# expression which the user entered, so care must be taken.

# Get the swirl state
getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Retrieve the log from swirl's state
getLog <- function(){
  getState()$log
}

submit_log <- function(){
  res<-FALSE
  selection <- getState()$val
if(selection %in% 1:5){
  res<-TRUE

  demande_num<-"Quel est votre num\xE9ro d'\xE9tudiant ? "
  Encoding(demande_num) <- "latin1"
  num_etud <- readline(demande_num)
  nom_etud <- readline("Quel est votre nom de famille ? ")
  demande_prenom<-"Quel est votre pr\xE9nom ? "
  Encoding(demande_prenom) <- "latin1"
  prenom_etud <- readline(demande_prenom)

  # Please edit the link below
  pre_fill_link1 <- "https://docs.google.com/forms/d/e/1FAIpQLSedErETojgRotoYLiI8ynXy6pcg7n_zMHOm40IJuBP5Ucm7Aw/viewform?usp=pp_url&entry.396843662="
  pre_fill_link2 <- "https://docs.google.com/forms/d/e/1FAIpQLSd1F_R0PF2TlNGt2fWokYQXXHI04VUmGuXAt28Zg3dLtrkZvw/viewform?usp=pp_url&entry.1337846868="
  pre_fill_link3 <- "https://docs.google.com/forms/d/e/1FAIpQLScu_WmWluTBJQm7sJXyVVkb0WM0W2RyhAUE6ZQYA77HgilMzw/viewform?usp=pp_url&entry.419018389="
  pre_fill_link4 <- "https://docs.google.com/forms/d/e/1FAIpQLScii7wiYhvVPqXJUwtoLmEuKitUDWBpk1tzJwNI050nq1XD5g/viewform?usp=pp_url&entry.1843359488="
  pre_fill_link5 <- "https://docs.google.com/forms/d/e/1FAIpQLSeALcMgvPVUbOglitGCfmLWEc5NaKk3Cp1dVhJvE2fZtedNcA/viewform?usp=pp_url&entry.1206664424="

  pre_fill_link <- switch(selection,
    pre_fill_link1,
    pre_fill_link2,
    pre_fill_link3,
    pre_fill_link4,
    pre_fill_link5
  )

  # Do not edit the code below
  if(!grepl("=$", pre_fill_link)){
    pre_fill_link <- paste0(pre_fill_link, "=")
  }

  p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}

  temp <- tempfile()
  log_ <- getLog()
  nrow_ <- max(unlist(lapply(log_, length)))
  log_tbl <- data.frame( p(log_$question_number, nrow_, NA),
                         p(log_$correct, nrow_, NA),
                         p(log_$attempt, nrow_, NA),
                         p(log_$skipped, nrow_, NA),
                         p(log_$datetime, nrow_, NA),
                        stringsAsFactors = FALSE)
  names(log_tbl) <- c(num_etud, nom_etud, prenom_etud,log_$lesson_name,"t")
  write.csv(log_tbl, file = temp, row.names = FALSE)
  encoded_log <- base64encode(temp)
  browseURL(paste0(pre_fill_link, encoded_log))
  print(encoded_log)
}
  return(res)
}
