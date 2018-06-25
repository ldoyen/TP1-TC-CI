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
  selection <- getState()$val
  trans<-TRUE
  if(selection == "Non"){
    demande_confirmation<-"Attention si vous ne validez pas votre le\xE7on maintenant,"
    Encoding(demande_confirmation) <- "latin1"
    message(demande_confirmation)
    demande_confirmation<-"il vous faudra la refaire pour pouvoir la valider."
    Encoding(demande_confirmation) <- "latin1"
    message(demande_confirmation)
    demande_confirmation<-"Etes-vous certain de ne pas vouloir valider votre TP ?"
    Encoding(demande_confirmation) <- "latin1"
    message(demande_confirmation)
    demande_confirmation<-"Pour ne pas valider r\xE9pondez TRUE ?"
    Encoding(demande_confirmation) <- "latin1"
    trans <- !(readline(demande_confirmation) == TRUE)
  }
  
  if (trans){
  groupe_etud <- readline("Quelle est votre groupe ? ")
  demande_num<-"Quelle est votre num\xE9ro d'\xE9tudiant ? "
  Encoding(demande_num) <- "latin1"
  num_etud <- readline(demande_num)
  nom_etud <- readline("Quelle est votre nom de famille ? ")
  demande_prenom<-"Quelle est votre pr\xE9nom ? "
  Encoding(demande_prenom) <- "latin1"
  prenom_etud <- readline(demande_prenom)

  # Please edit the link below
  pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLSedErETojgRotoYLiI8ynXy6pcg7n_zMHOm40IJuBP5Ucm7Aw/viewform?usp=pp_url&entry.396843662="
  
  # Do not edit the code below
  if(!grepl("=$", pre_fill_link)){
    pre_fill_link <- paste0(pre_fill_link, "=")
  }
  
  p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}
  
  temp <- tempfile()
  log_ <- getLog()
  nrow_ <- max(unlist(lapply(log_, length)))
  log_tbl <- data.frame(groupe = groupe_etud,
                        num = num_etud,
                        nom = nom_etud,
                        prenom = prenom_etud,
                        course_name = rep(log_$course_name, nrow_),
                        lesson_name = rep(log_$lesson_name, nrow_),
                        question_number = p(log_$question_number, nrow_, NA),
                        correct = p(log_$correct, nrow_, NA),
                        attempt = p(log_$attempt, nrow_, NA),
                        skipped = p(log_$skipped, nrow_, NA),
                        datetime = p(log_$datetime, nrow_, NA),
                        stringsAsFactors = FALSE)
  write.csv(log_tbl, file = temp, row.names = FALSE)
  encoded_log <- base64encode(temp)
  browseURL(paste0(pre_fill_link, encoded_log))
  print(encoded_log)
  }
  return(TRUE)
}

#answear test to known if the value of the answear is between b_inf and b_sup
test_between <- function(b_inf,b_sup){
  n<-length(b_inf)
  res<-TRUE
  e <- get("e", parent.frame())
  e<-e$value
  for(i in 1:n){
    res<-res&(e[i] >= b_inf[i])&(e[i] <= b_sup[i])
  }
  return(res)
}
