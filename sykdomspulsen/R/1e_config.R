set_config <- function() {

  set_definitions()
  set_db()
  set_progressr()
  set_email()
  set_git_cred()

  packageStartupMessage(glue::glue("sykdomspulsen {utils::packageVersion('sykdomspulsen')}"))

  set_permissions()
  #packageStartupMessage("hi2")
  set_tasks()
  #packageStartupMessage("hi3")

}

set_progressr <- function(){
  progressr::handlers(progressr::handler_progress(
    format = "[:bar] :current/:total (:percent) in :elapsedfull, eta: :eta",
    clear = FALSE
  ))
}

set_email <- function(){
  config$email$mm <- reticulate::import("marrow.mailer", delay_load = TRUE)
  config$email$values <- list(
    host = Sys.getenv("EMAIL_HOST"),
    port = Sys.getenv("EMAIL_PORT"),
    username = Sys.getenv("EMAIL_USERNAME"),
    password = Sys.getenv("EMAIL_PASSWORD"),
    author = Sys.getenv("EMAIL_AUTHOR")
  )
  if(!(.Platform$OS.type=="windows" & !reticulate::py_available())){
    config$email$mailer <- config$email$mm$Mailer(reticulate::dict(
      transport = reticulate::dict(
        use = 'smtp',
        host = config$email$values$host,
        port = config$email$values$port,
        username = config$email$values$username,
        password = config$email$values$password,
        tls = 'required',
        debug = TRUE
      )
    ))
  }

}


set_git_cred <- function(){
  config$git_cred <- git2r::cred_token()
}




