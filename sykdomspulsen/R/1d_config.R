set_config <- function() {

  set_border()
  set_db()
  set_progressr()
  set_email()
  set_git_cred()

  config$def <- list(

    # variables that are common to many tasks
    smallMunicips = c(
      "municip1151",
      "municip1835",
      "municip1252",
      "municip1739"
    ),

    age = list(
      norsyss=list(
        "total" = c(0:105),
        "0-4" = c(0:4),
        "5-14" = c(5:14),
        "15-19" = c(15:19),
        "20-29" = c(20:29),
        "30-64" = c(30:64),
        "65+" = c(65:105)
      )
    ),

    # norsyss specific things that arent relevant to any other tasks
    norsyss = list(
      long_names = list(
        gastro_vk_ot="Mage-tarminfeksjoner",
        respiratoryexternal_vk_ot="Luftveisinfeksjoner"
      ),
      short_names = list(
        gastro_vk_ot="Mage-tarm",
        respiratoryexternal_vk_ot="Luftvei"
      )
    )
  )

  #packageStartupMessage("hi1")
  set_permissions()
  #packageStartupMessage("hi2")
  set_tasks()
  #packageStartupMessage("hi3")

}


set_border <- function() {
  if (sc::config$is_production) {
    config$border <- 2020
  } else {
    config$border <- 2020
  }
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




