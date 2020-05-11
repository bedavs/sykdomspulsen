ui_normomo_ssi <- function(data, argset, schema) {
  # tm_run_task("ui_normomo_ssi")
  # data <- tm_get_data("ui_normomo_ssi", index_plan=1)
  # argset <- tm_get_argset("ui_normomo_ssi", index_plan=1, index_argset = 1)
  # schema <- tm_get_schema("ui_normomo_ssi")


  folder <- fs::dir_ls(sc::path("output", "normomo"))
  folder <- max(folder)
  folder <- fs::dir_ls(fs::path(folder, "ssi"), regexp="norway")
  yrwk <- stringr::str_extract(folder, "[0-9][0-9][0-9][0-9]-[0-9][0-9]$")
  yrwk <- stringr::str_replace(yrwk, "-", " ")
  folder <- fs::dir_ls(folder, regexp = "COMPLETE")
  file <- fs::dir_ls(folder)

  html <- glue::glue(
    "Dear EuroMOMO hub,<br><br>",
    "Please find attached the current week's results.<br><br>",
    "Sincerely,<br><br>",
    "Norway"
  )

  if(config$permissions$ui_normomo_email_ssi$has_permission()){
    mailr(
      subject = glue::glue("[euromomo input] [Norway] [{yrwk}]"),
      html = html,
      to = e_emails(
        "ui_normomo_ssi",
        is_final = config$permissions$ui_normomo_email_ssi$is_final()
      ),
      attachments = file,
      is_final = config$permissions$ui_normomo_email_ssi$is_final()
    )
    config$permissions$ui_normomo_email_ssi$revoke_permission()
  }
}
