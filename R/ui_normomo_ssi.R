#'ui_normomo_ssi
#'
#' @export
ui_normomo_ssi <- function(data, argset, schema) {
  # data <- tm_get_data("ui_normomo_ssi", index_plan=1)
  # argset <- tm_get_argset("ui_normomo_ssi", index_plan=1, index_argset = 1)
  # schema <- tm_get_schema("ui_normomo_ssi")

  folder <- fs::dir_ls(path("output", "normomo", argset$today, "ssi"), regexp = "norway")
  folder <- fs::dir_ls(folder, regexp = "COMPLETE")
  file <- fs::dir_ls(folder)

  html <- glue::glue(
    "Dear EuroMOMO hub,<br><br>",
    "Please find attached the current week's results.<br><br>",
    "Sincerely,<br><br>",
    "Norway"
  )

  fd::mailgun(
    subject = glue::glue("[euromomo input] [Norway] [{stringr::str_replace(normomo_yrwk(), '-', ' ')}]"),
    html = html,
    to = fd::e_emails("ui_normomo_ssi", is_final = actions[["normomo_email"]]$is_final()),
    attachments = file,
    is_final = actions[["normomo_email"]]$is_final()
  )
}
