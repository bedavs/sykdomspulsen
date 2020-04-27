set_permissions <- function() {
  config$permissions <- list()

  config$permissions[["ui_normomo_email_ssi"]] <- Permission$new(
    key = "ui_normomo_email_ssi",
    value = fhi::isoyearweek(),
    production_days = c(2, 3)
  )
}
