set_permissions <- function() {
  config$permissions <- list()

  config$permissions[["ui_normomo_email_ssi"]] <- Permission$new(
    key = "ui_normomo_email_ssi",
    value = fhi::isoyearweek(),
    production_days = c(2, 3)
  )

  config$permissions[["ui_normomo_email_internal"]] <- Permission$new(
    key = "ui_normomo_email_internal",
    value = fhi::isoyearweek(),
    production_days = c(2, 3)
  )

  config$permissions[["ui_norsyss_kht_email"]] <- Permission$new(
    key = "ui_norsyss_kht_email",
    value = fhi::isoyearweek(),
    production_days = c(3)
  )

}
