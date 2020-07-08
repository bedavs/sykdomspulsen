set_permissions <- function() {
  sc::add_permission(
    name = "ui_normomo_email_ssi",
    permission = sc::Permission$new(
      key = "ui_normomo_email_ssi",
      value = fhi::isoyearweek(),
      production_days = c(2, 3)
    )
  )

  sc::add_permission(
    name = "ui_normomo_email_internal",
    permission = sc::Permission$new(
      key = "ui_normomo_email_internal",
      value = fhi::isoyearweek(),
      production_days = c(2, 3)
    )
  )

  sc::add_permission(
    name = "ui_norsyss_kht_email",
    permission = sc::Permission$new(
      key = "ui_norsyss_kht_email",
      value = as.character(lubridate::today()), #fhi::isoyearweek(),
      production_days = c(3)
    )
  )

}
