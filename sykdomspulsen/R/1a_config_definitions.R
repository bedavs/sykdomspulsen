set_definitions <- function() {
  if (sc::config$is_production) {
    config$border <- 2020
  } else {
    config$border <- 2020
  }

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
      ),
      diags = list(
        "influensa" = c("R80"),
        "gastro" = c("D11", "D70", "D73"),
        "respiratory" = c("R05", "R74", "R78", "R83"),
        "respiratoryexternal" = c("R05", "R74", "R78", "R83"),
        "respiratoryinternal" = c("R05", "R74", "R83"),
        "lungebetennelse" = c("R81"),
        "bronkitt" = c("R78"),
        "skabb" = c("S72"),

        "hoste" = c("R05"),
        "akkut_ovre_luftveisinfeksjon" = c("R74"),
        "luftveisinfeksjon_ika" = c("R83"),
        "luftveissykdom_ika" = c("R99"),
        "virusinfeksjon_ika" = c("A77"),
        "rxx_for_covid19" = c(
          "R01",
          "R02",
          "R03",
          "R04",
          "R05",
          "R06",
          "R07",
          "R08",
          "R09",
          "R21",
          "R24",
          "R25",
          "R27",
          #"R270000",
          "R29",
          #"R71",
          "R72",
          "R74",
          "R75",
          "R76",
          "R77",
          "R78",
          "R79",
          "R80",
          "R81",
          "R82",
          "R83",
          #"R95",
          #"R96",
          "R99",
          "R991"
          #"R9910000"
        ),

        "covid19" = c("R991", "R992"),
        "covid19_r991" = c("R991"),
        "covid19_r992" = c("R992"),
        "engstelig_luftveissykdom_ika" = c("R27")
      )
    )
  )
}
