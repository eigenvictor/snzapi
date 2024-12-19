.onLoad <- function(libname, pkgname) {

  .snz_provider <-
    rsdmx::SDMXServiceProvider(
      agencyId = "STATSNZ",
      name = "Statistics New Zealand",
      scale = "national",
      country = "NZ",
      builder = rsdmx::SDMXREST21RequestBuilder(
        regUrl = "https://api.data.stats.govt.nz/rest",
        repoUrl = "https://api.data.stats.govt.nz/rest",
        compliant = FALSE
      )
    )

  rsdmx::addSDMXServiceProvider(.snz_provider)

}
