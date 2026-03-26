if (requireNamespace("renv", quietly = TRUE)) {
  renv::deactivate()
}

library(rsconnect)

shiny_acc <- Sys.getenv("SHINY_ACC_NAME")
shiny_token <- Sys.getenv("TOKEN")
shiny_secret <- Sys.getenv("SECRET")

rsconnect::setAccountInfo(
  name   = shiny_acc,
  token  = shiny_token,
  secret = shiny_secret
)

options(repos = c(CRAN = "https://cran.rstudio.com/"))

rsconnect::deployApp(
  appDir         = ".",
  appFiles       = c("app.R", "cargas7.R", "data/Sessions_micro01.xlsx"),
  appName        = "cargas_fisicas_7",
  account        = shiny_acc,
  server         = "shinyapps.io",
  forceUpdate    = TRUE,
  launch.browser = FALSE
)
