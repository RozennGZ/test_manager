# credentials <- data.frame(
#   user = c("shiny", "shinymanager"), # mandatory
#   password = c("azerty", "12345"), # mandatory
#   start = c("2019-04-15"), # optinal (all others)
#   expire = c(NA, "2019-12-31"),
#   admin = c(FALSE, TRUE),
#   comment = "Simple and secure authentification mechanism
#   for single ‘Shiny’ applications.",
#   stringsAsFactors = FALSE
# )

library(shiny)
library(shinymanager)

library(keyring)
 #key_set("R-shinymanager-key", "obiwankenobi")

shinymanager::create_db(
  credentials_data = credentials,
  sqlite_path = "database.sqlite", # will be created
  passphrase = key_get("R-shinymanager-key", "obiwankenobi")
  # passphrase = "passphrase_wihtout_keyring"
)
