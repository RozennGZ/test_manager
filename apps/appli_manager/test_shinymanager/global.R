# credentials <- data.frame(
#   user = c("shiny", "shinymanager"), # mandatory
#   password = c("azerty", "12345"), # mandatory
#   start = c("2019-04-15",NA), # optinal (all others)
#   expire = c(NA, "2022-12-31"),
#   admin = c(FALSE, TRUE),
#   comment = "Simple and secure authentification mechanism
#   for single ‘Shiny’ applications.",
#   stringsAsFactors = FALSE
# )

library(shiny)
library(shinymanager)

#library(keyring)
# keyring_create("R-shinymanager-key")
# key_set_with_value(service="shinymanager-key",password="Tsuki", username="obiwankenobi",keyring = "R-shinymanager-key")
# 
# shinymanager::create_db(
#   credentials_data = credentials,
#   sqlite_path = "apps/appli_manager/test_shinymanager/database.sqlite", # will be created
#   passphrase = key_get("shinymanager-key", "obiwankenobi",keyring="R-shinymanager-key")
#   # passphrase = "passphrase_wihtout_keyring"
# )
