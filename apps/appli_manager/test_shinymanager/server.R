function(input, output, session) {
  # check_credentials directly on sqlite db
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "database.sqlite",
      passphrase = key_get("shinymanager-key", "obiwankenobi",keyring="R-shinymanager-key")
      # passphrase = "passphrase_wihtout_keyring"
    )
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
 
  # your classic server logic
  
}