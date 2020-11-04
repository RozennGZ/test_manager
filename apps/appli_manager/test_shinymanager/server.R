function(input, output, session) {
  # check_credentials directly on sqlite db
  res_auth <- secure_server(

    check_credentials = check_credentials(
      "database.sqlite",
      passphrase = keyring::key_get(service="shinymanager-key", username="obiwankenobi",keyring="R-shinymanager-key")
    )
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
 
  # your classic server logic
  
}