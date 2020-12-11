function(input, output, session) {
  # check_credentials directly on sqlite db
  res_auth <- secure_server(

    check_credentials = check_credentials(
      "database.sqlite",
      passphrase = "Tsuki")
    )
  
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
 
  # your classic server logic
  
}