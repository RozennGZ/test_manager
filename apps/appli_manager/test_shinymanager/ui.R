secure_app(fluidPage(
  tags$h2("My secure application"),
  verbatimTextOutput("auth_output")

)  , enable_admin = TRUE)