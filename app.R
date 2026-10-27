source("R/bootstrap.R", local=TRUE)
tm_load(getwd(), environment())
config <- tm_init(tm_config())
shiny::shinyApp(ui=tm_ui(), server=function(input,output,session) tm_server(input,output,session,config))
