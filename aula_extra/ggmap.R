
library(ggmap)
register_google("AIzaSyBYywlmcr-wCCTKbOmHiiWMqI8eTQn6HUY")
tudo <- ggmap::geocode("Avenida Paulista, 1000", 
                       output = "all")


