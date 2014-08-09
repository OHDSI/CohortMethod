  pw <- "msuchard"
  user <- "ohdsi"
  server <- "10.44.156.60/ohdsi"
  
  
  
  
  ccdFit <- fitCyclopsModel(ccdData, 
                        prior = prior("laplace", 0.1, useCrossValidation = TRUE),
                        control = control(cvType = "auto", noiseLevel = "quiet"))