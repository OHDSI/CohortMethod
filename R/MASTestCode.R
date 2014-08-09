  pw <- "msuchard"
  user <- "ohdsi"
  server <- "10.44.156.60/ohdsi"
  
  
  
  
  cyclopsFit <- fitCyclopsModel(cyclopsData, 
                        prior = prior("laplace", 0.1, useCrossValidation = TRUE),
                        control = control(cvType = "auto", noiseLevel = "quiet"))