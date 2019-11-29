#### Algorithmus 1: train_depth ################################################
#
# train_depth(data, n_halfspace, subsample, scope, seed):
  # data enthält die Trainingsdaten (zi,i=1,…,n, in der obigen Notation)
  # n_halfspace ist die Anzahl gezogener Halbräume (Notation im Paper: t)
  # subsample ist der Anteil der Daten der zur Berechnung für jeden Halbraum 
  # zufällig gezogen werden soll (im Paper: =ψ|D|, default sollte 1 sein)
  # scope ist im Paper λ (default sollte 1 sein)
  # seed für den RNG
train_depth <- function(data, n_halfspace, subsample = 1, scope = 1, seed) {
  set.seed(seed)
  
  for (i in seq_len(n_halfspace)) {
    # generate random direction in dataspace
    data_dim <- ncol(data)
    random_direction <- get_direction(data_dim)
    
    # generate subsample
    random_observation <- sample(nrow(data), size = subsample*nrow(data))
    random_sample <- data[random_observation,]
    
    # project Subsample on direction
    projection <- project_on_direction(random_direction, random_sample)
    
    # select halfspace indicator
    halfspace <- select_halfspace(projection, scope)
    
    # Get mass distribution
    mass_upper <- get_mass(projection, "upper")
    mass_lower <- 1 - mass_upper
    

  }
}


get_direction <- function(data_dim) {
  # get norm ditributed direction
  norm_dir <- rnorm(data_dim)
  # standardize direction
  standard_dir <- sqrt(norm_direction ^ 2)
  
  standard_dir
}



select_halfspace <- function(projection, scope) {
  # get locations for min max and mid of projection
  min = min(projection)
  max = max(projection)
  half_range = (scope / 2) * (max - min)
  mid = 0.5 * (max(projection) + min(projection))
  
  # get intevall for halfspace
  halfspace_range <- c(mid + half_range, mid - half_range)
  
  # get halfspace
  halfspace <- runif(1, min = halfspace_range[2], max = halfspace_range[1])
  
  halfspace
}



#### Algorithmus 2: evaluate_depth #############################################
#
# evaluate_depth(data, halfspaces)
# evaluate_depth() berechnet für jeden Punkt x in data die halfspace mass auf 
# Basis der von train_depth zurückgegeben halfspaces, also
# 
# data enthält die Test- bzw. Validierungsdaten (x, in der obigen Notation), deren Tiefe bezüglich der im zweiten Argument definierten Halbräume bestimmt werden soll.
# halfspaces ist das von train_depth zurückgelieferte Objekt. Wie Sie das genau strukturieren bleibt Ihnen überlassen (und will wohlüberlegt sein).
# Ihre Funktionen sollen auch für höherdimensionale x,zi∈Rd mit beliebigem d>2 funktionieren. Das macht vor allem das korrekte zufällige Ziehen der Halbräume evtl. etwas schwieriger.