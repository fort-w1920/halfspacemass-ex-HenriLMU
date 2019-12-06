#### Algorithmus 1: train_depth ################################################
#
# train_depth(data, n_halfspace, subsample, scope, seed):
# data enthält die Trainingsdaten (zi,i=1,…,n, in der obigen Notation)
# n_halfspace ist die Anzahl gezogener Halbräume (Notation im Paper: t)
# subsample ist der Anteil der Daten der zur Berechnung für jeden Halbraum 
# zufällig gezogen werden soll (im Paper: =ψ|D|, default sollte 1 sein)
# scope ist im Paper λ (default sollte 1 sein)
# seed für den RNG
train_depth <- function(data, n_halfspace, 
                        subsample = 1, scope = 1,
                        seed = 4163) {
  
  checkmate::assert_numeric(subsample, lower = 0, upper = 1, len = 1)
  data <- as.matrix(data)
  set.seed(seed)
  
  halfspaces <- list()
  
  for (i in seq_len(n_halfspace)) {
    # generate random direction in dataspace
    data_dim <- ncol(data)
    random_direction <- get_direction(data_dim)
    
    # generate subsample
    random_observation <- sample(nrow(data), size = subsample*nrow(data))
    random_sample <- data[random_observation,]
    
    # project Subsample on direction; reduction in 1 dim
    projection <- project_on_direction(random_direction, random_sample)
    
    # select halfspace indicator
    random_mid <- select_halfspace(projection, scope)
    
    # Get mass distribution
    mass_upper <- get_mass(projection, random_mid, "upper")
    mass_lower <- get_mass(projection, random_mid, "lower")
    
    
    halfspaces[[i]] <- list(direction = random_direction, 
                            middle = random_mid, 
                            upper = mass_upper, lower = mass_lower)
  }
  
  halfspaces
}

# get a direction in n-dimensional space
get_direction <- function(data_dim) {
  # get norm ditributed direction
  standard_dir <- rnorm(data_dim)
  as.matrix(standard_dir)
}

# project ndmensional space on 1D number
project_on_direction <- function(random_direction, random_sample) {
  
  dot_prod <- random_sample %*% random_direction
  
  dot_prod
}

# Select number that represents the halfspace
select_halfspace <- function(projection, scope) {
  # get locations for min max and mid of projection
  min = min(projection)
  max = max(projection)
  half_range = (scope / 2) * (max - min)
  mid = 0.5 * (max(projection) + min(projection))
  
  # get intevall for halfspace
  halfspace_range <- c(mid + half_range, mid - half_range)
  
  # get random middle
  random_mid <- runif(1, min = halfspace_range[2], max = halfspace_range[1])
  
  random_mid
}

# get proportion on mass higher and lower then random middle
get_mass <- function(projection, random_mid, mass= c("upper", "lower")) {
  
  # how many percent of all objects are projected upper respectively 
  # lower to the mid choosen
  if (mass == "upper") sum(projection >= random_mid) / nrow(projection)
  else sum(projection < random_mid) / nrow(projection)
  
}


#### Algorithmus 2: evaluate_depth #############################################
#
# evaluate_depth(data, halfspaces)
# evaluate_depth() berechnet für jeden Punkt x in data die halfspace mass auf 
# Basis der von train_depth zurückgegeben halfspaces, also
# 
# data enthält die Test- bzw. Validierungsdaten (x, in der obigen Notation)
# , deren Tiefe bezüglich der im zweiten Argument definierten Halbräume bestimmt werden soll.
# halfspaces ist das von train_depth zurückgelieferte Objekt. 
# Wie Sie das genau strukturieren bleibt Ihnen überlassen (und will wohlüberlegt sein).
# Ihre Funktionen sollen auch für höherdimensionale x,zi∈Rd mit beliebigem d>2 funktionieren.
# Das macht vor allem das korrekte zufällige Ziehen der Halbräume evtl. etwas schwieriger.


evaluate_depth <- function(data, halfspaces, metric = c("mass", "depth")) {
  
  data_matrix <- as.matrix(data)
  
  # get a matrix with the projection of each iteration (cols)
  #                                  on each object of the data (rows)
  halfspace_matrix <- sapply(1:length(halfspaces), FUN = add_mass,
                             data_matrix = data_matrix,
                             halfspaces = halfspaces)
  
  # Early Exit if halfspaces measured by mass (mean)
  if (metric == "mass") return(rowMeans(halfspace_matrix))
  
  # Otherwise return the minimum
  halfspace_depth <- apply(halfspace_matrix, 1, FUN = min)
  halfspace_depth
  # match(halfspace_depth, unique(halfspace_depth)) - 1
  
}


add_mass <- function(x, data_matrix, halfspaces) {
  
  # project direction onto data 
  direction <- halfspaces[[x]][["direction"]]
  data_projected <- data_matrix %*% direction 
  halfspace_x <- vector(mode = "numeric", length = nrow(data_projected))
  
  # get halfspace values
  lower <- which(data_projected < halfspaces[[x]][["middle"]])
  halfspace_x[lower] <-  halfspaces[[x]][["lower"]]
  halfspace_x[-lower] <-  halfspaces[[x]][["upper"]]
  
  halfspace_x
}

