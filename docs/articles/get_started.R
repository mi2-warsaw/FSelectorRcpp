## ----setup, include = FALSE-------------------------------------------------------------------------------------------------------------------------
htmltools::tagList(rmarkdown::html_dependency_font_awesome())
library(knitr)
opts_chunk$set(
    comment = "",
    fig.width = 12,
    message = FALSE,
    warning = FALSE,
    tidy.opts = list(
        keep.blank.line = TRUE,
        width.cutoff = 150
        ),
    options(width = 150),
    eval = TRUE
)

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------
#  install.packages('FSelectorRcpp') # stable release version on CRAN
#  devtools::install_github('mi2-warsaw/FSelectorRcpp') # dev version
#  # windows users should have Rtools for devtools installation
#  # https://cran.r-project.org/bin/windows/Rtools/

## ---------------------------------------------------------------------------------------------------------------------------------------------------
library(magrittr)
library(FSelectorRcpp)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
information_gain(               # Calculate the score for each attribute
    formula = Species ~ .,      # that is on the right side of the formula.
    data = iris,                # Attributes must exist in the passed data.
    type  = "infogain",         # Choose the type of a score to be calculated.
    threads = 2                 # Set number of threads in a parallel backend.
  ) %>%                          
  cut_attrs(                    # Then take attributes with the highest rank.
    k = 2                       # For example: 2 attrs with the higehst rank.
  ) %>%                         
  to_formula(                   # Create a new formula object with 
    attrs = .,                  # the most influencial attrs.
    class = "Species"           
  ) %>%
  glm(
    formula = .,                # Use that formula in any classification algorithm.
    data = iris,                
    family = "binomial"         
  )
  

## ---------------------------------------------------------------------------------------------------------------------------------------------------
evaluator_R2_lm <-    # Create a scorer function.
  function(
    attributes,       # That takes the currently considered subset of attributes
    data,             # from a specified dataset.
    dependent = 
      names(data)[1]  # To find features that best describe the dependent variable.
  ) {
    summary(          # In this situation we take the r.squared statistic
      lm(             # from the summary of a linear model object.
        to_formula(   # This is the score to use to choose between considered 
          attributes, # subsets of attributes.
          dependent
        ),
        data = data)
    )$r.squared
  }

feature_search(          # feature_search work in 2 modes - 'exhaustive' and 'greedy'
  attributes = 
    names(iris)[-1],     # It takes attribues and creates combinations of it's subsets.
  fun = evaluator_R2_lm, # And it calculates the score of a subset that depends on the 
  data = iris,           # evaluator function passed in the `fun` parameter.
  mode = "exhaustive",   # exhaustive - means to check all possible 
  sizes =                # attributes' subset combinations 
    1:length(attributes) # of sizes passed in sizes.
)$all

