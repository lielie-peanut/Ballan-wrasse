###sex change
sex_change <- function(L,Li,deltaL, b){
  1/(1+exp(-b(L-(Li+deltaL))))
}

###