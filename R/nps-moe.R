nps_moe <- function(promoters, passives, detractors) {
  total <- promoters + passives + detractors
  nps <- (promoters - detractors)/total
  var <- (1-nps)^2*promoters/total + (nps)^2*passives/total + (-1-nps)^2*detractors/total
  moe <-  100 * sqrt(var)/sqrt(total)
}
