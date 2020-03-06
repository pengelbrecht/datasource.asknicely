datasource_asknicely <- function(start, end, period_unit) {

# Options -----------------------------------------------------------------
  options(lubridate.week.start = 1) # TODO get rid of this
  tzone <- "CET" # TODO get rid of this
  api_key <- Sys.getenv("ASKNICELY_API_KEY")
  domain_key <- Sys.getenv("ASKNICELY_DOMAIN_KEY")
  # TODO add memoise

  page_size <- 50000
  page_number <- 1
  since_time <- 0
  format <- "csv"
  url <-
    glue(
      "https://{domain_key}.asknice.ly/api/v1/responses/asc/{page_size}/{page_number}/{since_time}/{format}/answered/json?X-apikey={api_key}"
    )
  responses <- httr::GET(url) %>% content() %>%
    mutate(responded = with_tz(as_datetime(responded), tzone = tzone),
           period = as_date(floor_date(responded, period_unit))) %>%
    filter(segment == "NPS (relationel)" & responded >= start & responded <= end) %>%
    mutate(
      promoter = ifelse(answer >= 9, 1, 0),
      passive = case_when(answer == 8 ~ 1,
                          answer == 7 ~ 1,
                          TRUE ~ 0),
      detractor = ifelse(answer <= 6, 1, 0),
    )

  nps <-
    responses %>% group_by(period) %>% summarise(
      promoters  = sum(promoter),
      passives   = sum(passive),
      detractors = sum(detractor)
    ) %>%
    mutate(
      total           = promoters + passives + detractors,
      nps             = (promoters - detractors) / total * 100,
      moe             = nps_moe(promoters, passives, detractors)
    ) %>% select(
      period,
      nps,
      responses = total,
      moe
    )
}

datasource_asknicely_rolling <- function(start, end, period_unit, rolling_periods = 4) {
  options(lubridate.week.start = 1)
  api_key <- "fe348b00d00f84d2120e049f027ab5f623af1a04135"
  domain_key <- "firmafonaps"
  page_size <- 50000
  page_number <- 1
  since_time <- 0
  format <- "csv"
  url <-
    glue(
      "https://{domain_key}.asknice.ly/api/v1/responses/asc/{page_size}/{page_number}/{since_time}/{format}/answered/json?X-apikey={api_key}"
    )
  responses <- httr::GET(url) %>% content() %>%
    mutate(responded = with_tz(as_datetime(responded), tzone = "CET"),
           period = as_date(floor_date(responded, period_unit))) %>%
    filter(segment == "NPS (relationel)" & responded >= start & responded <= end) %>%
    mutate(
      promoter = ifelse(answer >= 9, 1, 0),
      passive = case_when(answer == 8 ~ 1,
                          answer == 7 ~ 1,
                          TRUE ~ 0),
      detractor = ifelse(answer <= 6, 1, 0),
    )

  nps <-
    responses %>% group_by(period) %>% summarise(
      promoters  = sum(promoter),
      passives   = sum(passive),
      detractors = sum(detractor)
    ) %>%
    mutate(
      total           = promoters + passives + detractors,
      nps             = (promoters - detractors) / total * 100,
      promoters_roll  = RcppRoll::roll_sum(promoters, rolling_periods, fill = NA, align = "right"),
      passives_roll   = RcppRoll::roll_sum(passives, rolling_periods, fill = NA, align = "right"),
      detractors_roll = RcppRoll::roll_sum(detractors, rolling_periods, fill = NA, align = "right"),
      total_roll      = promoters_roll + passives_roll + detractors_roll,
      nps_roll        = (promoters_roll - detractors_roll) / total_roll * 100,
      moe_roll        = nps_moe(promoters_roll, passives_roll, detractors_roll),
    ) %>% select(
      period,
      nps_roll,
      responses_roll = total_roll,
      moe_roll
    ) %>%
    filter(!is.na(nps_roll))
}
