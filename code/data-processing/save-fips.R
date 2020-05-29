## save fips codes as data object in package

fips_codes <- read_csv("data-raw/state_fips_codes.csv") %>%
  bind_rows(
    data.frame(
      state='US',
      state_code='US',
      state_name='United States',
      stringsAsFactors=FALSE)) %>%
  select(location_name = state, location = state_code)

save(fips_codes, file = "data/fips_codes.rdata")
