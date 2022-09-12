read_xlsx("mid/googlesheets/ial_gs.xlsx") -> ial_gs
read_xlsx("mid/googlesheets/ses_gs.xlsx") -> ses_gs

# Create a backup of previous manual verification table ----
read_xlsx("mid/_fuzzyjoin/_verify_ial.xlsx",
          col_types = c(rep("guess", 13), "text")) %>%
  mutate(MARCATEMPORAL = as.character(MARCATEMPORAL)) -> prev_fuzz_ial
read_xlsx("mid/_fuzzyjoin/_verify_ses.xlsx",
          col_types = c(rep("guess", 13), "text")) %>%
  mutate(MARCATEMPORAL = as.character(MARCATEMPORAL)) -> prev_fuzz_ses

write_xlsx(prev_fuzz_ial,
           paste0("mid/_fuzzyjoin/",
                  today(),
                  "_verify_ial.xlsx"))
write_xlsx(prev_fuzz_ses,
           paste0("mid/_fuzzyjoin/",
                  today(),
                  "_verify_ses.xlsx"))

# Attempt fuzzy joins from egtpi table on forms response sheets ----
ial_gs %>%
  filter(!ROW_ID %in%
           (prev_fuzz_ial %>%
              filter(REVISADO != "") %>%
              pull(ROW_ID))) %>%
  fuzzy_ubigeo(bd_egtpi) -> fuzzy_ial
ses_gs %>%
  filter(!ROW_ID %in%
           (prev_fuzz_ses %>%
              filter(REVISADO != "") %>%
              pull(ROW_ID))) %>%
  fuzzy_ubigeo(bd_egtpi) -> fuzzy_ses

# Reformat columns to output manual verification table ----
fuzzy_verify(fuzzy_ial) -> verify_ial
fuzzy_verify(fuzzy_ses) -> verify_ses

# Pre-load manual verification column with results of previous table ----
prev_fuzz_ial %>%
  bind_rows(verify_ial) %>%
  arrange(-ROW_ID) %>%
  mutate(REVISADO = case_when(dist == 0 ~ "OK", TRUE ~ REVISADO)) -> verify_ial

prev_fuzz_ses %>%
  bind_rows(verify_ses) %>%
  arrange(-ROW_ID) %>%
  mutate(REVISADO = case_when(dist == 0 ~ "OK", TRUE ~ REVISADO)) -> verify_ses

write_xlsx(verify_ial, "mid/_fuzzyjoin/_verify_ial.xlsx")
write_xlsx(verify_ses, "mid/_fuzzyjoin/_verify_ses.xlsx")