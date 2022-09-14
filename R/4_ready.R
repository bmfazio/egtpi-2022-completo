# Read verification tables and ensure no missings in verification column ----
read_xlsx("mid/_fuzzyjoin/_verify_ial.xlsx",
          col_types = c(rep("guess", 13), "text")) -> ready_fuzz_ial
read_xlsx("mid/_fuzzyjoin/_verify_ses.xlsx",
          col_types = c(rep("guess", 13), "text")) -> ready_fuzz_ses

if(any(is.na(ready_fuzz_ial$REVISADO)|any(ready_fuzz_ial$REVISADO == ""))){
  stop("Faltan verificaciones manuales en tabla IAL")
}

if(any(is.na(ready_fuzz_ses$REVISADO)|any(ready_fuzz_ses$REVISADO == ""))){
  stop("Faltan verificaciones manuales en tabla SESIONES")
}

# Prepare to join full forms data with correct UBIGEO ----
ready_fuzz_ial %>%
  mutate(UBIGEO = case_when(REVISADO == "OK" ~ UBIGEO,
                            REVISADO == "NO" ~ "DROP",
                            TRUE ~ REVISADO)) %>%
  filter(UBIGEO != "DROP") %>%
  select(UBIGEO, ROW_ID) %>%
  unique -> matched_ial

ready_fuzz_ses %>%
  mutate(UBIGEO = case_when(REVISADO == "OK" ~ UBIGEO,
                            REVISADO == "NO" ~ "DROP",
                            TRUE ~ REVISADO)) %>%
  filter(UBIGEO != "DROP") %>%
  select(UBIGEO, ROW_ID) %>%
  unique -> matched_ses

ial_gs %>%
  left_join(matched_ial,
            by = "ROW_ID") -> ready_ial

ses_gs %>%
  left_join(matched_ses,
            by = "ROW_ID") -> ready_ses

# Collapse to relevant columns ----
### FALTA CONSTRUIR MENSAJES DETALLADOS PARA DESCRIBIR LO QUE FALTA SOLO CALCULARE PUNTAJES POR AHORA
ready_ial %>%
  transmute(UBIGEO, REGISTRO_IAL_2022 = "SI") %>%
  unique -> pre_ial

ready_ses %>%
  select(UBIGEO,
         FECHA = `Fecha de la sesión IAL`,
         H1_ACTA = `Acta de sesión`,
         H1_QUORUM = `Indicar si la sesión cumple con el quórum mínimo`,
         H2_ACTA = `Acta de homologación del Padrón Nominal`,
         H3_MAPA = `Herramienta de sectorización`,
         H4_SEGN = `Herramienta para el seguimiento nominal`,
         H5_COBE = `Herramienta para el análisis de cobertura`) %>%
  filter(!is.na(FECHA)) %>%
  mutate(FECHA = ymd(FECHA),
         MMAAAA = sprintf("%06d", as.numeric(paste0(month(FECHA), year(FECHA)))),
         H1 = ifelse(!is.na(H1_ACTA) & !is.na(H1_QUORUM), 1, 0),
         H2 = ifelse(!is.na(H2_ACTA) & H1 == 1, 1, 0),
         H3 = ifelse(!is.na(H3_MAPA) & H1 == 1, 1, 0),
         H4 = ifelse(!is.na(H4_SEGN) & H1 == 1, 1, 0),
         H5 = ifelse(!is.na(H5_COBE) & H1 == 1, 1, 0),
         TOTAL = H1+H2+H3+H4+H5) %>%
  group_by(UBIGEO, MMAAAA) -> pre_ses