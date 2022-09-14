ready_ses %>%
  select(MARCATEMPORAL, ROW_ID, UBIGEO,
         FECHA = `Fecha de la sesión IAL`,
         H1_ACTA = `Acta de sesión`,
         H1_QUORUM = `Indicar si la sesión cumple con el quórum mínimo`,
         H2_ACTA = `Acta de homologación del Padrón Nominal`,
         H3_MAPA = `Herramienta de sectorización`,
         H4_SEGN = `Herramienta para el seguimiento nominal`,
         H5_COBE = `Herramienta para el análisis de cobertura`) %>%
  mutate(MARCATEMPORAL = ymd(substr(MARCATEMPORAL, 1, 10)),
         FECHA = substr(FECHA, 1, 10),
         MMAAAA = case_when(
           is.na(FECHA) ~ "Sin fecha",
           TRUE ~ paste0(year(FECHA),"/",
                         sprintf("%02d", as.numeric(month(FECHA))))),
         H1 = ifelse(!is.na(H1_ACTA) & !is.na(H1_QUORUM), 1, 0),
         H2 = ifelse(!is.na(H2_ACTA) & H1 == 1, 1, 0),
         H3 = ifelse(!is.na(H3_MAPA) & H1 == 1, 1, 0),
         H4 = ifelse(!is.na(H4_SEGN) & H1 == 1, 1, 0),
         H5 = ifelse(!is.na(H5_COBE) & H1 == 1, 1, 0),
         TOTAL = H1+H2+H3+H4+H5) %>%
  group_by(UBIGEO, MMAAAA) %>%
  left_join(
    bd_egtpi %>%
      select(UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO,
             SM2022 = `SELLO MUNICIPAL`),
    by = "UBIGEO") %>%
  transmute(
    MARCATEMPORAL, ROW_ID,
    UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO, SM2022,
    MMAAAA, FECHA, H1_QUORUM, H1_ACTA,
    H2_ACTA, H3_MAPA, H4_SEGN, H5_COBE, TOTAL) %>%
  nest(H1 = matches("^((FECHA)|(H1))")) %>%
  relocate(H1, .before = H2_ACTA) %>%
  mutate(LINK_EVAL =
           ifelse(!(MMAAAA %in% paste0("2022/0", 4:8))|
                    MARCATEMPORAL > "2022-09-04",
                  NA, paste0(UBIGEO,"-",ROW_ID))) %>%
  select(-MARCATEMPORAL) %>%
  left_join(
    rev_total,
    by = c("UBIGEO", "ROW_ID")) %>%
  mutate(ESTADO = ifelse(is.na(ESTADO), "En revisión", ESTADO),
         ESTADO = ifelse(is.na(LINK_EVAL), "-", ESTADO),
         TOTAL = ifelse(is.na(TOTAL_EV), TOTAL, TOTAL_EV)) %>%
  select(-TOTAL_EV) %>%
  mutate(FILTRAR_REV = ESTADO) %>%
  nest(H1 = starts_with("H1"),
       H2 = starts_with("H2"),
       H3 = starts_with("H3"),
       H4 = starts_with("H4"),
       H5 = starts_with("H5"),
       ESTADO = starts_with("ESTADO")) %>%
  relocate(TOTAL, .after = H5) %>%
  relocate(LINK_EVAL, .after = ESTADO) -> table_data

table_data %>%
  unnest(c(H1, H2, H3, H4, H5, ESTADO)) %>% unnest(H1) %>%
  arrange(UBIGEO, ymd(FECHA)) %>%
  transmute(
    UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO, SM2022, MMAAAA,
    H1 = ifelse(is.na(H1_ACTA), "NO", "SI"),
    H2 = ifelse(is.na(H2_ACTA), "NO", "SI"),
    H3 = ifelse(is.na(H3_MAPA), "NO", "SI"),
    H4 = ifelse(is.na(H4_SEGN), "NO", "SI"),
    H5 = ifelse(is.na(H5_COBE), "NO", "SI"),
    TOTAL, ESTADO, ESTADO_OBS,
    H1_EV, H2_EV, H3_EV, H4_EV, H5_EV) -> table_descargable