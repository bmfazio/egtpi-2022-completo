read_xlsx("../../datasets/midis/priorizados-anexo01-cierre-brechas-v2.xlsx") %>%
  pivot_wider(id_cols = ubigeo, names_from = anexo, values_from = anexo) %>%
  rename(UBIGEO = ubigeo,
         cdb_anexo1 = `1`,
         cdb_anexo2 = `2`,
         cdb_anexo3 = `3`) %>%
  mutate(UBIGEO = sprintf("%06d", UBIGEO),
         cdb_anexo1 = ifelse(is.na(cdb_anexo1), "NO", "SI"),
         cdb_anexo2 = ifelse(is.na(cdb_anexo2), "NO", "SI"),
         cdb_anexo3 = ifelse(is.na(cdb_anexo3), "NO", "SI")) -> cdb_ubi

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
  mutate(MMAAAA = sprintf("%06d", as.numeric(paste0(month(FECHA), year(FECHA)))),
         H1 = ifelse(!is.na(H1_ACTA) & !is.na(H1_QUORUM), 1, 0),
         H2 = ifelse(!is.na(H2_ACTA) & H1 == 1, 1, 0),
         H3 = ifelse(!is.na(H3_MAPA) & H1 == 1, 1, 0),
         H4 = ifelse(!is.na(H4_SEGN) & H1 == 1, 1, 0),
         H5 = ifelse(!is.na(H5_COBE) & H1 == 1, 1, 0),
         TOTAL = H1+H2+H3+H4+H5) %>%
  group_by(UBIGEO, MMAAAA) %>%
  select(UBIGEO, MMAAAA, H1, H2, H3, H4, H5, TOTAL) %>%
  mutate(MAXTOTAL = max(TOTAL)) %>%
  filter(MAXTOTAL == TOTAL) %>%
  mutate(SAMETOTAL = 1:n()) %>%
  filter(SAMETOTAL == 1) %>%
  select(-SAMETOTAL, -MAXTOTAL) -> rready_ses

full_join(
  ready_ial %>% transmute(UBIGEO, REGISTRO_IAL_2022 = "SI") %>% unique,
  rready_ses %>%
    filter(MMAAAA %in% c("042022", "052022", "062022", "072022", "082022")) %>%
    pivot_wider(id_cols = UBIGEO, names_from = MMAAAA,
                values_from = c(H1, H2, H3, H4, H5, TOTAL)),
  by = "UBIGEO") %>%
  relocate(
    any_of(c("H1_052022", "H2_052022", "H3_052022", "H4_052022", "H5_052022", "TOTAL_052022",
             "H1_062022", "H2_062022", "H3_062022", "H4_062022", "H5_062022", "TOTAL_062022",
             "H1_072022", "H2_072022", "H3_072022", "H4_072022", "H5_072022", "TOTAL_072022",
             "H1_082022", "H2_082022", "H3_082022", "H4_082022", "H5_082022", "TOTAL_082022")),
    .after = "TOTAL_042022"
  ) -> append_bd

egtpi %>%
  select(UBIGEO, IAL_CONFORMADA) %>%
  arrange(UBIGEO) %>%
  left_join(append_bd, by = "UBIGEO") %>%
  mutate(IAL_CONFORMADA = case_when(!is.na(REGISTRO_IAL_2022) ~ "SI",
                                    TRUE ~ IAL_CONFORMADA)) %>%
  left_join(cdb_ubi, by = "UBIGEO") %>%
  select(-REGISTRO_IAL_2022) %>%
  replace_na(list(cdb_anexo1 = "NO", cdb_anexo2 = "NO", cdb_anexo3 = "NO")) -> ready_bd