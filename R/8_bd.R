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

full_join(
  ready_ial %>% transmute(UBIGEO, REGISTRO_IAL_2022 = "SI") %>% unique,
  input_p2 %>%
    filter(!is.na(FECHA)) %>%
    mutate(MMAAAA = paste0(sprintf("%02d", as.numeric(month(FECHA))),
                           year(FECHA))) %>%
    filter(MMAAAA %in% rangoMMAAAA) %>%
    arrange(UBIGEO, FECHA) %>%
    group_by(UBIGEO, MES) %>%
    filter(PRIORIDAD >= min(1, max(PRIORIDAD))) %>%
    filter(TOTAL == max(TOTAL)) %>%
    filter(FECHA == max(FECHA)) %>%
    filter(PRIORIDAD == max(PRIORIDAD)) %>%
    filter(ROW_ID == max(ROW_ID)) %>%
    distinct(UBIGEO, FECHA, ESTADO, PRIORIDAD, TOTAL,
             .keep_all = TRUE) %>%
    ungroup %>%
    select(UBIGEO, MMAAAA, H1, H2, H3, H4, H5, TOTAL) %>%
    pivot_wider(id_cols = UBIGEO, names_from = MMAAAA,
                values_from = c(H1, H2, H3, H4, H5, TOTAL)),
  by = "UBIGEO") %>%
  bdcols_relocate(rangoMMAAAA) -> append_bd

bd_egtpi %>%
  select(UBIGEO, IAL_CONFORMADA) %>%
  arrange(UBIGEO) %>%
  left_join(append_bd, by = "UBIGEO") %>%
  mutate(IAL_CONFORMADA = case_when(!is.na(REGISTRO_IAL_2022) ~ "SI",
                                    TRUE ~ IAL_CONFORMADA)) %>%
  left_join(cdb_ubi, by = "UBIGEO") %>%
  select(-REGISTRO_IAL_2022) %>%
  replace_na(
    list(cdb_anexo1 = "NO",
         cdb_anexo2 = "NO",
         cdb_anexo3 = "NO")) -> ready_bd

bd_egtpi %>%
  select(UBIGEO, DEPARTAMENTO, PROVINCIA, DISTRITO,
         ESCALAMIENTO2022 = `ESCALAMIENTO\r\n2022\r\nTOTAL`,
         SM2022 = `SELLO MUNICIPAL`,
         PPSS_RESPONSABLE) %>%
  left_join(
    ready_bd %>%
      select(-starts_with("H")) %>%
      select(-starts_with("cdb")),
    by = "UBIGEO") -> ready_tableau