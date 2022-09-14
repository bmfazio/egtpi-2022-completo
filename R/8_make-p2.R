# sesiones:
# UBIGEO | FECHA | ESTADO (cumple/en revision/observado)
# PRIORIDAD (esto pa ver con cual me quedo)
# H1, H2, H3, H4, H5, TOTAL, MES

bd_egtpi %>%
  select(UBIGEO) %>%
  full_join(tibble(MES = meses_sm), by = character()) %>%
  left_join(
    input_p2 %>%
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
      select(UBIGEO, ESTADO, MES, H1, H2, H3, H4, H5),
    by = c("UBIGEO", "MES")) %>%
  pivot_wider(names_from = MES,
              values_from = c(H1, H2, H3, H4, H5, ESTADO)) %>%
  transmute(
    UBIGEO,
    H1_4, H2_4, H3_4, H4_4, H5_4, ESTADO_4,
    H1_5, H2_5, H3_5, H4_5, H5_5, ESTADO_5,
    H1_6, H2_6, H3_6, H4_6, H5_6, ESTADO_6,
    H1_7, H2_7, H3_7, H4_7, H5_7, ESTADO_7,
    H1_8, H2_8, H3_8, H4_8, H5_8, ESTADO_8) %>%
  replace_na(list(H1_4 = 0, H2_4 = 0, H3_4 = 0, H4_4 = 0, H5_4 = 0,
                  H1_5 = 0, H2_5 = 0, H3_5 = 0, H4_5 = 0, H5_5 = 0,
                  H1_6 = 0, H2_6 = 0, H3_6 = 0, H4_6 = 0, H5_6 = 0,
                  H1_7 = 0, H2_7 = 0, H3_7 = 0, H4_7 = 0, H5_7 = 0,
                  H1_8 = 0, H2_8 = 0, H3_8 = 0, H4_8 = 0, H5_8 = 0,
                  ESTADO_4 = "", ESTADO_5 = "", ESTADO_6 = "", ESTADO_7 = "", ESTADO_8 = "")) -> data_p2