suppressMessages(
  read_xlsx(where_bd,
            sheet = "2. DISTRITAL", skip = 1)
) -> bd_egtpi

# Cargar registros via forms
gs4_deauth()
read_sheet("1AkdDiPTsq2OfKnla4HBRglxG4gt1tfRjVqdyOCNbOms") %>%
  rename(DEPARTAMENTO = Región,
         PROVINCIA = Provincia,
         DISTRITO = Distrito,
         MARCATEMPORAL = `Marca temporal`) %>%
  mutate(PROVINCIA = str_squish(toupper(PROVINCIA)),
         DISTRITO = str_squish(toupper(DISTRITO)),
         MARCATEMPORAL = as.character(MARCATEMPORAL),
         DNI = as.character(DNI),
         `Número del dispositivo legal` = as.character(`Número del dispositivo legal`),
         `Número de celular` = as.character(`Número de celular`),
         ROW_ID = 1:n()) %>%
  slice(-1) %>%
  mutate(`Fecha de aprobación del dispositivo legal` = case_when(
    ROW_ID == 124 ~ "2022-02-07",
    ROW_ID == 361 ~ "2022-05-27",
    ROW_ID == 373 ~ "2020-12-03",
    ROW_ID == 631 ~ "2022-01-21",
    TRUE ~ as.character(`Fecha de aprobación del dispositivo legal`)) %>%
      as_date) -> ial_gs

read_sheet("1ZZhlElA70agj_hCvpKNeCzMcO1F7OLhheaZz86OYGuI",
           .name_repair = "minimal") %>%
  select(-matches('^$')) %>%
  rename(DEPARTAMENTO = Región,
         PROVINCIA = Provincia,
         DISTRITO = Distrito,
         MARCATEMPORAL = `Marca temporal`) %>%
  mutate(PROVINCIA = str_squish(toupper(PROVINCIA)),
         DISTRITO = str_squish(toupper(DISTRITO)),
         MARCATEMPORAL = as.character(MARCATEMPORAL),
         DNI = as.character(DNI),
         `Nombre completo` = as.character(`Nombre completo`),
         Cargo = as.character(Cargo),
         `Número de  celular` = as.character(`Número de  celular`),
         ROW_ID = 1:n()) %>%
  slice(
    -c(
    # REGISTROS DE PRUEBA
    1:3,16,19,21,22,54,315, 
    # ERRORES DE REGISTRO
    392,766,919,
    # SOLICITADO POR CE HUANUCO
    1651,1652,1708,2728,2729,2730,2732,2733,2734)
  ) %>%
  filter(tolower(PROVINCIA) != "prueba",
         tolower(DISTRITO) != "prueba",
         tolower(DNI) != "prueba",
         tolower(`Nombre completo`) != "prueba",
         tolower(Cargo) != "prueba") %>%
  mutate(# ajustes manuales
    `Fecha de la sesión IAL` = `Fecha de la sesión IAL` %>%
      lapply(as.character) %>%
      lapply(function(x)ifelse(length(x)==0, NA, x)) %>%
      unlist,
    `Fecha de la sesión IAL` = case_when(
        ROW_ID == 501 ~ "2022-02-04",
        ROW_ID == 503 ~ "2022-03-04",
        ROW_ID == 504 ~ "2022-05-04",
        ROW_ID == 538 ~ "2022-05-12",
        ROW_ID == 754 ~ "2022-04-27",
        ROW_ID == 1558~ "2022-06-01",
        ROW_ID == 2554~ "2022-07-13",
        ROW_ID == 2853~ "2022-07-21",
        ROW_ID == 3982~ NA_character_,
        ROW_ID == 4087~ "2022-08-22",
        TRUE ~ `Fecha de la sesión IAL`) %>%
      as_date
  ) -> ses_gs

# Store the last versions of googledrive files being used
# ESTA PARTE DEBERIA REDISENARSE:
# 1. Codigo siempre deberia poder correr offline con la ultima version que se descargo
# 2. La actualizacion deberia ser voluntaria y dejar un registro claro del momento en que se hizo la descarga
# POR LO TANTO:
# 1. Separar codigo: primero descarga y almacena, luego se lee y transforma/ejecuta/etc
# 2. Preguntar al correr los scripts si se desea actualizar o correr la misma version
write_xlsx(ial_gs, "mid/googlesheets/ial_gs.xlsx")
write_xlsx(ses_gs, "mid/googlesheets/ses_gs.xlsx")

read_sheet(eval_results) %>%
  filter(tolower(`Código del registro a evaluar [NO MODIFICAR]`) != "prueba",
         tolower(`DNI de la persona que evalúa`) != "prueba") %>%
  group_by(`Código del registro a evaluar [NO MODIFICAR]`) %>%
  mutate(ORDEN = n():1) %>%
  filter(ORDEN == 1) %>%
  select(-ORDEN) %>%
  ungroup() %>%
  transmute(
    UBIGEO = substr(`Código del registro a evaluar [NO MODIFICAR]`, 1, 6),
    ROW_ID = as.numeric(substr(`Código del registro a evaluar [NO MODIFICAR]`, 8, 999)),
    H1_EV = ifelse(`Evaluar herramientas [Herramienta 1: Sesión IAL + quórum]` == "Cumple",
                   1, 0),
    H2_EV = ifelse(`Evaluar herramientas [Herramienta 2: Acta de homologación]` == "Cumple",
                   1, 0),
    H3_EV = ifelse(`Evaluar herramientas [Herramienta 3: Mapa de sectorización]` == "Cumple",
                   1, 0),
    H4_EV = ifelse(`Evaluar herramientas [Herramienta 4: Seguimiento nominal]` == "Cumple",
                   1, 0),
    H5_EV = ifelse(`Evaluar herramientas [Herramienta 5: Análisis de cobertura]` == "Cumple",
                   1, 0),
    TOTAL_EV = ifelse(H1_EV == 0, 0, H1_EV+H2_EV+H3_EV+H4_EV+H5_EV),
    ESTADO = `Resultado de la evaluación`,
    ESTADO_OBS = `Ingrese su observación`) -> rev_total