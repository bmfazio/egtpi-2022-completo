crosstalk_data <- SharedData$new(table_data)

filter_depa <- filter_select(
  id = "DEPA", label = "DEPARTAMENTO", group = ~ DEPARTAMENTO,
  sharedData = crosstalk_data, multiple = FALSE)
filter_prov <- filter_select(
  id = "PROV", label = "PROVINCIA", group = ~ PROVINCIA,
  sharedData = crosstalk_data, multiple = FALSE)
filter_dist <- filter_select(
  id = "DIST", label = "DISTRITO", group = ~ DISTRITO,
  sharedData = crosstalk_data, multiple = FALSE)
filter_mes <- filter_select(
  id = "MMAAAA", label = "MES", group = ~ MMAAAA,
  sharedData = crosstalk_data, multiple = FALSE)
filter_rev <- filter_select(
  id = "revisado", label = "REVISADO", group = ~ FILTRAR_REV,
  sharedData = crosstalk_data, multiple = FALSE
)
filter_sm2022 <- filter_select(
  id = "sm2022", label = "SELLO MUNICIPAL", group = ~ SM2022,
  sharedData = crosstalk_data, multiple = FALSE
)

crosstalk_table <-  reactable(
  crosstalk_data,
  compact = TRUE,
  sortable = FALSE,
  #filterable = TRUE,
  showSortIcon = TRUE,
  highlight = TRUE,
  defaultPageSize = 7,
  defaultSorted = list(UBIGEO = "asc"),
  columns = list(
    UBIGEO = colDef(show = FALSE),
    ROW_ID = colDef(show = FALSE),
    FILTRAR_REV = colDef(show = FALSE),
    SM2022 = colDef(show = FALSE),
    MMAAAA = colDef(name = "MES", width = 60),
    TOTAL = colDef(width = 55,
                   header = with_tooltip("TOTAL",
                                         "Puntaje es provisional para registros en revisión")),
    LINK_EVAL = colDef(
      name = "",
      width = 65,
      filterable = FALSE,
      cell = function(value, index){
        eval_link <- tags$a(href = paste0(eval_url, value),
                            target = "_blank", strong("Evaluar"))
        if(is.na(value)){eval_link <- tags$div(em(""))}
        eval_link
      }),
    H1 = colDef(
      name = "H1: Sesión IAL",
      width = 120,
      filterable = FALSE,
      cell = function(value, index){
        value <- value[[1]][[1]]
        fecha <- ifelse(is.na(value[[1]]), "-", value[[1]])
        quorum <- ifelse(is.na(value[[2]]), "NO", "SI")
        url <- value[[3]]
        evi <- tags$a(href = url, target = "_blank",
                      strong("Ver evidencia"))
        if(is.na(url)){evi <- tags$div(em("Sin evidencia"))}
        list(evi,
             tags$div(strong("Fecha: "), fecha),
             tags$div(strong("Quórum: "), quorum))
      },
      style = function(value){
        color <- ifelse(value[[2]], "#91ff99", "#f78e88")
        list(background = color)
      }),
    H2 = colDef(
      name = "H2: Homologación",
      width = 120,
      filterable = FALSE,
      cell = function(value, index){
        value <- value[[1]]
        evi <- tags$a(href = value, target = "_blank",
                      strong("Ver evidencia"))
        if(is.na(value)){evi <- tags$div(em("Sin evidencia"))}
        evi
      },
      style = function(value){
        color <- ifelse(value[[2]], "#91ff99", "#f78e88")
        list(background = color)
      }),
    H3 = colDef(
      name = "H3: Sectorización",
      width = 120,
      filterable = FALSE,
      cell = function(value, index){
        value <- value[[1]]
        evi <- tags$a(href = value, target = "_blank",
                      strong("Ver evidencia"))
        if(is.na(value)){evi <- tags$div(em("Sin evidencia"))}
        evi
      },
      style = function(value){
        color <- ifelse(value[[2]], "#91ff99", "#f78e88")
        list(background = color)
      }),
    H4 = colDef(
      name = "H4: Seg. Nominal",
      width = 120,
      filterable = FALSE,
      cell = function(value, index){
        value <- value[[1]]
        evi <- tags$a(href = value, target = "_blank",
                      strong("Ver evidencia"))
        if(is.na(value)){evi <- tags$div(em("Sin evidencia"))}
        evi
      },
      style = function(value){
        color <- ifelse(value[[2]], "#91ff99", "#f78e88")
        list(background = color)
      }),
    H5 = colDef(
      name = "H5: Cobertura",
      width = 100,
      filterable = FALSE,
      cell = function(value, index){
        value <- value[[1]]
        evi <- tags$a(href = value, target = "_blank",
                      strong("Ver evidencia"))
        if(is.na(value)){evi <- tags$div(em("Sin evidencia"))}
        evi
      },
      style = function(value){
        color <- ifelse(value[[2]], "#91ff99", "#f78e88")
        list(background = color)
      }),
    ESTADO = colDef(
      width = 75,
      cell = function(value, index){
        estado_txt <- value[[1]]
        estado_obs <- value[[2]]
        estado <- tags$a(href = paste0('javascript:alert("', estado_obs, '");'),
                         "Observado")
        if(estado_txt != "Observado"){estado <- tags$div(estado_txt)}
        estado
      })
  )
)

div(
  style = "margin-top: 10px; margin-bottom: 5px",
  h1(paste0(
    "Tablero de revisión EGTPI (Actualizado al ",
    substr(Sys.time(),1,16),")"),
    style = "display: inline"),
  tags$a(
    href = "revision-egtpi.xlsx", style = "float: right",
    tags$img(src = excel_icon,
             width = "30px", height = "30px"))
) -> title_div

div(
  bscols(widths = c(NA),
         div(filter_depa),
         div(filter_prov),
         div(filter_dist),
         div(filter_mes),
         div(filter_rev),
         div(filter_sm2022))
) -> filter_div

div(crosstalk_table) -> table_div