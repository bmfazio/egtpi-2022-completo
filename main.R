"in/EGTPI_BD_22_JULIO.xlsx" -> where_bd
c("042022", "052022", "062022", "072022", "082022", "092022") -> rangoMMAAAA
eval_url <- "https://docs.google.com/forms/d/e/1FAIpQLSfEE7M3ygPDZYHG-071Mc34n-CDXtlLkzv1ez7p8yGCc0L3PQ/viewform?usp=pp_url&entry.1107513281="
eval_results <- "14fenF9h4qJynn9AqlTpZ1bJknaF8iWG1MaLpmE1RHrU"
excel_icon <- "excel.png"
meses_sm <- 4:8

source("R/1_setup.R", encoding = "utf-8")
source("R/2_load.R", encoding = "utf-8")
source("R/3_fuzzyjoin.R", encoding = "utf-8")

source("R/4_ready.R", encoding = "utf-8")
# Usado para construir BD y actualizar SM:
write_xlsx(ready_ial, "out/ready_ial.xlsx")
write_xlsx(ready_ses, "out/ready_ses.xlsx")
# Usado para Tableau
write_xlsx(ready_tableau, "out/egtpi_tableau_data.xlsx")

source("R/5_bd.R", encoding = "utf-8")
write_xlsx(ready_bd, "out/bd_egtpi_unido.xlsx")

source("R/6_rev-data.R", encoding = "utf-8")
write_xlsx(table_descargable, "out/table_descargable.xlsx")

source("R/7_input-p2.R", encoding = "utf-8")
source("R/8_make-p2.R", encoding = "utf-8")
write_xlsx(data_p2, "out/data_p2.xlsx")

source("R/9_dashboard-table.R", encoding = "utf-8")
saveRDS(list(title_div, filter_div, table_div), "mid/rmd-objs/sm-table.rds")

rmarkdown::render(
  "index.Rmd",
  output_format = "html_document",
  output_file = "tablero/index.html")