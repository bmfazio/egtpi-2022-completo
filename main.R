"in/EGTPI_BD_22_JULIO.xlsx" -> where_bd
c("042022", "052022", "062022", "072022", "082022", "092022") -> rangoMMAAAA

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