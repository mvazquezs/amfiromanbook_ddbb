### Carrega de fitxers

source('R/00_setup.R')
source('data/01_data_golvin/00_dimensions_amfiteatres_golvin.R')
source('data/02_data_vazquez/00_dimensions_amfiteatres_vazquez.R')
source('R/02_tab_summary.R')
source('R/03_imputacio.R')

### carrega del setup
  amphi_load_packages(
    update_packages = FALSE)

### Carrega de dades
df_golv <- load_dimensions_golvin(
  filtrar_provincia = c('hispania', 'panonia', 'britania'),
  filtrar_pais = NULL,
  seleccionar_columnes = c(contains('amplada'), contains('alcada'), 'bib'),
  retornar_originals = FALSE,
  format_llarg = TRUE)

df_vazq <- load_dimensions_vazquez(
  filtrar_edifici = 'amphitheater',
  filtrar_provincia = c('hispania', 'panonia', 'britania'),
  filtrar_pais = NULL,
  seleccionar_columnes = c(contains('amplada'), contains('alcada'), 'bib'),
  retornar_originals = FALSE,
  format_llarg = TRUE)


### Taula descriptiva 'Golvin original'
tab_01_ori_golv <- tab_summary(
    df = df_golv,
    grup_by = 'nom',
    seleccio_variables = c(contains('amplada'), contains('alcada')),
    stats_adicionals = TRUE,
    sd_num = 2,
    q_lower = 0.1,
    q_upper = 0.99,
    na.rm = TRUE,
    bind_rows = FALSE,
    digits = 2)

### Taula descriptiva 'Vazquez original'
tab_01_ori_vazq <- tab_summary(
    df = df_vazq,
    grup_by = 'nom',
    seleccio_variables = c(contains('amplada'), contains('alcada')),
    stats_adicionals = TRUE,
    sd_num = 2,
    q_lower = 0.1,
    q_upper = 0.99,
    na.rm = TRUE,
    bind_rows = FALSE,
    digits = 2)

### Per test de comparació

