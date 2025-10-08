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
df_ample_golv <- load_dimensions_golvin(
  filtrar_provincia = c('hispania', 'panonia', 'britania'),
  filtrar_pais = NULL,
  seleccionar_columnes = c(contains('amplada'), contains('alcada'), -contains('cavea'), 'bib'),
  retornar_originals = FALSE,
  format_llarg = FALSE)

df_ample_vazq <- load_dimensions_vazquez(
  filtrar_edifici = 'amphitheater',
  filtrar_provincia = c('hispania', 'panonia', 'britania'),
  filtrar_pais = NULL,
  seleccionar_columnes = c(contains('amplada'), contains('alcada'), -contains('cavea'), 'bib'),
  retornar_originals = FALSE,
  format_llarg = FALSE)

### Taula descriptiva 'Vazquez original'
tab_01_ori_vazq <- tab_summary(
    df = df_ample_vazq,
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
tab_02_trunc <- imputacio_estadistics(
  df = df_ample_vazq,
  seleccio_variables = c(contains('amplada'), contains('alcada')),
  grup_by = 'nom',
  grup_by_reserva = 'provincia_romana',
  metode_imputacio = 'truncada',
  valor_trim = 0.1,
  report_imputacio = TRUE,
  retornar_original = TRUE)

tab_02_missforest <- imputacio_missforest(
  df = df_ample_vazq,
  seleccio_variables = c(contains('amplada'), contains('alcada')),
  grup_by <- 'nom',
  grup_by_reserva <- 'provincia_romana',
  optim_mtry = TRUE,
  ntree = 100,
  maxiter = 10,
  verbose = TRUE,
  set_seed = 19810424,
  report_imputacio = FALSE,
  retornar_original = TRUE)
