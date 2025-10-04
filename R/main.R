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

df_llarg_golv <- load_dimensions_golvin(
  filtrar_provincia = c('hispania', 'panonia', 'britania'),
  filtrar_pais = NULL,
  seleccionar_columnes = c(contains('amplada'), contains('alcada'), -contains('cavea'), 'bib'),
  retornar_originals = FALSE,
  format_llarg = TRUE)

df_llarg_vazq <- load_dimensions_vazquez(
  filtrar_edifici = 'amphitheater',
  filtrar_provincia = c('hispania', 'panonia', 'britania'),
  filtrar_pais = NULL,
  seleccionar_columnes = c(contains('amplada'), contains('alcada'), -contains('cavea'), 'bib'),
  retornar_originals = FALSE,
  format_llarg = TRUE)


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
  tab_02_mitjana_vazq <- imputacio_val_perduts(
    df = df_llarg_vazq,
    grup_by = c('nom', 'variable'),
    seleccio_variables = 'valor',
    metode_imputacio = 'mitjana_aritmetica',
    metode_reserva = NULL,
    valor_trim = NULL,
    retornar_originals = TRUE)

  tab_02_geometrica_vazq <- imputacio_val_perduts(
    df = df_llarg_vazq,
    grup_by = c('nom', 'variable'),
    seleccio_variables = 'valor',
    metode_imputacio = 'mitjana_geometrica',
    metode_reserva = NULL,
    valor_trim = NULL,
    retornar_originals = TRUE)
  
  tab_02_truncada_vazq <- imputacio_val_perduts(
    df = df_llarg_vazq,
    grup_by = c('nom', 'variable'),
    seleccio_variables = 'valor',
    metode_imputacio = 'mitjana_truncada',
    metode_reserva = NULL,
    valor_trim = 0.1,
    retornar_originals = TRUE)
  
  tab_02_winsor_vazq <- imputacio_val_perduts(
    df = df_llarg_vazq,
    grup_by = c('nom', 'variable'),
    seleccio_variables = 'valor',
    metode_imputacio = 'mitjana_winsoritzada',
    metode_reserva = NULL,
    valor_trim = 0.1,
    retornar_originals = TRUE)

  tab_02_mediana_vazq <- imputacio_val_perduts(
    df = df_llarg_vazq,
    grup_by = c('nom', 'variable'),
    seleccio_variables = 'valor',
    metode_imputacio = 'mediana',
    metode_reserva = NULL,
    valor_trim = NULL,
    retornar_originals = TRUE)

  tab_02_missforest_vazq <- imputacio_val_perduts(
    df = df_llarg_vazq,
    grup_by = NULL,
    seleccio_variables = c('nom', 'variable', 'valor'),
    metode_imputacio = 'missmissForest',
    metode_reserva = 'mediana',
    valor_trim = NULL,
    retornar_originals = TRUE)