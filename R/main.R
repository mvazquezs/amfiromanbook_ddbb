source('R/00_setup.R')
source('data/01_data_golvin/00_dimensions_amfiteatres_golvin.R')
source('data/02_data_vazquez/00_dimensions_amfiteatres_vazquez.R')
source('R/02_tab_summary.R')
source('R/03_imputacio.R')

df_vazq <- load_dimensions_vazquez(
  filtrar_edifici = 'amphitheater',
  filtrar_provincia = c('hispania', 'panonia', 'britania'),
  filtrar_pais = NULL,
  seleccionar_columnes = c(contains('amplada'), contains('alcada'), 'bib'),
  retornar_originals = FALSE)

df_golv <- load_dimensions_golvin(
  filtrar_provincia = c('hispania', 'panonia', 'britania'),
  filtrar_pais = NULL,
  seleccionar_columnes = c(contains('amplada'), contains('alcada'), 'bib'),
  retornar_originals = FALSE)