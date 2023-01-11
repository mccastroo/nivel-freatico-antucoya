library(plumber)

#* @apiTitle Variables JIGSAW para Nivel Freatico
#* @apiDescription 

grades_names <- c(
  'TON', 'CUT', 'CUS', 'NO3', 'CO3', 'MENA1', 'CAL1',	
  'IQS1', 'ACIDO', 'MENA2', 'MENA3', 'CAL2', 'CAL3', 'CAL4', 'IQS2', 'IQS3',
  'IQS4',	'UGM_NIR', 'SZO', 'CAO', 'PGL', 'CHL', 'SER', 'QZ', 'FEO', 'CAL', 'YES'	
  )

folds <- 12
variablesNumericas <- c('SZO', 'CAO', 'PGL', 'CHL', 'ACIDO', 'SER', 'QZ', 'FEO', 'YES')

#* Función que concluye las variables de JIGSAW para el modelo predictivo del Nivel Freatico
#* @param datos_camiones Este .json es el que se obtiene de la tabla 'by_custom_cycle_time_detail'
#* @param datos_grades Este .json es el que obtiene de la tabla 'grades_qualities'
#* @post /jigsaw_variables
#* @serializer unboxedJSON
function(datos_camiones, datos_grades) {
  
  datos_camiones <- jsonlite::fromJSON(datos_camiones) %>%
    base::as.data.frame() %>%
    dplyr::as_tibble()
  
  datos_grades <- jsonlite::fromJSON(datos_grades) %>%
    base::as.data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::select(grade_id, start_date, qualities) %>%
    tidyr::separate(qualities, grades_names, sep = ',')

  datos_finales_camiones <- datos_camiones %>%
    dplyr::left_join(datos_grades, by = 'grade_id') %>%
    dplyr::group_by(id, grade_id) %>%
    dplyr::filter(start_date <= time_full) %>%
    dplyr::arrange(start_date) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::rename('UGM' = 'UGM_NIR') %>%
    dplyr::mutate(UGM = as.numeric(UGM)) %>%
    dplyr::mutate_at(variablesNumericas, .funs = ~as.numeric(.x)) %>%
    dplyr::filter(time >= lubridate::ymd('2020-01-01'), dump == 'CHANCADO', UGM %in% c(10, 20, 21, 24, 30, 40))

  # Variables Númericas -----------------------------------------------------

  FoldsVarsNumericos <- datos_finales_camiones %>%
    dplyr::select(id_mod, variablesNumericas) %>%
    dplyr::group_by(id_mod) %>%
    dplyr::mutate(ID = 1:dplyr::n(), .after = id_mod) %>%
    dplyr::mutate() %>%
    dplyr::mutate(G = ceiling(ID/dplyr::n()*folds), .after = ID) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(cols = variablesNumericas, names_to = 'Medicion', values_to = 'Valor') %>%
    dplyr::filter(Valor >= 0) %>%
    dplyr::select(-ID) %>%
    dplyr::group_by(id_mod, G, Medicion) %>%
    dplyr::summarise_all(
      .funs = list(
        minimo = min,
        maximo = max,
        mediana = median,
        promedio = mean,
        desviacion = sd,
        varianza = var
        )
      ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = c(G, Medicion),
      names_sep = '_',
      values_from = c('minimo', 'promedio', 'mediana', 'desviacion', 'varianza', 'maximo'),
      names_prefix = 'Fold'
      )

  ModuloVarsNumericos <- datos_finales_camiones %>%
    dplyr::select(id_mod, variablesNumericas) %>%
    tidyr::pivot_longer(cols = variablesNumericas, names_to = 'Medicion', values_to = 'Valor') %>%
    dplyr::filter(Valor >= 0) %>%
    dplyr::group_by(id_mod, Medicion) %>%
    dplyr::summarise_all(
      .funs = list(
        minimo = min,
        maximo = max,
        mediana = median,
        promedio = mean,
        desviacion = sd,
        varianza = var
        )
      ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = Medicion,
      names_sep = '_',
      values_from = c('minimo', 'promedio', 'mediana', 'desviacion', 'varianza', 'maximo')
    )

  # UGM ---------------------------------------------------------------------

  UGM_modulo_separacion <- datos_finales_camiones %>%
    dplyr::group_by(id_mod) %>%
    dplyr::mutate(ID = 1:dplyr::n()) %>%
    dplyr::mutate(G = ceiling(ID/dplyr::n()*folds), .after = id_mod) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(id_mod, G, UGM) %>%
    dplyr::summarise(N = dplyr::n()) %>%
    dplyr::ungroup(UGM) %>%
    dplyr::mutate(P = N/sum(N)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-N) %>%
    tidyr::pivot_wider(names_from = c('UGM', 'G'),  names_glue = '{G}_UGM_{UGM}', values_from = 'P', names_sort = TRUE)  %>%
    base::replace(is.na(.), 0)

  UGM_modulo_estadisticos <- UGM_modulo_separacion %>%
    tidyr::pivot_longer(cols = -id_mod, values_to = 'Porcentaje') %>%
    tidyr::separate(name, sep = "_", into = c("Fold", "H", "UGM")) %>%
    dplyr::select(-H) %>%
    dplyr::group_by(id_mod, UGM) %>%
    dplyr::summarise(
      minimo = min(Porcentaje),
      promedio = mean(Porcentaje),
      mediana = median(Porcentaje),
      desviacion = sd(Porcentaje),
      varianza = var(Porcentaje),
      maximo = max(Porcentaje)
      ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = UGM,
      names_sep = '_UGM_',
      values_from = c('minimo', 'promedio', 'mediana', 'desviacion', 'varianza', 'maximo')
      )

  UGM_modulo_general <- datos_finales_camiones %>%
    dplyr::group_by(id_mod, UGM) %>%
    dplyr::summarise(N = dplyr::n()) %>%
    dplyr::ungroup(UGM) %>%
    dplyr::mutate(P = N/sum(N)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-N) %>%
    tidyr::pivot_wider(names_from = UGM, values_from = P, names_prefix = 'O_UGM_') %>%
    base::replace(is.na(.), 0)

  variablesJigsaw <- UGM_modulo_general %>%
    dplyr::inner_join(UGM_modulo_estadisticos, by = 'id_mod') %>%
    dplyr::inner_join(ModuloVarsNumericos, by = 'id_mod') %>%
    dplyr::inner_join(FoldsVarsNumericos, by = 'id_mod')

  return(variablesJigsaw)
}