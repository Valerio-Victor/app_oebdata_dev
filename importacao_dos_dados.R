

tictoc::tic()


################################################################################
################################################################################
# PACOTES NECESSÁRIOS


library(magrittr, include.only = '%>%')


################################################################################
################################################################################
# ORGANIZAÇÃO DO LAYOUT GRÁFICO


#' Aqui serão organizados a estrutura dos gráficos formatados em plotly, de modo
#' a retirar a maior parte das configurações que, em geral, não são utilizadas.
plotly_layout <- c('hoverClosestCartesian',
                   'hoverCompareCartesian',
                   'resetScale2d',
                   'zoom2d',
                   'sendDataToCloud',
                   'editInChartStudio',
                   'pan2d',
                   'select2d',
                   'lasso2d',
                   'drawclosedpath',
                   'drawopenpath',
                   'zoomIn2d',
                   'zoomOut2d',
                   'drawcircle',
                   'drawrect',
                   'drawline')


################################################################################
################################################################################
# NÍVEL DE ATIVIDADE


#' Importação do número índice do IPCA para posterior cálculo do deflator, isto
#' é, razão entre o número índice no período t e o último número índice
#' disponível:
#' '/t/1737/n1/all/v/2266/p/last%20325/d/v2266%2013'
#' '/t/1737/n1/all/v/2266/p/last%20151/d/v2266%2013'
#' '/t/1737/n1/all/v/2266/p/all/d/v2266%2013'

deflator <- sidrar::get_sidra(api = '/t/1737/n1/all/v/2266/p/last%20325/d/v2266%2013') %>%
  janitor::clean_names() %>%
  dplyr::select(mes_codigo, valor) %>%
  dplyr::transmute(data = lubridate::ym(mes_codigo),
                   indice = valor)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> tabela exportável feita
#' Importação dos dados de PIB a valores correntes e cálculo do valor real (a
#' partir do deflator cálculo acima - IPCA), cálculo das variáções anuais e
#' trimestrais com base nos valores reais e cálculo dos acumulados nominais e
#' reais:
pib_vc <- sidrar::get_sidra(api = '/t/1846/n1/all/v/all/p/all/c11255/90687,90691,90696,90706,90707,93404,93405,93406,93407,93408/d/v585%200') %>%
  janitor::clean_names() %>%
  dplyr::select(valor, trimestre_codigo, setores_e_subsetores) %>%
  dplyr::rename('valor' = valor,
                'trimestre' = trimestre_codigo,
                'setores' = setores_e_subsetores) %>%
  dplyr::mutate(setores = dplyr::case_when(
    setores == 'PIB a preços de mercado'~ 'PIB (preços de mercado)',
    setores == 'Despesa de consumo das famílias' ~ 'Consumo das <br> Famílias',
    setores == 'Despesa de consumo da administração pública' ~ 'Gastos do <br> Governo',
    setores == 'Formação bruta de capital fixo' ~ 'Formação Bruta de <br> Capital Fixo',
    setores == 'Exportação de bens e serviços' ~ 'Exportações',
    setores == 'Importação de bens e serviços (-)' ~ 'Importações',
    setores == 'Agropecuária - total' ~ 'Agropecuária',
    setores == 'Indústria - total' ~ 'Indústria',
    setores == 'Serviços - total' ~ 'Serviços',
    setores == 'Impostos líquidos sobre produtos' ~ 'Impostos Líquidos <br> sobre Produtos'),
    trimestre = lubridate::yq(trimestre),
    ano = lubridate::year(trimestre)) %>%
  dplyr::left_join(deflator, by = c('trimestre' = 'data')) %>%
  dplyr::group_by(setores) %>%
  dplyr::mutate(deflator = (dplyr::last(indice)/indice)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(valor_nominal = valor,
                valor_real = valor*deflator) %>%
  dplyr::select(-valor) %>%
  dplyr::group_by(setores) %>%
  dplyr::mutate(
    var_anual = round((valor_real-dplyr::lag(valor_real, 4))/dplyr::lag(valor_real, 4), 4),
    var_trim = round((valor_real-dplyr::lag(valor_real, 1))/dplyr::lag(valor_real, 1), 4)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(ano, setores) %>%
  dplyr::summarise(ano = ano,
                   trimestre = trimestre,
                   setores = setores,
                   valor_nominal = valor_nominal,
                   valor_real = valor_real,
                   var_anual = var_anual,
                   var_trim = var_trim,
                   acumulado_nominal = cumsum(valor_nominal),
                   acumulado_real = cumsum(valor_real)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(trimestre >= lubridate::ym(201001))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> tabela exportável feita
#' Construindo a tabela de valores de PIB com o pacote ractable. A ideia é
#' adicionar 'interação' de cores nas taxas de crescimento, barras no valor
#' deflacionado.

tabela_pib_valores <- pib_vc %>%
  dplyr::arrange(desc(trimestre)) %>%
  dplyr::filter(setores == 'PIB (preços de mercado)') %>%
  dplyr::select(!c(ano, setores, acumulado_nominal, valor_nominal)) %>%
  dplyr::mutate(trimestre = paste0('0',
                                   lubridate::quarter(trimestre),
                                  '-',
                                  lubridate::year(trimestre)),
                acumulado_real = scales::dollar(acumulado_real,
                                                big.mark = '.',
                                                prefix = 'R$ ',
                                                decimal.mark = ','),
                valor_real = scales::dollar(valor_real,
                                            big.mark = '.',
                                            prefix = 'R$ ',
                                            decimal.mark = ','),
                var_anual = scales::percent(var_anual,
                                            accuracy = 0.01,
                                            decimal.mark = ','),
                var_trim = scales::percent(var_trim,
                                           accuracy = 0.01,
                                           decimal.mark = ',')) %>%
reactable::reactable(
  columns = list(
    trimestre = reactable::colDef(name = 'TRIMESTRE-ANO',
                                  align = 'center'),
    acumulado_real = reactable::colDef(name = 'ACUMULADO NO ANO (em Milhões)',
                                       align = 'center'),
    valor_real = reactable::colDef(name = 'VALORES REAIS (em Milhões)',
                                   align = 'center'),
    var_anual = reactable::colDef(name = 'VARIAÇÃO INTERANUAL (Base: valores reais)',
                                  align = 'center'),
    var_trim = reactable::colDef(name = 'VARIAÇÃO MARGINAL (Base: valores reais)',
                                 align = 'center')
  ),
  defaultColDef = reactable::colDef(headerStyle = list(background = '#440154',
                                                       color = '#fff')),
  bordered = TRUE,
  highlight = TRUE,
  resizable = TRUE,
  pagination = FALSE,
  showPageInfo = FALSE
)


#
#
# Produto Interno Bruto (Número Índice): ----------------------------------


#' Importação dos dados de PIB número índice (de volume com ajuste sazonal) e
#' alteração de base (de 1995 para 2010) para posterior cálculo das variáções
#' anuais e trimestrais:
agregados_indice <- sidrar::get_sidra(api = '/t/1621/n1/all/v/all/p/all/c11255/90687,90691,90696,90707,93404,93405,93406,93407,93408/d/v584%202') %>%
  janitor::clean_names() %>%
  dplyr::select(valor, trimestre_codigo, setores_e_subsetores) %>%
  dplyr::rename('valor' = valor,
                'trimestre' = trimestre_codigo,
                'setores' = setores_e_subsetores) %>%
  dplyr::filter(trimestre >= 200901) %>%
  dplyr::mutate(setores = dplyr::case_when(
    setores == 'PIB a preços de mercado'~ 'PIB (preços de mercado)',
    setores == 'Despesa de consumo das famílias' ~ 'Consumo das <br> Famílias',
    setores == 'Despesa de consumo da administração pública' ~ 'Gastos do <br> Governo',
    setores == 'Formação bruta de capital fixo' ~ 'Formação Bruta de <br> Capital Fixo',
    setores == 'Exportação de bens e serviços' ~ 'Exportações',
    setores == 'Importação de bens e serviços (-)' ~ 'Importações',
    setores == 'Agropecuária - total' ~ 'Agropecuária',
    setores == 'Indústria - total' ~ 'Indústria',
    setores == 'Serviços - total' ~ 'Serviços'),
    trimestre = lubridate::yq(trimestre),
    ano = lubridate::year(trimestre)) %>%
  dplyr::group_by(setores) %>%
  dplyr::mutate(
    var_anual = round((valor-dplyr::lag(valor, 4))/dplyr::lag(valor, 4), 4),
    var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(valor = valor*100/head(valor,1)) %>%
  dplyr::ungroup()


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> tabela exportável feita
#' Gráfico apenas do número índice do PIB para introdução de uma perspectiva
#' temporal:
grafico_pib_indice <- agregados_indice %>%
  dplyr::filter(setores == 'PIB (preços de mercado)') %>%
  dplyr::select(trimestre, valor) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Trimestre-Ano: ',
                 paste0('0', lubridate::quarter(trimestre),
                        '-',
                        lubridate::year(trimestre)),
                 '<br>Valor:', scales::number(valor,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01))
  )) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = trimestre,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = 'Índice (base: 01-2010 = 100)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_pib_indice <- plotly::ggplotly(grafico_pib_indice, tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Produto Interno Bruto',
                                            '<br>',
                                            '<sup>',
                                            'Índice de Volume com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


#' Gráfico com as variações do número índice, sendo estas as variações marginal
#' e interanual
grafico_var_pib_indice <- agregados_indice %>%
  dplyr::filter(setores == 'PIB (preços de mercado)') %>%
  dplyr::select(trimestre, var_anual, var_mensal) %>%
  tidyr::pivot_longer(cols = !trimestre,
                      names_to = 'ref',
                      values_to = 'valores') %>%
  dplyr::mutate(ref = dplyr::case_when(ref == 'var_anual' ~ 'Interanual',
                                       ref == 'var_mensal' ~ 'Marginal')) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Trimestre-Ano: ',
                 paste0('0', lubridate::quarter(trimestre),
                        '-',
                        lubridate::year(trimestre)),
                 '<br> Valor:', scales::percent(valores,
                                                 big.mark = '.',
                                                 decimal.mark = ',',
                                                 accuracy = 0.01),
                 '<br> Referência:', ref))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valores,
                                            color = ref),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = trimestre,
                                            y = valores,
                                            color = ref),
                     size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = '',
                y = '(%)') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                              decimal.mark = ',')) +
  ggplot2::theme(legend.position = 'bottom')+
  ggplot2::scale_color_manual(breaks = c('Interanual',
                                         'Marginal'),
                              values = c('#FBE625',
                                         '#440154'))


grafico_var_pib_indice <- plotly::ggplotly(grafico_var_pib_indice, tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('Variação do Produto Interno Bruto',
                                            '<br>',
                                            '<sup>',
                                            'Índice de Volume com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


#' Gráfico com a inserção do pib pontecial para comparação com o hiato de
#' produto. Para isso, foi calculado o potencial com o filtro HP:
potencial <- agregados_indice %>%
  dplyr::filter(setores == 'PIB (preços de mercado)') %>%
  dplyr::select(trimestre, valor) %>%
  dplyr::mutate(potencial = mFilter::hpfilter(valor,
                                              freq = as.numeric(nrow(.)),
                                              type = 'lambda') %>%
                  purrr::pluck('trend')) %>%
  dplyr::mutate(hiato = ((valor/potencial)-1))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> tabela exportável feita
grafico_real_potencial <- potencial %>%
  dplyr::select(!hiato) %>%
  tidyr::pivot_longer(-trimestre, names_to = 'ref', values_to = 'valores') %>%
  dplyr::mutate(ref = dplyr::case_when(ref == 'valor' ~ 'PIB Efetivo',
                                       ref == 'potencial' ~ 'PIB Potencial')) %>%
  ggplot2::ggplot(ggplot2::aes(
    group = 1,
    text = paste('Trimestre-Ano: ', paste0('0', lubridate::quarter(trimestre),
                                  '-',
                                  lubridate::year(trimestre)),
                 '<br> Valor:', scales::number(valores,
                                                big.mark = '.',
                                                decimal.mark = ',',
                                                accuracy = 0.01),
                 '<br> Referência:', ref,
                 '<br> ----------',
                 '<br> Técnica: Filtro Hodrick-Prescott'))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valores,
                                            color = ref),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = trimestre,
                                            y = valores,
                                            color = ref),
                     size = 0.75) +
  ggplot2::labs(color = '',
                x = '',
                y = 'Índice (Base: 01-2010 = 100)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ',')) +
  ggplot2::theme(legend.position = 'bottom')+
  ggplot2::scale_color_manual(breaks = c('PIB Efetivo',
                                         'PIB Potencial'),
                              values = c('#FBE625',
                                         '#440154'))


grafico_real_potencial <- plotly::ggplotly(grafico_real_potencial, tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('PIB Efetivo versus PIB Potencial',
                                            '<br>',
                                            '<sup>',
                                            'Séries com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


#' Gráfico com o hiato do produto apenas. Também foi adicionada uma linha vermelha
#' no zero para compreender a relação entre PIB potencial e PIB efetivo
grafico_hiato <- potencial %>%
  dplyr::select(!c(valor, potencial)) %>%
  ggplot2::ggplot(ggplot2::aes(
    group = 1,
    text = paste('Trimestre-Ano: ', paste0('0', lubridate::quarter(trimestre),
                                  '-',
                                  lubridate::year(trimestre)),
                 '<br> Valor:', scales::percent(hiato,
                                                big.mark = '.',
                                                decimal.mark = ',',
                                                accuracy = 0.01),
                 '<br> ----------',
                 '<br> Técnica: Filtro Hodrick-Prescott'))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = hiato),
                     size = 0.75,
                     color = '#440154') +
  ggplot2::geom_point(mapping = ggplot2::aes(x = trimestre,
                                            y = hiato),
                     size = 0.75,
                     color = '#440154') +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',')) +
  ggplot2::theme(legend.position = 'bottom')


grafico_hiato <- plotly::ggplotly(grafico_hiato, tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Hiato do Produto',
                                            '<br>',
                                            '<sup>',
                                            'Diferença Percentual',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


#' Adicionando a tabela de valores correntes para a ótica do produto. Aqui serão
#' agrupados os serviços, indústria e agropecuária.
cor_ref <- '#440154'


pib_vc_setores <- pib_vc %>%
  dplyr::arrange(desc(trimestre)) %>%
  dplyr::filter(setores == 'Indústria' |
                  setores == 'Serviços' |
                  setores == 'Agropecuária')


tabela_pib_setores_valores <- pib_vc_setores %>%
  dplyr::select(!c(ano, acumulado_nominal, valor_nominal)) %>%
  dplyr::mutate(trimestre = paste0('0',
                                   lubridate::quarter(trimestre),
                                   '-',
                                   lubridate::year(trimestre)),
                acumulado_real = scales::dollar(acumulado_real,
                                                big.mark = '.',
                                                prefix = 'R$ ',
                                                decimal.mark = ','),
                valor_real = scales::dollar(valor_real,
                                            big.mark = '.',
                                            prefix = 'R$ ',
                                            decimal.mark = ','),
                var_anual = scales::percent(var_anual,
                                            accuracy = 0.01,
                                            decimal.mark = ','),
                var_trim = scales::percent(var_trim,
                                           accuracy = 0.01,
                                           decimal.mark = ',')) %>%
  reactable::reactable(
    columns = list(
      setores = reactable::colDef(name = 'SETORES ECONÔMICOS'),
      trimestre = reactable::colDef(name = 'TRIMESTRE-ANO',
                                    align = 'center'),
      acumulado_real = reactable::colDef(name = 'ACUMULADO NO ANO (em Milhões)',
                                        align = 'center'),
      valor_real = reactable::colDef(name = 'VALORES REAIS (em Milhões)',
                                     align = 'center'),
      var_anual_icon = reactable::colDef(show = FALSE),
      var_trim_icon = reactable::colDef(show = FALSE),
      var_anual = reactable::colDef(name = 'VARIAÇÃO INTERANUAL (Base: valores reais)',
                                    align = 'center'),
      var_trim = reactable::colDef(name = 'VARIAÇÃO MARGINAL (Base: valores reais)',
                                   align = 'center')
    ),
    defaultColDef = reactable::colDef(headerStyle = list(background = cor_ref,
                                                         color = '#fff')),
    bordered = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    pagination = FALSE,
    showPageInfo = FALSE,
    groupBy = 'setores'
  )


#' Gráfico apenas do número índice do PIB para introdução de uma perspectiva
#' temporal (analisando todos os setores econômicos):
grafico_pib_setores_indice <- agregados_indice %>%
  dplyr::filter(setores == c('Agropecuária', 'Indústria', 'Serviços')) %>%
  dplyr::select(trimestre, valor, setores, var_mensal, var_anual) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Trimestre-Ano: ',
                 paste0('0', lubridate::quarter(trimestre),
                        '-',
                        lubridate::year(trimestre)),
                 '<br>Valor:', scales::number(valor,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01),
                 '<br>Variação Marginal', scales::percent(var_mensal,
                                                          accuracy = 0.01,
                                                          decimal.mark = ','),
                 '<br>Variação Interanual', scales::percent(var_anual,
                                                            accuracy = 0.01,
                                                            decimal.mark = ','))
  )) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valor,
                                            color = setores),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = trimestre,
                                            y = valor,
                                            color = setores),
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = 'Índice (Base: 01-2010 = 100)',
                color = '') +
  ggplot2::scale_color_viridis_d(direction = -1) +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_pib_setores_indice <- plotly::ggplotly(grafico_pib_setores_indice,
                                       tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Produção por Setor Econômico',
                                            '<br>',
                                            '<sup>',
                                            'Índice de Volume com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


#' Gráfico com as variações do número índice, sendo estas as variações marginal
#' e interanual (analisando todos os setores econômicos):
#' DEPRECADO!!!!!!!!!!!!!!!!!!!!!!!!
# grafico_var_pib_setores_indice <- agregados_indice %>%
#   dplyr::filter(setores == c('Agropecuária', 'Indústria', 'Serviços')) %>%
#   dplyr::select(trimestre, var_anual, var_mensal, setores) %>%
#   tidyr::pivot_longer(cols = !c(trimestre, setores),
#                       names_to = 'ref',
#                       values_to = 'valores') %>%
#   dplyr::mutate(ref = dplyr::case_when(ref == 'var_anual' ~ 'Interanual',
#                                        ref == 'var_mensal' ~ 'Marginal')) %>%
#   ggplot2::ggplot(mapping = ggplot2::aes(
#     group = 1,
#     text = paste('Trimestre-Ano: ',
#                  paste0('0', lubridate::quarter(trimestre),
#                         '-',
#                         lubridate::year(trimestre)),
#                  '<br> Índice:', scales::percent(valores,
#                                                  big.mark = '.',
#                                                  decimal.mark = ',',
#                                                  accuracy = 0.01),
#                  '<br> Referência:', ref))) +
#   ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
#                                             y = valores,
#                                             color = ref,
#                                             frame = setores),
#                      size = 0.75) +
#   ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
#   ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
#   ggplot2::labs(color = '',
#                 x = 'Trimestres',
#                 y = 'Variações') +
#   ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = '.',
#                                                               decimal.mark = ',')) +
#   ggplot2::theme(legend.position = 'bottom')+
#   ggplot2::scale_color_manual(breaks = c('Interanual',
#                                          'Marginal'),
#                               values = c('#3C8DBC',
#                                          '#414487FF'))
#
#
# grafico_var_pib_setores_indice <- plotly::ggplotly(grafico_var_pib_setores_indice,
#                                            tooltip = c('text')) %>%
#   plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
#                  title = list(text = paste0('Variação da Produção por Setor Econômico',
#                                             '<br>',
#                                             '<sup>',
#                                             'Índice de Volume com Ajuste Sazonal',
#                                             '<br>')),
#                  margin = list(l = 50, t = 50)) %>%
#   plotly::animation_button(label = 'Trocar') %>%
#   plotly::animation_slider(currentvalue = list(prefix = 'Setor: ',
#                                                font = list(color='black'))) %>%
#   plotly::config(modeBarButtonsToRemove = plotly_layout)


#' Adicionando a tabela de valores correntes para a ótica da demanda. Aqui serão
#' agrupados consumo agregado, investimento agregado, gastos do governo, exportações
#' importações.
cor_ref <- '#440154'


pib_vc_demanda <- pib_vc %>%
  dplyr::arrange(desc(trimestre)) %>%
  dplyr::filter(setores == 'Consumo das <br> Famílias' |
                  setores == 'Formação Bruta de <br> Capital Fixo' |
                  setores == 'Gastos do <br> Governo' |
                  setores == 'Exportações' |
                  setores == 'Importações') %>%
  dplyr::mutate(setores = dplyr::case_when(setores == 'Consumo das <br> Famílias' ~ 'Consumo das Famílias',
                                           setores == 'Formação Bruta de <br> Capital Fixo' ~ 'Formação Bruta de Capital Fixo',
                                           setores == 'Gastos do <br> Governo' ~ 'Gastos do Governo',
                                           TRUE ~ setores))


tabela_pib_demanda_valores <- pib_vc_demanda %>%
  dplyr::select(!c(ano, acumulado_nominal, valor_nominal)) %>%
  dplyr::mutate(trimestre = paste0('0',
                                   lubridate::quarter(trimestre),
                                   '-',
                                   lubridate::year(trimestre)),
                acumulado_real = scales::dollar(acumulado_real,
                                               big.mark = '.',
                                               prefix = 'R$ ',
                                               decimal.mark = ','),
                valor_real = scales::dollar(valor_real,
                                            big.mark = '.',
                                            prefix = 'R$ ',
                                            decimal.mark = ','),
                var_anual = scales::percent(var_anual,
                                            accuracy = 0.01,
                                            decimal.mark = ','),
                var_trim = scales::percent(var_trim,
                                           accuracy = 0.01,
                                           decimal.mark = ',')) %>%
  reactable::reactable(
    columns = list(
      setores = reactable::colDef(name = 'COMPONENTES DA DEMANDA AGREGADA'),
      trimestre = reactable::colDef(name = 'TRIMESTRE-ANO',
                                    align = 'center'),
      acumulado_real = reactable::colDef(name = 'ACUMULADO NO ANO (em Milhões)',
                                        align = 'center'),
      valor_real = reactable::colDef(name = 'VALORES REAIS (em Milhões)',
                                     align = 'center'),
      var_anual_icon = reactable::colDef(show = FALSE),
      var_trim_icon = reactable::colDef(show = FALSE),
      var_anual = reactable::colDef(name = 'VARIAÇÃO INTERANUAL (Base: valores reais)',
                                    align = 'center'),
      var_trim = reactable::colDef(name = 'VARIAÇÃO MARGINAL (Base: valores reais)',
                                   align = 'center')
    ),
    defaultColDef = reactable::colDef(headerStyle = list(background = cor_ref,
                                                         color = '#fff')),
    bordered = TRUE,
    highlight = TRUE,
    resizable = TRUE,
    pagination = FALSE,
    showPageInfo = FALSE,
    groupBy = 'setores'
  )


#' Gráfico apenas do número índice do PIB para introdução de uma perspectiva
#' temporal (analisando todos os setores institucionais):
grafico_pib_demanda_indice <- agregados_indice %>%
  dplyr::filter(setores == 'Consumo das <br> Famílias' |
                  setores == 'Formação Bruta de <br> Capital Fixo' |
                  setores == 'Gastos do <br> Governo' |
                  setores == 'Exportações' |
                  setores == 'Importações') %>%
  dplyr::select(trimestre, valor, setores, var_mensal, var_anual) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Trimestre-Ano: ',
                 paste0('0', lubridate::quarter(trimestre),
                        '-',
                        lubridate::year(trimestre)),
                 '<br>Valor:', scales::number(valor,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01),
                 '<br> Variação Marginal:', scales::percent(var_mensal,
                                                            accuracy = 0.01,
                                                            decimal.mark = ','),
                 '<br> Variação Interanual:', scales::percent(var_anual,
                                                              accuracy = 0.01,
                                                              decimal.mark = ','))
  )) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valor,
                                            color = setores),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = trimestre,
                                            y = valor,
                                            color = setores),
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = 'Índice (Base: 01-2010 = 100)',
                color = '') +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_pib_demanda_indice <- plotly::ggplotly(grafico_pib_demanda_indice,
                                               tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Componentes da Demanda Agregada',
                                            '<br>',
                                            '<sup>',
                                            'Índice de Volume com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação dos dados da Pesquisa Industrial Mensal ---------------------------
# pim_sa_indice_api <-
# sidrar::get_sidra(
# api = '/t/3651/n1/all/v/3134/p/all/c543/129278,129283,129300/d/v3134%201'
# )
#
# pim_sa_indice <- pim_sa_indice_api %>%
#   janitor::clean_names() %>%
#   dplyr::select(valor, mes_codigo, grandes_categorias_economicas) %>%
#   dplyr::rename('valor' = valor,
#                 'data' = mes_codigo,
#                 'setores' = grandes_categorias_economicas) %>%
#   dplyr::mutate(setores = dplyr::case_when(setores == '1 Bens de capital' ~ 'Bens de Capital',
#                                            setores == '2 Bens intermediários' ~ 'Bens Intermediários',
#                                            setores == '3 Bens de consumo' ~ 'Bens de Consumo'),
#                 data = lubridate::ym(data)) %>%
#   dplyr::group_by(setores) %>%
#   dplyr::mutate(
#     var_anual = round((valor-dplyr::lag(valor, 12))/dplyr::lag(valor, 12), 4),
#     var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
#   tidyr::drop_na() %>%
#   dplyr::filter(data >= lubridate::ymd('2010-01-01')) %>%
#   dplyr::mutate(valor = valor*100/head(valor,1)) %>%
#   dplyr::ungroup()


pim_sa_indice_api <-
  sidrar::get_sidra(
    api = '/t/8158/n1/all/v/11600/p/all/c543/129278,129283,129300/d/v11600%205'
  )

pim_sa_indice <- pim_sa_indice_api %>%
  janitor::clean_names() %>%
  dplyr::select(valor, mes_codigo, grandes_categorias_economicas) %>%
  dplyr::rename('valor' = valor,
                'data' = mes_codigo,
                'setores' = grandes_categorias_economicas) %>%
  dplyr::mutate(setores = dplyr::case_when(setores == '1 Bens de capital' ~ 'Bens de Capital',
                                           setores == '2 Bens intermediários' ~ 'Bens Intermediários',
                                           setores == '3 Bens de consumo' ~ 'Bens de Consumo'),
                data = lubridate::ym(data)) %>%
  dplyr::group_by(setores) %>%
  dplyr::mutate(
    var_anual = round((valor-dplyr::lag(valor, 12))/dplyr::lag(valor, 12), 4),
    var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na() %>%
  dplyr::filter(data >= lubridate::ymd('2010-01-01')) %>%
  dplyr::mutate(valor = valor*100/head(valor,1)) %>%
  dplyr::ungroup()

# Gráfico da Pesquisa Industrial Mensal:
grafico_pim_sa_indice <- pim_sa_indice %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::number(valor,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01),
                 '<br>Variação Marginal:', scales::percent(var_mensal,
                                                           big.mark = '.',
                                                           decimal.mark = ',',
                                                           accuracy = 0.01),
                 '<br>Variação Interanual:', scales::percent(var_anual,
                                                             big.mark = '.',
                                                             decimal.mark = ',',
                                                             accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor,
                                            color = setores),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor,
                                             color = setores),
                      size = 0.75) +
  ggplot2::labs(x = '',
                y = 'Índice (Base: 2010 = 100)',
                color = '') +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ',',
                                                             big.mark = '.'))


grafico_pim_sa_indice <- plotly::ggplotly(grafico_pim_sa_indice,
                                          tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Pesquisa Industrial Mensal',
                                            '<br>',
                                            '<sup>',
                                            'Índice de Volume com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)



# Importação dos dados da Pesquisa Mensal do Comércio --------------------------
# pmc_sa_indice_api <-
# sidrar::get_sidra(
# api = '/t/3417/n1/all/v/all/p/all/c11046/40312/d/v1186%201,v1190%201'
# )


pmc_sa_indice_api <-
sidrar::get_sidra(
api = '/t/8186/n1/all/v/11707/p/all/c11046/all/d/v11707%205'
)

# pmc_sa_indice <- pmc_sa_indice_api %>%
#   janitor::clean_names() %>%
#   dplyr::select(valor, mes_codigo, variavel) %>%
#   dplyr::rename('valor' = valor,
#                 'data' = mes_codigo,
#                 'tipo' = variavel) %>%
#   dplyr::mutate(tipo = dplyr::case_when(tipo == 'Índice de volume de vendas no comércio varejista ampliado' ~ 'Volume de Vendas',
#                                         tipo == 'Índice de receita nominal de vendas no comércio varejista ampliado' ~ 'Receita Nominal'),
#                 data = lubridate::ym(data)) %>%
#   dplyr::group_by(tipo) %>%
#   dplyr::mutate(
#     var_anual = round((valor-dplyr::lag(valor, 12))/dplyr::lag(valor, 12), 4),
#     var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
#   tidyr::drop_na() %>%
#   dplyr::filter(data >= lubridate::ymd('2010-01-01')) %>%
#   dplyr::mutate(valor = valor*100/head(valor,1)) %>%
#   dplyr::ungroup()


pmc_sa_indice <- pmc_sa_indice_api %>%
  janitor::clean_names() %>%
  dplyr::select(valor, mes_codigo, tipos_de_indice) %>%
  dplyr::rename('valor' = valor,
                'data' = mes_codigo,
                'tipo' = tipos_de_indice) %>%
  dplyr::mutate(tipo = dplyr::case_when(tipo == 'Índice de volume de vendas no comércio varejista ampliado' ~ 'Volume de Vendas',
                                        tipo == 'Índice de receita nominal de vendas no comércio varejista ampliado' ~ 'Receita Nominal'),
                data = lubridate::ym(data)) %>%
  dplyr::group_by(tipo) %>%
  dplyr::mutate(
    var_anual = round((valor-dplyr::lag(valor, 12))/dplyr::lag(valor, 12), 4),
    var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na() %>%
  dplyr::filter(data >= lubridate::ymd('2010-01-01')) %>%
  dplyr::mutate(valor = valor*100/head(valor,1)) %>%
  dplyr::ungroup()


# Gráfico da Pesquisa Mensal do Comércio:
grafico_pmc_sa_indice <- pmc_sa_indice %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::number(valor,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01),
                 '<br>Variação Marginal:', scales::percent(var_mensal,
                                                           big.mark = '.',
                                                           decimal.mark = ',',
                                                           accuracy = 0.01),
                 '<br>Variação Interanual:', scales::percent(var_anual,
                                                             big.mark = '.',
                                                             decimal.mark = ',',
                                                             accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor,
                                            color = tipo),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor,
                                             color = tipo),
                      size = 0.75) +
  ggplot2::labs(x = '',
                y = 'Índice (Base: 2010 = 100)',
                color = '') +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ',',
                                                             big.mark = '.'))


grafico_pmc_sa_indice <- plotly::ggplotly(grafico_pmc_sa_indice,
                                          tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Pesquisa Mensal de Comércio',
                                            '<br>',
                                            '<sup>',
                                            'Índice com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação dos dados da Pesquisa Mensal de Serviços --------------------------
# pms_sa_indice_api <-
# sidrar::get_sidra(
# api = '/t/6442/n1/all/v/all/p/all/c11046/40312/d/v8676%201,v8677%201'
# )


pms_sa_indice_api <-
  sidrar::get_sidra(
    api = '/t/8161/n1/all/v/11622/p/all/c11046/all/d/v11622%205'
  )


# pms_sa_indice <- pms_sa_indice_api %>%
#   janitor::clean_names() %>%
#   dplyr::select(valor, mes_codigo, variavel) %>%
#   dplyr::rename('valor' = valor,
#                 'data' = mes_codigo,
#                 'tipo' = variavel) %>%
#   dplyr::mutate(tipo = dplyr::case_when(tipo == 'Índice de volume de serviços' ~ 'Volume de Serviços',
#                                         tipo == 'Índice de receita nominal de serviços' ~ 'Receita Nominal'),
#                 data = lubridate::ym(data)) %>%
#   dplyr::group_by(tipo) %>%
#   dplyr::mutate(
#     var_anual = round((valor-dplyr::lag(valor, 12))/dplyr::lag(valor, 12), 4),
#     var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
#   tidyr::drop_na() %>%
#   dplyr::filter(data >= lubridate::ymd('2010-01-01')) %>%
#   dplyr::mutate(valor = valor*100/head(valor,1)) %>%
#   dplyr::ungroup()

pms_sa_indice <- pms_sa_indice_api %>%
  janitor::clean_names() %>%
  dplyr::select(valor, mes_codigo, tipos_de_indice) %>%
  dplyr::rename('valor' = valor,
                'data' = mes_codigo,
                'tipo' = tipos_de_indice) %>%
  dplyr::mutate(tipo = dplyr::case_when(tipo == 'Índice de volume de serviços' ~ 'Volume de Serviços',
                                        tipo == 'Índice de receita nominal de serviços' ~ 'Receita Nominal'),
                data = lubridate::ym(data)) %>%
  dplyr::group_by(tipo) %>%
  dplyr::mutate(
    var_anual = round((valor-dplyr::lag(valor, 12))/dplyr::lag(valor, 12), 4),
    var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na() %>%
  dplyr::filter(data >= lubridate::ymd('2010-01-01')) %>%
  dplyr::mutate(valor = valor*100/head(valor,1)) %>%
  dplyr::ungroup()


# Gráfico da Pesquisa Mensal de Serviços:
grafico_pms_sa_indice <- pms_sa_indice %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::number(valor,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01),
                 '<br>Variação Marginal:', scales::percent(var_mensal,
                                                           big.mark = '.',
                                                           decimal.mark = ',',
                                                           accuracy = 0.01),
                 '<br>Variação Interanual:', scales::percent(var_anual,
                                                             big.mark = '.',
                                                             decimal.mark = ',',
                                                             accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor,
                                            color = tipo),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor,
                                             color = tipo),
                      size = 0.75) +
  ggplot2::labs(x = '',
                y = 'Índice (Base: 2010 = 100)',
                color = '') +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ',',
                                                             big.mark = '.'))


grafico_pms_sa_indice <- plotly::ggplotly(grafico_pms_sa_indice,
                                          tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Pesquisa Mensal de Serviços',
                                            '<br>',
                                            '<sup>',
                                            'Índice com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)



# Importação dos dados do Índice do Banco Central ------------------------------
ibc_br_api <- httr::GET('http://api.bcb.gov.br/dados/serie/bcdata.sgs.24364/dados?formato=json&dataInicial=01/01/2001') %>%
  httr::content(simplifyDataFrame =  TRUE) %>%
  dplyr::rename('data' = data,
                'valor' = valor) %>%
  dplyr::mutate(data = lubridate::dmy(data),
                valor = as.numeric(valor))


ibc_br <- ibc_br_api %>%
  dplyr::mutate(
    var_anual = round((valor-dplyr::lag(valor, 12))/dplyr::lag(valor, 12), 4),
    var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na() %>%
  dplyr::filter(data >= lubridate::ymd('2010-01-01')) %>%
  dplyr::mutate(valor = valor*100/head(valor,1))


# Gráfico do IBC-br:
grafico_ibc_br <- ibc_br %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::number(valor,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01),
                 '<br>Variação Marginal:', scales::percent(var_mensal,
                                                           big.mark = '.',
                                                           decimal.mark = ',',
                                                           accuracy = 0.01),
                 '<br>Variação Interanual:', scales::percent(var_anual,
                                                             big.mark = '.',
                                                             decimal.mark = ',',
                                                             accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor),
                      color = '#440154',
                      size = 0.75) +
  ggplot2::labs(x = '',
                y = 'Índice (Base: 2010 = 100)',
                color = '') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ',',
                                                             big.mark = '.'))


grafico_ibc_br <- plotly::ggplotly(grafico_ibc_br,
                                   tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IBC-BR',
                                            '<br>',
                                            '<sup>',
                                            'Índice de Volume com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)



#' #' Gráfico com as variações do número índice, sendo estas as variações marginal
#' #' e interanual (analisando todos os setores econômicos):
#' #' DEPRECADO!!!!!!!!!!!!!!!!!!!!!!!!
#' grafico_var_demanda_indice <- agregados_indice %>%
#'   dplyr::filter(setores == 'Consumo das <br> Famílias' |
#'                   setores == 'Formação Bruta de <br> Capital Fixo' |
#'                   setores == 'Gastos do <br> Governo' |
#'                   setores == 'Exportações' |
#'                   setores == 'Importações') %>%
#'   dplyr::select(trimestre, var_anual, var_mensal, setores) %>%
#'   tidyr::pivot_longer(cols = !c(trimestre, setores),
#'                       names_to = 'ref',
#'                       values_to = 'valores') %>%
#'   dplyr::mutate(ref = dplyr::case_when(ref == 'var_anual' ~ 'Interanual',
#'                                        ref == 'var_mensal' ~ 'Marginal')) %>%
#'   ggplot2::ggplot(mapping = ggplot2::aes(
#'     group = 1,
#'     text = paste('Trimestre-Ano: ',
#'                  paste0('0', lubridate::quarter(trimestre),
#'                         '-',
#'                         lubridate::year(trimestre)),
#'                  '<br> Índice:', scales::percent(valores,
#'                                                  big.mark = '.',
#'                                                  decimal.mark = ',',
#'                                                  accuracy = 0.01),
#'                  '<br> Referência:', ref))) +
#'   ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
#'                                             y = valores,
#'                                             color = ref,
#'                                             frame = setores),
#'                      size = 0.75) +
#'   ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
#'   ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
#'   ggplot2::labs(color = '',
#'                 x = 'Trimestres',
#'                 y = 'Variações') +
#'   ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = '.',
#'                                                               decimal.mark = ',')) +
#'   ggplot2::theme(legend.position = 'bottom')+
#'   ggplot2::scale_color_manual(breaks = c('Interanual',
#'                                          'Marginal'),
#'                               values = c('#3C8DBC',
#'                                          '#414487FF'))
#'
#'
#' grafico_var_demanda_indice <- plotly::ggplotly(grafico_var_demanda_indice,
#'                                                tooltip = c('text')) %>%
#'   plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
#'                  title = list(text = paste0('Variação da Produção por Setor Econômico',
#'                                             '<br>',
#'                                             '<sup>',
#'                                             'Índice de Volume com Ajuste Sazonal',
#'                                             '<br>')),
#'                  margin = list(l = 50, t = 50)) %>%
#'   plotly::animation_button(label = 'Trocar') %>%
#'   plotly::animation_slider(currentvalue = list(prefix = 'Setor: ',
#'                                                font = list(color='black'))) %>%
#'   plotly::config(modeBarButtonsToRemove = plotly_layout)


################################################################################
################################################################################
# TAXA DE CÂMBIO


# Importação de Taxa de Câmbio Nominal Real/Dólar: ------------------------
cambio_nominal_dolar_d <- BETS::BETSget(10813)

tx_cambio_nominal_dolar_mm <- cambio_nominal_dolar_d %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>%
  dplyr::mutate(ano_mes = paste0(lubridate::year(date),
                                 lubridate::month(date))) %>%
  dplyr::filter(date >= lubridate::ymd('2009-12-01')) %>%
  dplyr::group_by(ano_mes) %>%
  dplyr::summarise(valor = mean(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ano_mes = lubridate::ym(ano_mes)) %>%
  dplyr::mutate(var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na()


# Gráfico do Cambio Nominal (Real/Dólar): ---------------------------------
grafico_nominal_dolar_mm <- tx_cambio_nominal_dolar_mm %>%
  dplyr::select(ano_mes, valor) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
  group = 1,
  text = paste('Mês-Ano: ',
               paste0(lubridate::month(ano_mes),
                      '-',
                      lubridate::year(ano_mes)),
               '<br>Taxa de Câmbio (média mensal):', scales::number(valor,
                                                     big.mark = '.',
                                                     decimal.mark = ',',
                                                     accuracy = 0.01), 'R$/US$',
               '<br>Taxa de Câmbio (último valor):', scales::number(dplyr::last(cambio_nominal_dolar_d$value),
                                                                     big.mark = '.',
                                                                     decimal.mark = ',',
                                                                     accuracy = 0.01), 'R$/US$'))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = '(R$/US$)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_nominal_dolar_mm <- plotly::ggplotly(grafico_nominal_dolar_mm,
                                             tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Taxa de Câmbio Nominal',
                                            '<br>',
                                            '<sup>',
                                            'Média Mensal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Variação Mensal da Taxa de Câmbio Nominal (Real/Dólar): -----------------
grafico_var_nominal_dolar_mm <- tx_cambio_nominal_dolar_mm %>%
  dplyr::select(ano_mes, var_mensal) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:',
                 scales::percent(var_mensal,
                                 big.mark = '.',
                                 decimal.mark = ',',
                                 accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = var_mensal),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = var_mensal),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ','))


grafico_var_nominal_dolar_mm <- plotly::ggplotly(grafico_var_nominal_dolar_mm,
                                             tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Taxa de Câmbio Nominal (R$/US$)',
                                            '<br>',
                                            '<sup>',
                                            'Variação da Média Mensal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação de Taxa de Câmbio Nominal Real/Euro: -------------------------
cambio_nominal_euro_d <- BETS::BETSget(21620)

tx_cambio_nominal_euro_mm <- cambio_nominal_euro_d %>%
  dplyr::mutate(date = lubridate::ymd(date)) %>%
  dplyr::mutate(ano_mes = paste0(lubridate::year(date),
                                 lubridate::month(date))) %>%
  dplyr::filter(date >= lubridate::ymd('2010-01-01')) %>%
  dplyr::group_by(ano_mes) %>%
  dplyr::summarise(valor = mean(value)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(ano_mes = lubridate::ym(ano_mes)) %>%
  dplyr::mutate(var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na()


# Gráfico do Cambio Nominal (Real/Euro): ----------------------------------
grafico_nominal_euro_mm <- tx_cambio_nominal_euro_mm %>%
  dplyr::select(ano_mes, valor) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Taxa de Câmbio (média mensal):', scales::number(valor,
                                                                      big.mark = '.',
                                                                      decimal.mark = ',',
                                                                      accuracy = 0.01), 'R$/€',
                 '<br> Taxa de Câmbio (último valor):', scales::number(dplyr::last(cambio_nominal_euro_d$value),
                                                                       big.mark = '.',
                                                                       decimal.mark = ',',
                                                                       accuracy = 0.01), 'R$/€'))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = '(R$/€)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_nominal_euro_mm <- plotly::ggplotly(grafico_nominal_euro_mm,
                                             tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Taxa de Câmbio Nominal',
                                            '<br>',
                                            '<sup>',
                                            'Média Mensal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Variação Mensal da Taxa de Câmbio Nominal (Real/Euro): ------------------
grafico_var_nominal_euro_mm <- tx_cambio_nominal_euro_mm %>%
  dplyr::select(ano_mes, var_mensal) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:',
                 scales::percent(var_mensal,
                                 big.mark = '.',
                                 decimal.mark = ',',
                                 accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = var_mensal),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = var_mensal),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ','))


grafico_var_nominal_euro_mm <- plotly::ggplotly(grafico_var_nominal_euro_mm,
                                                 tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Taxa de Câmbio Nominal (R$/€)',
                                            '<br>',
                                            '<sup>',
                                            'Variação da Média Mensal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação de Taxa de Câmbio Real (Real/Dólar): -------------------------
cambio_real_dolar_m <- BETS::BETSget(11758)

tx_cambio_real_dolar_m <- cambio_real_dolar_m %>%
  dplyr::mutate(ano_mes = lubridate::ymd(date),
                valor = value) %>%
  dplyr::select(!c(value, date)) %>%
  dplyr::filter(ano_mes >= lubridate::ymd('2009-12-01')) %>%
  dplyr::mutate(var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(valor = round(valor*100/57.13,2))


# Gráfico do Cambio Real (Real/Dólar): ------------------------------------
grafico_real_dolar_m <- tx_cambio_real_dolar_m %>%
  dplyr::select(ano_mes, valor) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:', scales::number(valor,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = 'Índice (IPA-DI | 2010 = 100)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_real_dolar_m <- plotly::ggplotly(grafico_real_dolar_m,
                                         tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Índice de Taxa de Câmbio Real',
                                            '<br>',
                                            '<sup>',
                                            'Dólar Americano',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Variação Mensal da Taxa de Câmbio Real (Real/Dólar): --------------------
grafico_var_real_dolar_m <- tx_cambio_real_dolar_m %>%
  dplyr::select(ano_mes, var_mensal) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:',
                 scales::percent(var_mensal,
                                 big.mark = '.',
                                 decimal.mark = ',',
                                 accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = var_mensal),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = var_mensal),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ','))


grafico_var_real_dolar_m <- plotly::ggplotly(grafico_var_real_dolar_m,
                                                 tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Índice de Taxa de Câmbio Real',
                                            '<br>',
                                            '<sup>',
                                            'Variação Mensal (Dólar Americano)',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação de Taxa de Câmbio Real (Real/Euro): --------------------------
cambio_real_euro_m <- BETS::BETSget(11772)

tx_cambio_real_euro_m <- cambio_real_euro_m %>%
  dplyr::mutate(ano_mes = lubridate::ymd(date),
                valor = value) %>%
  dplyr::select(!c(value, date)) %>%
  dplyr::filter(ano_mes >= lubridate::ymd('2009-12-01')) %>%
  dplyr::mutate(var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(valor = round(valor*100/110.15,2))


# Gráfico do Cambio Real (Real/Euro): -------------------------------------
grafico_real_euro_m <- tx_cambio_real_euro_m %>%
  dplyr::select(ano_mes, valor) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:', scales::number(valor,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = 'Índice (IPA-DI | 2010 = 100)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_real_euro_m <- plotly::ggplotly(grafico_real_euro_m,
                                         tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Índice de Taxa de Câmbio Real',
                                            '<br>',
                                            '<sup>',
                                            'Euro',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Variação Mensal da Taxa de Câmbio Real (Real/Euro): ---------------------
grafico_var_real_euro_m <- tx_cambio_real_euro_m %>%
  dplyr::select(ano_mes, var_mensal) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:',
                 scales::percent(var_mensal,
                                 big.mark = '.',
                                 decimal.mark = ',',
                                 accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = var_mensal),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = var_mensal),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ','))


grafico_var_real_euro_m <- plotly::ggplotly(grafico_var_real_euro_m,
                                             tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Índice de Taxa de Câmbio Real',
                                            '<br>',
                                            '<sup>',
                                            'Variação Mensal (Euro)',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação do Saldo em Transações Correntes (Milhões de U$): ------------
saldo_transacoes_correntes <- BETS::BETSget(22701)

saldo_tc <- saldo_transacoes_correntes %>%
  dplyr::mutate(ano_mes = lubridate::ymd(date),
                valor = value/1000) %>%
  dplyr::select(!c(value, date)) %>%
  dplyr::filter(ano_mes >= lubridate::ymd('2009-12-01')) %>%
  dplyr::mutate(var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na()


# Gráfico do Saldo em Transações Correntes (em Bilhões): ------------------
grafico_saldo_tc <- saldo_tc %>%
  dplyr::select(ano_mes, valor) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:', scales::number(valor,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01), '(US$ Bilhões)'))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = 'US$ (Bilhões)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_saldo_tc <- plotly::ggplotly(grafico_saldo_tc,
                                     tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Transações Correntes',
                                            '<br>',
                                            '<sup>',
                                            '(Saldo)',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação Conta Financeira Líquida (Milhões de U$): ------------
saldo_conta_financeira <- BETS::BETSget(22863)

saldo_cf <- saldo_conta_financeira %>%
  dplyr::mutate(ano_mes = lubridate::ymd(date),
                valor = value/1000) %>%
  dplyr::select(!c(value, date)) %>%
  dplyr::filter(ano_mes >= lubridate::ymd('2009-12-01')) %>%
  dplyr::mutate(var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na()


# Gráfico na Conta Financeira (em Bilhões): ------------------
grafico_saldo_cf <- saldo_cf %>%
  dplyr::select(ano_mes, valor) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:', scales::number(valor,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01), '(US$ Bilhões)'))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = 'US$ (Bilhões)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_saldo_cf <- plotly::ggplotly(grafico_saldo_cf,
                                     tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Conta Financeira',
                                            '<br>',
                                            '<sup>',
                                            '(Líquida)',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação Reservas Internacionais (Milhões de U$): ------------
reservas_internacionais <- BETS::BETSget(3546)

dados_reservas_internacionais <- reservas_internacionais %>%
  dplyr::mutate(ano_mes = lubridate::ymd(date),
                valor = value/1000) %>%
  dplyr::select(!c(value, date)) %>%
  dplyr::filter(ano_mes >= lubridate::ymd('2009-12-01')) %>%
  dplyr::mutate(var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na()


# Gráfico Reservas Internacionais (em Bilhões): ------------------
grafico_reservas_internacionais <- dados_reservas_internacionais %>%
  dplyr::select(ano_mes, valor) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:', scales::number(valor,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01), '(US$ Bilhões)'))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = 'US$ (Bilhões)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_reservas_internacionais <- plotly::ggplotly(grafico_reservas_internacionais,
                                     tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Reservas Internacionais',
                                            '<br>',
                                            '<sup>',
                                            '(Estoque Total)',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)




# Transações Correntes acumulado em 12 meses em relação ao PIB - mensal : ------------
tc_pib <- BETS::BETSget(23079)

dados_tc_pib <- tc_pib %>%
  dplyr::mutate(ano_mes = lubridate::ymd(date),
                valor = value/1000) %>%
  dplyr::select(!c(value, date)) %>%
  dplyr::filter(ano_mes >= lubridate::ymd('2009-12-01')) %>%
  dplyr::mutate(var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na()


# Gráfico Transações Correntes acumulado em 12 meses em relação ao PIB - mensal: ------------------
grafico_tc_pib <- dados_tc_pib %>%
  dplyr::select(ano_mes, valor) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:', scales::percent(-valor,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = -valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = -valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = '(%)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ','))


grafico_tc_pib <- plotly::ggplotly(grafico_tc_pib,
                                   tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Déficit em Transações Correntes em Relação ao PIB',
                                            '<br>',
                                            '<sup>',
                                            '(Acumulado em 12 meses)',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Investimento Direto no País acumulado em 12 meses em relação ao PIB - mensal  ------------
id_pib <- BETS::BETSget(23080)

dados_id_pib <- id_pib %>%
  dplyr::mutate(ano_mes = lubridate::ymd(date),
                valor = value/1000) %>%
  dplyr::select(!c(value, date)) %>%
  dplyr::filter(ano_mes >= lubridate::ymd('2009-12-01')) %>%
  dplyr::mutate(var_mensal = round((valor-dplyr::lag(valor, 1))/dplyr::lag(valor, 1), 4)) %>%
  tidyr::drop_na()


# Gráfico Investimento Direto no País acumulado em 12 meses em relação ao PIB - mensal ------------------
grafico_id_pib <- dados_id_pib %>%
  dplyr::select(ano_mes, valor) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(ano_mes),
                        '-',
                        lubridate::year(ano_mes)),
                 '<br>Valor:', scales::percent(valor,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = ano_mes,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::labs(x = '',
                y = '(%)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ','))


grafico_id_pib <- plotly::ggplotly(grafico_id_pib,
                                   tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Investimento Direto em relação ao PIB',
                                            '<br>',
                                            '<sup>',
                                            '(Acumulado em 12 meses)',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


################################################################################
################################################################################
# TAXA DE INFLAÇÃO


# Importação do IPCA (Índices e Variações Cheias)
ipca_cheio_api <-
sidrar::get_sidra(
api = '/t/1737/n1/all/v/63,69,2265,2266/p/all/d/v63%202,v69%202,v2265%202,v2266%2013'
)

ipca_cheio <- ipca_cheio_api %>%
  janitor::clean_names() %>%
  dplyr::select(variavel, mes_codigo, valor) %>%
  dplyr::mutate(mes_codigo = lubridate::ym(mes_codigo)) %>%
  dplyr::group_by(variavel) %>%
  dplyr::filter(mes_codigo >= lubridate::ymd('2010-01-01')) %>%
  dplyr::ungroup()


#' Gráfico do Número Índice do IPCA a partir de 2010 (a base foi alterada a partir
#' de 2010 para manter o padrão com o PIB):
grafico_ipca_indice <- ipca_cheio %>%
  dplyr::filter(variavel == 'IPCA - Número-índice (base: dezembro de 1993 = 100)') %>%
  dplyr::mutate(valor = round((valor*100)/dplyr::first(valor),2)) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(mes_codigo),
                        '-',
                        lubridate::year(mes_codigo)),
                 '<br>Valor:', scales::number(valor,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = mes_codigo,
                                            y = valor),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = mes_codigo,
                                             y = valor),
                      color = '#440154',
                      size = 0.75) +
  ggplot2::labs(x = '',
                y = 'Índice (base: 01-2010 = 100)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_ipca_indice <- plotly::ggplotly(grafico_ipca_indice,
                                        tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IPCA',
                                            '<br>',
                                            '<sup>',
                                            'Número Índice',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Gráfico da Variação Mensal do IPCA a partir de 2010:
grafico_ipca_var_mensal <- ipca_cheio %>%
  dplyr::filter(variavel == 'IPCA - Variação mensal') %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(mes_codigo),
                        '-',
                        lubridate::year(mes_codigo)),
                 '<br>Valor:', scales::percent(valor/100,
                                              big.mark = '.',
                                              decimal.mark = ',',
                                              accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = mes_codigo,
                                            y = valor/100),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = mes_codigo,
                                             y = valor/100),
                      color = '#440154',
                      size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                              big.mark = '.'))


grafico_ipca_var_mensal <- plotly::ggplotly(grafico_ipca_var_mensal,
                                            tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IPCA',
                                            '<br>',
                                            '<sup>',
                                            'Variação Mensal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# # Gráfico da variação acumulada em 12 meses do IPCA a partir de 2010:
# # DEPRECADO!!!!!!!!!!!!!!!!!!!
# grafico_ipca_var_acum_12 <- ipca_cheio %>%
#   dplyr::filter(variavel == 'IPCA - Variação acumulada em 12 meses') %>%
#   ggplot2::ggplot(mapping = ggplot2::aes(
#     group = 1,
#     text = paste('Mês-Ano: ',
#                  paste0(lubridate::month(mes_codigo),
#                         '-',
#                         lubridate::year(mes_codigo)),
#                  '<br>Valor:', scales::percent(valor/100,
#                                                big.mark = '.',
#                                                decimal.mark = ',',
#                                                accuracy = 0.01)))) +
#   ggplot2::geom_line(mapping = ggplot2::aes(x = mes_codigo,
#                                             y = valor/100),
#                      color = '#3C8DBC',
#                      size = 0.75) +
#   ggplot2::geom_point(mapping = ggplot2::aes(x = mes_codigo,
#                                              y = valor/100),
#                       color = '#3C8DBC',
#                       size = 0.75) +
#   ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
#   ggplot2::labs(x = 'Meses',
#                 y = '(%) <sup>') +
#   ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
#   ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
#                                                               big.mark = '.'))
#
#
# grafico_ipca_var_acum_12 <- plotly::ggplotly(grafico_ipca_var_acum_12,
#                                             tooltip = c('text')) %>%
#   plotly::layout(title = list(text = paste0('IPCA',
#                                             '<br>',
#                                             '<sup>',
#                                             'Variação Acumulada em 12 Meses',
#                                             '<br>')),
#                  margin = list(l = 50, t = 50)) %>%
#   plotly::config(modeBarButtonsToRemove = plotly_layout)


# Gráfico da variação acumulada no ano do IPCA a partir de 2010:
grafico_ipca_var_acum_ano <- ipca_cheio %>%
  dplyr::filter(variavel == 'IPCA - Variação acumulada no ano') %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(mes_codigo),
                        '-',
                        lubridate::year(mes_codigo)),
                 '<br>Valor:', scales::percent(valor/100,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = mes_codigo,
                                            y = valor/100),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = mes_codigo,
                                             y = valor/100),
                      color = '#440154',
                      size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%) <sup>') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                              big.mark = '.'))


grafico_ipca_var_acum_ano <- plotly::ggplotly(grafico_ipca_var_acum_ano,
                                             tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IPCA',
                                            '<br>',
                                            '<sup>',
                                            'Variação Acumulada no Ano',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação do IPCA (Índices e Variações por Gruposs)
ipca_grupo_peso_api <-
sidrar::get_sidra(
api = '/t/7060/n1/all/v/66/p/last%201/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v66%204'
)


ipca_grupo_peso <- ipca_grupo_peso_api %>%
  janitor::clean_names() %>%
  dplyr::select(variavel, mes_codigo,
                geral_grupo_subgrupo_item_e_subitem, valor) %>%
  dplyr::rename('grupo' = geral_grupo_subgrupo_item_e_subitem) %>%
  dplyr::mutate(mes_codigo = lubridate::ym(mes_codigo)) %>%
  dplyr::mutate(grupo = dplyr::case_when(
    grupo == '1.Alimentação e bebidas' ~ 'Alimentação e Bebidas',
    grupo == '2.Habitação' ~ 'Habitação',
    grupo == '3.Artigos de residência' ~ 'Artigos de Residência',
    grupo == '4.Vestuário' ~ 'Vestuário',
    grupo == '5.Transportes' ~ 'Transportes',
    grupo == '6.Saúde e cuidados pessoais' ~ 'Saúde e Cuidados Pessoais',
    grupo == '7.Despesas pessoais' ~ 'Despesas Pessoais',
    grupo == '8.Educação' ~ 'Educação',
    grupo == '9.Comunicação' ~ 'Comunicação'
  ))


# Contribuição de cada Grupo (no último mês):
grafico_ipca_peso_grupo <- ipca_grupo_peso %>%
  dplyr::mutate(grupo = forcats::fct_reorder(grupo, valor)) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(mes_codigo),
                        '-',
                        lubridate::year(mes_codigo)),
                 '<br>Valor:', scales::percent(valor/100,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01)))) +
  ggplot2::geom_col(mapping = ggplot2::aes(x = valor/100,
                                           y = grupo),
                    fill = '#440154',
                    color = '#440154',
                    size = 0.75) +
  ggplot2::labs(x = '(%) <br>',
                y = '',
                color = '') +
  ggplot2::scale_x_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                              big.mark = '.'))


grafico_ipca_peso_grupo <- plotly::ggplotly(grafico_ipca_peso_grupo,
                                            tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IPCA',
                                            '<br>',
                                            '<sup>',
                                            'Peso Mensal por Grupos',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


#Importação das variações mensais e cálculo da variação acumulada no ano:
var_ipca_aliment <- BETS::BETSget(code = 1635,
                                  data.frame = TRUE,
                                  from = '2010-01-01') %>%
  dplyr::mutate(Grupo = 'Alimentos e Bebidas') %>%
  dplyr::rename('Data' = date,
                'Valor' = value)


var_ipca_habit <- BETS::BETSget(code = 1636,
                                data.frame = TRUE,
                                from = '2010-01-01') %>%
  dplyr::mutate(Grupo = 'Habitação') %>%
  dplyr::rename('Data' = date,
                'Valor' = value)


var_ipca_art_res <- BETS::BETSget(code = 1637,
                                  data.frame = TRUE,
                                  from = '2010-01-01') %>%
  dplyr::mutate(Grupo = 'Artigos de Residência') %>%
  dplyr::rename('Data' = date,
                'Valor' = value)


var_ipca_vest <- BETS::BETSget(code = 1638,
                               data.frame = TRUE,
                               from = '2010-01-01') %>%
  dplyr::mutate(Grupo = 'Vestuário') %>%
  dplyr::rename('Data' = date,
                'Valor' = value)


var_ipca_transp <- BETS::BETSget(code = 1639,
                                 data.frame = TRUE,
                                 from = '2010-01-01') %>%
  dplyr::mutate(Grupo = 'Transportes') %>%
  dplyr::rename('Data' = date,
                'Valor' = value)


var_ipca_comunic <- BETS::BETSget(code = 1640,
                                  data.frame = TRUE,
                                  from = '2010-01-01') %>%
  dplyr::mutate(Grupo = 'Comunicação') %>%
  dplyr::rename('Data' = date,
                'Valor' = value)


var_ipca_saud_cuid <- BETS::BETSget(code = 1641,
                                    data.frame = TRUE,
                                    from = '2010-01-01') %>%
  dplyr::mutate(Grupo = 'Saúde e Cuidados Pessoais') %>%
  dplyr::rename('Data' = date,
                'Valor' = value)


var_ipca_desp_pessoais <- BETS::BETSget(code = 1642,
                                        data.frame = TRUE,
                                        from = '2010-01-01') %>%
  dplyr::mutate(Grupo = 'Despesas Pessoais') %>%
  dplyr::rename('Data' = date,
                'Valor' = value)


var_ipca_educ <- BETS::BETSget(code = 1643,
                               data.frame = TRUE,
                               from = '2010-01-01') %>%
  dplyr::mutate(Grupo = 'Educação') %>%
  dplyr::rename('Data' = date,
                'Valor' = value)


var_ipca_grupo <- rbind(var_ipca_aliment,
                     var_ipca_habit,
                     var_ipca_art_res,
                     var_ipca_vest,
                     var_ipca_transp,
                     var_ipca_comunic,
                     var_ipca_saud_cuid,
                     var_ipca_desp_pessoais,
                     var_ipca_educ)


ipca_grupo <- var_ipca_grupo %>%
  dplyr::mutate(valor_1 = (Valor/100)+1,
                ano = lubridate::year(Data)) %>%
  dplyr::group_by(Grupo, ano) %>%
  dplyr::summarise(Data = Data,
                   Valor = Valor,
                   valor_1 = cumprod(valor_1)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Acumulado_Ano = round(((valor_1)-1)*100,2)) %>%
  dplyr::select(Data, Grupo, Valor, Acumulado_Ano) %>%
  dplyr::rename('data' = Data,
                'grupo' = Grupo,
                'valor' = Valor,
                'valor_acum_ano' = Acumulado_Ano)


# Gráfico da Variação Mensal do IPCA por Grupo:
grafico_ipca_var_mensal_grupo <- ipca_grupo %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::percent(valor/100,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01),
                 '<br>Grupo:', grupo))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor/100,
                                            color = grupo),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor/100,
                                             color = grupo),
                      size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%)',
                color = '') +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                              big.mark = '.'))


grafico_ipca_var_mensal_grupo <- plotly::ggplotly(grafico_ipca_var_mensal_grupo,
                                                  tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IPCA',
                                            '<br>',
                                            '<sup>',
                                            'Variação Mensal (por Grupos)',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Gráfico de variação acumulada no ano do IPCA por grupo:
grafico_ipca_var_acum_ano_grupo <- ipca_grupo %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::percent(valor_acum_ano/100,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01),
                 '<br>Grupo:', grupo))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor_acum_ano/100,
                                            color = grupo),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor_acum_ano/100,
                                             color = grupo),
                      size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%) <sup>',
                color = '') +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                              big.mark = '.'))


grafico_ipca_var_acum_ano_grupo <- plotly::ggplotly(grafico_ipca_var_acum_ano_grupo,
                                                    tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IPCA ',
                                            '<br>',
                                            '<sup>',
                                            'Variação Acumulada no Ano (por Grupos)',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação do Índice de Difusão da Inflação:
dados_difusao <- BETS::BETSget(code = 21379,
                         data.frame = TRUE,
                         from = '2010-01-01') %>%
  dplyr::rename('data' = date,
                'valor' = value)


grafico_difusao <- dados_difusao %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::percent(valor/100,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor/100),
                     color = '#440154',
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor/100),
                      color = '#440154',
                      size = 0.75) +
  ggplot2::labs(x = '',
                y = '(%)') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                              big.mark = '.'))


grafico_difusao <- plotly::ggplotly(grafico_difusao,
                                    tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Índice de Difusão',
                                            '<br>',
                                            '<sup>',
                                            'IPCA',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação dos Preços Livres e Monitorados em uma Economia:
itens_livres <- BETS::BETSget(code = 11428,
                            data.frame = TRUE,
                            from = '2010-01-01') %>%
  dplyr::rename('data' = date,
                'Livres' = value)


itens_monitorados <- BETS::BETSget(code = 4449,
                             data.frame = TRUE,
                             from = '2010-01-01') %>%
  dplyr::rename('data' = date,
                'Monitorados' = value)


itens_livres_monitorados <- dplyr::left_join(itens_livres, itens_monitorados,
                                             by = c('data' = 'data')) %>%
  tidyr::pivot_longer(-data, names_to = 'item', values_to = 'valor')


# Gráfico dos preços Livres e Monitorados:
grafico_livres_monitorados <- itens_livres_monitorados %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::percent(valor/100,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01),
                 '<br>Item:', item))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor/100,
                                            color = item),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor/100,
                                             color = item),
                      size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%) <sup>',
                color = '') +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                              big.mark = '.'))


grafico_livres_monitorados <- plotly::ggplotly(grafico_livres_monitorados,
                                               tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IPCA (Variação Mensal)',
                                            '<br>',
                                            '<sup>',
                                            'Itens Livres versus Itens Monitorados',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Gráfico dos preços Livres e Monitorados (acumulado no ano):
grafico_livres_monitorados_acum <- itens_livres_monitorados %>%
  dplyr::mutate(ano = lubridate::year(data),
                valor_cum = 1+(valor/100)) %>%
  dplyr::group_by(ano, item) %>%
  dplyr::summarise(data = data,
                   valor = valor,
                   valor_cum = cumprod(valor_cum)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(valor_cum = round((valor_cum-1)*100,2)) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::percent(valor_cum/100,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01),
                 '<br>Item:', item))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor_cum/100,
                                            color = item),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor_cum/100,
                                             color = item),
                      size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%)',
                color = '') +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                              big.mark = '.'))


grafico_livres_monitorados_acum <- plotly::ggplotly(grafico_livres_monitorados_acum,
                                               tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IPCA (Variação Acumulada no Ano)',
                                            '<br>',
                                            '<sup>',
                                            'Itens Livres versus Itens Monitorados',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Importação dos Preços Comercializáveis e Não Comercializáveis em uma Economia:
itens_comercializaveis <- BETS::BETSget(code = 4447,
                                        data.frame = TRUE,
                                        from = '2010-01-01') %>%
  dplyr::rename('data' = date,
                'Comercializáveis' = value)


itens_nao_comercializaveis <- BETS::BETSget(code = 4448,
                                            data.frame = TRUE,
                                            from = '2010-01-01') %>%
  dplyr::rename('data' = date,
                'Não Comercializáveis' = value)


itens_comercializaveis_nao_comercializaveis <- dplyr::left_join(itens_comercializaveis,
                                                                itens_nao_comercializaveis,
                                             by = c('data' = 'data')) %>%
  tidyr::pivot_longer(-data, names_to = 'item', values_to = 'valor')


# Gráfico dos Comercializáveis e Não Comercializáveis:
grafico_comercializaveis_nao_comercializaveis <- itens_comercializaveis_nao_comercializaveis %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::percent(valor/100,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01),
                 '<br>Item:', item))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor/100,
                                            color = item),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor/100,
                                             color = item),
                      size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%) <sup>',
                color = '') +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                              big.mark = '.'))


grafico_comercializaveis_nao_comercializaveis <- plotly::ggplotly(grafico_comercializaveis_nao_comercializaveis,
                                                                  tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IPCA (Variação Mensal)',
                                            '<br>',
                                            '<sup>',
                                            'Comercializáveis versus Não Comercializáveis',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


# Gráfico dos Comercializáveis e Não Comercializáveis (Acumulado no Ano):
grafico_comercializaveis_nao_comercializaveis_acum <- itens_comercializaveis_nao_comercializaveis %>%
  dplyr::mutate(ano = lubridate::year(data),
                valor_cum = 1+(valor/100)) %>%
  dplyr::group_by(ano, item) %>%
  dplyr::summarise(data = data,
                   valor = valor,
                   valor_cum = cumprod(valor_cum)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(valor_cum = round((valor_cum-1)*100,2)) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::percent(valor_cum/100,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01),
                 '<br>Item:', item))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data,
                                            y = valor_cum/100,
                                            color = item),
                     size = 0.75) +
  ggplot2::geom_point(mapping = ggplot2::aes(x = data,
                                             y = valor_cum/100,
                                             color = item),
                      size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = '',
                y = '(%)',
                color = '') +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',',
                                                              big.mark = '.'))


grafico_comercializaveis_nao_comercializaveis_acum <- plotly::ggplotly(grafico_comercializaveis_nao_comercializaveis_acum,
                                                                  tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('IPCA (Variação Acumulada no Ano)',
                                            '<br>',
                                            '<sup>',
                                            'Comercializáveis versus Não Comercializáveis',
                                            '<br>')),
                 margin = list(l = 50, t = 50),
                 legend = list(orientation = 'h', x = 0.05, y = -0.15)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


################################################################################
################################################################################
# TAXA DE JUROS
# Importação e Tratamento dos dados:
selic <- BETS::BETSget(code = '1178',
                       from = '2012-06-01',
                       data.frame = TRUE) %>%
  dplyr::rename('data' = date,
                'selic' = value) %>%
  dplyr::mutate(selic = selic/100)


expectativa_inflacao <- rbcb::get_twelve_months_inflation_expectations(
  indic = 'IPCA',
  start_date = '2012-06-01') %>%
  dplyr::filter(smoothed == 'S' & base == 0) %>%
  dplyr::select(date, mean) %>%
  dplyr::rename('data' = date,
                'expec_ipca' = mean) %>%
  dplyr::mutate(expec_ipca = expec_ipca/100)


juros <- dplyr::full_join(selic,
                          expectativa_inflacao,
                          by = c('data' = 'data')) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(juro_ex_ante = (((1+selic)/(1+expec_ipca))-1))


ntnb50 <- GetTDData::read.TD.files(dl.folder = 'TD Files')


juro_neutro <- ntnb50 %>%
  dplyr::filter(asset.code == 'NTN-B 150850') %>%
  dplyr::rename('data' = ref.date,
                'ntnb50' = yield.bid) %>%
  dplyr::select(data, ntnb50) %>%
  dplyr::mutate(juro_neutro = purrr::pluck(
    mFilter::hpfilter(ntnb50, freq = 10000, type ='lambda'), 'trend'))


juros <- dplyr::full_join(juros,
                          juro_neutro,
                          by = c('data' = 'data')) %>%
  tidyr::drop_na()


# Visualização de dados:
selic_graf <- juros %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::day(data),'-',
                                  lubridate::month(data),'-',
                                  lubridate::year(data)),
                 '<br>Valor:', scales::percent(selic,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01), 'ao ano'))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data, y = selic),
            size = 0.75,
            color = '#440154') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                     decimal.mark = ',',
                                                     accuracy = 0.01)) +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = '',
                y = '(% ao ano)')


selic_graf <- plotly::ggplotly(selic_graf,
                               tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('Taxa Básica de Juros (Nominal)',
                                            '<br>',
                                            '<sup>',
                                            'SELIC Anualizada',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


expectativa_graf <- juros %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::day(data),'-',
                                  lubridate::month(data),'-',
                                  lubridate::year(data)),
                 '<br>Valor:', scales::percent(expec_ipca,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01), 'ao ano'))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data, y = expec_ipca),
                     size = 0.75,
                     color = '#440154') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                              decimal.mark = ',',
                                                              accuracy = 0.01)) +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = '',
                y = '(% ao ano)')


expectativa_graf <- plotly::ggplotly(expectativa_graf,
                                     tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('Expectativa de Inflação',
                                            '<br>',
                                            '<sup>',
                                            'Expectativa para o IPCA 12 meses à Frente',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


politica_monetaria_graf <- juros %>%
  dplyr::select(data, juro_ex_ante, juro_neutro) %>%
  tidyr::pivot_longer(!data, names_to = 'ref', values_to = 'valores') %>%
  dplyr::mutate(ref = dplyr::case_when(ref == 'juro_ex_ante' ~ 'Taxa de Básica Juros Real Ex-Ante',
                                       ref == 'juro_neutro' ~ 'Taxa de Juros Neutro')) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::day(data),'-',
                                  lubridate::month(data),'-',
                                  lubridate::year(data)),
                 '<br>Valor:', scales::percent(valores,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01), 'ao ano'))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = data, y = valores, color = ref),
                     size = 0.75) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                              decimal.mark = ',',
                                                              accuracy = 0.01)) +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = '',
                y = '(% ao ano)') +
  ggplot2::scale_color_manual(breaks = c('Taxa de Básica Juros Real Ex-Ante',
                                         'Taxa de Juros Neutro'),
                              values = c('#440154',
                                         '#FBE625'))


politica_monetaria_graf <- plotly::ggplotly(politica_monetaria_graf,
                                            tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('Juros Real versus Juros Neutro',
                                            '<br>',
                                            '<sup>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


# Formação da Lista de Tabelas e Figuras do Nível de Atividade: -----------
resultados_nivel_atividade <- list(
  tabela_pib_vc = tabela_pib_valores,
  grafico_pib_indice = grafico_pib_indice,
  grafico_var_pib_indice = grafico_var_pib_indice,
  grafico_real_potencial = grafico_real_potencial,
  grafico_hiato = grafico_hiato,
  tabela_pib_setores_vc = tabela_pib_setores_valores,
  grafico_pib_setores_indice = grafico_pib_setores_indice,
  tabela_pib_demanda_vc = tabela_pib_demanda_valores,
  grafico_pib_demanda_indice = grafico_pib_demanda_indice,
  grafico_pim_sa_indice = grafico_pim_sa_indice,
  grafico_pmc_sa_indice = grafico_pmc_sa_indice,
  grafico_pms_sa_indice = grafico_pms_sa_indice,
  grafico_ibc_br = grafico_ibc_br
)


saveRDS(resultados_nivel_atividade,
        file = 'resultados_nivel_atividade.rds')


# Formação da Lista de Tabelas e Figuras do Setor Externo: ----------------
resultados_setor_externo <- list(
  grafico_nominal_dolar = grafico_nominal_dolar_mm,
  grafico_nominal_euro = grafico_nominal_euro_mm,
  grafico_var_nominal_dolar = grafico_var_nominal_dolar_mm,
  grafico_var_nominal_euro = grafico_var_nominal_euro_mm,
  grafico_real_dolar = grafico_real_dolar_m,
  grafico_real_euro = grafico_real_euro_m,
  grafico_var_real_dolar = grafico_var_real_dolar_m,
  grafico_var_real_euro = grafico_var_real_euro_m,
  grafico_saldo_tc = grafico_saldo_tc,
  grafico_saldo_cf = grafico_saldo_cf,
  grafico_r_i = grafico_reservas_internacionais,
  grafico_tc_pib = grafico_tc_pib,
  grafico_id_pib = grafico_id_pib
)


saveRDS(resultados_setor_externo,
        file = 'resultados_setor_externo.rds')



# Formação da Lista de Figuras da Taxa de Inflação: -----------------------
resultados_inflacao <- list(
  grafico_ipca_indice = grafico_ipca_indice,
  grafico_ipca_var_mensal = grafico_ipca_var_mensal,
  grafico_ipca_var_acum_ano = grafico_ipca_var_acum_ano,
  grafico_ipca_peso_grupo = grafico_ipca_peso_grupo,
  grafico_ipca_var_mensal_grupo = grafico_ipca_var_mensal_grupo,
  grafico_ipca_var_acum_ano_grupo = grafico_ipca_var_acum_ano_grupo,
  grafico_difusao = grafico_difusao,
  grafico_livres_monitorados = grafico_livres_monitorados,
  grafico_livres_monitorados_acum = grafico_livres_monitorados_acum,
  grafico_comercializaveis_nao_comercializaveis = grafico_comercializaveis_nao_comercializaveis,
  grafico_comercializaveis_nao_comercializaveis_acum = grafico_comercializaveis_nao_comercializaveis_acum
)


saveRDS(resultados_inflacao,
        file = 'resultados_inflacao.rds')


# Formação de Lista de Figuras da Política Monetária: ---------------------
resultados_politica_monetaria <- list(
  selic_graf = selic_graf,
  expectativa_graf = expectativa_graf,
  politica_monetaria_graf = politica_monetaria_graf
)


saveRDS(resultados_politica_monetaria,
        file = 'resultados_politica_monetaria.rds')


# Formação da Lista de Tabelas e Figuras do Total: ------------------------
resultados_total <- list(
  tabela_pib_vc = tabela_pib_valores,
  grafico_pib_indice = grafico_pib_indice,
  grafico_var_pib_indice = grafico_var_pib_indice,
  grafico_real_potencial = grafico_real_potencial,
  grafico_hiato = grafico_hiato,
  tabela_pib_setores_vc = tabela_pib_setores_valores,
  grafico_pib_setores_indice = grafico_pib_setores_indice,
  tabela_pib_demanda_vc = tabela_pib_demanda_valores,
  grafico_pib_demanda_indice = grafico_pib_demanda_indice,
  grafico_pim_sa_indice = grafico_pim_sa_indice,
  grafico_pmc_sa_indice = grafico_pmc_sa_indice,
  grafico_pms_sa_indice = grafico_pms_sa_indice,
  grafico_ibc_br = grafico_ibc_br,

  grafico_nominal_dolar = grafico_nominal_dolar_mm,
  grafico_nominal_euro = grafico_nominal_euro_mm,
  grafico_var_nominal_dolar = grafico_var_nominal_dolar_mm,
  grafico_var_nominal_euro = grafico_var_nominal_euro_mm,
  grafico_real_dolar = grafico_real_dolar_m,
  grafico_real_euro = grafico_real_euro_m,
  grafico_var_real_dolar = grafico_var_real_dolar_m,
  grafico_var_real_euro = grafico_var_real_euro_m,
  grafico_saldo_tc = grafico_saldo_tc,
  grafico_saldo_cf = grafico_saldo_cf,
  grafico_r_i = grafico_reservas_internacionais,
  grafico_tc_pib = grafico_tc_pib,
  grafico_id_pib = grafico_id_pib,

  grafico_ipca_indice = grafico_ipca_indice,
  grafico_ipca_var_mensal = grafico_ipca_var_mensal,
  grafico_ipca_var_acum_ano = grafico_ipca_var_acum_ano,
  grafico_ipca_peso_grupo = grafico_ipca_peso_grupo,
  grafico_ipca_var_mensal_grupo = grafico_ipca_var_mensal_grupo,
  grafico_ipca_var_acum_ano_grupo = grafico_ipca_var_acum_ano_grupo,
  grafico_difusao = grafico_difusao,
  grafico_livres_monitorados = grafico_livres_monitorados,
  grafico_livres_monitorados_acum = grafico_livres_monitorados_acum,
  grafico_comercializaveis_nao_comercializaveis = grafico_comercializaveis_nao_comercializaveis,
  grafico_comercializaveis_nao_comercializaveis_acum = grafico_comercializaveis_nao_comercializaveis_acum,

  selic_graf = selic_graf,
  expectativa_graf = expectativa_graf,
  politica_monetaria_graf = politica_monetaria_graf
)


saveRDS(resultados_total,
        file = 'resultados_total.rds')


# Formação de Tabelas: ----------------------------------------------------
excel_deflator <- deflator %>%
  dplyr::rename('Data' = data,
                'Deflator (IPCA)' = indice)


excel_pib_vc <- pib_vc %>%
  dplyr::select(!ano) %>%
  dplyr::mutate(setores = stringr::str_remove_all(setores, ' <br>')) %>%
  dplyr::rename('Setores (Econômicos e Institucionais)' = setores,
                'Data' = trimestre,
                'Valor Corrente' = valor_nominal,
                'Valor Real' = valor_real,
                'Variação Interanual (Base: Valores Reais)' = var_anual,
                'Variação Marginal (Base: Valores Reais)' = var_trim,
                'Acumulado no Ano (Valor Corrente)' = acumulado_nominal,
                'Acumulado no Ano (Valor Real)' = acumulado_real)


excel_agregados_indice <- agregados_indice %>%
  dplyr::select(!ano) %>%
  dplyr::mutate(setores = stringr::str_remove_all(setores, ' <br>')) %>%
  dplyr::rename('Setores (Econômicos e Institucionais)' = setores,
                'Data' = trimestre,
                'Número Índice (Base: 2010 = 100)' = valor,
                'Variação Interanual' = var_anual,
                'Variação Marginal' = var_mensal)


excel_potencial <- potencial %>%
  dplyr::mutate(potencial = as.numeric(potencial)) %>%
  dplyr::rename('Data' = trimestre,
                'PIB Efetivo (Índice)' = valor,
                'PIB Potencial (Índice)' = potencial,
                'Hiato do Produto' = hiato)


excel_pim_sa_indice <- pim_sa_indice %>%
  dplyr::rename('Número Índice' = valor,
                'Data' = data,
                'Categorias Econômicas' = setores,
                'Variação Interanual' = var_anual,
                'Variação Marginal' = var_mensal)


excel_pmc_sa_indice <- pmc_sa_indice %>%
  dplyr::rename('Número Índice' = valor,
                'Data' = data,
                'Tipos de Índice' = tipo,
                'Variação Interanual' = var_anual,
                'Variação Marginal' = var_mensal)


excel_pms_sa_indice <- pms_sa_indice %>%
  dplyr::rename('Número Índice' = valor,
                'Data' = data,
                'Tipos de Índice' = tipo,
                'Variação Interanual' = var_anual,
                'Variação Marginal' = var_mensal)


excel_ibc_br <- ibc_br %>%
  dplyr::rename('Número Índice' = valor,
                'Data' = data,
                'Variação Interanual' = var_anual,
                'Variação Marginal' = var_mensal)


tabelas_nivel_de_atividade <- list('Deflator' = excel_deflator,
                                   'PIB (Valores)' = excel_pib_vc,
                                   'PIB (Número Índice)' = excel_agregados_indice,
                                   'PIB Potencial' = excel_potencial,
                                   'PIM' = excel_pim_sa_indice,
                                   'PMC' = excel_pmc_sa_indice,
                                   'PMS' = excel_pms_sa_indice,
                                   'IBC-BR' = excel_ibc_br)


#openxlsx::write.xlsx(tabelas_nivel_de_atividade, 'oeb_nivel_de_atividade.xlsx')
saveRDS(tabelas_nivel_de_atividade,
        file = 'oeb_nivel_de_atividade.rds')


excel_tx_cambio_nominal_dolar_mm <- tx_cambio_nominal_dolar_mm %>%
  dplyr::arrange(desc(ano_mes)) %>%
  dplyr::rename('Data' = ano_mes,
                'Taxa de Câmbio Nominal (Média Mensal - R$/US$)' = valor,
                'Variação Marginal' = var_mensal)


excel_tx_cambio_nominal_euro_mm <- tx_cambio_nominal_euro_mm %>%
  dplyr::arrange(desc(ano_mes)) %>%
  dplyr::rename('Data' = ano_mes,
                'Taxa de Câmbio Nominal (Média Mensal - R$/€)' = valor,
                'Variação Marginal' = var_mensal)


excel_tx_cambio_real_dolar_m <- tx_cambio_real_dolar_m %>%
  dplyr::arrange(desc(ano_mes)) %>%
  dplyr::rename('Data' = ano_mes,
                'Taxa de Câmbio Real (Dólar)' = valor,
                'Variação Marginal' = var_mensal)


excel_tx_cambio_real_euro_m <- tx_cambio_real_euro_m %>%
  dplyr::arrange(desc(ano_mes)) %>%
  dplyr::rename('Data' = ano_mes,
                'Taxa de Câmbio Real (Euro)' = valor,
                'Variação Marginal' = var_mensal)


excel_saldo_tc <- saldo_tc %>%
  dplyr::arrange(desc(ano_mes)) %>%
  dplyr::rename('Data' = ano_mes,
                'Saldo em Transações Correntes (US$ - Bilhões)' = valor,
                'Variação Marginal' = var_mensal)


excel_saldo_cf <- saldo_cf %>%
  dplyr::arrange(desc(ano_mes)) %>%
  dplyr::rename('Data' = ano_mes,
                'Saldo Líquido em Conta Financeira (US$ - Bilhões)' = valor,
                'Variação Marginal' = var_mensal)


excel_dados_reservas_internacionais <- dados_reservas_internacionais %>%
  dplyr::arrange(desc(ano_mes)) %>%
  dplyr::rename('Data' = ano_mes,
                'Estoque Total em Reservas Internacionais (US$ - Bilhões)' = valor,
                'Variação Marginal' = var_mensal)


excel_dados_tc_pib <- dados_tc_pib %>%
  dplyr::arrange(desc(ano_mes)) %>%
  dplyr::rename('Data' = ano_mes,
                'Déficit em Transações Correntes em Relação ao PIB (Acumulado em 12 meses)' = valor,
                'Variação Marginal' = var_mensal)


excel_dados_id_pib <- dados_id_pib %>%
  dplyr::arrange(desc(ano_mes)) %>%
  dplyr::rename('Data' = ano_mes,
                'Investimento Direto em relação ao PIB (Acumulado em 12 meses)' = valor,
                'Variação Marginal' = var_mensal)


tabelas_taxa_de_cambio <- list('Câmbio Nominal (Dólar)' = excel_tx_cambio_nominal_dolar_mm,
                               'Câmbio Nominal (Euro)' = excel_tx_cambio_nominal_euro_mm,
                               'Câmbio Real (Dólar)' = excel_tx_cambio_real_dolar_m,
                               'Câmbio Real (Euro)' = excel_tx_cambio_real_euro_m,
                               'Saldo em TC' = excel_saldo_tc,
                               'Saldo em CF' = excel_saldo_cf,
                               'Reservas Internacionais' = excel_dados_reservas_internacionais,
                               'TC em Proporção do PIB' = excel_dados_tc_pib,
                               'ID em Proporção do PIB' = excel_dados_id_pib)


#openxlsx::write.xlsx(tabelas_taxa_de_cambio, 'oeb_taxa_de_cambio.xlsx')
saveRDS(tabelas_taxa_de_cambio,
        file = 'oeb_taxa_de_cambio.rds')


excel_ipca_cheio <- ipca_cheio %>%
  dplyr::rename('Data' = mes_codigo,
                'Variável' = variavel,
                'Valor' = valor)


excel_ipca_grupo_peso <- ipca_grupo_peso %>%
  dplyr::select(!variavel) %>%
  dplyr::rename('Data' = mes_codigo,
                'Grupo' = grupo,
                'Peso no Mês (%)' = valor)


excel_ipca_grupo <- ipca_grupo %>%
  dplyr::rename('Data' = data,
                'Grupo' = grupo,
                'Variação Mensal (por Grupos)' = valor,
                'Variação Acumulada no Ano (por Grupos)' = valor_acum_ano)


excel_dados_difusao <- dados_difusao %>%
  dplyr::rename('Data' = data,
                'Índice de Difusão (IPCA)' = valor)


excel_itens_livres_monitorados <- itens_livres_monitorados %>%
  dplyr::rename('Data' = data,
                'Tipo de Item' = item,
                'Variação Mensal (por Tipo de Item)' = valor)


excel_itens_comercializaveis_nao_comercializaveis <- itens_comercializaveis_nao_comercializaveis %>%
  dplyr::rename('Data' = data,
                'Tipo de Item' = item,
                'Variação Mensal (por Tipo de Item)' = valor)


tabelas_taxa_de_inflacao <- list(
  'IPCA Cheio' = excel_ipca_cheio,
  'Peso por Grupo (IPCA)' = excel_ipca_grupo_peso,
  'IPCA por Grupo' = excel_ipca_grupo,
  'Índice de Difusão' = excel_dados_difusao,
  'Livres e Monitorados' = excel_itens_livres_monitorados,
  'Comercializáveis e Não Comer' = excel_itens_comercializaveis_nao_comercializaveis)


#openxlsx::write.xlsx(tabelas_taxa_de_inflacao, 'oeb_taxa_de_inflacao.xlsx')
saveRDS(tabelas_taxa_de_inflacao,
        file = 'oeb_taxa_de_inflacao.rds')


excel_juros <- juros %>%
  dplyr::mutate(selic = selic*100,
                expec_ipca = expec_ipca*100,
                juro_ex_ante = juro_ex_ante*100,
                ntnb50 = ntnb50*100,
                juro_neutro = juro_neutro*100) %>%
  dplyr::rename('Data' = data,
                'SELIC (% ao ano)' = selic,
                'Expectativa IPCA (% ao ano 12 meses a frente)' = expec_ipca,
                'Juros Real Ex-Ante (% ao ano)' = juro_ex_ante,
                'NTN-B50 (% ao ano)' = ntnb50,
                'Juro Neutro (% ao ano)' = juro_neutro)


tabelas_politica_monetaria <- list(
  'Política Monetária' = excel_juros
)


#openxlsx::write.xlsx(tabelas_politica_monetaria, 'oeb_politica_monetaria.xlsx')
saveRDS(tabelas_taxa_de_inflacao,
        file = 'oeb_politica_monetaria.rds')


tabelas_total <- list('Deflator' = excel_deflator,
                      'PIB (Valores)' = excel_pib_vc,
                      'PIB (Número Índice)' = excel_agregados_indice,
                      'PIB Potencial' = excel_potencial,
                      'PIM' = excel_pim_sa_indice,
                      'PMC' = excel_pmc_sa_indice,
                      'PMS' = excel_pms_sa_indice,
                      'IBC-BR' = excel_ibc_br,
                      'Câmbio Nominal (Dólar)' = excel_tx_cambio_nominal_dolar_mm,
                      'Câmbio Nominal (Euro)' = excel_tx_cambio_nominal_euro_mm,
                      'Câmbio Real (Dólar)' = excel_tx_cambio_real_dolar_m,
                      'Câmbio Real (Euro)' = excel_tx_cambio_real_euro_m,
                      'Saldo em TC' = excel_saldo_tc,
                      'Saldo em CF' = excel_saldo_cf,
                      'Reservas Internacionais' = excel_dados_reservas_internacionais,
                      'TC em Proporção do PIB' = excel_dados_tc_pib,
                      'ID em Proporção do PIB' = excel_dados_id_pib,
                      'IPCA Cheio' = excel_ipca_cheio,
                      'Peso por Grupo (IPCA)' = excel_ipca_grupo_peso,
                      'IPCA por Grupo' = excel_ipca_grupo,
                      'Índice de Difusão' = excel_dados_difusao,
                      'Livres e Monitorados' = excel_itens_livres_monitorados,
                      'Comercializáveis e Não Comer' = excel_itens_comercializaveis_nao_comercializaveis,
                      'Política Monetária' = excel_juros)


# openxlsx::write.xlsx(tabelas_total, 'oeb_conjuntura.xlsx')
saveRDS(tabelas_total,
        file = 'oeb_conjuntura.rds')


tictoc::toc()

beepr::beep(sound = 8)























