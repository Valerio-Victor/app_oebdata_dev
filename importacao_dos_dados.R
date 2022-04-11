
#
#
# Carregando o Pipe:
library(magrittr, include.only = '%>%')


#' Aqui serão organizados a estrutura dos gráficos formatados em plotly, de modo
#' a retirar a maior parte das configurações que, em geral, não são utilizadas.
plotly_layout <- c('hoverClosestCartesian',
                   'hoverCompareCartesian',
                   'toggleSpikelines',
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


#
#
# Produto Interno Bruto (Valores Correntes): ------------------------------


#' Importação do número índice do IPCA para posterior cálculo do deflator, isto
#' é, razão entre o número índice no período t e o último número índice
#' disponível:
deflator <- sidrar::get_sidra(api = '/t/1737/n1/all/v/2266/p/last%20325/d/v2266%2013') %>%
  janitor::clean_names() %>%
  dplyr::select(mes_codigo, valor) %>%
  dplyr::transmute(data = lubridate::ym(mes_codigo),
                   indice = valor)



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


#' Construindo a tabela de valores de PIB com o pacote ractable. A ideia é
#' adicionar 'interação' de cores nas taxas de crescimento, barras no valor
#' deflacionado.

tabela_pib_valores <- pib_vc %>%
  dplyr::arrange(desc(trimestre)) %>%
  dplyr::filter(setores == 'PIB (preços de mercado)') %>%
  dplyr::select(!c(ano, setores, acumulado_nominal, acumulado_real)) %>%
  dplyr::mutate(trimestre = paste0('0',
                                   lubridate::quarter(trimestre),
                                  '-',
                                  lubridate::year(trimestre)),
                valor_nominal = scales::dollar(valor_nominal,
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
    valor_nominal = reactable::colDef(name = 'VALORES CORRENTES (em Bilhões)',
                                      align = 'center'),
    valor_real = reactable::colDef(name = 'VALORES REAIS (em Bilhões)',
                                   align = 'center'),
    var_anual = reactable::colDef(name = 'VARIAÇÃO INTERANUAL (Base: valores reais)',
                                  align = 'center'),
    var_trim = reactable::colDef(name = 'VARIAÇÃO MARGINAL (Base: valores reais)',
                                 align = 'center')
  ),
  defaultColDef = reactable::colDef(headerStyle = list(background = '#3C8DBC',
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
                 '<br>Índice:', scales::number(valor,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01))
  )) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valor),
                     color = '#3C8DBC',
                     size = 0.75) +
  ggplot2::labs(x = 'Trimestres',
                y = 'Base: 01-2010 = 100') +
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
                 '<br> Índice:', scales::percent(valores,
                                                 big.mark = '.',
                                                 decimal.mark = ',',
                                                 accuracy = 0.01),
                 '<br> Referência:', ref))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valores,
                                            color = ref),
                     size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = 'Trimestres',
                y = 'Variações') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                              decimal.mark = ',')) +
  ggplot2::theme(legend.position = 'bottom')+
  ggplot2::scale_color_manual(breaks = c('Interanual',
                                         'Marginal'),
                              values = c('#3C8DBC',
                                         '#414487FF'))


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
                 '<br> Índice:', scales::number(valores,
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
  ggplot2::labs(color = '',
                x = 'Trimestres',
                y = 'Base: 01-2010 = 100') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ',')) +
  ggplot2::theme(legend.position = 'bottom')+
  ggplot2::scale_color_manual(breaks = c('PIB Efetivo',
                                         'PIB Potencial'),
                              values = c('#3C8DBC',
                                         '#414487FF'))


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
                     color = '#3C8DBC') +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(x = 'Trimestres',
                y = 'Diferença') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(decimal.mark = ',')) +
  ggplot2::theme(legend.position = 'bottom')


grafico_hiato <- plotly::ggplotly(grafico_hiato, tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Hiato do Produto',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


#' Adicionando a tabela de valores correntes para a ótica do produto. Aqui serão
#' agrupados os serviços, indústria e agropecuária.
cor_ref <- '#3C8DBC'


pib_vc_setores <- pib_vc %>%
  dplyr::arrange(desc(trimestre)) %>%
  dplyr::filter(setores == 'Indústria' |
                  setores == 'Serviços' |
                  setores == 'Agropecuária')


tabela_pib_setores_valores <- pib_vc_setores %>%
  dplyr::select(!c(ano, acumulado_nominal, acumulado_real)) %>%
  dplyr::mutate(trimestre = paste0('0',
                                   lubridate::quarter(trimestre),
                                   '-',
                                   lubridate::year(trimestre)),
                valor_nominal = scales::dollar(valor_nominal,
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
      valor_nominal = reactable::colDef(name = 'VALORES CORRENTES (em Bilhões)',
                                        align = 'center'),
      valor_real = reactable::colDef(name = 'VALORES REAIS (em Bilhões)',
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
  dplyr::select(trimestre, valor, setores) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Trimestre-Ano: ',
                 paste0('0', lubridate::quarter(trimestre),
                        '-',
                        lubridate::year(trimestre)),
                 '<br>Índice:', scales::number(valor,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01))
  )) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valor,
                                            frame = setores),
                     color = '#3C8DBC',
                     size = 0.75) +
  ggplot2::labs(x = 'Trimestres',
                y = 'Base: 01-2010 = 100') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_pib_setores_indice <- plotly::ggplotly(grafico_pib_setores_indice,
                                       tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Produção por Setor Econômico',
                                            '<br>',
                                            '<sup>',
                                            'Índice de Volume com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::animation_button(label = 'Trocar') %>%
  plotly::animation_slider(currentvalue = list(prefix = 'Setor: ',
                                               font = list(color='black'))) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


#' Gráfico com as variações do número índice, sendo estas as variações marginal
#' e interanual (analisando todos os setores econômicos):
grafico_var_pib_setores_indice <- agregados_indice %>%
  dplyr::filter(setores == c('Agropecuária', 'Indústria', 'Serviços')) %>%
  dplyr::select(trimestre, var_anual, var_mensal, setores) %>%
  tidyr::pivot_longer(cols = !c(trimestre, setores),
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
                 '<br> Índice:', scales::percent(valores,
                                                 big.mark = '.',
                                                 decimal.mark = ',',
                                                 accuracy = 0.01),
                 '<br> Referência:', ref))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valores,
                                            color = ref,
                                            frame = setores),
                     size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = 'Trimestres',
                y = 'Variações') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                              decimal.mark = ',')) +
  ggplot2::theme(legend.position = 'bottom')+
  ggplot2::scale_color_manual(breaks = c('Interanual',
                                         'Marginal'),
                              values = c('#3C8DBC',
                                         '#414487FF'))


grafico_var_pib_setores_indice <- plotly::ggplotly(grafico_var_pib_setores_indice,
                                           tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('Variação da Produção por Setor Econômico',
                                            '<br>',
                                            '<sup>',
                                            'Índice de Volume com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::animation_button(label = 'Trocar') %>%
  plotly::animation_slider(currentvalue = list(prefix = 'Setor: ',
                                               font = list(color='black'))) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


#' Adicionando a tabela de valores correntes para a ótica da demanda. Aqui serão
#' agrupados consumo agregado, investimento agregado, gastos do governo, exportações
#' importações.
cor_ref <- '#3C8DBC'


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
  dplyr::select(!c(ano, acumulado_nominal, acumulado_real)) %>%
  dplyr::mutate(trimestre = paste0('0',
                                   lubridate::quarter(trimestre),
                                   '-',
                                   lubridate::year(trimestre)),
                valor_nominal = scales::dollar(valor_nominal,
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
      valor_nominal = reactable::colDef(name = 'VALORES CORRENTES (em Bilhões)',
                                        align = 'center'),
      valor_real = reactable::colDef(name = 'VALORES REAIS (em Bilhões)',
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
  dplyr::select(trimestre, valor, setores) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(
    group = 1,
    text = paste('Trimestre-Ano: ',
                 paste0('0', lubridate::quarter(trimestre),
                        '-',
                        lubridate::year(trimestre)),
                 '<br>Índice:', scales::number(valor,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01))
  )) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valor,
                                            frame = setores),
                     color = '#3C8DBC',
                     size = 0.75) +
  ggplot2::labs(x = 'Trimestres',
                y = 'Base: 01-2010 = 100') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(decimal.mark = ','))


grafico_pib_demanda_indice <- plotly::ggplotly(grafico_pib_demanda_indice,
                                               tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('COMPONENTES DA DEMANDA AGREGADA',
                                            '<br>',
                                            '<sup>',
                                            'Índice de Volume com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::animation_button(label = 'Trocar') %>%
  plotly::animation_slider(currentvalue = list(prefix = 'Setor: ',
                                               font = list(color='black'))) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


#' Gráfico com as variações do número índice, sendo estas as variações marginal
#' e interanual (analisando todos os setores econômicos):
grafico_var_demanda_indice <- agregados_indice %>%
  dplyr::filter(setores == 'Consumo das <br> Famílias' |
                  setores == 'Formação Bruta de <br> Capital Fixo' |
                  setores == 'Gastos do <br> Governo' |
                  setores == 'Exportações' |
                  setores == 'Importações') %>%
  dplyr::select(trimestre, var_anual, var_mensal, setores) %>%
  tidyr::pivot_longer(cols = !c(trimestre, setores),
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
                 '<br> Índice:', scales::percent(valores,
                                                 big.mark = '.',
                                                 decimal.mark = ',',
                                                 accuracy = 0.01),
                 '<br> Referência:', ref))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valores,
                                            color = ref,
                                            frame = setores),
                     size = 0.75) +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = 'Trimestres',
                y = 'Variações') +
  ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                              decimal.mark = ',')) +
  ggplot2::theme(legend.position = 'bottom')+
  ggplot2::scale_color_manual(breaks = c('Interanual',
                                         'Marginal'),
                              values = c('#3C8DBC',
                                         '#414487FF'))


grafico_var_demanda_indice <- plotly::ggplotly(grafico_var_demanda_indice,
                                               tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('Variação da Produção por Setor Econômico',
                                            '<br>',
                                            '<sup>',
                                            'Índice de Volume com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::animation_button(label = 'Trocar') %>%
  plotly::animation_slider(currentvalue = list(prefix = 'Setor: ',
                                               font = list(color='black'))) %>%
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

# Formação da Lista de Tabelas e Figuras: ---------------------------------
resultados <- list(
  tabela_pib_vc = tabela_pib_valores,
  grafico_pib_indice = grafico_pib_indice,
  grafico_var_pib_indice = grafico_var_pib_indice,
  grafico_real_potencial = grafico_real_potencial,
  grafico_hiato = grafico_hiato,
  tabela_pib_setores_vc = tabela_pib_setores_valores,
  grafico_pib_setores_indice = grafico_pib_setores_indice,
  grafico_var_pib_setores_indice = grafico_var_pib_setores_indice,
  tabela_pib_demanda_vc = tabela_pib_demanda_valores,
  grafico_pib_demanda_indice = grafico_pib_demanda_indice,
  grafico_var_pib_demanda_indice = grafico_var_demanda_indice
)


resultados[['tabela_pib_vc']]
resultados[['grafico_pib_indice']]
resultados[['grafico_var_pib_indice']]
resultados[['grafico_real_potencial']]
resultados[['grafico_hiato']]
resultados[['tabela_pib_setores_vc']]
resultados[['grafico_pib_setores_indice']]
resultados[['grafico_var_pib_setores_indice']]
resultados[['tabela_pib_demanda_vc']]
resultados[['grafico_pib_demanda_indice']]
resultados[['grafico_var_pib_demanda_indice']]


saveRDS(resultados, file = './app/resultados.rds')
#resultados <- readRDS("~/PROJETOS_R/2022/projetos/app_macro_ce/resultados.rds")






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







































#
valor_nominal <- pib_vc %>%
  dplyr::filter(trimestre == dplyr::last(trimestre),
                setores == 'PIB (preços de mercado)') %>%
  dplyr::select(valor_nominal) %>%
  dplyr::mutate(valor_nominal = scales::number(valor_nominal,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01))

#
acumulado_nominal <- pib_vc %>%
  dplyr::filter(trimestre == dplyr::last(trimestre),
                setores == 'PIB (preços de mercado)') %>%
  dplyr::select(trimestre, acumulado_nominal)
  dplyr::mutate(acumulado_nominal = scales::number(acumulado_nominal,
                                                   big.mark = '.',
                                                   decimal.mark = ',',
                                                   accuracy = 0.01))

#
data_deflator <- pib_vc %>%
  dplyr::filter(trimestre == dplyr::last(trimestre),
                setores == 'PIB (preços de mercado)') %>%
  dplyr::select(trimestre) %>%
  dplyr::mutate(trimestre = paste0(lubridate::month(trimestre,
                                                    abbr = FALSE,
                                                    label = TRUE),
                                   '/',
                                   lubridate::year(trimestre)))

#
pib_vc_st_graf <- pib_vc %>%
  dplyr::filter(setores == 'PIB (preços de mercado)') %>%
  ggplot2::ggplot(ggplot2::aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::month(trimestre,
                                                   abbr = FALSE,
                                                   label = TRUE),
                                  '/',
                                  lubridate::year(trimestre)),
                 '<br> ----------',
                 '<br> Valor:', scales::number(valor_real/1000,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01),
                 '<br> Acumulado no Ano:', scales::number(acumulado_real/1000,
                                                          big.mark = '.',
                                                          decimal.mark = ',',
                                                          accuracy = 0.01),
                 '<br> ----------',
                 '<br> Variação Interanual:', scales::percent(var_anual,
                                                              suffix = '%',
                                                              decimal.mark = ',',
                                                              accuracy = 0.01),
                 '<br> Variação Trimestral:', scales::percent(var_mensal,
                                                              suffix = '%',
                                                              decimal.mark = ',',
                                                              accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valor_real/1000),
                     size = 0.75,
                     color = '#2A788EFF') +
  ggplot2::labs(color = '',
                y = '(Valores em Bilhões de Reais)',
                x = '<br>') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(big.mark = '.')) +
  ggplot2::theme(legend.position = 'bottom')

#
plotly::ggplotly(pib_vc_st_graf, tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Produto Interno Bruto',
                                            '<br>',
                                            '<sup>',
                                            'Série Deflacionada pelo IPCA a preços de ',
                                            data_deflator,
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)

#
#
# Produto Interno Bruto (Número Índice)-----------------------------------------

#
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
  dplyr::mutate(valor = valor*100/head(valor,1))

#
pib_indice <- agregados_indice %>%
  dplyr::filter(setores == 'PIB (preços de mercado)') %>%
  dplyr::mutate(potencial = mFilter::hpfilter(valor,
                                              freq = as.numeric(nrow(.)),
                                              type = 'lambda') %>%
                  purrr::pluck('trend')) %>%
  dplyr::mutate(hiato = 100*((valor/potencial)-1))

View(pib_indice)

#
pib_indice_graf <- pib_indice %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(ano, setores, var_anual, var_mensal, hiato)) %>%
  tidyr::pivot_longer(-trimestre, names_to = 'variaveis', values_to = 'valores') %>%
  dplyr::mutate(variaveis = dplyr::case_when(variaveis == 'potencial' ~ 'PIB Potencial',
                                             variaveis == 'valor' ~ 'PIB Efetivo')) %>%
  ggplot2::ggplot(ggplot2::aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::month(trimestre,
                                                   abbr = FALSE,
                                                   label = TRUE),
                                  '/',
                                  lubridate::year(trimestre)),
                 '<br> ----------',
                 '<br> Índice:', scales::number(valores,
                                                big.mark = '.',
                                                decimal.mark = ',',
                                                accuracy = 0.01),
                 '<br> Referência:', variaveis))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = valores,
                                            color = variaveis),
                     size = 0.75) +
  ggplot2::labs(color = '',
                y = 'Número Índice (Base: 2010 = 100)',
                x = '<br>') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(big.mark = '.')) +
  ggplot2::theme(legend.position = 'bottom')+
  ggplot2::scale_color_manual(breaks = c('PIB Efetivo',
                                         'PIB Potencial'),
                              values = c('#2A788EFF',
                                         '#414487FF'))

#
plotly::ggplotly(pib_indice_graf, tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('PIB Efetivo versus PIB Potencial',
                                            '<br>',
                                            '<sup>',
                                            'Séries com Ajuste Sazonal',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)

#
hiato_indice_graf <- pib_indice %>%
  dplyr::ungroup() %>%
  dplyr::select(trimestre, hiato) %>%
  ggplot2::ggplot(ggplot2::aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::month(trimestre,
                                                   abbr = FALSE,
                                                   label = TRUE),
                                  '/',
                                  lubridate::year(trimestre)),
                 '<br> ----------',
                 '<br> Técnica: Filtro Hodrick-Prescott',
                 '<br> Valor:', scales::percent(hiato/100,
                                                big.mark = '.',
                                                decimal.mark = ',',
                                                accuracy = 0.01)))) +
  ggplot2::geom_line(mapping = ggplot2::aes(x = trimestre,
                                            y = hiato),
                     size = 0.75,
                     color = '#2A788EFF') +
  ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = '#D44292') +
  ggplot2::labs(color = '',
                y = 'Porcentagem',
                x = '<br>') +
  ggplot2::scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::scale_y_continuous(labels = scales::number_format(big.mark = '.')) +
  ggplot2::theme(legend.position = 'bottom')

#
plotly::ggplotly(hiato_indice_graf, tooltip = c('text')) %>%
  plotly::layout(title = list(text = paste0('Hiato do Produto',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)




























################################################################################
################################################################################






































#
#
# Pesquisa Industrial Mensal ---------------------------------------------------
pim_sa_indice_api <- sidrar::get_sidra(api = '/t/3651/n1/all/v/3134/p/all/c543/129278,129283,129300/d/v3134%201') %>%
  janitor::clean_names() %>%
  dplyr::select(valor, mes_codigo, grandes_categorias_economicas) %>%
  dplyr::rename('valor' = valor,
                'mes' = mes_codigo,
                'setores' = grandes_categorias_economicas) %>%
  dplyr::mutate(setores = dplyr::case_when(setores == '1 Bens de capital' ~ 'Bens de Capital',
                                           setores == '2 Bens intermediários' ~ 'Bens Intermediários',
                                           setores == '3 Bens de consumo' ~ 'Bens de Consumo'))
#
#
# Pesquisa Mensal do Comércio --------------------------------------------------
pmc_sa_indice_api <- sidrar::get_sidra(api = '/t/3417/n1/all/v/all/p/all/c11046/40312/d/v1186%201,v1190%201') %>%
  janitor::clean_names() %>%
  dplyr::select(valor, mes_codigo, variavel) %>%
  dplyr::rename('valor' = valor,
                'mes' = mes_codigo,
                'tipo' = variavel) %>%
  dplyr::mutate(tipo = dplyr::case_when(tipo == 'Índice de volume de vendas no comércio varejista ampliado' ~ 'Volume de Vendas',
                                        tipo == 'Índice de receita nominal de vendas no comércio varejista ampliado' ~ 'Receita Nominal'))
#
#
# Pesquisa Mensal de Serviços --------------------------------------------------
pms_sa_indice_api <- sidrar::get_sidra(api = '/t/6442/n1/all/v/all/p/all/c11046/40312/d/v8676%201,v8677%201') %>%
  janitor::clean_names() %>%
  dplyr::select(valor, mes_codigo, variavel) %>%
  dplyr::rename('valor' = valor,
                'mes' = mes_codigo,
                'tipo' = variavel) %>%
  dplyr::mutate(tipo = dplyr::case_when(tipo == 'Índice de volume de serviços' ~ 'Volume de Serviços',
                                        tipo == 'Índice de receita nominal de serviços' ~ 'Receita Nominal'))
#
#
# Índice do Banco Central ------------------------------------------------------
ibc_br_api <- httr::GET('http://api.bcb.gov.br/dados/serie/bcdata.sgs.24364/dados?formato=json&dataInicial=01/01/2001') %>%
  httr::content(simplifyDataFrame =  TRUE) %>%
  dplyr::rename('mes' = data,
                'valor' = valor) %>%
  dplyr::mutate(mes = lubridate::dmy(mes),
                valor = as.numeric(valor))
#
#
# Utilização da Capacidade Industrial ------------------------------------------
uci_api <- httr::GET('http://api.bcb.gov.br/dados/serie/bcdata.sgs.1344/dados?formato=json&dataInicial=30/04/1970') %>%
  httr::content(simplifyDataFrame =  TRUE) %>%
  dplyr::rename('trimestre' = data,
                'valor' = valor) %>%
  dplyr::mutate(trimestre = lubridate::dmy(trimestre),
                valor = as.numeric(valor))











