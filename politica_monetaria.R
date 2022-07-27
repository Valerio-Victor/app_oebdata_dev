

# PACOTES: ----------------------------------------------------------------
library(magrittr, include.only = '%>%')
library(ggplot2)


# TAXA DE JURO NEUTRO: ----------------------------------------------------
# Download do Excel:
GetTDData::download.TD.data(asset.codes = 'NTN-B')


# Importação do Excel:
ntnb50 <- GetTDData::read.TD.files(dl.folder = 'TD Files')


# Tratamento dos Dados:
ntnb50 <- ntnb50 %>%
  dplyr::filter(asset.code == 'NTN-B 150850')


juros_natural <- ntnb50 %>%
  dplyr::mutate(data = lubridate::ymd(ref.date)) %>%
  dplyr::mutate(ano = lubridate::year(data),
                mes = lubridate::month(data)) %>%
  dplyr::select(data, ano, mes, yield.bid) %>%
  dplyr::rename('ntnb50' = yield.bid) %>%
  dplyr::group_by(ano, mes) %>%
  dplyr::summarise(ano = ano,
                   mes = mes,
                   ntnb50 = mean(ntnb50)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(data = lubridate::ym(paste(ano, mes, sep = '-'))) %>%
  dplyr::distinct_all() %>%
  dplyr::select(data, ntnb50) %>%
  dplyr::mutate(juro_neutro = purrr::pluck(
    mFilter::hpfilter(ntnb50, freq = 14400, type ='lambda'), 'trend')) %>%
  tidyr::pivot_longer(!data, names_to = 'ref', values_to = 'valores')


# Visualização de Dados:
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


juros_natural_graf <- juros_natural %>%
  dplyr::filter(ref == 'juro_neutro') %>%
  ggplot(mapping = aes(
    group = 1,
    text = paste('Mês-Ano: ',
                 paste0(lubridate::month(data),
                        '-',
                        lubridate::year(data)),
                 '<br>Valor:', scales::percent(valores,
                                                big.mark = '.',
                                                decimal.mark = ',',
                                                accuracy = 0.01), 'ao ano',
                 '<br> ----------',
                 '<br> Técnica: Filtro Hodrick-Prescott'))) +
  geom_line(mapping = aes(x = data, y = valores),
            size = 0.75,
            color = '#440154') +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                     decimal.mark = ',',
                                                     accuracy = 0.01)) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = 'Meses',
                y = '(% ao ano)')


juros_natural_graf <- plotly::ggplotly(juros_natural_graf,
                                       tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('Taxa de Juro Neutro',
                                            '<br>',
                                            '<sup>',
                                            'Cálculo a Partir da NTN−B 2050',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


juros_natural_graf


# TAXA DE JURO REAL (EX ANTE): --------------------------------------------
library(magrittr, include.only = '%>%')
library(ggplot2)


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


# Visualização de Dados:
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


selic_graf <- juros %>%
  ggplot(mapping = aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::day(data),'-',
                                 lubridate::month(data),'-',
                                 lubridate::year(data)),
                 '<br>Valor:', scales::percent(selic,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01), 'ao ano'))) +
  geom_line(mapping = aes(x = data, y = selic),
            size = 0.75,
            color = '#440154') +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                     decimal.mark = ',',
                                                     accuracy = 0.01)) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = 'Períodos',
                y = '(% ao ano)')


selic_graf


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


selic_graf


expectativa_graf <- juros %>%
  ggplot(mapping = aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::day(data),'-',
                                  lubridate::month(data),'-',
                                  lubridate::year(data)),
                 '<br>Valor:', scales::percent(expec_ipca,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01), 'ao ano'))) +
  geom_line(mapping = aes(x = data, y = expec_ipca),
            size = 0.75,
            color = '#440154') +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                     decimal.mark = ',',
                                                     accuracy = 0.01)) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = 'Períodos',
                y = '(% ao ano)')


expectativa_graf


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


expectativa_graf


real_ex_ante_graf <- juros %>%
  ggplot(mapping = aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::day(data),'-',
                                  lubridate::month(data),'-',
                                  lubridate::year(data)),
                 '<br>Valor:', scales::percent(juro_ex_ante,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01), 'ao ano'))) +
  geom_line(mapping = aes(x = data, y = juro_ex_ante),
            size = 0.75,
            color = '#440154') +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                     decimal.mark = ',',
                                                     accuracy = 0.01)) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = 'Períodos',
                y = '(% ao ano)')


real_ex_ante_graf


real_ex_ante_graf <- plotly::ggplotly(real_ex_ante_graf,
                               tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('Taxa de Básica de Juros Real Ex-Ante',
                                            '<br>',
                                            '<sup>',
                                            'SELIC Deflacionada pela Expectativa de Inflação 12 meses à Frente',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


real_ex_ante_graf


juro_neutro_graf <- juros %>%
  ggplot(mapping = aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::day(data),'-',
                                  lubridate::month(data),'-',
                                  lubridate::year(data)),
                 '<br>Valor:', scales::percent(juro_neutro,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01), 'ao ano'))) +
  geom_line(mapping = aes(x = data, y = juro_neutro),
            size = 0.75,
            color = '#440154') +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                     decimal.mark = ',',
                                                     accuracy = 0.01)) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  ggplot2::labs(color = '',
                x = 'Períodos',
                y = '(% ao ano)')


juro_neutro_graf


juro_neutro_graf <- plotly::ggplotly(juro_neutro_graf,
                                     tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('Taxa de Básica de Juros Real Ex-Ante',
                                            '<br>',
                                            '<sup>',
                                            'SELIC Deflacionada pela Expectativa de Inflação 12 meses à Frente',
                                            '<br>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


juro_neutro_graf


politica_monetaria_graf <- juros %>%
  dplyr::select(data, juro_ex_ante, juro_neutro) %>%
  tidyr::pivot_longer(!data, names_to = 'ref', values_to = 'valores') %>%
  dplyr::mutate(ref = dplyr::case_when(ref == 'juro_ex_ante' ~ 'Taxa de Básica Juros Real Ex-Ante',
                                       ref == 'juro_neutro' ~ 'Taxa de Juros Neutro')) %>%
  ggplot(mapping = aes(
    group = 1,
    text = paste('Data: ', paste0(lubridate::day(data),'-',
                                  lubridate::month(data),'-',
                                  lubridate::year(data)),
                 '<br>Valor:', scales::percent(valores,
                                               big.mark = '.',
                                               decimal.mark = ',',
                                               accuracy = 0.01), 'ao ano'))) +
  geom_line(mapping = aes(x = data, y = valores, color = ref),
            size = 0.75) +
  scale_y_continuous(labels = scales::percent_format(big.mark = '.',
                                                     decimal.mark = ',',
                                                     accuracy = 0.01)) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y') +
  labs(color = '',
       x = 'Períodos',
       y = '(% ao ano)') +
  scale_color_manual(breaks = c('Taxa de Básica Juros Real Ex-Ante',
                                'Taxa de Juros Neutro'),
                     values = c('#440154',
                                '#FBE625'))


politica_monetaria_graf


politica_monetaria_graf <- plotly::ggplotly(politica_monetaria_graf,
                                            tooltip = c('text')) %>%
  plotly::layout(legend = list(orientation = 'h', x = 0.05, y = -0.15),
                 title = list(text = paste0('Juros Real versus Juros Neutro',
                                            '<br>',
                                            '<sup>')),
                 margin = list(l = 50, t = 50)) %>%
  plotly::config(modeBarButtonsToRemove = plotly_layout)


politica_monetaria_graf





