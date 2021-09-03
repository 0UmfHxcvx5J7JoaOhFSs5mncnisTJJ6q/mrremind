#' Share of Industry Subsectors in FE Use
#' 
#' Calculates industry subsector shares in final energy carrier use for the 
#' `fixed_shares` realisation of the `industry` module. 
#' 
#' For the region mapping `regionmapping_21_EU11.csv`, these are based on IEA 
#' data from `calcOutput(type = 'FEdemand', subtype = 'FE')`, for all other 
#' region mappings on vintage data which is ultimately based on Enerdata data. 
#' 
#' @note There is a discrepancy between the shares calculated from these two 
#' sources, that will affect REMIND emission reporting.
#' 
#' @return A [`magpie`][magclass::magclass] object.
#' 
#' @author Lavinia Baumstark
#' @author Michaja Pehl
#' 
#' @seealso [`calcOutput()`][madrat::calcOutput].
#' @md
#' 
#' @importFrom assertr verify
#' @importFrom dplyr as_tibble select filter inner_join tribble group_by 
#'   summarise mutate n ungroup 
#' @importFrom quitte character.data.frame
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider pivot_longer complete
#'
#' @export
calcShareIndFE <- function() {
  if ('regionmapping_21_EU11.csv' != getConfig('regionmapping')) {
    x <- readSource("REMIND_11Regi", subtype = "shareIndFE")
    w <- calcOutput("IO", subtype = "output", aggregate = FALSE)[,2010,]
  } else {
    # link fixed_shares fety and subsector names to subsector fetys ----
    mixer <- tribble(
      ~pf.fixed_shares,   ~sector.fixed_shares,   ~pf.subsectors,
      'fesoi',            'cement',               'feso_cement',
      'fesoi',            'chemicals',            'feso_chemicals',
      'fesoi',            'steel',                'feso_steel',
      'fesoi',            'otherInd',             'feso_otherInd',
      'fehoi',            'cement',               'feli_cement',
      'fehoi',            'chemicals',            'feli_chemicals',
      'fehoi',            'steel',                'feli_steel',
      'fehoi',            'otherInd',             'feli_otherInd',
      'fegai',            'cement',               'fega_cement',
      'fegai',            'chemicals',            'fega_chemicals',
      'fegai',            'steel',                'fega_steel',
      'fegai',            'otherInd',             'fega_otherInd',
      'fehei',            'cement',               NA,
      'fehei',            'chemicals',            NA,
      'fehei',            'steel',                NA,
      'fehei',            'otherInd',             'fehe_otherInd',
      'feeli',            'cement',               'feel_cement',
      'feeli',            'chemicals',            'feelhth_chemicals',
      'feeli',            'chemicals',            'feelwlth_chemicals',
      'feeli',            'steel',                'feel_steel_primary',
      'feeli',            'steel',                'feel_steel_secondary',
      'feeli',            'otherInd',             'feelhth_otherInd',
      'feeli',            'otherInd',             'feelwlth_otherInd')
    
    # calculate 2005 NECHEM ----
    NECHEM <- inner_join(
      readSource('IEA', 'EnergyBalances') %>% 
        `[`(,2005,'NECHEM', pmatch = TRUE) %>% 
        `[`(,,'TOTAL', pmatch = TRUE, invert = TRUE) %>% 
        as.data.frame() %>% 
        as_tibble() %>% 
        select(iso3c = .data$Region, product = .data$Data1, flow = .data$Data2,
               value = .data$Value) %>% 
        filter(0 != .data$value) %>% 
        # ktoe * 4.1868e-5 EJ/ktoe = EJ
        mutate(value = .data$value * 4.1868e-05),
      
      toolGetMapping('structuremappingIO_outputs.csv', 'sectoral') %>% 
        as_tibble() %>% 
        select(product = .data$iea_product, flow = .data$iea_flows, 
               pf.in = .data$REMINDitems_in, 
               pf.fixed_shares = .data$REMINDitems_out) %>% 
        filter(grepl('^fe..i', .data$pf.fixed_shares)) %>% 
        distinct(.data$product, .data$pf.fixed_shares) %>% 
        mutate(
          pf.fixed_shares = sub('^(fe..i).*', '\\1', .data$pf.fixed_shares)) %>% 
        # modify Blast Furnace/Coke Oven/Gas Works outputs to exclude the
        # 'solids' kludge and make them only produce gases
        filter(!(.data$product %in% c('BLFURGS', 'COKEOVGS', 'GASWKSGS', 
                                      'OGASES')
                 & 'fesoi' == .data$pf.fixed_shares)) %>%
        distinct() %>% 
        group_by(.data$product) %>% 
        mutate(count = n()) %>% 
        ungroup() %>% 
        verify(1 == .data$count, 
               description = paste('verify structuremappingIO_outputs.csv',
                                   'to give 1-to-1 mapping of NECHEM',
                                   'products to REMIND enty')) %>% 
        select(-.data$count),
      
      'product'
    ) %>% 
      group_by(.data$iso3c, .data$pf.fixed_shares) %>% 
      summarise(value = sum(.data$value), .groups = 'drop') %>% 
      inner_join(
        mixer %>% 
          filter('chemicals' == .data$sector.fixed_shares),
        
        'pf.fixed_shares'
      )

    # calculate 2005 subsector FE demand ----
    x <- calcOutput(type = 'FEdemand', subtype = 'FE', aggregate = FALSE) %>% 
      `[`(,2005,'gdp_SSP2', pmatch = TRUE) %>% 
      as.data.frame() %>% 
      as_tibble() %>% 
      select('iso3c' = 'Region', 'pf' = 'Data2', 'value' = 'Value') %>% 
      character.data.frame() %>% 
      inner_join(mixer, c('pf' = 'pf.subsectors')) %>% 
      # add FE and NECHEM to calculate shares ----
      bind_rows(
        NECHEM %>% 
          rename(pf = .data$pf.subsectors)
      ) %>% 
      select(-'pf', !!sym('pf') := 'pf.fixed_shares') %>% 
      group_by(!!sym('iso3c'), !!sym('pf'), !!sym('sector.fixed_shares')) %>% 
      summarise(!!sym('value') := sum(!!sym('value'), na.rm = TRUE), 
                .groups = 'drop_last') %>% 
      mutate(!!sym('value') := !!sym('value') / sum(!!sym('value'), 
                                                    na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter('otherInd' != !!sym('sector.fixed_shares')) %>% 
      pivot_wider(names_from = 'pf', values_fill = 0) %>% 
      mutate(!!sym('feh2i') := !!sym('fegai')) %>% 
      pivot_longer(matches('^fe..i$'), names_to = 'pf', ) %>% 
      complete(
        !!sym('iso3c'), 
        !!sym('sector.fixed_shares') := c('cement', 'chemicals', 'steel'),
        !!sym('pf') := c('fesoi', 'fehoi', 'fegai', 'feh2i', 'fehei', 'feeli'),
        fill = list(value = 0)) %>% 
      select('region' = 'iso3c', 'type' = 'pf', 
             'variable' = 'sector.fixed_shares', 'value') %>% 
      group_by(.data$region, .data$type) %>% 
      summarise(total = sum(.data$value)) %>% 
      verify(1 >= .data$total, 
             description = 'Verify subsector shares not exceeding 1.') %>% 
      as.magpie()
    
    w <- calcOutput('IO', subtype = 'output', aggregate = FALSE)[,2015,]
  }
  
  w <- w[,,intersect(getNames(x, dim = 1), getNames(w, dim = 2))] 
  w <- dimSums(w, dim = c(3.1, 3.3))
  # duplicate fegai as a weight for feh2i
  w <- mbind(w, setNames(w[,,'fegai'], 'feh2i'))
  
  return(list(x           = x,
              weight      = w,
              unit        = "ratio",
              description = "share of industry sub-sectors in FE use"))
}
