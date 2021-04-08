#' Apply corrections to IEA data needed for Industry subsectors
#' 
#' Apply corrections to IEA data to cope with fragmentary time series and 
#' replace outputs from blast furnaces and coke ovens, that are inputs into 
#' industry subsectors, by their respective inputs.  
#' The corrections done by this function are rather rudimentary and crude. This
#' gets smoothed away in regional aggregation. But do not use the resulting 
#' country-level data without additional scrutiny.
#' 
#' Use regional or global averages if IEA industry data lists energy use only as
#' "non-specified". 
#' Outputs from blast furnaces (\code{BLFURGS}, \code{OGASES}) and coke ovens 
#' (\code{OVENCOKE}, \code{COKEOVGS}, \code{COALTAR}, \code{NONCRUDE}), that are
#' inputs into industry subsectors, are replaced with the respective inputs 
#' based on regional averages. 
#' Used internally in \code{\link{calcIO}} for subtype 
#' \code{output_Industry_subsectors}.
#'
#' @param data MAgPIE object containing the IEA Energy Balances data
#' 
#' @param ieamatch mapping of IEA product/flow combinations to REMIND 
#'        \code{sety}/\code{fety}/\code{te} combinations as used in 
#'        \code{\link{calcIO}}.
#'
#' @return a MAgPIE object
#' 
#' @author Michaja Pehl
#' 
#' @importFrom rlang .data
#' @importFrom readr read_delim cols col_skip col_character
#' @importFrom quitte cartesian interpolate_missing_periods overwrite
#'             character.data.frame interpolate_missing_periods_
#' @importFrom dplyr mutate inner_join group_by summarise anti_join left_join
#'             tbl_df rename
#' @importFrom assertr not_na assert
#' @importFrom tidyr spread gather complete nesting_
#' @importFrom magclass getRegions getYears getNames

fix_IEA_data_for_Industry_subsectors <- function(data, ieamatch) {
  
  # all industry subsector flows
  TOTIND_flows <- c('IRONSTL',  'CHEMICAL', 'NONFERR', 'NONMET',   'TRANSEQ', 
                    'MACHINE',  'MINING',   'FOODPRO', 'PAPERPRO', 'WOODPRO', 
                    'CONSTRUC', 'TEXTILES')
  
  # all products associated with those flows
  products_to_fix <- ieamatch %>%
    filter(!!sym('iea_flows') %in% TOTIND_flows) %>%
    getElement('iea_product') %>%
    unique()

  region_mapping <- read_delim(
    file = toolGetMapping(name = getConfig('regionmapping'), type = 'regional', 
                          returnPathOnly = TRUE),
    delim = ';',
    col_names = c('country', 'iso3c', 'region'),
    col_types = cols(country = col_skip(),
                     iso3c   = col_character(),
                     region  = col_character()),
    skip = 1)

  # ---- extend industry subsector time series ----
  # subset of data containing industry subsector products and flows
  data_industry <- data[,,cartesian(products_to_fix,
                                    c(TOTIND_flows, 'TOTIND', 'INONSPEC'))] %>%
    as.data.frame() %>%
    as_tibble() %>% 
    select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2',
           value = 'Value') %>%
    mutate(year = as.integer(as.character(.data$year))) %>%
    filter(0 != .data$value) %>%
    inner_join(region_mapping, 'iso3c') %>%
    assert(not_na, .data$region)

  # all products that are consumed only in the non-specified subsector of
  # industry are "suspicious" and are therefore fixed
  data_to_fix <- inner_join(
    data_industry %>%
      filter('TOTIND' != !!sym('flow')) %>%
      group_by(!!sym('iso3c'), !!sym('region'), !!sym('year'), 
               !!sym('product')) %>%
      summarise(total = sum(!!sym('value'), na.rm = TRUE), .groups = 'drop'),

    data_industry %>%
      filter(.data$flow %in% c('TOTIND', 'INONSPEC')) %>%
      spread(.data$flow, .data$value),

    c('iso3c', 'region', 'year', 'product')
  ) %>%
    filter(  abs(1 - (.data$total / .data$TOTIND)) > 1e-3 
           | .data$INONSPEC == .data$TOTIND) %>%
    select(.data$iso3c, .data$region, .data$year, .data$product, .data$TOTIND)

  # use all non-suspicious data to calculate regional and global averages
  data_for_fixing <- anti_join(
    data_industry %>%
      filter('TOTIND' != !!sym('flow')),

    data_to_fix %>%
      select(-'TOTIND'),

    c('iso3c', 'region', 'year', 'product')
  ) %>% 
    as_tibble()

  data_for_fixing <- full_join(
    # compute global averages
    data_for_fixing %>%
      group_by(!!!syms(c('year', 'product', 'flow'))) %>%
      summarise(value = sum(!!sym('value')), .groups = 'drop_last') %>%
      mutate(global_share = !!sym('value') / sum(!!sym('value'))) %>%
      ungroup() %>% 
      select(-'value') %>%
      # and expand to all regions
      mutate(region = NA_character_) %>%
      complete(nesting(!!!syms(c('year', 'product', 'flow', 'global_share'))),
               region = unique(region_mapping$region)),

    # compute regional averages
    data_for_fixing %>%
      group_by(!!!syms(c('year', 'region', 'product', 'flow'))) %>%
      summarise(value = sum(!!sym('value')), .groups = 'drop_last') %>%
      mutate(regional_share = !!sym('value') / sum(!!sym('value'))) %>%
      ungroup() %>%
      select(-'value'),

    c('year', 'region', 'product', 'flow')
  ) %>%
    # use regional averages if available, global averages otherwise
    mutate(value = ifelse(!is.na(!!sym('regional_share')), 
                          !!sym('regional_share'),
                          !!sym('global_share'))) %>%
    select(-'regional_share', -'global_share') %>% 
    interpolate_missing_periods_(
      periods = list(year = sub('^y([0-9]{4})$', '\\1', getYears(data)) %>% 
                       as.integer() %>% 
                       sort()), 
      expand.values = TRUE, method = 'linear') %>%
    group_by(!!!syms(c('year', 'region', 'product'))) %>% 
    mutate(value = !!sym('value') / sum(!!sym('value'))) %>% 
    ungroup()

  # calculated fixed data
  data_industry_fixed <- left_join(
    data_to_fix,
    data_for_fixing,
    c('region', 'year', 'product')
  ) %>%
    # replace "suspicious" data with averages
    mutate(value = !!sym('TOTIND') * !!sym('value')) %>%
    select('iso3c', 'region', 'year', 'product', 'flow', 'value') %>%
    assert(not_na, !!sym('value')) %>%
    overwrite(data_industry) %>%
    select(COUNTRY = 'iso3c', TIME = 'year', PRODUCT = 'product', 
           FLOW = 'flow', Value = 'value') %>%
    as.magpie()

  # replace fixed data
  data[getRegions(data_industry_fixed),
       getYears(data_industry_fixed),
       getNames(data_industry_fixed)] <- data_industry_fixed
  
  # ---- factor replacement function ----
  calc_industry_input_factors <- function(data_trans, data) {
    # This helper function takes a data frame with columns region, year, 
    # product, and value.  It calculates the factors with which outputs
    # (positive values) will be replaced by inputs (negative values) in the 
    # IRONSTL subsector and the rest of industry (TOTIND - IRONSTL), allocating
    # all transformation losses to IRONSTL.
    # FIXME: industry transformation losses or **all** transformation losses?
    
    # net inputs and outputs, disregarding E/T flow differences
    data_trans <- data_trans %>% 
      group_by(!!sym('region'), !!sym('year'), !!sym('product')) %>% 
      summarise(value = sum(!!sym('value')), .groups = 'drop') %>% 
      character.data.frame()
    
    outputs_trans <- data_trans %>% 
      filter(0 < !!sym('value')) %>% 
      distinct(!!sym('product')) %>% 
      pull('product')
    
    full_join(
      # coke oven inputs  
      data_trans %>% 
        filter(0 > !!sym('value')) %>% 
        group_by(!!sym('region'), !!sym('year'), !!sym('product')) %>% 
        summarise(inputs = abs(sum(!!sym('value'))), .groups = 'drop'),
      
      # blast furnace outputs
      data_trans %>% 
        filter(0 < !!sym('value')) %>% 
        group_by(!!sym('region'), !!sym('year')) %>% 
        summarise(outputs = sum(!!sym('value')), .groups = 'drop'),
      
      c('region', 'year')
    ) %>% 
      assert(not_na, everything()) %>%
      full_join(
        data[,,cartesian(outputs_trans, c('TFC', 'IRONSTL')), 
             pmatch = 'left'] %>% 
          as.data.frame() %>% 
          as_tibble() %>% 
          filter(0 != !!sym('Value')) %>% 
          select(iso3c = 'Region', year = 'Year', name = 'Data2',
                 value = 'Value') %>% 
          character.data.frame() %>% 
          mutate(year = as.integer(as.character(!!sym('year')))) %>% 
          inner_join(region_mapping, 'iso3c') %>% 
          group_by(!!sym('region'), !!sym('year'), !!sym('name')) %>% 
          summarise(value = sum(!!sym('value')), .groups = 'drop') %>% 
          pivot_wider() %>% 
          mutate(non_steel = !!sym('TFC') - !!sym('IRONSTL')),
        
        c('region', 'year')
      ) %>% 
      assert(not_na, everything()) %>% 
      mutate(
        industry_share_inputs  = !!sym('TFC') / !!sym('outputs'),
        non_steel_share_inputs = !!sym('non_steel') / !!sym('TFC')
                               * !!sym('industry_share_inputs'),
        steel_share_inputs     = !!sym('industry_share_inputs') 
                               - !!sym('non_steel_share_inputs')) %>% 
      select('region', 'year', 'product', 
             'steel_share_inputs', 'non_steel_share_inputs')
  }
  
  # ---- calculate factors to replace blast furnace outputs ----
  # get subset of data pertaining to blast furnaces
  flows_BLASTFUR <- grep('^(TOTAL|MRENEW)', 
                         grep('[ET]BLASTFUR', getNames(data), value = TRUE), 
                         value = TRUE, invert = TRUE)
  
  data_BLASTFUR <- data[,,flows_BLASTFUR] %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2', 
           value = 'Value') %>% 
    filter(0 != !!sym('value')) %>% 
    mutate(year = as.integer(as.character(!!sym('year'))))
  
  # save output products from blast furnaces for replacement further upstream
  # (coke ovens) and downstream (industry)
  outputs_BLASTFUR <- data_BLASTFUR %>% 
    filter(0 < !!sym('value')) %>% 
    getElement('product') %>% 
    unique() %>% 
    as.character()
  
  # calculate the factor with which blast furnace outputs (i.e. industry inputs) 
  # are replaced by blast furnace inputs
  factors_BLASTFUR <- data_BLASTFUR %>% 
    inner_join(region_mapping, 'iso3c') %>% 
    group_by(!!sym('region'), !!sym('year'), !!sym('product'), 
             !!sym('flow')) %>% 
    summarise(value = sum(!!sym('value')), .groups = 'drop') %>% 
    calc_industry_input_factors(data)

  # ---- replace coke oven outputs with coke oven inputs ----
  
  # get subset of data pertaining to coke ovens
  flows_COKEOVS <- grep('^(TOTAL|MRENEW)', 
                         grep('[ET]COKEOVS', getNames(data), value = TRUE), 
                         value = TRUE, invert = TRUE)

  data_COKEOVS <- data[,,flows_COKEOVS] %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2',
           value = 'Value') %>% 
    filter(0 != !!sym('value')) %>% 
    mutate(year = as.integer(as.character(!!sym('year'))))
  
  # save output products from coke ovens for replacement downstream (industry)
  outputs_COKEOVS <- data_COKEOVS %>% 
    filter(0 < !!sym('value')) %>% 
    getElement('product') %>% 
    unique() %>% 
    as.character()

  # replace inputs into coke ovens, that are themselves outputs of blast 
  # furnaces, by the appropriate amount of blast furnace inputs
  data_COKEOVS <- bind_rows(
    # for each Joule of blast furnace output, use factor Joules of all blast 
    # furnace inputs instead
    data_COKEOVS %>% 
      filter(!!sym('product') %in% outputs_BLASTFUR) %>% 
      inner_join(region_mapping, 'iso3c') %>% 
      group_by(!!sym('region'), !!sym('year'), !!sym('product')) %>% 
      summarise(value = sum(!!sym('value')), .groups = 'drop') %>% 
      rename(product.replace = 'product') %>% 
      # for replacing coke oven inputs, use total industry factors
      inner_join(
        factors_BLASTFUR %>% 
          pivot_longer(c(-'region', -'year', -'product')) %>% 
          group_by(!!sym('region'), !!sym('year'), !!sym('product')) %>% 
          summarise(factor = sum(!!sym('value')), .groups = 'drop'), 
        
        c('region', 'year')) %>% 
      group_by(!!sym('region'), !!sym('year'), !!sym('product')) %>% 
      summarise(value = sum(!!sym('value') * !!sym('factor')), 
                .groups = 'drop'),
    
    data_COKEOVS %>% 
      filter(!(!!sym('product') %in% outputs_BLASTFUR)) %>% 
      inner_join(region_mapping, 'iso3c') %>% 
      group_by(!!sym('region'), !!sym('year'), !!sym('product')) %>% 
      summarise(value = sum(!!sym('value')), .groups = 'drop')
  ) %>% 
    group_by(!!sym('region'), !!sym('year'), !!sym('product')) %>% 
    summarise(value = sum(!!sym('value')), .groups = 'drop')
  
  # calculate the factor with which coke oven outputs (i.e. industry inputs) 
  # are replaced by coke oven inputs
  factors_COKEOVS <- calc_industry_input_factors(data_COKEOVS, data)
  
  # ---- replace blast furnace and coke oven outputs by respective inputs ----
  
  # get data that needs to be replaced: blast furnace and coke oven outputs that
  # are inputs in industry sectors
  REMIND_industry_flows <- c(
    TOTIND_flows, 
    'INONSPEC', 'AGRICULT', 'FISHING',
    'MAINELEC', 'AUTOELEC', 'MAINCHP', 'AUTOCHP', 'MAINHEAT', 'AUTOHEAT',
    'NONENUSE')
  
  replace_product.flow <- cartesian(c(outputs_BLASTFUR, outputs_COKEOVS), 
                                    REMIND_industry_flows)
  
  data_to_fix <- data[,,replace_product.flow] %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    select(iso3c = 'Region', year = 'Year', product = 'Data1', flow = 'Data2', 
           value = 'Value') %>% 
    filter(0 != !!sym('value')) %>% 
    mutate(year = as.integer(as.character(!!sym('year'))))
  
  # replace blast furnace outputs
  data_for_fixing <- bind_rows(
    data_to_fix %>% 
      filter(!!sym('product') %in% outputs_BLASTFUR,
             !!sym('flow') %in% REMIND_industry_flows,
             0 != !!sym('value')) %>% 
      select(-'product') %>% 
      inner_join(region_mapping, 'iso3c') %>% 
      inner_join(factors_BLASTFUR, c('region', 'year')) %>% 
      group_by(!!sym('iso3c'), !!sym('year'), !!sym('product'), 
               !!sym('flow')) %>% 
      summarise(
        value = sum(!!sym('value') * ifelse('IRONSTL' == !!sym('flow'),
                                            !!sym('steel_share_inputs'),
                                            !!sym('non_steel_share_inputs'))), 
                .groups = 'drop') %>% 
      character.data.frame(),
    
    data_to_fix %>% 
      filter( !(!!sym('product') %in% outputs_BLASTFUR)
            | !(!!sym('flow')    %in% REMIND_industry_flows)) %>% 
      character.data.frame()
  )
  
  # replace coke oven outputs
  data_for_fixing <- bind_rows(
    data_for_fixing %>% 
      filter(!!sym('product') %in% outputs_COKEOVS,
             !!sym('flow') %in% REMIND_industry_flows,
             0 != !!sym('value')) %>% 
      select(-'product') %>% 
      inner_join(region_mapping, 'iso3c') %>% 
      inner_join(factors_COKEOVS, c('region', 'year')) %>% 
      group_by(!!sym('iso3c'), !!sym('year'), !!sym('product'), 
               !!sym('flow')) %>% 
      summarise(
        value = sum(!!sym('value') * ifelse('IRONSTL' == !!sym('flow'),
                                            !!sym('steel_share_inputs'),
                                            !!sym('non_steel_share_inputs'))),
        .groups = 'drop') %>% 
      character.data.frame(),
    
    data_for_fixing %>% 
      filter(!( !!sym('product') %in% outputs_COKEOVS 
              & !!sym('flow') %in% REMIND_industry_flows))
  ) %>% 
    group_by(!!sym('iso3c'), !!sym('year'), !!sym('product'), 
             !!sym('flow')) %>% 
    summarise(value = sum(!!sym('value')), .groups = 'drop') %>% 
    character.data.frame() %>% 
    rename(Region = 'iso3c', Year = 'year', Data1 = 'product', Data2 = 'flow',
           Value = 'value') %>%
    as.magpie(spatial = 'Region', temporal = 'Year')
  
  # replace NAs with zeros
  data_for_fixing[is.na(data_for_fixing)] <- 0
  
  # replace
  regions_keep <- getRegions(data)
  years_keep   <- getYears(data)
  names_keep   <- setdiff(getNames(data), replace_product.flow)
  
  regions_replace <- getRegions(data_for_fixing)
  years_replace   <- getYears(data_for_fixing)
  names_replace   <- getNames(data_for_fixing)
  
  data_fixed <- new.magpie(cells_and_regions = sort(regions_keep), 
                    years = sort(years_keep),
                    names = unique(c(names_keep, names_replace)),
                    fill = 0)
  
  data_fixed[regions_keep,years_keep,names_keep] <- (
    data[regions_keep,years_keep,names_keep])
  
  data_fixed[regions_replace,years_replace,names_replace] <- (
      data_fixed[regions_replace,years_replace,names_replace]
    + data_for_fixing[regions_replace,years_replace,names_replace])
  
  return(data_fixed)
}
