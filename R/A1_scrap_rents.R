# Scrap house, apartments and comercial real-estate rent and sell prices for 12 main Brazilian cities
# Runs every month
# SOurce: Zap Imoveis, biggest real-estate website in Brazil


# Load libraries
source('setup.R')


option <- 'alguel'
type <- 'casas'
cidade <- 'go+goiania'

scrap_prices <- function(type='all',option='all',cidade){ # Function to scrap real-estate prices
  
  message('Working on ', cidade)
  
  df_city <- cities[city == cidade]
  
  scrap_option <- function(option){ # option: aluguel (rent) or venda (buy)
    
    message('Working on ', option)
    
    scrap_type <- function(type){ # type: apartment, house, or comercial places
      
      message('Working on ', type)
      
      link <- paste0('https://www.zapimoveis.com.br/aluguel/',type,'/',cidade,'/?pagina=') # search link from ZapImoveis
      
      nodes <- c('.simple-card__listing-prices', # CSS of prices, description and addres
                 '.simple-card__text', 
                 '.simple-card__address', 
                 '.simple-card__amenities')
      
      names <- c('prices','description','address','amenities')
      
      results <- ralger::titles_scrap(paste0(link,1)) %>% as.data.frame() # Extract number of results to limit range of search
      results <- stringr::str_split_fixed(results[1,1],' ',n=2) %>% as.data.frame()
      results <- as.numeric(gsub('[.]','', results$V1))
      
      range <- seq(1,round(results/24,0),1) # 24 results per page
      
      df <- ralger::tidy_scrap(paste0(link,range), nodes = nodes, colnames = names) %>% data.table::setDT()
      
      df <- df[, prices := stringr::str_squish(prices)] # Remove excessive whitespace
      df <- df[, amenities := stringr::str_squish(amenities)]
      
      prices1 <- stringr::str_split_fixed(df$prices, ' /mês', n= 2) %>% as.data.frame() # Cleans rent/buy price, land tax etc
      prices2 <- stringr::str_split_fixed(prices1$V2, 'IPTU', n= 2) %>% as.data.frame()
      
      df <- df[, rent := prices1$V1]
      df <- df[, cond := gsub('condomínio','',prices2$V1)]
      df <- df[, iptu := prices2$V2]
      
      amenities1 <- stringr::str_split_fixed(df$amenities,' ',5) %>% as.data.frame() # Cleans amenities (area, no of bedrooms)
      
      df <- df[, area_m2 := amenities1$V1]
      df <- df[, bedroom := amenities1$V3]
      df <- df[, vagas := amenities1$V4]
      df <- df[, baths := amenities1$V5]
      
      df <- df[,.(rent,cond,iptu,description,address,area_m2,bedroom,vagas,baths)]
      
      readr::write_rds(df, here::here('data',option,df_city$city_ab,paste0(type,'_',df_city$city_ab,'.rds'))) # Saves it
      
    }
    
      if (type == "all") { # All types
      
        x = c('apartamentos','casas','loja-salao','conjunto-comercial-sala','casa-comercial','predio-inteiro')
      
      } else (x = type)
    
      purrr::walk(.x = x,.f = scrap_type) # Apply function 1 (all types)
  
  }
  
    if (option == "all") { # All options
    
    y = c('aluguel','venda')
    
    } else (y = option)
  
  purrr::walk(.x =y,.f = scrap_option) # Apply function 2 (both options)
  
}

cidades <- cities$city # 12 biggest brazilian cities

purrr::map(.x=cidades,.f=scrap_prices) # Apply wrapper function