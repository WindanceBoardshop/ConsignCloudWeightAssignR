# This updates products in ConsignCloud using the Consign Cloud API


library(tidyverse)
library(httr2)
print('libraries loaded succesfully')

CCLogin <- list()

#get Github API key from github Secrets
# if you need to ligin its also in an emial somewhere. ;)
CCLogin$API_KEY <- Sys.getenv("CC_API_KEY")



# get all items from ConsignCLoud

url <- "https://api.consigncloud.com/api/v1/items"

# get all tiems that have greater than 0 quantity. 

params <- list(
  "limit" = "100",
  "quantity:gte" = "0",
  "include" = "weight",
  "include" = "quantity",
  "include" = "list_on_shopify"
)


response <- list( next_cursor = "starter_value") #
CC_items <- list()

# LOOP THIS SO IT RETURNS ALL RESULTS uses next_cursor var

while(!is.null(response[["next_cursor"]])){


response <- request(url) %>% 
  req_auth_bearer_token(CCLogin$API_KEY) %>% 
  req_url_query(!!!params) %>%
  req_perform()
  
  
 response <-  resp_body_json(response)
 
# append items to the consign Cloud items list
 
 CC_items <- append(CC_items, response$data)
 
 # update parameters to include new pagination code
 
 params <- list(
   "limit" = "100",
   "quantity:gte" = "0",
   "include" = "weight",
   "include" = "quantity",
   "include" = "list_on_shopify",
   "cursor" = response[["next_cursor"]]
 )
 
 Sys.sleep(1)
 
 }
  
print("consign Cloud items retirieved")
# now that we have THOSE PRODUCTS WE CAN Assign weights based on category
# looks like that category information is totally jacked and wierd sooooooo gottat get that info too and match it up with the items info



url <- "https://api.consigncloud.com/api/v1/item-categories"
# get all tiems that have greater than 0 quantity. 

params <- list(
  "limit" = "100"
)


# LOOP THIS SO IT RETURNS ALL RESULTS uses next_cursor var

  
  CC_categories <- request(url) %>% 
    req_auth_bearer_token(CCLogin$API_KEY) %>% 
    req_url_query(!!!params) %>%
    req_perform() %>% 
    resp_body_json() %>% 
    tibble %>%
    unnest_longer(1) %>% 
    unnest_wider(1)
  
  print("consign CLoud Items Retrieved")
  #   Now unnest and filter for all items that have 0 weight from items list and join with categories list
  #  need to add weights to the case when statements. is there a way to update this so that when a new category is created it can autopoulate in this list??????
  
  CC_items_df <- CC_items %>% 
    tibble %>% 
    unnest_wider(1) %>% 
    filter(weight == 0) %>% # include all items with 0 weight. 
    filter( is.na(deleted)) %>% # exclude all deleted items
    filter(quantity > 0) %>% 
    left_join(CC_categories, by = c("category" = "id")) %>% 
    mutate( weight = case_when(
                                category == "02155493-85cf-40a3-818a-2b65ca3e3598" ~  30,  # Used Wings                     
                                category == "067ec6ab-ec1b-4d17-ace5-1a9fdca9f947" ~  30,  #  Used Foil Masts                
                                category == "0edbff29-4d6a-443e-a843-5263c80a462b" ~  35,  # Used Foil Lower Planes         
                                category == "0f586576-88cc-44ad-9808-81adde4d0d24" ~  25,  #  Used Harnesses                 
                                category == "28adb691-1c65-48e6-aa2b-8dd3ffe53585" ~  15,  # Used Kite Bars                 
                                category == "29a256e9-3480-47b9-98ac-66bfb9c244ec" ~  15,  #  Used Foil Rear Wing Stabilizers
                                category == "4a45320b-24ca-41ac-b014-71ad59c7b071" ~  50,  # Used Kiteboards                
                                category == "775ac34f-4210-4687-8f9b-6450d6f2df72" ~ 250,  #  Used Wing Boards               
                                category == "80413eb2-431f-4f11-af87-fead185870e5" ~  40,  # Used Foil Complete Systems     
                                category == "8853b42a-54e9-4473-b643-03a5fa74c9d5" ~ 175,  #  Used Wing Boards (Inflatable)  
                                category == "9da58dcc-f6ab-40cd-8474-2887aebc5dde" ~  30,  # Used Kites                     
                                category == "ab6ad778-2ebe-4d24-8a0d-3d59b95f3467" ~ 300,  #  Used Windsurf Boards           
                                category == "bd2e12f9-83e7-4dbf-a9be-ac4a0d72f493" ~  40,  # Used Windsurf Sails            
                                category == "bd72d582-937a-46e4-9973-da5bff5bee65" ~  10,  #  Used Misc Gear                 
                                category == "bf2231ce-1258-41e9-b16f-30f5568b6cf4" ~  35,  # Used Windsurf Booms            
                                category == "cc3511b8-a034-48ea-a7cf-1af8a601fc1b" ~  20,  #  Used Foil Fuselages            
                                category == "d2421837-69f2-4ddf-8dda-2a9590f632c7" ~  30,  # Used Foil Front Wings          
                                category == "e23f6f10-3547-466a-8999-8a0c54514e72" ~  35,  #  Used Windsurf Masts            
                                category == "f963cef9-4c38-4f12-9e91-953649581d07" ~  45,
                                category == "" ~ 250)) # Used Board Bags
 

# now I need to upload these item changes to consign cloud

  
# I should also do this: when MSRP is not set, then MSRP = Tag Price. Then We dont need to add tag price. 
  

# update all items with no weights. 
  
if(nrow(CC_items_df)>0){ # check if there are any products to update
  
counter <- 0
  
for(i in 1:nrow(CC_items_df)){
  
 url <- paste0("https://api.consigncloud.com/api/v1/items/", CC_items_df$id[i])
 
  params <- list(
    "weight" = CC_items_df$weight[i],
    "weight_unit" = "pounds"
  )
  
  response <- request(url) %>% 
    req_auth_bearer_token(CCLogin$API_KEY) %>% 
    req_headers("Content-Type" = "application/json") %>%  # Explicitly set JSON format
    req_body_json(params) %>%  # Send data in the request body
    req_method("PATCH") %>% 
    req_perform()
  
  Sys.sleep(10)
  
  counter <- counter +1
  
  print(counter)
  
  }
  
}else{print("No item weights to update")}

# now lets do the same thing with MSRP

CC_items_MSRP <- CC_items %>% 
  tibble %>% 
  unnest_wider(1) %>% 
  unnest_wider(custom_fields, names_sep = '_') %>% 
  unnest_wider(custom_fields_3, names_sep = '_') %>% 
  filter(is.na(custom_fields_3_value)) %>% # include all items with 0 weight. 
  filter(is.na(deleted)) %>% # exclude all deleted items
  filter(quantity >0)

#now update these items with an API patch


# update all items with no weights. 

if(nrow(CC_items_MSRP)>0){ # check if there are any products to update
  
  counter <- 0
  
  for(i in 1:nrow(CC_items_MSRP)){
    
    url <- paste0("https://api.consigncloud.com/api/v1/items/", CC_items_MSRP$id[i])
    
    params <- list(
      "custom_fields_map" = list("msrp" = CC_items_MSRP$tag_price[i])
    )
    
    response <- request(url) %>% 
      req_auth_bearer_token(CCLogin$API_KEY) %>% 
      req_headers("Content-Type" = "application/json") %>%  # Explicitly set JSON format
      req_body_json(params) %>%  # Send data in the request body
      req_method("PATCH") %>% 
      req_perform()
    
    Sys.sleep(10)
    
    counter <- counter +1
    
    print(counter)
    
  }
  
  print("item MSRP's succesfully updated")
  
}else{print("No item MSRP's to update")}

  



################################################################333


