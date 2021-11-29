# data_load <- function(x){

# Load references

filename <- "./Dataset/R/References.RData"
load(filename)

# Load aggregate balance sheet

filename <- "./Dataset/R/Aggregate_bs.RData"
load(filename)

# Load individual balance sheets

filename <- "./Dataset/R/Individual_bs.RData"
load(filename)

# Load Turnover

load("./Dataset/R/Turnover.RData")



# # Census
# 
# load("./Dataset/R/Census.RData")
# 
# # Interest Rates
# 
# load("./Dataset/R/Interest Rates.RData")





# if (x == 1){  
#   list_load <- list(r,a,i,g) 
#   return(list_load)
# } else {
#   list_load <- list(mget(r),mget(a),mget(i)) 
#   return(list_load)
#   }


# Census

# filename <- "./Dataset/R/Census_1901.Rds"
# df_c1 <- readRDS(filename)
# 
# filename <- "./Dataset/R/Census_1911.Rds"
# df_c11 <- readRDS(filename)
# 
# # Securities
# 
# filename <- "./Dataset/R/LSE.rds"
# df_lse <- readRDS(filename)
# 
# # Others
# 
# filename <- "./Dataset/R/Losses.rds"
# df_losses <- readRDS(filename)
# 
# filename <- "./Dataset/R/Turnover.rds"
# df_turn <- readRDS(filename)
# 
# filename <- "./Dataset/R/USnotes.rds"
# df_usnotes <- readRDS(filename)
# 
# filename <- "./Dataset/R/Curtis_Dominion.rds"
# df_dom <- readRDS(filename)
# 
# filename <- "./Dataset/R/Curtis_Price Index.rds"
# df_pi <- readRDS(filename)

# }
