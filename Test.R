test2

url_1 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/2b55e11d-7e75-4bbb-b526-69a06c0c4731/download/public_150k_plus_230101.csv"
csv_data_1 <- read.csv(url(url_1), header = TRUE)

url_2 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/5f700a26-02f9-4d97-94a3-e3c2c43871eb/download/public_up_to_150k_1_230101.csv"
csv_data_2 <- read.csv(url(url_2), header = TRUE)

url_3 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/b785dfac-7d99-4bc0-9ab2-e87fe855174e/download/public_up_to_150k_2_230101.csv"
csv_data_3 <- read.csv(url(url_3), header = TRUE)

url_4 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/6899d4ff-7f2a-4455-a18f-592118e8e052/download/public_up_to_150k_3_230101.csv"
csv_data_4 <- read.csv(url(url_4), header = TRUE)

url_5 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/a100fcb3-7708-4683-aa63-e5a594264e21/download/public_up_to_150k_4_230101.csv"
csv_data_5 <- read.csv(url(url_5), header = TRUE)

url_6 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/f4f85ef0-6279-4e81-baac-eefbbc3ebc2d/download/public_up_to_150k_5_230101.csv"
csv_data_6 <- read.csv(url(url_6), header = TRUE)

url_7 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/03bab509-ad0f-4dbd-88f1-99599dbd3dfc/download/public_up_to_150k_6_230101.csv"
csv_data_7 <- read.csv(url(url_7), header = TRUE)

url_8 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/2cea4fbe-2fb5-4307-8d00-5c7203d333f7/download/public_up_to_150k_7_230101.csv"
csv_data_8 <- read.csv(url(url_8), header = TRUE)

url_9 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/35375b26-8bd5-4868-b89d-ab02ccbf2b43/download/public_up_to_150k_8_230101.csv"
csv_data_9 <- read.csv(url(url_9), header = TRUE)

url_10 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/2f6e4ccd-0311-43dc-b721-8bc07f586fa2/download/public_up_to_150k_9_230101.csv"
csv_data_10 <- read.csv(url(url_10), header = TRUE)

url_11 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/d2a0b6cd-414a-44af-9c0d-55259e5ebf20/download/public_up_to_150k_10_230101.csv"
csv_data_11 <- read.csv(url(url_11), header = TRUE)

url_12 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/262eb7fc-e074-45ca-a977-f6d8d223e1b3/download/public_up_to_150k_11_230101.csv"
csv_data_12 <- read.csv(url(url_12), header = TRUE)

url_13 <- "https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/dd54d47b-63e9-41c4-ae13-8e12c8ca4ea1/download/public_up_to_150k_12_230101.csv"
csv_data_13 <- read.csv(url(url_13), header = TRUE)

FDIC <- read.csv("/Users/worthsmacbookair/Desktop/Financials_L1_formatted_3_22_2023.csv")
FDIC_2019 <- read.csv("/Users/worthsmacbookair/Desktop/Financials_L1_2019_formatted_3_22_2023 copy.csv")
library(data.table)
library(parallel)
library(pbapply)

# Define the list of data tables
dt_list <- list(csv_data_1, csv_data_2, csv_data_3, csv_data_4, csv_data_5, csv_data_6, csv_data_7, csv_data_8, csv_data_9, csv_data_10, csv_data_11, csv_data_12, csv_data_13)

for (i in 1:13) {
  setDT(get(paste0("csv_data_", i)))
}

# Combine data tables row-wise using rbindlist
combined_data <- rbindlist(dt_list)
view(combined_data)
# Print combined data table
print(combined_data)
# Set up a cluster
num_cores <- detectCores()
cl <- makeCluster(num_cores)




# Use parLapply() to combine data tables in parallel
combined_data <- pbapply(parLapply(cl, dt_list, function(x) {
  return(x)
}), rbindlist, fill = TRUE)

# Stop the cluster
stopCluster(cl)

#Bindlist
library(data.table)

# Get list of CSV files in working directory
csv_files <- list.files(pattern = "*.csv")

# Create empty data table to store combined data
combined_data <- data.table()

# Loop over CSV files and append to combined_data
for (file in dt_list) {
  # Initialize data table to store chunk of data
  chunk_data <- data.table()
  # Read in CSV file in chunks of 100,000 rows
  for (i in seq(1, 1e6, 1e5)) {
    chunk <- fread(file, skip = i - 1, nrows = 1e5)
    if (nrow(chunk) == 0) {
      break
    }
    chunk_data <- rbindlist(list(chunk_data, chunk))
  }
  # Append chunk of data to combined_data
  combined_data <- rbindlist(list(combined_data, chunk_data))
}

# Set keys to improve performance
#setkey(combined_data, )

# Print first few rows of combined data
head(combined_data)


# Sum of CurrentApprovalAmount by ServicingLenderName
bank_totals_servicing <- aggregate(CurrentApprovalAmount ~ ServicingLenderName, data = csv_data_1, FUN = sum)

# Sort the bank totals in descending order
bank_totals_servicing_sorted <- bank_totals_servicing[order(-bank_totals_servicing$CurrentApprovalAmount), ]

# Get the top 100 totals
bank_totals_servicing_top100 <- head(bank_totals_servicing_sorted, 100)

# Print the top 100 totals
print(bank_totals_servicing_top100)

# Sum of CurrentApprovalAmount by OriginatingLender
bank_totals_originating <- aggregate(CurrentApprovalAmount ~ OriginatingLender, data = csv_data_1, FUN = sum)

# Sort the bank totals in descending order
bank_totals_originating_sorted <- bank_totals_originating[order(-bank_totals_originating$CurrentApprovalAmount), ]

# Get the top 100 totals
bank_totals_originating_top100 <- head(bank_totals_originating_sorted, 100)

# Convert text in the OriginatingLender column to uppercase
bank_totals_originating_top100$OriginatingLender <- toupper(bank_totals_originating_top100$OriginatingLender)


# Print the top 100 totals
print(bank_totals_originating_top100)

library(dplyr)

FDIC <- FDIC %>% 
  mutate(Total.Deposits = Total.Deposits * 1000)
FDIC_2019 <- FDIC_2019 %>% 
  mutate(Total.Deposits = Total.Deposits * 1000)

data_merged_2022 <- merge(bank_totals_originating_top100, FDIC, by.x = "OriginatingLender", by.y = "Institution.Name", all = TRUE)
data_merged_2022$PPP <- 100 * data_merged_2022$CurrentApprovalAmount / data_merged_2022$Total.Deposits
# Order the data table by PPP from biggest to smallest
data_merged_2022_ordered <- data_merged_2022_noNA[order(-data_merged_2022_noNA$PPP), ]

# Print the ordered data table
print(data_merged_2022_ordered)

data_merged_2019 <- merge(bank_totals_originating_top100, FDIC_2019, by.x = "OriginatingLender", by.y = "Institution.Name", all = TRUE)
data_merged_2019$PPP <- 100 * data_merged_2019$CurrentApprovalAmount / data_merged_2019$Total.Deposits
data_merged_2019_noNA <- data_merged_2019[!is.na(data_merged_2019$PPP), ]

# Order the data table by PPP from biggest to smallest
data_merged_2019_ordered <- data_merged_2019_noNA[order(-data_merged_2019_noNA$PPP), ]

# Print the ordered data table
print(data_merged_2019_ordered)
# Create a subset of data_merged_2019_ordered with Total.Deposits >= 1.649e+10
data_subset <- subset(data_merged_2019_ordered, Total.Deposits >= 1.649e+10)
data_subset <- data_subset[-c(23,24),]
# Print the resulting data table
print(data_subset)


# Calculate the mean and median of Total.Deposits
# Obtain a summary of the data
summary_data <- summary(data_subset)

# Print the summary statistics for numeric variables
print(summary_data[, sapply(summary_data, is.numeric)])

data_subset$ticker <- c("PNFP", "FHN", "CFR", "FULT", "FNB", "HWC", "MTB", "VLY", "RY", "HBAN", "FCNCA", "CM", "WAL", "COLB", "BOKF", "UMBF", "CBSH", "SBNY", "KEY", "PWBK","SNV", "CMA",
                        "EWBC", "PB", "BMO", "BMO", "PNC", "RF", "SIVB", "ASB", "BKU", "CFG", "TFC", "TD", "BPOP", "USB", "FRC", "JPM", "BAC", "HSBC", "WFC", "COF")

# Load the quantmod package
library(quantmod)

# Create a vector with the stock symbols
symbols <- c("PNFP", "FHN", "CFR", "FULT", "FNB", "HWC", "MTB", "VLY", "RY", "HBAN", "FCNCA", "CM", "WAL", "COLB", "BOKF", "UMBF", "CBSH", "SBNY", "KEY", "PWBK","SNV", "CMA", "EWBC", "PB", "BMO", "BMO", "PNC", "RF", "SIVB", "ASB", "BKU", "CFG", "TFC", "TD", "BPOP", "USB", "FRC", "JPM", "BAC", "HSBC", "WFC", "COF")

# Download the stock data for the symbols
getSymbols(symbols)

bank_performance <- monthlyReturn(PNFP, FHN, CFR, FULT, FNB, HWC, MTB, VLY, RY, HBAN, FCNCA, CM, WAL, COLB, BOKF, UMBF, CBSH, SBNY, KEY, PWBK,SNV, CMA, EWBC, PB, BMO, BMO, PNC, RF, SIVB, ASB, BKU, CFG, TFC, TD, BPOP, USB, FRC, JPM, BAC, HSBC, WFC, COF)


monthly_returns <- lapply(mget(symbols), monthlyReturn)
monthly_returns_df <- do.call(cbind, monthly_returns)
monthly_returns_last <- data.frame(do.call(rbind, lapply(monthly_returns, tail, n = 1)))

rownames(df) <- c(""PNFP", "FHN", "CFR", "FULT", "FNB", "HWC", "MTB", "VLY", "RY", "HBAN", "FCNCA", "CM", "WAL", "COLB", "BOKF", "UMBF", "CBSH", "SBNY", "KEY", "PWBK","SNV", "CMA", "EWBC", "PB", "BMO", "BMO", "PNC", "RF", "SIVB", "ASB", "BKU", "CFG", "TFC", "TD", "BPOP", "USB", "FRC", "JPM", "BAC", "HSBC", "WFC", "COF"")

