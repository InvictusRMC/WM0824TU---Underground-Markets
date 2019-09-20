library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)



### General statements

# Import csv files
items <- read.csv("items.csv")
feedbacks <- read.csv("feedbacks.csv")

# Set constants

marketplaces <- c("Alphabay", "Silk Road 1", "Evolution", "Black Market Reloaded", "Agora", "Pandora", "Hydra", "Silk Road 2")
# All
categories <- c("malware", "app", "website", "hosting", "exploits", "botnet", "e-mail", "phone", "RAT", "other - account", "other - custom", "other - fake", "other - guide", "other - pirated software", "other - voucher/invite/codes/lottery/gift", "other")
# Without other
categories1 <-c("malware", "app", "website", "hosting", "exploits", "botnet", "e-mail", "phone", "RAT", "cash-out")
# Without "cash-out" & other
categories2 <-c("malware", "app", "website", "hosting", "exploits", "botnet", "e-mail", "phone", "RAT")
# Only other
categories3 <- c("other - account", "other - custom", "other - fake", "other - guide", "other - pirated software", "other - voucher/invite/codes/lottery/gift", "other")
### Graph for Average turnover / marketplace / day

# colors
cols <- c("red", "green", "yellow", "blue", "orange", "purple", "cyan", "magenta", "darkolivegreen1", "pink", "aquamarine4", "lavender", "brown", "beige", "maroon", "aquamarine", "darkolivegreen", "burlywood1", "navy", "grey", "black")


### Graph for total revenue per category

out <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("marketplace", "category", "amount", "normalized")
colnames(out) <- x

for (mp in marketplaces) {
    orders <- subset(feedbacks, marketplace == mp)
    total_revenue = sum(orders$order_amount_usd, na.rm = TRUE)
    
    amounts <- data.frame(matrix(ncol = 2, nrow = 0))
    x <- c("category", "amount")
    colnames(amounts) <- x
    
    for (ct in categories) {
      tmp <- subset(orders, category == ct)
      revenue_ct = sum(tmp$order_amount, na.rm = TRUE)
      normalized_revenue_ct = round( ((revenue_ct / total_revenue)*100), digits=2)
      value = c(mp, ct, revenue_ct, normalized_revenue_ct)
      #amounts[nrow(amounts)+1, ] <- value
      if (normalized_revenue_ct != 0){
        out[nrow(out)+1, ] <- value
      }
    }
}

p <- ggplot(out, aes(x = marketplace, y = as.numeric(amount), fill = category, linetype = category)) + 
  geom_bar(data=out, position = "fill", stat = "identity") +
  #scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=0), legend.key.size = unit(0.5, "cm")) +
  xlab("Marketplaces") + 
  ylab("Categories") +
  scale_fill_manual(values=cols)


### Graph for average revenue  per category per month
out <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("date", "category", "order_amount")
colnames(out) <- x

for (ct in categories) {
  if (ct != "") {
    print(ct)
    orders <- subset(feedbacks, category == ct)
    orders$date <- format(as.Date(orders$date), "%Y-%m")
    dates <- orders$date
    unique_dates <- unique(orders$date)
    turnover <- orders$order_amount_usd
    df <- data.frame(date = dates, category=orders$category, order_amount = turnover)
    
    for (dt in unique_dates) {
      tmp <- subset(df, date == dt)
      length_tmp <- nrow(tmp)
      tmp <- ddply(tmp,"date", numcolwise(sum))
      tmp$category <- ct
      tmp$date <- paste(dt, "15", sep="-")
      tmp$order_amount <- tmp$order_amount / 1000
      out <- rbind(out, tmp)
    }
  }
}

out$date <- as.Date(out$date)
out <- out[order(out$date),]
p3 <- ggplot(data=out, aes(x=date, order_amount, group=category)) + geom_line(aes(color = category, linetype = category)) + 
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  labs(x = "Date (Month)", y = "Turnover (1000 USD)") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Total Turnover per Category per Month (Only Other)")

############################################
### Graph Total turnover per marketplace ###
############################################

#counter <- 0

# Set data frame for ouput
out <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("date", "marketplace", "order_amount")
colnames(out) <- x

# iterate over mps
for (mp in marketplaces) {
  if (mp != "") {
    print(mp)
    
    # Get all feedbacks for current mp
    orders <- subset(feedbacks, marketplace == mp)
    
    # Rip out days
    orders$date <- format(as.Date(orders$date), "%Y-%m")
    
    # Dates
    dates <- orders$date
    
    # Unique dates
    unique_dates <- unique(orders$date)
    
    # Turnover
    turnover <- orders$order_amount_usd
    
    # DF with only categories, dates & amounts
    df <- data.frame(date = dates, category=orders$category, order_amount = turnover)
    
    #out <- data.frame(matrix(ncol = 3, nrow = 0))
    #x <- c("date", "marketplace", "order_amount")
    #colnames(out) <- x
    
    # Loop over all unique dates
    for (dt in unique_dates) {
      # Get all feedbacks with current date
      tmp <- subset(df, date == dt)
      
      # Amount of feedbacks
      length_tmp <- nrow(tmp)
      
      # Sum all amounts, leading to 1 entry
      tmp <- ddply(tmp,"date", numcolwise(sum))
      
      # Divide by 1000 readability
      tmp$order_amount <- tmp$order_amount / 1000
      
      # Store mp
      tmp$marketplace <- mp
      
      # Set to middle of month
      tmp$date <- paste(dt, "15", sep="-")
      
      # Store
      out <- rbind(out, tmp)
    }
  }
}

# Sort on date
out$date <- as.Date(out$date)
out <- out[order(out$date),]

# Plot
p1 <- ggplot(data=out, aes(x=date, order_amount, group=marketplace)) + 
  geom_line(aes(color = marketplace, linetype = marketplace)) + 
  scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
  labs(x = "Time (Month)", y = "Total Turnover per Marketplace (1000 USD)") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Total Turnover per Marketplace per Month")


### Final plot

out <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("vendor", "marketplace", "amount")
colnames(out) <- x

no_listings <- nrow(items)
unique_vendors <- unique(items$vendor_hash) 
unique_vendors <- unique_vendors[unique_vendors != ""]

for (vr in unique_vendors) {
  if (vr != "") {
    vr_listings <- subset(items, vendor_hash == vr)
    unique_mps <- unique(vr_listings$marketplace)
    
    for (mp in unique_mps) {
      if (mp != "") {
        tmp <- subset(vr_listings, marketplace == mp)
        no_listings_vr <- nrow(tmp);
        out[nrow(out)+1, ] <- c(vr, mp, no_listings_vr)
      }
    }
  }
}
