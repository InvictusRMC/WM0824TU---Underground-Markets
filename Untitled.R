library(ggplot2)
library(ggthemes)
library(extrafont)
library(plyr)
library(scales)

items <- read.csv("items.csv")
feedbacks <- read.csv("feedbacks.csv")

colourCount = length(categories)
getPalette = colorRampPalette(brewer.pal(9, "Set3"))

marketplaces <- c("Alphabay", "Silk Road 1", "Evolution", "Black Market Reloaded", "Agora", "Pandora", "Hydra", "Silk Road 2")
categories <- c("malware", "app", "website", "hosting", "exploits", "botnet", "e-mail", "phone", "RAT", "cash-out")
categories1 <-c("malware", "app", "website", "hosting", "exploits", "botnet", "e-mail", "phone", "RAT", "cash-out")
categories2 <- c("other - account", "other - custom", "other - fake", "other - guide", "other - pirated software", "other - voucher/invite/codes/lottery/gift", "other")
### Graph for Average turnover / marketplace / day
cols <- c("blue", "black", "red", "green", "purple", "orange", "brown", "yellow", "darkolivegreen1", "goldenrod4", "deeppink", "darkslategrey", "lightcoral", "mediumorchid1", "mediumvioletred", "red4", "thistle4")

lim_min <- min(as.Date(feedbacks$date), na.rm = TRUE)
lim_max <- max(as.Date(feedbacks$date), na.rm = TRUE)
marketplaces <- unique(items$marketplace)
marketplaces <- marketplaces[marketplaces != ""]
counter <- 0
for (mp in marketplaces) {
  if (mp != "") {
    print(mp)
    orders <- subset(feedbacks, marketplace == mp)
    dates <- orders$date
    unique_dates <- unique(orders$date)
    turnover <- orders$order_amount_usd
    df <- data.frame(date = dates, order_amount = turnover)
  
    out <- data.frame(matrix(ncol = 2, nrow = 0))
    x <- c("date", "order_amount")
    colnames(out) <- x
    
    for (dt in unique_dates) {
      tmp <- subset(df, date == dt)
      length_tmp <- nrow(tmp)
      tmp <- ddply(tmp,"date", numcolwise(sum))
      tmp$order_amount <- tmp$order_amount/length_tmp
      out <- rbind(out, tmp)
    }
    out$date <- as.Date(out$date)
    out <- out[order(out$date),]
    if (counter == 0) {
      plot(out$date, out$order_amount, xlim=c(lim_min, lim_max), ylim=c(0, 600), type="l", col=cols[counter])
    } else {
      lines(out$date, out$order_amount, col=cols[counter])
    }
    counter <- counter + 1
  }
}


### Graph for total revenue per category
counter <- 0

# out <- data.frame(matrix(ncol = length(marketplaces), nrow = length(categories)))
# x <- marketplaces
# y <- categories
# colnames(out) <- x
# rownames(out) <- y
out <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("marketplace", "category", "amount", "normalized")
colnames(out) <- x
counter <- 0

for (mp in marketplaces) {
    orders <- subset(feedbacks, marketplace == mp)
    total_revenue = sum(orders$order_amount_usd, na.rm = TRUE)
    
    amounts <- data.frame(matrix(ncol = 2, nrow = 0))
    x <- c("category", "amount")
    colnames(amounts) <- x
    
    #amounts <- data.frame(matrix(ncol = 3, nrow = 0))
    #x <- c("marketplace", "category", "amount")
    #colnames(amounts) <- x
    
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
    
    # out[mp] <- amounts$amount
    # out <- rbind(out, amounts)
}

barplot(names.arg=marketplaces, cex.names=0.5, width=50, as.matrix(out))

p1 <- ggplot(data=out, aes(x=marketplace, y=amount, fill=category)) + geom_col(aes(fill = category), width = 0.7)

p <- ggplot(out, aes(x = marketplace, y = as.numeric(amount), fill = category)) + 
  geom_bar(position = "fill",stat = "identity") +
  #scale_y_continuous(labels = scales::percent_format()) + 
  theme(axis.text.x = element_text(angle=0), legend.key.size = unit(0.5, "cm")) +
  xlab("Marketplaces") + 
  ylab("Categories")


  


plot <- ggplot(out, aes(class))





### Graph for average revenue  per category per month
lim_min <- min(as.Date(feedbacks$date), na.rm = TRUE)
lim_max <- max(as.Date(feedbacks$date), na.rm = TRUE)
#lim_min <- format(as.Date("2011-06-07")) format="%Y-%m")
#lim_max <- as.Date("2017-05", format="%Y-%m")
categories <- categories2

output <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("date", "category", "order_amount")
colnames(output) <- x

counter <- 0

for (ct in categories) {
  if (ct != "") {
    print(ct)
    orders <- subset(feedbacks, category == ct)
    orders$date <- format(as.Date(orders$date), "%Y-%m")
    dates <- orders$date
    unique_dates <- unique(orders$date)
    turnover <- orders$order_amount_usd
    df <- data.frame(date = dates, category=orders$category, order_amount = turnover)

    out <- data.frame(matrix(ncol = 3, nrow = 0))
    x <- c("date", "category", "order_amount")
    colnames(out) <- x
    
    for (dt in unique_dates) {
      tmp <- subset(df, date == dt)
      length_tmp <- nrow(tmp)
      tmp <- ddply(tmp,"date", numcolwise(sum))
      tmp$order_amount <- total_revenue
      tmp$category <- ct
      tmp$date <- paste(dt, "15", sep="-")
      out <- rbind(out, tmp)
      output <- rbind(output, tmp)
    }
    
    out$date <- as.Date(out$date)
    out <- out[order(out$date),]
    if (counter == 0) {
      p3 <- ggplot(data=out, aes(x=date, order_amount, group=category)) + geom_line(aes(color = category, linetype = category)) + 
        scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
        labs(x = "Month", y = "Average Turnover per Category") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Average Turnover per Category per Month")
      #plot(out$date, out$order_amount, xlim=c(lim_min, lim_max), ylim=c(0, 3000), type="l", col=cols[counter])
      #legend(x="topright", y=0.95, legend=categories,
      #       col=cols, lty=1:2, cex=0.8)
    } else {
      p3 <- p3 + geom_line(data=out, aes(x=date, order_amount, group=category, color = category, linetype = category))
    }
    counter <- counter + 1
  }
}





out3 <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("category", "amount", "date")
colnames(out3) <- x

categories <- c("cash-out")
for (ct in categories) {
  print(ct)
  orders <- subset(feedbacks, category == ct)
  entries <- data.frame(date=orders$date, amount=orders$order_amount_usd)
  unique_dates <- unique(orders$date)
  
  
  for (dt in unique_dates) {
    write(dt)
    date_entries <- subset(entries, date=dt)
    ct_value = sum(date_entries$amount, na.rm = TRUE);
    normalized = ct_value / nrow(date_entries)
    out3[nrow(out3)+1, ] <- c(ct, normalized, dt)
  }

  
}

out3 <- out3[order(out3$date),]
p2 <- ggplot(data=out3, aes(x=as.Date(date), y=amount)) + geom_line()


### Graph 1
counter <- 0
for (mp in marketplaces) {
  if (mp != "") {
    print(mp)
    orders <- subset(feedbacks, marketplace == mp)
    orders$date <- format(as.Date(orders$date), "%Y-%m")
    dates <- orders$date
    unique_dates <- unique(orders$date)
    turnover <- orders$order_amount_usd
    df <- data.frame(date = dates, category=orders$category, order_amount = turnover)
    
    out <- data.frame(matrix(ncol = 3, nrow = 0))
    x <- c("date", "marketplace", "order_amount")
    colnames(out) <- x
    
    for (dt in unique_dates) {
      tmp <- subset(df, date == dt)
      length_tmp <- nrow(tmp)
      tmp <- ddply(tmp,"date", numcolwise(sum))
      tmp$order_amount <- tmp$order_amount/length_tmp
      tmp$marketplace <- mp
      tmp$date <- paste(dt, "15", sep="-")
      out <- rbind(out, tmp)
    }
    
    out$date <- as.Date(out$date)
    out <- out[order(out$date),]
    if (counter == 0) {
      p1 <- ggplot(data=out, aes(x=date, order_amount, group=marketplace)) + geom_line(aes(color = marketplace, linetype = marketplace)) + 
        scale_x_date(date_breaks = "1 year", labels = date_format("%Y")) +
        labs(x = "Month", y = "Average Turnover per Category") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5)) + ggtitle("Average Turnover per Marketplace per Month")
      #plot(out$date, out$order_amount, xlim=c(lim_min, lim_max), ylim=c(0, 3000), type="l", col=cols[counter])
      #legend(x="topright", y=0.95, legend=categories,
      #       col=cols, lty=1:2, cex=0.8)
    } else {
      p1 <- p1 + geom_line(data=out, aes(x=date, order_amount, group=marketplace, color = marketplace, linetype = marketplace))
    }
    counter <- counter + 1
  }
}
