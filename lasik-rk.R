## http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3129752/

library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

data <- read.csv("table1.csv")
for (i in c(3,5,6)) data[,i] <- as.factor(data[,i])

ggplot(data, aes(x = preopSE, y = postopSE, colour = as.factor(postopUCVA))) +
        geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
        geom_point(size = 4) +
        xlab("Pre-op Spherical Equivalent") +
        ylab("Post-op Spherical Equivalent") +
        labs(colour = "Post-op UCVA") +
        scale_colour_brewer(palette = "RdYlGn") +
        scale_x_continuous(breaks = seq(-14, 3)) +
        theme(axis.title.x = element_text(vjust = -0.7),
              axis.title.y = element_text(vjust = 1.5),
              legend.key = element_blank(),
              panel.background = element_rect(fill = "lavender")
        )       

x1 <- table(data$preopBCVA)
x2 <- table(data$postopBCVA)
Preop <- round(cumsum(x1[4:1])/nrow(data), 2)*100
Preop <- as.data.frame(Preop)
Preop$va <- row.names(Preop)
Postop <- round(cumsum(x2[2:1])/nrow(data), 2)*100
Postop <- as.data.frame(Postop)
Postop$va <- row.names(Postop)
df <- left_join(Preop, Postop, by = "va")
df1 <- melt(df, id = "va")
df1$va <- as.factor(df1$va)
df1$va <- factor(df1$va, levels = rev(levels(df1$va)))

ggplot(df1, aes(x = va, y = value)) +
        geom_hline(yintercept = seq(0, 100, 25), color = "grey", linetype = "dashed") +
        geom_bar(stat = "identity", colour = "black", fill = "lavender", width = 0.7) +
        xlab("BCVA") + ylab("Cumulative percentage of observations") +
        facet_wrap(~ variable, scales = "free_y") +
        theme(panel.background = element_blank(),
              panel.grid = element_blank(),
              panel.border = element_rect(fill = NA),
              axis.text = element_text(colour = "gray20")
        )

