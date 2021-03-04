#Code for Fig. R1 in Savage, P. E., Loui, P., Tarr, B., Schachner, A., Glowacki, L., Mithen, S., & Fitch, W. T. (2021). Author response: Toward inclusive theories of the evolution of musicality. Behavioral and Brain Sciences.
#Code written by Sam Passmore

library(googlesheets4)
library(stringr)
library(ggplot2)
library(ggrepel)
library(irr)

## Get raw codings from google sheets
ratings = read.csv("https://raw.githubusercontent.com/comp-music-lab/social-bonding/main/Ratings%20of%20Savage%20and%20Mehr%20commentaries.csv")

# give Savage responses numbers & Mehr responses letters
ratings$ID = c(1:35, LETTERS[1:25])

# get all columns rating Savage et al.
savage_idx  = str_detect(colnames(ratings), pattern = "Savage.et.al.")
# get all columns rating Mehr et al. 
mehr_idx    = str_detect(colnames(ratings), pattern = "Mehr.et.al.")

colnames(ratings)[savage_idx]
colnames(ratings)[mehr_idx]

average_savage = rowMeans(ratings[,savage_idx], na.rm = TRUE)
average_mehr   = rowMeans(ratings[,mehr_idx], na.rm = TRUE)

# Inter-rater reliability
savage_icc = icc(ratings[,c(3,5)], model="twoway", type="agreement")
mehr_icc = icc(ratings[,c(4,6)], model="twoway", type="agreement")
combined_icc = icc(rbind(as.matrix(ratings[1:35,c(3,5)]),as.matrix(ratings[36:60,c(4,6)])), model="twoway", type="agreement")

cat("ICC Agreement amongst Savage et al. ratings:", round(savage_icc$value, 2))
cat("ICC Agreement amongst Mehr et al. ratings:", round(mehr_icc$value, 2))
cat("ICC Agreement amongst all ratings:", round(combined_icc$value, 2))

plot_df = data.frame(article = ratings$Target.article,
                     author = ratings$Authors,
                     savage = average_savage,
                     mehr = average_mehr,
                     ID = ratings$ID,
                     content = ratings$content,
                     mentioned = ifelse(ratings$mentioned == 1, 2, 1)
)

idx = plot_df$ID %in% c("17", "19", "18", "15", "13", "11", "12", "9", "4", "V", "M", "8",
                        "28", "E", "J", "A", "2", "29", "16", "W", "Y", "6", "5")

jitter = 0.05

pdf("~/Desktop/ratings_figure.pdf", width = 8, height = 5.6)
ggplot(data = plot_df, 
       aes(x = savage, y = mehr, fill = article)) + 
  
  geom_hline(yintercept=0, colour = "lightgrey", linetype="dashed") +
  geom_vline(xintercept = 0, colour = "lightgrey", linetype="dashed") +
  # geom_jitter(aes(colour = article), cex = 7, position = position_jitter(seed = 1, width = jitter, height = jitter)) +
  # geom_text(data = plot_df[!idx,], aes(label = ID), position = position_jitter(seed = 1, width = jitter, height = jitter)) +
  
  geom_point(aes(fill = article), cex = 8, pch=21) +
  geom_text(data = plot_df[!idx,], aes(label = ID, fontface = mentioned), colour = "black") +
  
  geom_text_repel(data = plot_df[idx,], aes(label = ID, fontface = mentioned),
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.25, "lines"),
                   segment.color = 'black',
                   color = 'black',
                   max.overlaps = 15,
                  min.segment.length = 0.01) +
  xlab("Social bonding hypothesis\n(Savage et al.)") + 
  ylab("Credible signalling hypothesis\n(Mehr et al.)") + 
  scale_x_continuous(breaks = c(-1, 0.75), labels = c("Strongly critical", "Strongly supportive")) +
  scale_y_continuous(breaks = c(-1, 1), labels = c("Strongly critical", "Strongly supportive")) +
  geom_segment(x=-0.9, xend =  0.9, y=-1.2, yend = -1.2, size=0.7, # xaxis
               arrow = grid::arrow(length = unit(0.4,"cm"), ends = "both"), col = "grey")   +
  geom_segment(x=-1.1, xend =  -1.1, y=-0.9, yend = 0.9, size=0.7, # yaxis
               arrow = grid::arrow(length = unit(0.4,"cm"), ends = "both"), col = "grey")   +
  coord_cartesian(ylim = c(-1.1, 1.1), clip="off") +
  scale_size_manual(values = c(4.5, 3)) +
  scale_color_manual(values = c("black", "white")) + 
  theme_minimal() + 
  theme(legend.position = "none", text = element_text(size=18),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dev.off()
