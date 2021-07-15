# STARS - Summer 2021 :  Data Visualization With R #############################
rm(list=ls())

# Be sure to adjust the file path to your working directory:
setwd("/Users/mikeseese/Desktop/STARS - Summer 2021/Stars_R_WorkingDirectory")


## Loading Necessary Packages ##################################################
library("tidyverse")
library("RCurl")
library("ggridges")
library("scales")
library("RColorBrewer")
library("COVID19")
library("reshape")
library("coefplot")
library("sjPlot")
library("broom")


## Why do we need to visualize data? ###########################################

### Example 1: We want to tell stories and share results #######################
# Let's get some DW-Nominate Data from Hall, et al. (https://voteview.com/)

a <- getURL('https://voteview.com/static/data/out/members/HSall_members.csv')
b <- read.csv(textConnection(a), header= TRUE)

# Clean up the data (focus on House + 110th Congress onward)
c <- b %>% filter(chamber == "House" & congress > 89)

# Plot the D1 Nominate scores 
x <- ggplot(c, aes(x = nominate_dim1, y = as.factor(congress))) +
  geom_density_ridges(scale = 2, aes(fill = as.factor(party_code))) + 
  labs(title = "The Growing Ideological Divide in the U.S. House",
       x = "DW Nominate - Dimension 1",
       y = "Congress",
       caption = "Data: Lewis, Poole, Rosenthal, Boche, Rudkin, and Sonnet (2021)") +
  scale_fill_manual(name = "Party", 
                    values = c("#0000FFA0", "#FF0000A0"),
                    labels = c("Democract", "Republican")) +
  theme_minimal()
x
ggsave("plot1.pdf", x, width = 11, height = 8.5)

### Example 2: We want to make sense of a lot of data ##########################
# Clean data to get party frequencies
d <- c %>% group_by(congress, party_code) %>% summarize(pfreq=n())
table(d$party_code)

# Deal with the independents
d$party_code[d$party_code == 328] <- 300
d$party_code[d$party_code == 329] <- 300

# Plot frequencies as stacked bars
x <- ggplot(d, aes(x = as.factor(congress), y = pfreq, fill = as.factor(party_code))) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "Party Composition of the U.S. House, 1967 - Present",
       x = "Congress",
       y = "Percentage of Members by Party",
       caption = "Data: Lewis, Poole, Rosenthal, Boche, Rudkin, and Sonnet (2021)") +
  scale_fill_manual(name = "Party", 
                    values = c("#0000FFA0", "#FF0000A0", "#999999"),
                    labels = c("Democract", "Republican", "Independent")) +
  # geom_hline(yintercept = 0.5) +
  theme_minimal()
x
ggsave("plot2.pdf", x, width = 11, height = 8.5)

rm(list=ls())


## Bad and Better Charts #######################################################

### Bad Plot ###################################################################
# Let's get some data from from the NCHS: 
# https://catalog.data.gov/dataset/nchs-death-rates-and-life-expectancy-at-birth

# Get the data and clean it up a bit
a <- getURL('https://data.cdc.gov/api/views/w9j2-ggv5/rows.csv?accessType=DOWNLOAD')
b <- read.csv(textConnection(a), header= TRUE)
c <- b %>% filter(Race == "All Races" & Sex == "Both Sexes")
d <- b %>% filter(Race == "White" & Sex == "Both Sexes")
e <- b %>% filter(Race == "Black" & Sex == "Both Sexes")

# Here's a terrible plot in base graphics
pdf("plot3.pdf", width = 11, height = 8.5)
plot(c$Year, c$Average.Life.Expectancy..Years., type = "l", cex.axis = 2)
lines(d$Year, d$Average.Life.Expectancy..Years.)
lines(e$Year, e$Average.Life.Expectancy..Years., col="green", lwd=3)
grid(nx = 60, col = "darkgray", lty = 1)
dev.off() 

### Better Plot ################################################################
# Clean up the previous plot some
pdf("plot4.pdf", width = 11, height = 8.5)
par(oma=c(1.5,0,0,0))
plot(c$Year, c$Average.Life.Expectancy..Years., type = "l",
     xlab = "Year",
     ylab = "Average Life Expectancy at Birth",
     ylim = c(20, 80),
     xaxp = c(1900, 2020, 12))
lines(d$Year, d$Average.Life.Expectancy..Years., col = "darkgoldenrod")
lines(e$Year, e$Average.Life.Expectancy..Years., col = "darkcyan")
abline(v = 1918, col = "gray50", lty = 2)
text(1915.5, 65, "1918 Flu Pandemic", 
     pos = 4, srt = 90, col = "gray50", cex = 0.75)
title(main = "U.S. Life Expectancy by Race and Year, 1900 - 2018")
legend("bottomright", inset = 0.025,
       c("Total Population", "White", "Black"), 
       lwd=c(2, 2, 2),
       col = c("black", "darkgoldenrod", "darkcyan"),
       xjust = 0,
       cex = 0.75) 
mtext("Data: U.S. Centers for Disease Control and Prevention, National Center for Health Statistics", 
      side = 1, line=0, adj=0.05, outer = TRUE, cex = 0.75)
dev.off()

rm(list=ls())

### Deceptive Charts ###########################################################
# Make up some data
x <- LETTERS[1:5]
y <- c(55, 56, 64, 57, 59)

a <- data.frame(x, y)

# Underscore an extreme value
p1 <- ggplot(a, aes(x = x, y = y)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = y), vjust = -0.25, size = 4) + 
  scale_y_continuous(limits = c(50,65), oob = rescale_none) +
  theme_classic() +
  theme(axis.title = element_blank()) +
  ggtitle("This chart emphasizes the extreme value of C")
p1
ggsave("plot5.pdf", p1, width = 11, height = 8.5)

# Underscore low variation
p2 <- ggplot(a, aes(x = x, y = y)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = y), vjust = -0.25, size = 4) + 
  scale_y_continuous(limits = c(0,100)) +
  theme_classic() +
  theme(axis.title = element_blank()) +
  ggtitle("This chart implies more uniformity")
p2
ggsave("plot6.pdf", p2, width = 11, height = 8.5)

# Equivalence
p3 <- ggplot(a, aes(x = x, y = y)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(label = y), vjust = -0.25, size = 4) + 
  scale_y_continuous(limits = c(0,2500)) +
  theme_classic() +
  theme(axis.title = element_blank()) +
  ggtitle("This chart gives the perception of equivalence")
p3
ggsave("plot7.pdf", p3, width = 11, height = 8.5)

# Why we don't use pie charts
p <- LETTERS[1:10]
q <- c(2, 3, 2, 0, 4, 18, 31, 20, 8, 12)
b <- data.frame(p, q)

clr <- brewer.pal(10, "Set3")

pdf("plot8.pdf", width = 11, height = 8.5)
par(mar=c(0,0,0,0))
pie(x = b$q[b$q != 0], labels = b$p[b$q != 0], col = clr)
dev.off()

rm(list=ls())


## Base Graphics ###############################################################

# Bring in some data from WZB-IPI covid Project
# Find the website here: https://wzb-ipi.github.io/corona/
# And the paper here: https://osf.io/preprints/socarxiv/ub3zd/
a <- read.csv("wzb_covid_june2021.csv", header = TRUE)

### Histograms, Densities, and Rug Plots #######################################
# What happens if we just use the command "plot"?
plot(x = a$cases_cum)

# There are some wild outliers.  What countries are these?
plot(x = a$cases_cum)
text(a$cases_cum[a$cases_cum > 15000000], 
     labels = a$X[a$cases_cum > 15000000], 
     cex = 0.75, pos = 1)

# Interesting, but not really what we want  let's try:
hist(a$cases_cum) 
hist(a$cases_cum, breaks = 50)

# I can't read the exponential notation, so lets try log:
a$cases_cum_log <- log(a$cases_cum)
hist(a$cases_cum_log) 

# Let's add the density now
d_ccl <- density(a$cases_cum_log)

hist(a$cases_cum_log, freq = FALSE) 
lines(d_ccl, col = "darkcyan")

plot(d_ccl, col = "darkcyan")
rug(a$cases_cum_log)

# Plot matrix using mfrow / mfcol (rows, columns)
par(mfrow = c(1, 2))
## Plot 1
hist(a$cases_cum_log, freq = FALSE) 
lines(d_ccl, col = "darkcyan")
## Plot 2
plot(d_ccl, col = "darkcyan")
rug(a$cases_cum_log)
## Clear
dev.off()

### Box Plots ##################################################################
boxplot(a$cases_cum_log)

boxplot(a$cases_cum_log ~ a$continent)
boxplot(a$cases_cum_log ~ a$continent, horizontal = TRUE)

### Bar Plots ##################################################################
# How many countries are there in each region?
b <- table(a$region)

barplot(b[c(-1)])
# We want to exclude the un-categorized countries

# Wtf... R isn't plotting all the names?  Because they don't fit...
barplot(b[c(-1)], cex.names = 0.5)

# Try something else: Filter down to just MENA countries
c <- a %>% filter(region == "Middle East & North Africa")

# New bar plot
barplot(c$pop_density, names.arg = c$geoid2, las = 2, cex.names = 0.75)

# Can we sort these bars?
d <- c[order(c$pop_density, decreasing = TRUE),]
barplot(d$pop_density, names.arg = d$geoid2, las = 2, cex.names = 0.75)

### Scatter Plots ##############################################################
plot(a$air_travel, a$cases_cum_log)

# Wow, that was easy.  But let's make it nicer:
plot(a$air_travel, a$cases_cum_log, 
     xlab = "Air Travel",
     ylab = "Cummulative Covid Cases (Log)")
abline(lm(a$cases_cum_log ~ a$air_travel), col = "darkcyan")
mtext("Data: https://wzb-ipi.github.io/corona/", side = 3)
title(main = "More Air Travel is Associated with Higher Covid Case Counts")

# What if we want to know the countries?
plot(a$air_travel, a$cases_cum_log, 
     col = "white",
     xlab = "Air Travel",
     ylab = "Cummulative Covid Cases (Log)")
text(a$air_travel, a$cases_cum_log, labels = a$geoid2, cex = 0.5)
abline(lm(a$cases_cum_log ~ a$air_travel), col = "darkcyan")
mtext("Data: https://wzb-ipi.github.io/corona/", side = 3)
title(main = "More Air Travel is Associated with Higher Covid Case Counts")

# But What about a confidence interval?  We need to run the model and predict:
model <- lm(cases_cum_log ~ air_travel, data = a)
xvalues <- data.frame(air_travel = seq(5 , 21, length.out = 100))
predictions <- predict(model, newdata = xvalues, interval = "confidence")

plot(a$air_travel, a$cases_cum_log, 
     col = "white",
     xlab = "Air Travel",
     ylab = "Cumulative Covid Cases (Log)")
text(a$air_travel, a$cases_cum_log, labels = a$geoid2, cex = 0.5)
abline(lm(a$cases_cum_log ~ a$air_travel), col = "darkcyan")
lines(xvalues[,1], predictions[,2], col = "gray", lty = 2)
lines(xvalues[,1], predictions[,3], col = "gray", lty = 2)
mtext("Data: https://wzb-ipi.github.io/corona/", side = 3)
title(main = "More Air Travel is Associated with Higher Covid Case Counts")

dev.off()
rm(list=setdiff(ls(), "a"))


## ggplot: The Grammar of Graphics #############################################

### Distributions ##############################################################
# Violin plots
table(a$continent, exclude = NULL)

gg1 <- ggplot(a, aes(x = continent, y = gov_effect, fill = continent)) +
  geom_violin() +
  geom_jitter(position=position_jitter(0.1))
gg1

gg2 <- ggplot(a, aes(x = continent, y = deaths_cum_log, fill = continent)) +
  geom_violin() +
  geom_boxplot(width = 0.25)
gg2

# Density plots
gg3 <- ggplot(a, aes(x = gdp_pc)) +
  geom_density()
gg3

gg4 <- ggplot(a, aes(x = gdp_pc)) +
  geom_density(aes(color = continent))
gg4

gg5 <- ggplot(a, aes(x = gdp_pc)) +
  geom_density() +
  facet_wrap(~ continent)
gg5

### Line / Time Series #########################################################
# Some other Covid data
# Documentation at: https://cran.r-project.org/web/packages/COVID19/readme/README.html
c  <- covid19(c("US"), level = 3)
d <- c %>% filter(administrative_area_level_3 == "San Diego")

gg6 <- ggplot(d, aes(x = date, y = confirmed)) +
  geom_line()
gg6

gg7 <- ggplot(d) +
  geom_line(aes(x = date, y = deaths))
gg7

# What if we wanted daily deaths?
d <- d %>% mutate(d_deaths = deaths - lag(deaths))

gg8 <- ggplot(d) +
  geom_line(aes(x = date, y = d_deaths)) +
  scale_x_date(date_labels = "%b-%Y", breaks = breaks_pretty(20)) +
  scale_y_continuous(breaks = breaks_pretty(10)) +
  theme(axis.text.x = element_text(angle = 45))
gg8

### Bar Plots ##################################################################
# Let's look at 1 July in the 4 states that I've lived in for more than a year:
e <- c %>% filter(administrative_area_level_2 == "Hawaii" | 
                    administrative_area_level_2 == "California" |
                    administrative_area_level_2 == "Massachusetts" |
                    administrative_area_level_2 == "Arizona")
e <- e %>% filter(date == "2021-07-01")

# And get mean cumulative case counts by state...
# Recall that the standard error is the SD / sqrt(n)
f <- e %>% data.frame() %>% 
  select(state = administrative_area_level_2, confirmed)

g <- f %>% group_by(state) %>%
  summarize(n = n(),
            mean = mean(confirmed),
            sd = sd(confirmed)) %>% 
  mutate(se = sd/sqrt(n))

# Now we can plot!
gg9 <- ggplot(g) +
  geom_bar(aes(x = state, y = mean), stat = "identity") +
  geom_errorbar(aes(x = state, ymin = mean-se, ymax = mean+se), 
                width=0.1, colour="darkcyan", size=0.75)
gg9

# Wow, those error bars are HUGE! We should double check the distributions:
gg10 <- ggplot(f, aes(x = state, y = confirmed, fill = state)) +
  geom_violin() +
  geom_point()
gg10

# Looks great, but isn't this deceptive?  Comparing HI and CA is prob not ok...
# Let's normalize the case counts by the population:
h <- e %>% mutate(mconfirmed = confirmed / population) %>%
  data.frame() %>% 
  select(state = administrative_area_level_2, mconfirmed) %>% 
  group_by(state) %>%
  summarize(n = n(),
            mean = mean(mconfirmed),
            sd = sd(mconfirmed)) %>% 
  mutate(se = sd/sqrt(n))

# Now we can re-use the code from gg9, and just sub in the new dataset:
gg11 <- ggplot(h) +
  geom_bar(aes(x = state, y = mean), stat = "identity") +
  geom_errorbar(aes(x = state, ymin = mean-se, ymax = mean+se), 
                width=0.1, colour="darkcyan", size=0.75)
gg11
ggsave("plot9.pdf", gg11, width = 11, height = 8.5)

rm(list=setdiff(ls(), "a"))


### Dot Plots ##################################################################
# Let's check out some dotplots
# Using some Pew data: What % of surveyed respondents in the lead up to the 2020
# election reported [ISSUE] as very important to their vote?
b <- read.csv("pew_data.csv")

# We have a problem though! Our data is wide, and we need it to be long...
c <- melt(b, id = "issues")

gg12 <- ggplot(c, aes(x = value, y = issues)) +
  geom_point(aes(color = variable))
gg12

# But that's super hard to read, let's try to sort these
gg13 <- ggplot(c, aes(x = value, y = reorder(issues, value))) +
  geom_point(aes(color = variable))
gg13

# Great, but this is still sort of hard to read, and the colors don't make sense:
gg14 <- ggplot(c, aes(x = value, y = reorder(issues, value))) +
  geom_line(aes(group = issues), color = "#999999") +
  geom_point(aes(color = variable), size = 5) +
  scale_color_manual(values = c("#0000FF", "#FF0000", "#999999"),
                     labels = c("Biden Voters", "Trump Voters", "All Voters")) +
  geom_text(aes(label = sprintf("%1.0f%%", value), color = variable), 
            size = 2,  nudge_y = 0.25) +
  scale_x_continuous(breaks = NULL) +
  labs(title = "Biden and Trump Voters Diverge on Which Issues Matter Most in 2020 Election", 
       subtitle = "Results of July/August 2020 Pew Research Poll of Registered Voters",
       x = "Percent of Registered Voters Saying Issue is Very Important to Their Vote in the 2020 Election.",
       y = "Issue Area") +
  theme_minimal() +
  theme(legend.title = element_blank())
gg14
ggsave("plot10.pdf", gg14, width = 11, height = 8.5)
# Keep in mind that red tends to bleed in video... 
# Try: https://htmlcolorcodes.com and https://color.adobe.com/create/color-wheel

rm(list=setdiff(ls(), "a"))


### Scatter Plots ##############################################################
plot(a$air_travel, a$deaths_cum_log)
abline(lm(a$deaths_cum_log ~ a$air_travel), col = "blue")

b <- a %>% filter(region != "")

gg15 <- ggplot(b, aes(x = air_travel, y = deaths_cum_log, color = region)) +
  geom_point()
gg15

# Let's try to scale by case count
gg16 <- ggplot(b, aes(x = air_travel, y = deaths_cum_log, 
                      size = cases_cum,
                      color = region)) +
  scale_size_continuous(range = c(1, 10), breaks = pretty_breaks()) +
  geom_point()
gg16

# And now let's try to add an ols line and ci
gg17 <- ggplot(b, aes(x = air_travel, y = deaths_cum_log)) +
  geom_point() +
  # geom_smooth(method = loess) +
  geom_smooth(method = lm)
gg17

# What happens if we add the color back in?
gg18 <- ggplot(b, aes(x = air_travel, y = deaths_cum_log,
                      color = region)) +
  geom_point() +
  geom_smooth(method = lm)
gg18
# It's calculating the interaction term!

# But that was hard af to read.  Let's try:
gg19 <- ggplot(b, aes(x = air_travel, y = deaths_cum_log)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~ region)
gg19

# Let's go back to plot gg17... can you clean it up so it looks pretty?
# Or better yet, can you make it look like the plot we made with base graphics?
gg20 <- ggplot(b, aes(x = air_travel, y = deaths_cum_log)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "More Air Travel is Associated with Higher Covid Case Counts",
       x = "Air Travel",
       y = "Cummulative Covid Deaths (Log)") +
  theme_minimal()
gg20
ggsave("plot11.pdf", gg20, width = 11, height = 8.5)

# Almost there, but I want ISO codes instead of points
gg20 <- ggplot(b, aes(x = air_travel, y = deaths_cum_log, label = geoid2)) +
  geom_text(size = 2, check_overlap = TRUE) +
  geom_smooth(method = lm,) +
  labs(title = "More Air Travel is Associated with Higher Covid Case Counts",
       x = "Air Travel",
       y = "Cummulative Covid Deaths (Log)") +
  theme_minimal()
gg20


### Coefficient Plots ##########################################################
# Let's run a regression:
m <- lm(deaths_cum_log ~ cases_cum_log + air_travel + gov_effect + share_older, data = b)
m

coefplot(m, intercept = FALSE)
# Wow, that was easy

# Can we make it harder?  Let's try to replicate this manually in ggplot
z1 <- tidy(m, conf.int = TRUE, conf.level = .95)
z1 <- z1 %>% dplyr::rename(low95 = conf.low, high95 = conf.high)

z2 <- tidy(m, conf.int = TRUE, conf.level = .9)
z2 <- z2 %>% dplyr::rename(low90 = conf.low, high90 = conf.high)

z3 <- z1 %>% full_join(z2)
z3$term <- dplyr::recode(z3$term,
                    "(Intercept)" = "Intercept",
                    "cases_cum_log" = "Cumulative Cases (Log)", 
                    "air_travel" = "Air Travel", 
                    "gov_effect" = "Government Effectiveness",
                    "share_older" = "Share of Older Population")

z3$term <- factor(z3$term, 
                levels = c("Intercept", 
                           "Cumulative Cases (Log)", 
                           "Air Travel", 
                           "Government Effectiveness", 
                           "Share of Older Population"), 
                ordered = TRUE)

# Now z3 is all the data we need for a dot and whisker plot
gg21 <- ggplot(z3 %>% filter(term != "Intercept")) +
  geom_hline(yintercept=0, lty="11", colour="grey30") +
  geom_errorbar(aes(term, ymin = low95, ymax = high95), width=0.1) +
  geom_errorbar(aes(term, ymin = low90, ymax = high90), lwd = 1.15, width=0) +
  geom_point(aes(term, estimate)) +
  labs(title = "Results of OLS Regression", 
       y = "Point Estimate",
       x = "Variable") +
  theme_light()
gg21
ggsave("plot12.pdf", gg21, width = 11, height = 8.5)



### Margins Plots ##############################################################
# Let's redefine the model and the sjPlot packace to estimate margins
m <- lm(deaths_cum_log ~ cases_cum_log + air_travel + gov_effect + share_older + as.factor(continent), data = b)
m

coefplot(m, intercept = FALSE)

gg22 <- plot_model(m, type = "pred", 
                   terms = "continent")
gg22

gg23 <- plot_model(m, type = "pred", 
                   terms = "air_travel")
gg23

gg24 <- plot_model(m, type = "pred", 
                   terms = c("air_travel", "continent"))
gg24

rm(list=ls())

# And that's it!
