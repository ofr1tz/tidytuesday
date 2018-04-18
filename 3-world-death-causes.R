require(tidyverse)
require(readxl)

# load data
data <- read_xlsx("data/global_mortality.xlsx")

# data processing
data <- data %>%
      replace(is.na(.), 0) %>%
      filter(country_code=="RWA") %>%
      gather(key="cause", value="percent", -(1:3)) %>%
      mutate(cause=factor(stringr::str_replace(cause, " \\(%\\)", "")))

# find most important death causes (maximum of at least 5%)
top <- data %>% 
      group_by(cause) %>%
      summarise(max=max(percent)) %>%
      arrange(desc(max)) %>%
      filter(max>5)

# select most important death causes and arrange by variance over time
data <- data %>%
      filter(cause %in% top$cause) %>%
      mutate(cause=fct_reorder(cause, percent, var, .desc=TRUE))

# plot death causes in rwanda over time
g <- ggplot(data, aes(x=year, 
                      y=percent,
                      fill=cause)) +
      geom_area(aes(col=cause),
                size=.01,
                alpha=.6) +
      labs(title="Share of death causes per year (Rwanda 1990-2016)",
           subtitle="Source: OurWorldInData.org | All death causes with a maximum share of at least 5%, arranged by variance over time (bottom = least variable).",
           x="Year",
           y="Share (%)")+
      theme_minimal()+
      theme(legend.position="none")

# prepare a direct labels and their positions
labels <- data %>%
      filter(year==max(year)) %>%
      mutate(x=2013.6-(cause=="Conflict")*19.5) %>%
      select(cause, percent, x) %>%
      arrange(desc(cause))

cum <- cumsum(labels$percent)
y <- NULL
for(i in 1:nrow(labels)) {
      if(i==1) y=cum[i]/2
      else if(i==nrow(labels)) y=c(y, cum[i]+5)
      else y=c(y, mean(c(cum[i], cum[i-1])))
}

labels <- labels %>%
      mutate(y=y,
             percent=paste0(format(round(percent,1), nsmall=1), "%"))

# add labels to plot
g <- g + 
      geom_text(data=labels,
                aes(x=x, y=y, label=cause),
                vjust=.5,
                size=4)+
      geom_text(data=labels,
                aes(x=2017.5, y=y, label=percent),
                vjust=.5,
                hjust=1,
                size=4)+
      coord_cartesian(ylim=c(0,97))

