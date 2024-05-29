# This is to create sankey diagram for the USA enery consuamption
# Date May, 2024

library(jsonlite)
library(networkD3)
library(webshot)
library (dplyr)

library(tidyr)
library(tibble)

library(readxl)


# Description
#data1 = source1 to Electricity
#data2 = source1 and Electric to Residential
#data3 = source1 and Electric to Commercial
#data4 = source1 and Electric to Industrial
#data5 = source1 and Electric to Transportation
#data6 = ....

# Read data 
data1 <- read_excel("L:/Belachew/Tools/R/SankeyDiagram/USA Data/Data/Table_2.6_Electric_Power_Sector_Energy_Consumption.xlsx", sheet = "Annual Data", skip = 10)
data2 <- read_excel("L:/Belachew/Tools/R/SankeyDiagram/USA Data/Data/Table_2.2_Residential_Sector_Energy_Consumption.xlsx", sheet = "Annual Data", skip = 10)
data3 <- read_excel("L:/Belachew/Tools/R/SankeyDiagram/USA Data/Data/Table_2.3_Commercial_Sector_Energy_Consumption.xlsx", sheet = "Annual Data", skip = 10)
data4 <- read_excel("L:/Belachew/Tools/R/SankeyDiagram/USA Data/Data/Table_2.4_Industrial_Sector_Energy_Consumption.xlsx", sheet = "Annual Data", skip = 10)
data5 <- read_excel("L:/Belachew/Tools/R/SankeyDiagram/USA Data/Data/Table_2.5_Transportation_Sector_Energy_Consumption.xlsx", sheet = "Annual Data", skip = 10)
#data6 <- read_excel("FundingTestDat/FLOWS DAT_3.xlsx", sheet = "Local_Funding")


# 1 Process Data 1
# extract the needed columns

#data1 = data1[-1,]
# Get the last row

#===================================================
#1 Source 1 to Electric

data1 <- data1 %>% slice(n())
names(data1) <- c('Year', 'Coal', 'Natural Gas','Petrolum', 'Total1','Nuclear','Hydro','Geothermal','Solar','Wind','Biomass')
df2 <- data1[ c(2:4,6:11)]
df2$"category" <- "Electricity"
#df2 <- data1[c(2: 13)] # this should be done by looking at the data content

# change dataframe df2 to long file
df3 = reshape2::melt(df2, id.vars = "category")
# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3

# set the value from trillion to quadrillion
#df4$value <- as.numeric(as.character(df4$value))/1000

# add category
#df4$"category" <- "Base Federal Formula Funding"
# Assuming your data frame is named df4
links_1 <- as.data.frame (df4)


#======================================
#2 Residential

data2 <- data2 %>% slice(n())

df2 <- data2[ c(3:4,6:8,11)]
names(df2) <- c('Natural Gas','Petrolum','Geothermal','Solar','Biomass','Electricity')
df2$"category" <- "Residential"
df3 = reshape2::melt(df2, id.vars = "category")

# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3

# set the value from trillion to quadrillion
#df4$value <- as.numeric(as.character(df4$value))/1000

# add category
#df4$"category" <- "Base Federal Formula Funding"
# Assuming your data frame is named df4
links_2 <- as.data.frame (df4)
# combine rows and convert it it data fram

#===============================
#3 Commercial

data3 <- data3 %>% slice(n())

df2 <- data3[ c(2:4,6:10,13)]
names(df2) <- c('Coal', 'Natural Gas','Petrolum','Hydro', 'Geothermal','Solar','Wind', 'Biomass','Electricity')
df2$"category" <- "Commercial"
df3 = reshape2::melt(df2, id.vars = "category")

# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3


# set the value from trillion to quadrillion
# df4$value <- as.numeric(as.character(df4$value))/1000

# add category
#df4$"category" <- "Base Federal Formula Funding"
# Assuming your data frame is named df4
links_3 <- as.data.frame (df4)

#===========================
#4 Industrial
data4 <- data4 %>% slice(n())

df2 <- data4[ c(2:4,6:10,13)]
names(df2) <- c('Coal', 'Natural Gas','Petrolum','Hydro', 'Geothermal','Solar','Wind', 'Biomass','Electricity')
df2$"category" <- "Industrial"
df3 = reshape2::melt(df2, id.vars = "category")

# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3

# set the value from trillion to quadrillion
#df4$value <- as.numeric(as.character(df4$value))/1000

# add category
#df4$"category" <- "Base Federal Formula Funding"
# Assuming your data frame is named df4
links_4 <- as.data.frame (df4)


#=================================
#5 Transportation

data5 <- data5 %>% slice(n())

df2 <- data5[ c(2:4,6,8)]
names(df2) <- c('Coal', 'Natural Gas','Petrolum', 'Biomass','Electricity')
df2$"category" <- "Transportation"
df3 = reshape2::melt(df2, id.vars = "category")

# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3

# set the value from trillion to quadrillion
#df4$value <- as.numeric(as.character(df4$value))/1000
# add category
#df4$"category" <- "Base Federal Formula Funding"
# Assuming your data frame is named df4
links_5 <- as.data.frame (df4)

# combine rows and convert it it data fram

links <- as.data.frame (rbind(links_1, links_2,links_3, links_4, links_5))

#links$target <- paste(links$target, " ", sep="")
links$group <- c(rep("A", 10), rep("B",10), rep("C", 10), rep("D", 8)) # sum of these numbers should be equal to the the numbe of rows to the long table.

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
  )

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1
  
p <- sankeyNetwork(Links = links,
              Nodes = nodes,
              Source = "IDsource",
              Target = "IDtarget",
              Value = "value", 
              NodeID = "name", 
              sinksRight=FALSE,
              nodeWidth=20,
              fontSize=13,
              nodePadding=20,
              width = 900,
              height = 600,
              LinkGroup = 'source')

p <- htmlwidgets::prependContent(p, htmltools::tags$h1("Estimated U.S. Energy Consumption in 2023, in Trillion Btu")) # add title
p <- htmlwidgets::appendContent(p, htmltools::tags$p("SPC, May 2024")) # add caption
  
p
  
#================================

# Saving the image to print file

#webshot::install_phantomjs()

#Save the plot as an HTML file
htmlwidgets::saveWidget(p, file = "sankey_plot_USA_Energy_Consumption.html")

# Use webshot to capture the HTML content and save it as a PNG file
webshot("sankey_plot_USA_Energy_Consumption.html", file = "sankey_plot_USA_Energy_Consumption.png", cliprect = "viewport")
  
  
  
