library(jsonlite)
library(networkD3)
library(webshot)
library (dplyr)



library(readxl)


  # Description
  #data1 = Base Federal Formula Funding
  #data2 = Base State Formula Funding
  #data3 = State Managed Programs with Federal Funding
  #data4 = Other Federal Funding (Includes Discretionary Grants)
  #data5 = State Managed Programs with State Funding
  #data6 = Local Funding

data1 <- read_excel("FundingTestDat/FLOWS DAT_3.xlsx", sheet = "Base_Federal")
data2 <- read_excel("FundingTestDat/FLOWS DAT_3.xlsx", sheet = "Base_State")
data3 <- read_excel("FundingTestDat/FLOWS DAT_3.xlsx", sheet = "State_Managed")
data4 <- read_excel("FundingTestDat/FLOWS DAT_3.xlsx", sheet = "Other_Federal")
data5 <- read_excel("FundingTestDat/FLOWS DAT_3.xlsx", sheet = "State_Managed_SF")
data6 <- read_excel("FundingTestDat/FLOWS DAT_3.xlsx", sheet = "Local_Funding")


# 1 Process Data 1
 # extract the needed columns
df2 <- data1[c(2: 13)] # this should be done by looking at the data content

# change dataframe df2 to long file
df3 = reshape2::melt(df2, id.vars = "Names")
# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3
# add category
df4$"category" <- "Base Federal Formula Funding"
# Assuming your data frame is named df4
df1 <- df4 %>%
  group_by(category, source) %>%
  summarize(total_value = sum(value))
names(df1) <- c('source', 'target', 'value')

# combine rows and convert it it data frame

links_1 <- as.data.frame (rbind(df1, df3))

#===========================
# 2 Process Data 2
# extract the needed columns
df2 <- data2[c(2: 5)] # this should be done by looking at the data content

# change dataframe df2 to long file
df3 = reshape2::melt(df2, id.vars = "Names")
# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3
# add category
df4$"category" <- "Base State Formula Funding"
# Assuming your data frame is named df4
df1 <- df4 %>%
  group_by(category, source) %>%
  summarize(total_value = sum(value))
names(df1) <- c('source', 'target', 'value')

# combine rows and convert it it data frame

links_2 <- as.data.frame (rbind(df1, df3))


#==========================================
# 3 Process Data 3
# extract the needed columns
df2 <- data3[c(2: 8)] # this should be done by looking at the data content

# change dataframe df2 to long file
df3 = reshape2::melt(df2, id.vars = "Names")
# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3
# add category
df4$"category" <- "State Managed Programs with Federal Funding"
# Assuming your data frame is named df4
df1 <- df4 %>%
  group_by(category, source) %>%
  summarize(total_value = sum(value))
names(df1) <- c('source', 'target', 'value')

# combine rows and convert it it data frame

links_3 <- as.data.frame (rbind(df1, df3))

#==================================================

# 4 Process Data 4
# extract the needed columns
df2 <- data4[c(2: 6)] # this should be done by looking at the data content

# change dataframe df2 to long file
df3 = reshape2::melt(df2, id.vars = "Names")
# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3
# add category
df4$"category" <- "Other Federal Funding (Includes Discretionary Grants)"
# Assuming your data frame is named df4
df1 <- df4 %>%
  group_by(category, source) %>%
  summarize(total_value = sum(value))
names(df1) <- c('source', 'target', 'value')

# combine rows and convert it it data frame

links_4 <- as.data.frame (rbind(df1, df3))


#========================================
# 5 Process Data 5
# extract the needed columns
df2 <- data5[c(2: 7)] # this should be done by looking at the data content

# change dataframe df2 to long file
df3 = reshape2::melt(df2, id.vars = "Names")
# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3
# add category
df4$"category" <- "State Managed Programs with State Funding"
# Assuming your data frame is named df4
df1 <- df4 %>%
  group_by(category, source) %>%
  summarize(total_value = sum(value))
names(df1) <- c('source', 'target', 'value')

# combine rows and convert it it data frame

links_5 <- as.data.frame (rbind(df1, df3))
#===================================================
# 6 Process Data 6
# extract the needed columns
df2 <- data6[c(2: 3)] # this should be done by looking at the data content

# change dataframe df2 to long file
df3 = reshape2::melt(df2, id.vars = "Names")
# Rename Columns
names(df3) <- c('target', 'source', 'value')
#rearrange the order of df3
df3 <- df3[ ,c('source', 'target', 'value')]
df4 <- df3
# add category
df4$"category" <- "Local Funding"
# Assuming your data frame is named df4
df1 <- df4 %>%
  group_by(category, source) %>%
  summarize(total_value = sum(value))
names(df1) <- c('source', 'target', 'value')

# combine rows and convert it it data frame

links_6 <- as.data.frame (rbind(df1, df3))

#=================================

# Putting all links together,

links <- as.data.frame (rbind(links_1, links_2, links_3,links_4,links_5, links_6))



# Replace 0 values with blank cells
#links$value[links$value== 0] <- NA

# Filter out rows where value is 0
links <- subset(links, value != 0)
# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

#links$IDsource <- match(links$source, nodes$name)-1 
#links$IDtarget <- match(links$target, nodes$name)-1
# Make the Network

#link_colors <- c("blue", "green", "red", "orange")
#link_colors <- nodes_colors$color




p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE, fontSize = 12,
                   linkColour = nodes$color,
                   #linkColour = link_colors)
				   width = 800,  # Set the width (adjust the value as needed)
                   height = 600) # Set the height (adjust the value as needed)
                 
p


#test area


#=====================================================



# Saving the image to print file

#webshot::install_phantomjs()

#Save the plot as an HTML file
htmlwidgets::saveWidget(p, file = "sankey_plot_Funds_all_wothout_zero.html")

# Use webshot to capture the HTML content and save it as a PNG file
webshot("sankey_plot_Funds_all_wothout_zero.html", file = "sankey_plot_Fund_all_wothout_zero.png", cliprect = "viewport")









