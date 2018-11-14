#loading the essential libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(scales) 

#importing import sheet of the complete excel
india_import <- read_excel("Export-Import.xlsx", sheet= "Import")
View(india_import)

#renaming the columns
india_import <- india_import %>%  rename(country="Reporter Name") %>% rename(partner="Partner Name") %>% 
  rename(product_group="Product Group")

str(india_import)
ls(india_import)
#?percent

#converting into tidy data
india_import <- melt(india_import, id.vars = c("country", "partner", "product_group", "Indicator"), 
                     measure.vars = c("1989","1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"),
                     variable.name = "Years", value.name = "Imports")


#creating line plot for import values
p1 <- ggplot(data=india_import %>% filter(Indicator== "Import (US$ Million)"), mapping = aes(x=Years, y=Imports)) 
  p1+ geom_line(aes(group=product_group, color= product_group), size= 1) + theme(axis.text.x = element_text(angle = 55, hjust=0.8), legend.position = "top") +
    scale_y_continuous(label=comma)+labs(x="Years", y="Import Value(US$, MIllion)", color="Product Group") 


#creating line plot for import share and showing the percentages as the import share of various product groups in the total share
p2 <- ggplot(data=india_import %>% filter(Indicator== "Import-Share"), mapping = aes(x=Years, y=Imports)) 
  p2 + geom_line(aes(group=product_group, color= product_group), size= 1) + theme(axis.text.x = element_text(angle = 55, hjust=0.8), legend.position = "top") +
   labs(x="Years", y="Import Share (in Percentages)", color="Product Group") + scale_y_continuous(labels=percent)
  
#Creating the shinyapp
  
  
  
  
  
  
  