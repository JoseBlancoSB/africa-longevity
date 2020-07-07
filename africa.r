# 
# This project establishes an easy way to study life expectancy of African
# countries from 1960 - 2018,
# 
# World Bank Life Expectancy
# Downloaded data, in .csv format, from this website:
#
# https://data.worldbank.org/indicator/SP.DYN.LE00.IN
#
# The download button downloaded a .zip file, which I unpacked and the result was
# a folder called
#
# file:///home/jose/R/data/population/africa/API_SP.DYN.LE00.IN_DS2_en_csv_v2_1120968
#
# Inside that folder, there are three files:
#
# 1) API_SP.DYN.LE00.IN_DS2_en_csv_v2_1120968.csv
#    Renamed to: life_expectancy.csv
#
# 2) Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2_1120968.csv
#    Renamed to: country-region-income-indicator.csv
#
# 3) Metadata_Indicator_API_SP.DYN.LE00.IN_DS2_en_csv_v2_1120968.csv
#    Renamed to:  life-expectancy-definition.csv
#
#
# Below are the countries of Africa (as of June, 2020), listed in alphabetical order.

# Angola
# Benin
# Botswana
# Burkina Faso
# Burundi
# Cabo Verde
# Cameroon
# Central African Republic (CAR)
# Chad
# Comoros
# Congo, Democratic Republic of the
# Congo, Republic of the
# Cote d'Ivoire
# Djibouti
# Egypt
# Equatorial Guinea
# Eritrea
# Eswatini (formerly Swaziland)
# Ethiopia
# Gabon
# Gambia
# Ghana
# Guinea
# Guinea-Bissau
# Kenya
# Lesotho
# Liberia
# Libya
# Madagascar
# Malawi
# Mali
# Mauritania
# Mauritius
# Morocco
# Mozambique
# Namibia
# Niger
# Nigeria
# Rwanda
# Sao Tome and Principe
# Senegal
# Seychelles
# Sierra Leone
# Somalia
# South Africa
# South Sudan
# Sudan
# Tanzania
# Togo
# Tunisia
# Uganda
# Zambia
# Zimbabwe

library(data.table)
library(readr)
library(tidyverse)
library(readxl)

#set working directory
setwd("/home/jose/R/data/population/africa")
getwd()
# Import the text file.

df <- read_csv("life_expectancy_by_country.csv", skip = 3)

# Imports a dataframe of 264 observations and 65 variables and copies it to an easy-named
# dataframe called df.

# Remove certain columns that we do not need.
df <- select(df, 1:2, 5:63)

# Detrmine the names of the columns

colnames(df)

# Create a vector with the country codes of all the countries on the continent of Africa

afr_codes <- c('DZA', 'AGO', 'BEN', 'BWA', 'BFA', 'BDI', 'CPV', 'CMR', 'CAF', 'TCD', 'COM',
               'COD', 'COG', 'CIV', 'DJI', 'EGY', 'GNQ', 'ERI', 'SWZ', 'ETH', 'GAB', 'GMB',
               'GHA', 'GIN', 'GNB', 'KEN', 'LSO', 'LBR', 'LBY', 'MDG', 'MWI', 'MLI', 'MRT',
               'MUS', 'MAR', 'MOZ', 'NAM', 'NER', 'NGA', 'RWA', 'STP', 'SEN', 'SYC', 'SLE',
               'ZAF', 'SSD', 'SOM', 'SDN', 'TZA', 'TGO', 'TUN', 'UGA', 'ZMB', 'ZWE')

class(df)

# Rename the column name 'Country Code' to 'country_code'

df <- rename(df, 'country_code' = 'Country Code') #For renaming dataframe column

# Rename the column name 'Country Name' to 'country_name'

df <- rename(df, 'country_name' = 'Country Name') #For renaming dataframe column
# Check to see that the column names were changed.
colnames(df)

# Correct an error.  Replace "GNO" with "GNQ" in afr_codes
# I fixed it in creating the vector, but useful to know how to do.
# afr_codes <- str_replace(afr_codes, "GNO", "GNQ")

# Create a dataframe with ONLY the countries in Africa

africa <- filter(df, country_code %in% c(afr_codes))

# Remove the df dataframe to clean up the environment.

rm(df)

# Export afr_codes to text file

# write.table(afr_codes, "afr_codes", sep=",")

# Transpose the data frame "africa" so that the observations become the 
# variables and the variables become the observations.  In other words, the
# names of the countries are accross the top, and the years are the observations,
# or row names.

africa_trans <- as_tibble(t(africa))

# Create a character vector with the names of the countries of Africa,
# which are all now across the first row.
vec_countries <- c(africa_trans[1,])
# Now "unlist" the list "vec_countries" so that it is a vector
vec_countries <- unlist(vec_countries)
# Convert the vector to a pure "character" vector

vec_c <- as.character(vec_countries)

######################################################
#
# Now I am going to rename the columns in africa_trans from "V1, V2,..." to the
# actual names of the countries.  The new names will be called "c_new", while the
# old names will be called "c_old".  I will create "c_new" just by assigning
# to "c_new" the vakues of "vec_c"; I could have just used "vec_c".
#
# The "c_old" is created just by running "colneames" on "africa_trans" and storing
# the values to "c_old".  Believe it or not, this took me a long time to figure out.
# It should not have, but what the heck; now I know it.

c_new <- vec_c

# Now I am going to rename the columns in africa_trans from "V1, V2,..." to the
# actual names of the countries.  The new names will be called "c_new", while the
# old names will be called "c_old".  I will create "c_new" just by assigning
# to "c_new" the vakues of "vec_c"; I could have just used "vec_c".
#
# The "c_old" is created just by running "colneames" on "africa_trans" and storing
# the values to "c_old".  Believe it or not, this took me a long time to figure out.
# It should not have, but what the heck; now I know it.

c_new <- vec_c
c_old <- colnames(africa_trans)

# Now I use "setnames" from the "data.table" package to apply the
# changes to the africa_trans dataframe.

africa_trans <- setnames(africa_trans, old = c_old, new = c_new)

#####################################################3

# Remove the first two rows of observations, which list the names of the 
# countries and the country codes.

africa_trans <- africa_trans[3:61, ]  # gets rid of observations 1 and 2

# If you look at the structure of the dataframe "africa_trans" you notice that
# all of the columns are "character" vectors.  They must be converted to numeric,
# then rounded and converted to integers.

str(africa_trans)

##############################################

# The following command uses lapply() to convert all the columns (variables) in
# africa_trans from charater datatype to numeric datatype

africa_trans[] <- lapply(africa_trans, function(x) if(is.character(x)) as.numeric(x) else x)

# Create a vector of years 1960 - 2018, so that you can add that vector as a variable
# to africa_trans.  Let's call the vector "years".

years <- c(1960:2018)

# Notice that the vector has the exact number of entries as there are observations in
# africa_trans, 59, corresponding to one observation per year.

# Now I would like to use dplyr's mutate() meethod to add a column named "years" to
# dataframe africa_trans.

africa_trans <- mutate(africa_trans, years = c(years))

# The above command works, but I would like the column 'years' to be the first
# column in the dataframe.

africa_trans <- africa_trans[, c(55, 1:54)]

##########################################################################
# Now that the data is sufficiently "tidy", we plot, using ggplot2
##########################################################################

# We can save the entire plot in a variable.  The letter "p" is convenient.

p <- ggplot(data = africa_trans) +
  geom_line(mapping = aes(x = years, y = Angola, color = "Angola"), size = 1.5) +
  geom_line(mapping = aes(x = years, y = Burundi, color = "Burundi"), size = 1.5) +
  geom_line(mapping = aes(x = years, y = Zimbabwe, color = "Zimbabwe"), size = 1.5) +
  labs(title="Life Expectancy of African Countries", 
       subtitle="Angola, Burundi, and Zimbabwe", 
       caption="Source, World Bank,  https://data.worldbank.org/indicator/SP.DYN.LE00.IN ", 
       y="Life Expectancy at Birth", 
       x= "1960 - 2018",
       color = 'Legend') 

# Plot the visualization by invoking the variable.

p

# Now we add "layers" to the plot
# Modify the scales

my_plot <- p + scale_y_continuous(limits = c(35, 65), breaks = seq(0, 65, by = 5)) +
  scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 5))

my_plot

##################  This works!!!  #################################

enhanced_plot <- my_plot + theme(axis.text.x = element_text(face = "bold", color = "black", 
                                     size = 10, angle = 30),
          axis.text.y = element_text(face = "bold", color = "magenta", 
                                     size = 10, angle = 30))

enhanced_plot  # Plots everything!!!!

enhanced_plot + scale_color_manual(values=c("red", "blue", "green"))

####################################### New stuff #################################

enhanced_plot + theme(
  legend.title = element_text(color = "black", size = 15),
  legend.text = element_text(color = "black", size = 13)
)



