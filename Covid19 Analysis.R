#the dataset was imported directly from the PC local files and name changed from "covid19 to "covid_df
covid_df <- covid19


#deploying the tibble library to be able to perform some basic functions on the imported table
library(tibble)


#let's look at the summary of the dataset
glimpse(covid_df)


#looking at the first few rows of the dataset
head(covid_df)


#taking a look at the headers of the table for proper understanding of the dataset
vector_cols <- colnames(covid_df)


#filtering for all states in the province so as to have an unbiased analysis
library(dplyr)
covid_df_all_states <- covid_df %>% filter(Province_State == "All States")


#separating the daily cases from the cumulative cases so that we can perform separate analysis
covid_df_all_states_daily <- covid_df_all_states %>% select(Date, Country_Region, 11:14)


#Extracting the top ten countries in number of people tested daily
top_ten_daily_tested <- covid_df_all_states_daily %>% #connecting the dataset
  group_by(Country_Region) %>% #using the dataset to group the data
  summarize(total_daily_tested = sum(daily_tested)) %>% #calculating the total of each country
  arrange(desc(total_daily_tested)) %>% head(10) #sorting the table from highest to lowest and extracting the top ten


##Extracting the top ten countries with highest daily positive cases
top_ten_daily_positive <- covid_df_all_states_daily %>% #connecting the dataset
  group_by(Country_Region) %>% #using the dataset to group the data
  summarize(total_daily_positive = sum(daily_positive)) %>% #calculating the total of each country
  arrange(desc(total_daily_positive)) %>% head(10) #sorting the table from highest to lowest and extracting the top ten


#Extracting the top ten countries with active cases
top_ten_active <- covid_df_all_states_daily %>% #connecting the dataset
  group_by(Country_Region) %>% #using the dataset to group the data
  summarize(total_active = sum(active)) %>% #calculating the total of each country
  arrange(desc(total_active)) %>% head(10) #sorting the table from highest to lowest and extracting the top ten


#Extracting the top ten countries with number of people hospitalized
top_ten_hospitalized <- covid_df_all_states_daily %>% #connecting the dataset
  group_by(Country_Region) %>% #using the dataset to group the data
  summarize(total_hospitalizedCurr = sum(hospitalizedCurr)) %>% #calculating the total of each country
  arrange(desc(total_hospitalizedCurr)) %>% head(10) #sorting the table from highest to lowest and extracting the top ten


#summarizing the tested, positive, active and hospitalized cases for all countries and sorting the table in descending order by the tested column
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% 
  group_by(Country_Region) %>% summarize(tested = sum(daily_tested),
                                         positive = sum(daily_positive),
                                         active = sum(active),
                                         hospitalized = sum(hospitalizedCurr)) %>% 
  arrange(desc(tested))


#Extracting the top ten countries in the database by the number tested
covid_top_ten <- head(covid_df_all_states_daily_sum, 10)


#vectorizing the variables of the database
countries <- covid_top_ten$Country_Region
tested_cases <- covid_top_ten$tested
positive_cases <- covid_top_ten$positive
active_cases <- covid_top_ten$active
hospitalized_cases <- covid_top_ten$hospitalized


#naming the vectors by the Country region
names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries


#Finding the top 3 positive cases to tested cases ratio with the vectors
positive_tested_to_p_3 <- round(positive_cases / tested_cases, 2) #the top three cases from the result are United Kingdom, United States and Turkey
positive_tested_to_p_3 <- positive_tested_to_p_3[c(7, 1, 5)] #the top three cases are positioned in the index


#creating a vector for the top 3 cases so that it can be converted into a matrix
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_state <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941,2980960, 0)


#combining the vectors into a matrix
covid_mat <- rbind(united_kingdom, united_state, turkey)


#renaming the column names for the matrix
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")


#putting all our results together
question <- "Which countries have the highest number of positive cases against the number of test"
answer <- c("Positive tested cases" = positive_tested_to_p_3)


#creating a list to store the dataframes, vectors and matrices
dataframes <- list(original_dataset = covid_df, daily_cases = covid_df_all_states_daily, 
                   all_states = covid_df_all_states_daily_sum, top_ten_tested = covid_top_ten)
matrices <- list(top_3_matrix = covid_mat)
vectors <- list(tested = tested_cases, positive = positive_cases, active = active_cases, hospitalized = hospitalized_cases)
data_structure_list <- list(dataframes, matrices, vectors)


#covid analysis list
covid_analysis_list <- list(question, answer, data_structure_list)


#this analysis shows that United Kingdom, United States and Turkey had higher were more hit by the virus



#a continuation of the analysis is carried out on the cumulative results of the Covid-19 dataset to gain a broader insight into the impact of the virus on the affected countries
#let's select the columns with relevant information for analyzing the cumulative results and name it covid_df_cum
covid_df_cum <- covid_df_all_states %>% select(2, 4:10)


#summarizing the total number of cases for the selected columns and calculating relevant percentages of cases by country
covid_df_cum_summ <- covid_df_cum %>% group_by(Country_Region) %>% 
  summarize(positive = sum(positive),
            recovered = sum(recovered),
            death = sum(death), 
            tested = sum(total_tested), 
            perc_positive = round(positive / tested * 100, 2),
            perc_recovered = round(recovered / tested * 100, 2),
            perc_death = round(death / positive * 100, 2)) %>% arrange(desc(tested))


#displaying information about the tested, positive and death cases
#top 5 countries by number of people tested
top_five_tested <- covid_df_cum_summ %>% 
  select(Country_Region, tested) %>% 
  head(5)


#top 5 countries by number of positive cases
top_five_positive <- covid_df_cum_summ %>% 
  select(Country_Region, positive) %>% 
  arrange(desc(positive)) %>% 
  head(5)


#top 5 countries by number of death
top_five_death <- covid_df_cum_summ %>% 
  select(Country_Region, death) %>% 
  arrange(desc(death)) %>% 
  head(5)


#top 5 countries by rate of positive cases
top_five_per_positive <- covid_df_cum_summ %>% 
  select(Country_Region, perc_positive) %>% 
  arrange(desc(perc_positive)) %>% 
  head(5)


#top 5 countries by death rate
top_five_perc_death <- covid_df_cum_summ %>% 
  select(Country_Region, perc_death) %>% 
  arrange(desc(perc_death)) %>% 
  head(5)


#summarizing the total number of cases for the selected columns and calculating relevant percentages of cases by continent
covid_df_cum_cont <- covid_df_cum %>% group_by(Continent_Name) %>% 
  summarize(positive = sum(positive),
            death = sum(death), 
            tested = sum(total_tested), 
            perc_positive = round(positive / tested * 100, 2),
            perc_death = round(death / positive * 100, 2),
            prop_death = round(perc_death * 100 / 17.87, 2)) %>% 
  arrange(desc(tested))


#ranking total tested by continent
cont_tested_rank <- covid_df_cum_cont %>% 
  select(Continent_Name, tested) %>% 
  arrange(desc(tested))


#ranking positive cases by continent
cont_positive_rank <- covid_df_cum_cont %>% 
  select(Continent_Name, positive) %>% 
  arrange(desc(positive))


#ranking death cases by continent
cont_death_rank <- covid_df_cum_cont %>% 
  select(Continent_Name, death) %>% 
  arrange(desc(death))


#ranking proportion of death from positive cases
prop_death_ranking <- covid_df_cum_cont %>% 
  select(Continent_Name, perc_death) %>% 
  arrange(desc(perc_death))


#ranking continents that account for the most death across all continents
perc_death_ranking <- covid_df_cum_cont %>% 
  select(Continent_Name, prop_death) %>% 
  arrange(desc(prop_death))

library(ggplot2)
a <- cont_death_rank %>% filter(death > 0)
pie(x = a$death, a$Continent_Name, col = c("green", "yellow", "red"))

top_five_death %>% ggplot(aes(x = Country_Region, y = death, fill = Country_Region)) + 
  geom_col() + labs(title = "Top Five Highest Covid Death (Country)", x = "Countries",
                    y = "Deaths")

top_five_per_positive %>% ggplot(aes(x = Country_Region, y = perc_positive, fill = Country_Region)) + 
  geom_col() + labs(title = "Countries ", x = "Countries",
                    y = "Deaths")