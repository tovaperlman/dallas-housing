# Housing Permanence in Dallas, TX

Authors: Tyler Bradford, Al-Jalil Gault, Tova Perlman

Welcome to our repository for the MUSA 801 Practicum course at UPenn's Weitzman School of Design. 

In this project, we will analyze and predict for housing permanency in the Dallas Metro area. 

Brief Introduction: 
Every year in America, government services attempt to identify and provide help to individuals experiencing homelessness, entering their information into the HMIS (Homeless Management Information System) in order to track clients through time and measure their ability to receive services and ultimately secure permanent housing. Housing interventions for clients include temporary services like Emergency Shelters, Safe Haven, Street Outreach, and Transitional Housing, and  more permanent longer term housing solutions including Rapid Re-Housing, Permanent Supportive Housing, and Other Permanent Housing. Using HMIS data, our client, the Metro Dallas Homeless Alliance (MDHA), seeks to gain a deeper understanding of where individuals in Dallas receive services and what underlying factors cause them to continue to return to homeless services even once they are housed. 
We’ve decided on a two-pronged approach to our use case based on our client, MDHA, and the Continuum of Care service providers. For MDHA, our use case evaluates longitudinal data entries of HMIS individuals and the trajectory of their experiences with housing services to predict outcomes for  individuals who have more recently entered the system (within the past 6 months). From this use case, we will predict the needs and demands on various program types in advance in order to make sure the housing supply is available. Our modeling will also provide a clear understanding of an individual's temporal movement through homeless services and predict their next steps.
The use case on a service provider level helps to inform individual providers on the likelihood of achieving success with a certain client. Thus, if we are able to input certain characteristics about the individual, we can help both the service provider and client determine the housing intervention that leads to the highest chance of success. For example, if our model can predict our client has a 60% likelihood of recidivating by being placed in Rapid Re-Housing but a 20% likelihood of recidivating if placed in Permanent Supportive Housing, then we might want to place the client in the latter group. Additionally, we want to pay extra attention to individuals experiencing chronic homelessness to evaluate what program types or specific service providers are more or less successful in housing individuals for long periods of time. 


We are excited to continue working with our client and refine our use case over time. The following questions will guide our exploratory data analysis:
Time - How can we use our data structures to tell the story of a client over time? 
At what time during the year are people more likely to enter or exit the system? - Tyler
Temporal data on length of stay in TH, RRH or PSH
Look at the date of engagement and move in data separately. How long does it take from being entered into the system to finding permanent housing?
Space - How are clients moving throughout space
How leverageable is the location data in the datasets? Since there is only one location value per individual per table, what inference can we derive from these spatial data?
What are the most popular and least popular services people need? Does this depend on time of year? Do different demographic groups require different types of services? Could do small multiple maps of services by demographic group
Recidivism - Predictors for chronic homeslessness 
Examination of most popular PriorResidence - is this a predictor for chronic homelessness?
Analysis of continuous stays per individual, average duration of stay, average time between recidivating
Is recidivism largely driven by internal factors (e.g., demographics, income) or external (e.g., services received) or both? Can we identify particular combinations of services that are more likely to result in recidivism?
What is the difference in housing interventions between individuals and families? - Tyler 
We could also group services under larger categories e.g. financial assistance vs services assistance - what is the difference in how people are using these services?
What types of income are individuals getting and from where? (Ex: TANF, SSI, Etc)
