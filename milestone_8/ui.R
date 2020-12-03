#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(shinythemes)
library(leaflet)
library(wordcloud)
library(tm)
library(wordcloud2)
library(gganimate)
library(broom.mixed)
library(gtsummary)
library(rstanarm)
library(markdown)
library(Rcpp)

ui <- navbarPage("The Impact of Coronavirus On Arts Organizations",
                 tabPanel("Explore Survey Demographics",
                          fluidPage(titlePanel("Explore Survey Demographics"),
                                    h3("Background and Motivations"),
                                    p("In March 2020, Americans for the Arts, the nation’s 
                                    largest arts advocacy organization, launched a 
                                    survey measuring the Economic Impact of Coronavirus 
                                    on the Arts and Culture Sector. Since then, more than 
                                    17,000 artists and arts organizations have responded to the survey, 
                                    providing firsthand insight into a sector that 
                                    has been especially hard hit by the economic downturn of the COVID-19 pandemic. 
                                    This project will aim to explore the results of that survey, investigating how COVID-19 has impacted 
                                      the economic landscape of arts organizations around the nation."),
                                    br(),
                                    br(),
                                    fluidRow(
                                      column(5, h3("When was the survey completed?"),
                                             p("Americans for the Arts first opened their 
                                               COVID-19 impact survey on March 13th, 2020, 
                                               right as a wave of lockdowns, closures, and 
                                               stay-at-home orders swept the nation. While 
                                               the survey has continued to be open up until the 
                                               present, the bulk of responses are from March through May.  
                                               This is important to keep in mind during analysis, as
                                               many reports of lost revenue pertain only to the amount 
                                               that an organization had lost within the first month or 
                                               so of the pandemic. Use the slider below to explore 
                                                 how many survey responses were garnered in a particular span of time."),
                                             sliderInput("dates", "Choose a Range of Dates",
                                                         min = as.Date("2020-03-13", "%Y-%m-%d"),
                                                         max = as.Date("2020-09-02", "%Y-%m-%d"), 
                                                         value = c(as.Date("2020-03-13", timeFormat = "%Y-%m-%d"),
                                                                   as.Date("2020-09-02", "%Y-%m-%d")))),
                                      column(7, plotOutput("num_org"))
                                    ),
                                    br(),
                                    br(),
                                    fluidRow(
                                      column(7, plotOutput("plot2")),
                                      column(5, h3("What types of organizations answered the survey?"),
                                             p("When most people hear the words “arts organizations,” they 
                                                 likely think of theaters, galleries, or museums. While these 
                                                 are important parts of the sector, arts organizations also 
                                                 encompass places like local, state and regional agencies that 
                                                 provide networking and financial support, media arts 
                                                 organizations, and community arts centers. Explore what 
                                                 the breakdown of organizations who answered the survey looked like in your state"),
                                             selectInput("var2", "State", c("Alaska", "Arizona", "Arkansas", 
                                                                            "California", "Colorado", "Connecticut",
                                                                            "Florida", "Georgia", "Hawaii", 
                                                                            "Idaho", "Illinois", "Indiana",
                                                                            "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", 
                                                                            "Massachusetts", "Michigan", 
                                                                            "Minnesota", "Mississippi", "Missouri", 
                                                                            "Montana", "Nebraska", 
                                                                            "Nevada", "New Hampshire", "New Jersey", "New Mexico",
                                                                            "New York", "North Carolina", 
                                                                            "North Dakota", "Ohio", 
                                                                            "Oklahoma", "Oregon", "Pennsylvania",
                                                                            "Rhode Island", "South Carolina", 
                                                                            "South Dakota", "Tennessee", 
                                                                            "Texas", "Utah", "Vermont",
                                                                            "Virginia", "West Virginia", 
                                                                            "Washington", "Wisconsin")))
                                    ),
                                    br(),
                                    br(),
                                    fluidRow(
                                      
                                      column(4, h3("What size of organization answered the survey?"),
                                             p("Arts organizations come in all sizes. While the majority of 
                                                 survey respondents were small nonprofits, several larger organizations
                                                 with self-reported annual budgets of upwards of $10 million responded
                                                 as well. Though they might not suffer the same risks of complete closure 
                                                 like smaller organizations do, these larger organizations also faced major
                                                 financial barriers due to the pandemic, as we will later explore.")),
                                      column(8, br(),
                                             plotOutput("plot1"))
                                    ),
                                    br(),
                                    br(),
                                    h3("Where were responses from?"),
                                    p("Responses to the coronavirus impact survey came from all over the nation.
                                      Explore the map below to see how many organizations responded in each zip code."),
                                    leafletOutput("map")
                          )),
                 tabPanel("Modeling Lost Revenue",
                          h2("The Task: Building a Model to Predict Revenue Loss"),
                          p("Understanding the amount of revenue that organizations have lost 
                            due to the pandemic is crucial in formulating relief measures that properly
                            address the needs of artists and arts organizations. The following page
                            considers several different models for predicting the amount of revenue
                            loss for an organization with an annual budget of between $100,000 and 
                            $249,000."),
                          br(),
                          br(),
                          fluidRow(
                            column(5, h3("Focusing on Smaller Organizations: Rationale"),
                                   p("As shown on the previous page, organizational budgets ranged 
                            from all volunteer to upwards of $10 million, resulting in a large range
                            of organizational revenue loss. 
                            For the purposes of building a model, I chose to focus on 
                            organizations with self-reported annual budgets of $100,000 
                            to $249,000 for several reasons. Firstly, budget was reported 
                            in broad categories rather than a continuous numerical variable, 
                            and there is less variation within categories for organizations 
                            of smaller budgets (this means that there will be less variation 
                            that occurs outside of the model). Furthermore, this was the 
                            second most common category, after organizations with an annual 
                            budget of less than $100,000, a category that I feared would be 
                            a confusing mix of individuals and organizations. Finally, as 
                            shown in the plots to the right, organizations of this size felt 
                            the least likely that they would survive the pandemic, 
                            suggesting a particular need for models to understand 
                            more about how much they were losing and what could be done to help.")),
                            column(7, br(), plotOutput("survival_budget"))),
                          br(),
                          br(),
                          fluidRow(
                            column(9, tableOutput("var_table")),
                            column(3, h3("Model Building Process"), p("Initially, I constructed
                                                                      three different models to predict lost
                                                                      revenue for small arts organizations with three levels
                                                                      of complexity. The simplest model used only one continuous 
                                                                      variable, the number of lost attendees. The medium model incorporated
                                                                      a factor variable for the type of organization, based on my hypothesis
                                                                      that front-facing organizations such as museums or galleries might have
                                                                      more to lose in the pandemic than networking organizations, such as 
                                                                      state arts agencies. The third and most complex model considered several
                                                                      additional variables that I thought could also indicate greater lost revenue
                                                                      for an organization; whether they had cancelled events, whether they had
                                                                      closed their business, 
                                                                      and the amount of money they had spent on unexpected new expenditures."))
                            
                          ),
                          br(),
                          br(),
                          fluidRow(
                            
                            column(8, h3("Model Selection"),
                                   p("As shown, the medium complexity model had the smallest RMSE,
                                     in cross validation, but it was not significantly different 
                                     enough from the simple model that would indicate the extra 
                                     complexity was worthwhile."),
                                   br(),
                                   p("Why might these more complex models have been less effective at predicting
                                     lost revenue? I think some of it may have had to do with the high variation
                                     in factors unexplained by any of the models; namely, how exactly the survey respondents
                                     estimating lost revenue. Survey participants were asked to 'Estimate 
                                     how much your organization's revenue  has decreased as a result of the coronavirus?', but
                                     it is possible that some people may have either provided an incorrect estimate or
                                     inadvertently included ambiguous events (i.e. do you count a concert that was scheduled
                                     for the future but will likely be canceled as lost revenue?). Furthermore, while I limited
                                     the survey response dates for this analysis from March 13th to June 1st in order to decrease
                                     the impact of time as a variable, one would expect an overall increase in lost revenue over even
                                     this short span of time (ultimately, I chose not to include time in the model due to the high 
                                     variation in number of responses per day). Because the number of lost attendees naturally
                                     matches with the span of time/method by which individual organizations calculated their lost revenue,
                                     it makes sense that it works effectively as a predictor on its own.")),
                            column(4, tableOutput("models_table"))),
                          br(),
                          br(),
                          fluidRow(
                            column(7, h3("Interpreting the Final Model"),
                                   p("The median of the posterior distribution of estimated revenue 
                                     loss for an organization with 0 lost attendees is $29,220.80, 
                                     suggesting that the pandemic has had a severe economic impact 
                                     on non-presenter arts organizations. For every additional lost 
                                     attendee, the predicted revenue loss to small arts organizations 
                                     is $1.45 (95% confidence interval: $1.24 to $1.65). The median 
                                     of the posterior distribution for sigma, the true standard 
                                     deviation of the lost revenue of small arts organizations, is 
                                     $31,490, suggesting that there is great variation, with some 
                                     organizations losing next to nothing and others losing over 50% 
                                     of their annual budget (note that this only takes into account the March-May timeframe).")),
                            column(4, offset = 1, tableOutput("final_mod"))),
                          br(),
                          br(),
                          fluidRow(
                            column(8, plotOutput("pred_plot")),
                            column(4, h3("Predicting Revenue Loss"),
                                   p("Use the slider below to make predictions about how much
                                     an organization of a budget between $100,000 and $249,000 
                                     with a certain number of lost attendees may have lost in revenue
                                     during the first few months of the pandemic."),
                                   sliderInput("attendees", "Number of Attendees",
                                               min = 0, max = 40000,
                                               value = 0, step = 200)))),
                 tabPanel("In Dialogue with the Pandemic",
                          fluidRow(
                            column(4, h3("Understanding Sector-Wide Impact"),
                                   p("To help put into perspective the widespread,
                                     national economic crisis that COVID-19 has caused 
                                     for arts organizations, take a look at unemployment rates.
                                     While unemployment rates for artists were lower than
                                     the general population at the beginning of 2020, they skyrocketed
                                     during the pandemic and have yet to come anywhere close to pre-pandemic
                                     levels. This suggests the need for special relief legislation target specifically
                                     at helping artists and arts organizations.")),
                            column(8, htmlOutput("animation"))
                          ),
                          fluidRow(
                            column(8, tabsetPanel(
                              tabPanel("Severity Financial Impact", plotOutput("states_1")), 
                              tabPanel("Likelihood Staff Reduction", plotOutput("states_2")), 
                              tabPanel("Chances of Survival", plotOutput("states_3")))
                            ),
                            column(4, h3("How did statewide case numbers affect arts organizations?"),
                                   p("Did arts organizations around the nation feel the effects
                                   of the pandemic similarly? The answer seems to be yes (at least
                                   for the March-May timeframe, which is what's shown in the plot).
                                   Despite some states such as New York, New Jersey, and Massachussets
                                   having much higher rates of COVID-19, arts organizations with
                                   budgets between $100,000 and $249,999 (the same sub-group predicted
                                   in the model) self-rated the economic damage done to their organizations
                                   at similar levels around the nation. This
                                   lack of an obvious relationship between statewide COVID-19 case rates and 
                                   economic impact on arts organizations, as shown by the nearly flat trendline,
                                   suggests that programs at the national level could be an effective
                                   way to address this widespread economic hardship.")))),
                 tabPanel("Textual Responses",
                          wordcloud2Output("cloud"),
                          br(),
                          br(),
                          fluidRow(
                            column(5, h3("Beyond Numbers: Stories of the Pandemic's Impact"),
                                   p("At the end of the survey, participants were asked an optional
                                       open response question: Is there anything else you would like to 
                                       share about the impact of COVID-19 on your organization? People
                                       responded to this prompt in a variety of ways. Some expressed 
                                       fear about the unknown nature of the pandemic and its impact on 
                                       the arts sector, or detailed losses in the forms of cancelled 
                                       performances, slashed income, and social isolation. Others 
                                       detailed the innovative ways their organization had adapted to
                                       provide services for their community and expressed hope that 
                                       the arts could serve as a unifying and comforting force for 
                                       a fractured society. In later months, many respondents expressed
                                       a sense of exhaustion at the difficult and often demoralizing work
                                       of being an artist or arts administrator in this time. Many responses
                                       touched on multiple of these themes."),
                                   br(),
                                   p("Working with large datasets can sometimes feel very distant from
                                         the individual lives and stories that make up your data. I hope that
                                         by exploring some of the most commonly used words in participants'
                                         responses to these questions and reading through a few of the actual
                                         responses(organized by theme), you can begin to get a sense of the 
                                         profound impact of COVID-19 on the individuals who help produce art
                                         in this country")),
                            column(6, offset = 1, h4("Choose a sentiment using the drop down menu below to explore individual
                                           responses to the impact survey"),
                                   selectInput("sentiment", "Sentiment of Response", c("Fear", "Loss", "Resilience",
                                                                                       "Exhaustion")),
                                   tableOutput("response_table")))),
                 tabPanel("About",
                          
                          h2("About this Project"),
                          br(),
                          h4("About the Data"),
                          p("The bulk of the data for this project came from the Americans for the Arts (AFTA) 
                            survey on 'The Economic Impact of Coronavirus on the Arts and Culture Sector'. 
                            While AFTA has not made the raw survey responses available to the public due to 
                            confidentiality reasons, you can learn more about the survey and specific results 
                            on the", a("interactive dashboard on their website.", href = "https://www.americansforthearts.org/by-topic/disaster-preparedness/the-economic-impact-of-coronavirus-on-the-arts-and-culture-sector")),
                            p("The data on coronavirus cases came from", a("The Covid Tracking Project.", href = "https://covidtracking.com/data/download"),
                              "The data unemployment data across sectors was reported from the U.S. Bureau of Labor Statistics and can be accessed", a("on their website.", href = "https://data.bls.gov/timeseries/LNU04032241?amp%253bdata_tool=XGtable&output_view=data&include_graphs=true")),
                          p("For the code used to create this project, check out my", a("Github Repository.", href = "https://github.com/renacohen/final_project")),
                          br(),
                          h4("About the Cause"),
                          p("If nothing else, I hope you took away from this project that artists and arts organizations have
                             been devastated by the economic effects of the COVID-19 pandemic. If you are interested in helping this
                             cause, consider reaching out to your local arts organizations, donating to an artist relief fund,
                             or contacting your representatitive to advocate for the arts using one of the many", a("advocacy tools offered by AFTA.", href = "https://www.americansforthearts.org/advocate")),
                          br(),
                          h4("Acknowlegements"),
                          p("First and foremost, I would like to thank my", strong("INCREDIBLE"), "TF Wyatt Hurt, whose patience, kindness, and penchant
                            for R-related memes gave me the motivation I needed to complete this project. I would also like to thank
                            head GOV-50 TF Tyler Simko for his incredible teaching skills and dedication to providing us all with 
                            such an excellent theoretical and coding background. Finally, I am very thankful to Randy Cohen, director
                            of Research at AFTA, as well as the rest of my AFTA colleagues for letting me explore their incredible data
                            for the purposes of this project."), 
                          h2("About Me"),
                          br(),
                          h4("Who am I?"),
                          p("An existential question I grapple with every day! But by way of a traditional
                          introduction, my name is Rena Cohen and I am a junior at Harvard studying
                          Women, Gender, and Sexuality with a secondary in Statistics. This project was 
                          completed as my final project for GOV 50, an introductory data science course.
                          In my free time, I love singing classical music, baking, thrifting, and hiking/running."),
                          br(),
                          h4("What is my relationship to Amerians for the Arts?"),
                          p("This summer, I was fortunate enough to serve as the Government Affairs and Education
                            intern at Americans for the Arts as part of the IOP's Director's Internship Program.
                            In this role, I helped to research and make advocacy materials to promote policies
                            that would include the arts and arts education in coronavirus relief efforts. As
                            an aspiring arts administrator, I was extremely grateful for the opportunity
                            to work with my colleagues at AFTA, and I hope that this project can serve
                            as a continuation to the incredible work that they do in ensuring all Americans
                            have access to create and experience artwork during this time.")))
