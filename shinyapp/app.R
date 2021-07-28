library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(stringr)
library(shinyjs)

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")


# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('navbar-brand');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic');
           }
           "

# user -------------------------------------------------------------
ui <- navbarPage(title = "Analyzing Vegetative Health using Landsat 8 Satellite Imagery",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 # main -----------------------------------------------------------
                 # tabPanel("Home", value = "home",
                 #          fluidRow(style = "margin: 6px;",
                 #                   align = "center",
                 #                   br("", style = "padding-top:10px;"),
                 #                   img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                 #                   br(""),
                 #                   h2(strong("Addressing Barriers to Health in Patrick County, Virginia"),
                 #                   br(""),
                 #                   h4("Data Science for the Public Good Program"),
                 #                   h4("University of Virginia"),
                 #                   h4("Biocomplexity Insititute"),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   p(tags$small(em('Last updated: August 2020')))
                 #                   )
                 #          )
                 # ),
                 
                 # main -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Analyzing Vegetative Health using Landsat 8 Satellite Imagery"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      h4("Biocomplexity Insititute"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(6,
                                          h2(strong("Project Background")),
                                          p(style= "text-align: justify;", "Human-nature interaction has long been a source of interest, study and analysis in the scientific community. As remote sensing and 
                                          data analysis technology become more sophisticated, the ability to create robust models to accurately measure and predict environmental 
                                          health increases. There are several pre-existing indices developed to determine the health of plant life from a remote sensing apparatus 
                                          like an airplane or, more commonly, a satellite. The first is the Normalized Difference Vegetation Index (NDVI). This measurement combines
                                           near-infrared (NIR) and red electromagnetic radiation from plants to produce an index that correlates closely with the true health of the 
                                           plant. Another index is the Enhanced Vegetative Index (EVI). According to the United States Geological Survey (USGS), “EVI corrects for 
                                           some atmospheric conditions and canopy background noise and is more sensitive in areas with dense vegetation”. This report uses these two
                                            indices as the main quantifiers of botanical health within a given area. "),
                                          
                                   ),
                                   column(6,
                                          h2(strong("Data Background")),
                                          p(style= "text-align: justify;","The Landsat 8 Satellite was launched in 2013 by NASA to collect high-resolution and electromagnetically diverse remote radiation data 
                                          about the Earth’s surface. The Landsat senses data from eleven distinct wavelengths of light, from the visible red, green and blue 
                                          wavelengths to infrared wavelengths for thermal imaging. These diverse ranges of sensing data hold the ability to filter and provide 
                                          insight into aspects of regions that do not appear visible in a standard RGB photograph. Landsat 8 also has a relatively high resolution, 
                                          with each pixel in most captured images corresponding to 30 meters of land area. In the panchromatic channel, used for detail, the 
                                          satellite reaches a detail rating of 15 meters per pixel. The Landsat 8 also provides data for the emission of aerosols, land surface 
                                          temperature, and cloud cover in a region of interest.
                                          
                                          This type of precise temporal sensing data provides a rich proving ground for many types of forecasting applications. One of the newest 
                                          and most powerful forecasting technologies is neural-network based machine learning. Neural networks provide a robust framework for 
                                          predicting nonlinear patterns from a large set of inputs. Using neural networks to accurately forecast metrics like NDVI and EVI would 
                                          prove useful to inform land-use policy and identify areas of concern."),
                                          
                                   ),
                                   
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2020'))))
                 ),
                 
                 # NDVI Predictions -----------------------------------------------------------
                 tabPanel("NDVI Predictions", value = "ndvi",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Floyd County: NDVI Predictions"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("What's the deal with NDVI?")),
                                          p("We examined Patrick County population sociodemographic and socioeconomic characteristics to better understand the 
                                            residents that the county serves."),
                                          p("We retrieved American Community Survey (ACS) data to calculate this information at census block group and census 
                                            tract levels. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. We used 
                                            the most recently available 5-year estimates from 2014/18 to compute percent Patrick County residents in a given block group or tract by age, race, ethnicity, 
                                            employment, health insurance coverage, and other relevant characteristics."),
                                          p("Our interactive plots visualize census block-group level sociodemographic characteristics of Patrick County residents.")),
                                   column(8,
                                          h4(strong("Map of NDVI Predictions")),
                                          selectInput("NDVIPredictions", "Select Year:", width = "100%", choices = c(
                                            "2021", "2022", "2023", "2024"
                                          )),
                                          p(strong("NDVI Predictions")),
                                          withSpinner(leafletOutput("NDVIMap"))
                                   ))
                 ),
                 
                 # older -----------------------------------------------------------
                 tabPanel("Data and Methodology", value = "older",
                          fluidRow(style = "margin: 6px;", align = "center",
                                   h1(strong("Using Landsat 8 Images"), align = "center"),
                                   #p("", style = "padding-top:10px;"),
                          )
                 ,
                 fluidRow(style = "margin: 6px;", align = "center",
                                   column(11,
                                          h4(strong("")),
                                          p(style = "text-align: justify;", "Launched in 2013, the Landsat 8 satellite is the latest in a series of Landsat predecessors dating back to the 1970’s. 
                                          The data captured on the Landsat 8 satellite is useful for two reasons: firstly, it uses high-resolution sensors. One pixel 
                                          of the Landsat 8’s color bands corresponds to 30 meters of earth, roughly the size of a baseball diamond as shown below. 
                                          There is also a panchromatic band that takes photographs at the 15m resolution, allowing for even higher-detail interpolation 
                                          of satellite images. The second significant advantage of using Landsat 8 satellite imagery is the diversity of wavelengths of light captured 
                                          in each photograph. The Landsat 8 captures eleven distinct “bands” of light:  "),
                                          img(src = "Picture1.png", style = "text-align:left;"),
                                          img(src = "Picture2.png", style = "text-align:right;", width = "550px"),
                                          
                                          img(src = "Picture3.png", style = "display: inline;"),
                                          img(src = "Picture4.png", style = "display: inline;"),
                                          p(style = "text-align: justify;", "The landsat 8 captures images corresponding to roughly 250x250 kilometer sections of earth. The different bands can be 
                                          combined to form all sorts of useful secondary images synthesized from the raw wavelengths. The image below is a true-color 
                                          synthesis of the red, green and blue bands of the Landsat Satellite of Las Angeles, California: "),
                                          img(src = "Picture5.png", style = "display: inline; float: center;", width = "550px")
                                   ),
                                   
                                            )
                 ),
                 
                 # wifi-----------------------------------------------------------
                 tabPanel("Derived Indices", value = "connectivity",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Derived Indices"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(12,
                                          #h4(strong("Computing Device Ownership and Internet Access Type")),
                                          p(style = "text-align: justify;", "Oftentimes, measurements of natural phenomena are too complex to be accurately 
                                          described solely with remote sensing. Calculated indices bridge the gap between satellite imagery and internal 
                                          vegetative processes. The indices of particular interest in this project are the Normalized Difference 
                                          Vegetative Index (NDVI) and the Normalized Difference Water Index (NDWI). NDVI is strongly correlated to the 
                                          overall health of plant foliage.1 The Normalized Difference Water Index is strongly correlated to the amount 
                                          of water held in plant foliage.2 These are both complex phenomena that would be impossible to calculate for 
                                          each individual pixel of a satellite image, but with the help of indices, we can synthesize a good indicator 
                                          of vegetative health using relatively little reflection data. The different wavelengths of light captured by 
                                          the Landsat 8 satellite can be used to synthesize the NDVI and NDWI indices of interest."),

                                          p(strong(style = "text-align: justify;","Normalized Difference Vegetative Index")),
                                          p(style = "text-align: justify;","The Normalized Difference Vegetative Index is derived from the Near Infrared 
                                          light and Red light emitted from plants. It is described in detail in Nathalie Pettorelli’s book The Normalized 
                                          Difference Vegetative Index. The Landsat 8 satellite picks up both of these wavelengths of light and the USGS 
                                          provides a formula for producing this index:  "),

                                          p(strong(align = "center","NDVI = (NIR - R) / (NIR + R)")),
                                          p(style = "text-align: justify;", "This allows us to create aerial maps of NDVI for a particular region by 
                                          combining individual pixel values. A Map of the NDVI of Southwest Virginia and Southern West Virginia is shown below: "),

                                          p(style = "text-align: justify;", "This allows us to create aerial maps of NDVI for a particular region by 
                                          From these types of aerial maps of derived indices, conclusions about distribution and trends in the vegetative
                                           health over time and throughout the region can be made. The Normalized Difference Vegetative Index has been 
                                           used in applications such as precision agriculture, drought monitoring, flooding and precipitation patterns.
                                            The aim of this product is to predict the NDVI by combining the band data from the Landsat 8 satellite to
                                             give an accurate prediction of how the NDVI will change over time."),  

                                          img(src = "Picture6.png", style = "display: inline"),   

                                          p(strong(style = "text-align: justify;","Normalized Difference Water Index")),
                                          p(style = "text-align: justify;","The Normalized Difference Water Index (NDWI) is highly correlated with the
                                           amount of water stored in the foliage of plants, as described in Bo-cai Gao’s paper.4 The NDWI is sometimes 
                                           described as the Normalized Difference Water Index or NDMI. The USGS also provides a formula to calculate 
                                           the NDWI by combining the Short-Wave Infrared with the Near Infrared wavelengths of light captured from the
                                            Landsat 8 satellite in the following formula: "),

                                          p(strong(align = "center","NDMI = (Band 5 – Band 6) / (Band 5 + Band 6)")),    

                                          p(style = "text-align: justify;","Like the NDVI, this formula allows for per-pixel calculation of this index to describe the distribution of water
                                           in vegetation throughout the new river valley, as shown in the image below:"),   

                                          img(src = "Picture7.png", style = "display: inline;"), 
                                          p(style = "text-align: justify;","By synthesizing specific wavelengths of light, the raw images are encoded as 16-bit positive integer values, 
                                          so they must be converted to 64-bit floating point values in order to be manipulated.  ")

                                           
                                          
                                          )
                                          
                                            )
                 ),
                 
                 # ems -----------------------------------------------------------
                 tabPanel("ML Methodolgy and Results", value = "ml",
                                 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Machine Learning Methodology"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(12,
                                          h4(strong("Introduction")),
                                          p(style = "text-align: justify;", "The main goal of this project is to use modern data-science techniques to produce a model that is able to predict the Normalized Difference Vegetative Index and the Normalized Difference Water Index to a high degree of accuracy at a high resolution using existing wavelength data captured by the Landsat 8. This opens the door to identify trends and areas of concern within a region that might otherwise look healthy. The tool used to accomplish this process is a feed-forward neural network that is trained off of existing satellite pairs. The model trained in this paper looks at an ability to predict vegetative health in two year intervals. The area of interest in this project is the New River Valley of Southwest Virginia and specifically Floyd County, Virginia. This is the region that the model was initially trained on.  

Raw satellite images were downloaded from the Earth Explorer via the United States Geological survey as Geotiff images of the different wavelengths of light captured. A great deal of data cleaning, conditioning and filtering had to be performed before the images could be fed into a machine learning model.  

The first such problem came when the raw images were encoded as 16-bit positive integer values, and needed to be converted to 64-bit floating point values in order to be manipulated. Otherwise, non-natural values result in high-values instead of negative ones that make the resulting indices and processing useless. The other technical nuances encountered when using the raw satellite images are described in the following sections."),

                                          #p(strong(style = "text-align: justify;","Normalized Difference Vegetative Index")),
                                          h4(strong("Top of Atmosphere Reflectance")),
                                          p(style = "text-align: justify;","When a satellite takes progressive images of the earth, it is rare that the sun is in the same position each time it takes a picture. Therefore, the angle of the sun needs to be taken into account when examining multiple images of the earth over time. The United States Geological Survey employs a formula to convert the raw intensity values captured by the Landsat to Top of Atmosphere reflectance. The values for the Sun Elevation angle and correction values are included in the metadata of each GeoTiff image. The United States Geological Survey Provides the following reference for converting raw values to Top of Atmosphere Reflectance:"),

                                          img(src = "Picture10.png", style = "display: inline"),
                                          p(tags$small("Reference: United States Geological Survey ")),

                                          h4(strong("Filtering Data")),
                                          p(style = "text-align: justify;", "It is important to note that the focus of this project is on vegetation. As it happens, when a satellite snaps a photograph of the earth, it tends to capture much more than just vegetation. Therefore, the team needed a way to filter out clouds and their shadows, water and non-vegetative urban areas. The team used the 9th Band of the Landsat 8 satellite to filter out clouds and developed an algorithm that searched for shadows based on dips in reflectance around clouds. The team also employed a method to filter out the lakes, ponds and rivers in a scene primarily using the Near Infrared Band (Band 5) in combination with others. This method is described in the paper Identification of Water Bodies in a Landsat 8 OLI Image Using a J48 Decision Tree. Once all of these filtration algorithms were applied, the satellite image of Southwest Virginia looked accordingly: "),
                                          
                                          img(src = "Picture11.png", style = "display: inline"),
                                          p(tags$small("A measure of NDVI after the region was filtered with the filtration algorithm. Note the absence of Urban areas such as Roanoke and Blacksburg/Christiansburg, Smith Mountain Lake and a large overhanging Cloud on the Scene.  ")),
                                          
                                          h4(strong("Aligning Photographs")),
                                          p(style = "text-align: justify;", "Because this model seeks to predict vegetative health in 2-year intervals, it is imperative that “before” and “after” data is gathered. Therefore, we chose landsat images taken of the exact same region of Southwest Virginia. The only problem was that the satellite images were virtually identical, but once the team began examining them closely, we realized that they were ever so slightly off. A detailed image of the displacement is shown below: "),
                                          img(src = "Picture12.png", style = "display: inline"),
                                          p(style = "text-align: justify;", "Therefore, the team had to develop an algorithm to align before and after images so that the training sets would line up properly at the pixel level. We managed to accomplish this by subtracting the Band 5 Near Infrared light reflectance for the two years and examining the change. Because features like water are well-absorbed by the NIR light, disparities in alignment became very apparent: "),
                                          img(src = "Picture13.png", style = "display: inline"),

                                          h4(strong("Creating Subsets")),
                                          p(style = "text-align: justify;", "Once the necessary areas had been filtered out so that the learning algorithm could focus on vegetation only, the image was divided into 20x20 pixel squares for each of the 11 distinct wavelengths of light emitted from that particular section. The model is input 11 different 20x20 subsets and forms a prediction of the same 20x20 pixel square of how the NDVI and NDWI changes.  "),
                                          img(src = "Picture14.png", style = "display: inline"),
                                          
                                          h4(strong("Building and Training a Neural Network")),
                                          p(style = "text-align: justify;", "Once all the 20x20 subsets of input and output pairs had been assembled, the team needed to create a neural network to handle the more than 4000 inputs per scene. This meant that in a single satellite image, there were going to be over 260,000,000 data points to train from. The team developed a feed-forward neural network with 5,475 neurons and over 4,000,000 weights connecting the neurons together to accommodate the large volume and diversity of data. The structure of the full neural network is below: "),
                                          img(src = "Picture15.png", style = "display: inline"),
                                          
                                          p(style = "text-align: justify;", "The first 85,000 input/output scenes were used to train the model and the roughly 16,000 remaining were set aside to test the conditioned model. Over time, scenes from different time periods and areas of Virginia were used to increase the robustness of the model.  "),

                                          h1(strong("Machine Learning Results"), align = "center"),
                                          h4(strong("Testing Accuracy ")),
                                          p(style = "text-align: justify;", "The testing results below are for two different time periods of Southwest Virginia and an Image of Central Virginia. The accuracy percentage is the average percentage correct of predicted results and on the true values. The Loss Function used in this training was Means Absolute Error, which is the absolute value of the distance from the predicted values to the true values. The Loss values in the table are the average losses for all 16,000 test samples.  "),
                                          img(src = "Picture16.png", style = "display: inline", width = "550px"),

                                          h4(strong("Visualizing Accuracy ")),
                                          p(style = "text-align: justify;", "For a particular training set of the Southwest Virginia subset from 2014 to 2016, the error over time is shown below:  "),
                                          img(src = "Picture17.png", style = "display: inline"),
                                          
                                          
                                          )
                                          
                                            )
                 ),
                 
                 
                 # food -----------------------------------------------------------
                 tabPanel("Food Access", value =  "food",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Food Access in Patrick County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(5,
                                          h4(strong("Food Access in Rural Areas")),
                                          p("Social determinants of health shape food access, a key factor in negative health outcomes. Rural area residents frequently face difficulties in accessing 
                                             healthy and nutritious food, and experience high rates of chronic illnesses like heart disease and diabetes, resulting in higher mortality rates and lower
                                             life expectancy compared to urban areas. Facilitating  access to nutritious and high-quality foods can lead to decreases in chronic disease prevalence. 
                                             Many Patrick County residents suffer from conditions like diabetes and obesity, and providing healthy food may support disease prevention."),
                                          p("We analyzed multiple data sources to give Patrick County actionable information on their residents’ food access that can inform county efforts ensuring equitable food access for all."),
                                          p("First, we examined", strong("food access at multiple distance thresholds by age and socioeconomic status."), "We used the 2017 United States Department of 
                                             Agriculture (USDA) Food Access Research Atlas, a central database created by the Economic Research Service that provides access indicators for different social groups.
                                             We created census tract-level maps that identify Patrick County areas where residents may have difficulty accessing nutritious foods, and highlight geographies 
                                             where this is the case for particularly vulnerable groups like low-income individuals and older adults."),
                                          p("Second, to better understand how residents must travel to obtain food, we constructed isochrones—shapes covering places within reach in the 
                                             same time frame given a start location and a mode of transportation—from Patrick County", strong("residential properties to locations of grocery stores 
                                             and farmers’ markets."), "We used Google Maps to identify these locations' latitude and longitude. We extracted locations of 
                                             Patrick County residential properties from 2019 CoreLogic, a proprietary dataset for US real estate with information on building characteristics. 
                                             Finally, we used the TravelTime Application Programming Interface (API) to calculate 10- and 15-minute car travel times from grocery locations. TravelTime 
                                             API aggregates data from Open Street Maps, transport timetables and speed profiles to generate isochrones. This allowed us to identify food deserts, or clusters 
                                             of properties that cannot reach a location with healthy food within a selected travel time range. These areas in the county could benefit from programs 
                                             facilitating access to produce."),
                                          p("Finally, Patrick County offers", strong("access to free food"), "at multiple locations. For community members that struggle with food security, these locations can 
                                             offer temporary assistance. We used GoogleMaps to locate food banks, food pantries, and community meal sites, geocoded their addresses, and mapped
                                             these resources along with notes on their target audiences.")
                                   ),
                                   column(7,
                                          tabsetPanel(
                                            tabPanel("Food Access",
                                                     p(""),
                                                     selectInput("usdadrop", "Select Variable:", width = "100%", choices = c(
                                                       "Percent Population with Low Food Access at 1 Mile" = "lapop1share",  
                                                       "Percent Population with Low Food Access at 10 Miles" = "lapop10share",
                                                       "Percent Children with Low Food Access at 1 Mile" = "lakids1share",
                                                       "Percent Children with Low Food Access at 10 Miles" = "lakids10share",
                                                       "Percent Low Income Population with Low Food Access at 1 Mile" = "lalowi1share",
                                                       "Percent Low Income Population with Low Food Access at 10 Miles" = "lalowi10share",
                                                       "Percent Older Adults with Low Food Access at 1 Mile" = "laseniors1share",
                                                       "Percent Older Adults with Low Food Access at 10 Miles" = "laseniors10share")
                                                     ),
                                                     p(strong("Map of Access at Census Tract Level")),
                                                     withSpinner(leafletOutput("usdaplot")),
                                                     p(tags$small("Data Source: USDA Food Access Research Atlas, 2017"))
                                            ),
                                            tabPanel("Grocery and Farmers' Market Coverage",
                                                     p(""),
                                                     selectInput("grocdrop", "Select Location:", width = "100%", choices = c(
                                                       "Mountain Meadow Farm and Craft Market",
                                                       "Lowes Foods of Stuart",
                                                       "Patrick County Local Farmers Market",
                                                       "Stuart Farmers Market",                
                                                       "W & W Produce",
                                                       "Walmart Supercenter",
                                                       "Poor Farmers Farm")),
                                                     p(strong("Percent Households Covered")),
                                                     withSpinner(tableOutput("groctable")),
                                                     p(strong("Map of Coverage")),
                                                     withSpinner(leafletOutput("grocplot")),
                                                     p(tags$small("Data Source: Google Maps; TravelTime API; CoreLogic, 2019."))
                                            ),
                                            tabPanel("Food Deserts",
                                                     p(""),
                                                     p(strong("Percent Households Covered")),
                                                     withSpinner(tableOutput("allgrctable")),
                                                     p(strong("Map of Food Deserts")),
                                                     withSpinner(leafletOutput("allgroc")),
                                                     p(tags$small("Data Source: Google Maps; TravelTime API; CoreLogic, 2019."))
                                            ),
                                            tabPanel("Food Security Resources",
                                                     p(""),
                                                     p(strong("Map of Food Security Resources")),
                                                     withSpinner(leafletOutput("othermap")),
                                                     p(tags$small("Data Source: Google Maps."))
                                            )
                                          )
                                   )
                          )
                 ),
                 # data -----------------------------------------------------------
                 tabPanel("Data and Measures", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data and Measures"), align = "center"),
                                   br()
                          ),
                          tabsetPanel(
                            tabPanel("Data Sources",
                                     h3("", align = "center"),
                                     br(""),
                                     column(4, 
                                            img(src = "data-hifld.png", style = "display: inline; float: left;", width = "100px"),
                                            p(strong("Homeland Infrastructure Foundation-Level Data."), "Homeland Infrastructure Foundation-Level Data (HIFLD) is a collection of public 
                                              source datasets at property level provided by the Department of Homeland Security. Since 2002, this HIFLD has provided quarterly 
                                              updated layers on topics from education to energy, including on health care facilities. We used HIFLD emergency medical services 
                                              station data at the latitude and longitude geographic level in our analyses."),
                                            br(""),
                                            img(src = "data-gmaps.png", style = "display: inline; float: left;", width = "130px"),
                                            p(strong("Google Maps."), "Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map
                                              of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within 
                                              a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to locate 
                                              all supermarkets, convenience stores, and farmers’ markets in Patrick County, and subsequently employed the information in calculating 
                                              grocery access and coverage isochrones.")
                                     ),
                                     column(4,
                                            img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                                            p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census 
                                            Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and 
                                            socioeconomic characteristics including employment, disability, and health insurance coverage. We used AC 2014/18 5-year 
                                            estimates to obtain census tract and census block group-level to explore Patrick County resident characteristics."),
                                            br(""),
                                            img(src = "data-connect.png", style = "display: inline; float: left;", width = "150px"),
                                            p(strong("CommonwealthConnect."), "The Virginia Tech CommonwealthConnect Wi-Fi Hotspot Map is an interactive map of free, publicly 
                                           available wi-fi hotspots in Virginia. Its goal is to provide an easily accessible map of areas where individuals can connect to the 
                                           internet for free, decreasing the constraints placed on families that do not have internet access at home. We used the 2020 wi-fi 
                                           hotspot map data to retrieve hotspot locations in Patrick County and subsequently employed the information in calculating hotspot 
                                           coverage isochrones."),
                                            br(""),
                                            img(src = "data-corelogic.png", style = "display: inline; float: left;", width = "120px"),
                                            p(strong("CoreLogic."), "CoreLogic is a supplier of proprietary US real estate and specialized business data at the property level. 
                                           This company provides data spanning over 50 years at the latitude and longitude level. Information available in the dataset includes 
                                           property characteristics, mortgage, foreclosures and performance. We used 2019 CoreLogic data to obtain the locations of all residential
                                           properties in Patrick County.")
                                     ),
                                     column(4,
                                            img(src = "data-traveltime.png", style = "display: inline; float: left;", width = "140px"),
                                            p(strong("TravelTime."), "TravelTime Application Programming Interface (API) aggregates data from OpenStreetMap, transport timetables and
                                           speed profiles to generate isochrones. An isochrone is a shape covering all locations that can be reached within the same timeframe 
                                           given a start location, departure time, and a mode of transportation. We used the TravelTime API to produce isochrones of 10- and 
                                           15-minute drive time interval from supermarkets, farmers' markets, and free wi-fi hotspots, and of 8-, 10-, and 12-minute drive 
                                           time intervals from all emergency medical service stations in Patrick County."),
                                            br(""),
                                            img(src = "data-ers.png", style = "display: inline; float: left;", width = "120px"),
                                            p(strong("Food Access Research Atlas."), "The United State Department of Agriculture Food Access Research Atlas is a data resource 
                                          created by the Economic Research Service that provides information on food access indicators at census tract level. The data allows 
                                          individuals to understand food access in communities based on factors like age and socioeconomic status. We used the 2017 Food Access
                                          Research Atlas to examine Patrick County residents’ food access at multiple distance thresholds and by resident characteristics.")
                                     )
                            ),
                            tabPanel("Measures",  
                                     h3(strong(""), align = "center"),
                                     selectInput("topic", "Select Topic:", width = "100%", choices = c(
                                       "All Measures",
                                       "Sociodemographic Measures",
                                       "Older Adult Population Measures",
                                       "Connectivity Measures",
                                       "Food Access Measures",
                                       "Health Care Access Measures")
                                     ),
                                     withSpinner(DTOutput("datatable"))
                            )
                          )
                 ),
                 
                 # contact -----------------------------------------------------------
                 tabPanel("Contact", value = "contact",
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h1(strong("Contact"), align = "center"),
                                   br(),
                                   h4(strong("UVA Data Science for the Public Good")),
                                   p("The", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"), 
                                     "is a summer immersive program held at the", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics', 'University of Virginia Biocomplexity Institute’s Social and Decision Analytics division (SDAD).'), 
                                     "In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around 
                              critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences 
                              to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program 
                              highlights, how to apply, and our annual symposium, please visit", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'the official Biocomplexity DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Leadership")),
                                          img(src = "fellow-Esha.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "fellow-seth.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-posadas.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/esha-dwibedi-83a63476/', 'Esha Dwibedi', target = '_blank'), "(Virginia Tech, Applied Microeconomics);"),
                                            p(a(href = 'https://www.linkedin.com/in/aviseth/', 'Avi Seth', target = '_blank'), "(Virginia Tech, Computer Science);"),
                                            p(a(href = 'https://www.linkedin.com/in/briannaposadas/', 'Dr. Brianna Posadas', target = '_blank'), "(Virginia Tech, Statistical and Data Science)."),
                                          p("", style = "padding-top:10px;")
                                   ),
                                   column(6, align = "center",
                                          h4(strong("DSPG Interns")),
                                          img(src = "team-mukora.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-rex.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          #img(src = "team-sallie.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://www.linkedin.com/in/tejapristavec/", 'Teja Pristavec', target = '_blank'), "(Project Lead, Research Assistant Professor);",
                                            a(href = "https://biocomplexity.virginia.edu/brandon-kramer", 'Brandon Kramer', target = '_blank'), "(Postdoctoral Research Associate);",
                                            a(href = 'https://biocomplexity.virginia.edu/sallie-keller', 'Sallie Keller', target = '_blank'), "(Division Director and Distinguished Professor)."),
                                          p("", style = "padding-top:10px;")
                                   )
                          ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h4(strong("Project Stakeholders")),
                                   p(a(href = 'https://www.linkedin.com/in/nancy-bell-aa293810/', 'Nancy Bell', target = '_blank'), "(Virginia Department of Health);",
                                     a(href = 'https://www.linkedin.com/in/terri-alt-3138b4101/', 'Terri Alt', target = '_blank'), "(Virginia Cooperative Extension, Patrick County at Virginia Tech)."),
                                   p("", style = "padding-top:10px;"),
                                   h4(strong("Acknowledgments")),
                                   p("We would like to thank Healthy Patrick County, an association of concerned Patrick County residents, and Brandon Kramer for their input to this project.")
                          )
                 ),
                 inverse = T)



# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  
  #NDVI Predictions
  
  var_NDVIMap <- reactive({
    input$NDVIPredictions
  })
  
  output$NDVIMap <- renderLeaflet({
    if(var_NDVIMap() == "2021") {
      ## outline of Floyd
      virginiaCounty <- st_read("data/VirginiaCounty.shp")
      f <- virginiaCounty[5,] %>% st_transform(crs = "+init=epsg:4326")
      m <- leaflet(options = leafletOptions(minzoom = 19))   %>%
        setView(lng = -80.3, lat = 36.91, zoom = 9.5) %>%
        addProviderTiles("CartoDB") %>%
        addPolygons(data = f,stroke = FALSE) 
      m
      #add in tiff prediction
      #addGeoRaster(m, my_file,
      #             colorOptions = colorOptions(
      #               palette = hcl.colors(256, palette = "viridis")
      #               , na.color = "transparent"
      #             ))
    }
  })
  
  
  # socio plots: done -----------------------------------------------------
  
  var <- reactive({
    input$sociodrop
  })
  #age65
  output$socioplot <- renderLeaflet({
    if(var() == "age65") {
      
      pal <- colorQuantile("Blues", domain = socdem_block$age65, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population age 65 or over:</strong>",
              round(socdem_block$age65, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$age65), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$age65),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      #under18
    }else if(var() == "under18"){
      pal <- colorQuantile("Blues", domain = socdem_block$under18, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population age 18 or under: </strong>",
              round(socdem_block$under18, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$under18), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$under18),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      #population-tract
    }else if(var() == "totalpop_trct"){
      pal <- colorQuantile("Blues", domain = socdem_tract$totalpop_trct, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>Total population: </strong>",
              formatC(socdem_tract$totalpop_trct, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$totalpop_trct), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$totalpop_trct),
                  title = "Total Population<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 0), " &ndash; ", round(cuts[-1], 0), ")")
                  })
      #population-block group
    }else if(var() == "totalpop_bgrp"){
      pal <- colorQuantile("Blues", domain = socdem_block$totalpop_bgrp, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>Total population: </strong>",
              formatC(socdem_block$totalpop_bgrp, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$totalpop_bgrp), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$totalpop_bgrp),
                  title = "Total Population<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 0), " &ndash; ", round(cuts[-1], 0), ")")
                  })
    }else if(var() == "black"){
      pal <- colorQuantile("Blues", domain = socdem_block$black, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population Black: </strong>",
              round(socdem_block$black, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$black), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$black),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "noba"){
      pal <- colorQuantile("Blues", domain = socdem_block$noba, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population without BA degree: </strong>",
              round(socdem_block$noba, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$noba), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$noba),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "unempl"){
      pal <- colorQuantile("Blues", domain = socdem_block$unempl, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population in labor force unemployed: </strong>",
              round(socdem_block$unempl, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$unempl), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$unempl),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "nohealthins2"){
      pal <- colorQuantile("Blues", domain = socdem_block$nohealthins2, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population without health insurance: </strong>",
              round(socdem_block$nohealthins2, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$nohealthins2), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$nohealthins2),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "snap"){
      pal <- colorQuantile("Blues", domain = socdem_block$snap, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population receiving public assistance or SNAP benefits: </strong>",
              round(socdem_block$snap, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$snap), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$snap),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "inpov"){
      pal <- colorQuantile("Blues", domain = socdem_tract$inpov, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population in poverty: </strong>",
              round(socdem_tract$inpov, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$inpov), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$inpov),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "hispanic"){
      pal <- colorQuantile("Blues", domain = socdem_tract$hispanic, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population Hispanic or Latino: </strong>",
              round(socdem_tract$hispanic, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$hispanic), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$hispanic),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "privateins"){
      pal <- colorQuantile("Blues", domain = socdem_tract$privateins, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population with private health insurance: </strong>",
              round(socdem_tract$privateins, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$privateins), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$privateins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      pal <- colorQuantile("Blues", domain = socdem_tract$publicins, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population with public health insurance: </strong>",
              round(socdem_tract$publicins, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$publicins), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$publicins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  
  # old plots - snap -----------------------------------------------
  var_old <- reactive({
    input$olddrop
  })
  var_hh <- reactive({
    input$hhdrop
  })
  output$oldplot <- renderLeaflet({
    # healthins wasn't coded properly so it's just all zeroes
    if(var_old() == "visdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$visdiff,
                     "_f" = olderadults$visdiff_f,
                     "_m" = olderadults$visdiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with vision difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "ambdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$ambdiff,
                     "_f" = olderadults$ambdiff_f,
                     "_m" = olderadults$ambdiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with ambulatory difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "cogdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$cogdiff,
                     "_f" = olderadults$cogdiff_f,
                     "_m" = olderadults$cogdiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with cognitive difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "carediff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$carediff,
                     "_f" = olderadults$carediff_f,
                     "_m" = olderadults$carediff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with self-care difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "ildiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$ildiff,
                     "_f" = olderadults$ildiff_f,
                     "_m" = olderadults$ildiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with independent living difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "disab") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$disab,
                     "_f" = olderadults$disab_f,
                     "_m" = olderadults$disab_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with any disability: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "inpov") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$inpov,
                     "_f" = olderadults$inpov_f,
                     "_m" = olderadults$inpov_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults in poverty: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else 
      # if(var_old() == "labfor")
    {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$labfor,
                     "_f" = olderadults$labfor_f,
                     "_m" = olderadults$labfor_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults in the labor force: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  output$householdplot <- renderLeaflet({
    if(var_hh() == "hhsixty_total") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_total,
                     "_f" = olderadults$hhsixty_total,
                     "_m" = olderadults$hhsixty_total)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_total, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a 60+ member: </strong>",
              round(olderadults$hhsixty_total, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_total),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_total),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_fhh") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_fhh,
                     "_f" = olderadults$hhsixty_fhh,
                     "_m" = olderadults$hhsixty_fhh)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_fhh, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a female 60+ member:</strong>",
              round(olderadults$hhsixty_fhh, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_fhh),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_fhh),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_mhh") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_mhh,
                     "_f" = olderadults$hhsixty_mhh,
                     "_m" = olderadults$hhsixty_mhh)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_mhh, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a male 60+ member: </strong>",
              round(olderadults$hhsixty_mhh, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_mhh),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_mhh),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_nonfam") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_nonfam,
                     "_f" = olderadults$hhsixty_nonfam,
                     "_m" = olderadults$hhsixty_nonfam)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_nonfam, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Single housholds with a 60+ member: </strong>",
              round(olderadults$hhsixty_nonfam, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_nonfam),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_nonfam),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_marr,
                     "_f" = olderadults$hhsixty_marr,
                     "_m" = olderadults$hhsixty_marr)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_marr, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Married households with a 60+ member: </strong>",
              round(olderadults$hhsixty_marr, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_marr),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_marr),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  
  # data and measures table: done ----------------------------------------
  var_topic <- reactive({
    input$topic
  })
  output$datatable <- renderDataTable({
    if(var_topic() == "All Measures"){
      table <- as.data.frame(measures_table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
    else{
      data <- switch(input$topic,
                     "Connectivity Measures" = "connectivity",
                     "Sociodemographic Measures" = "demographics",
                     "Food Access Measures" = "food access",
                     "Health Care Access Measures" = "health",
                     "Older Adult Population Measures" = "older adults")
      table <- subset(measures_table, Topic == data)
      table <- as.data.frame(table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
  })
  
  # device: done ---------------------------------------------------------
  
  output$deviceplot <- renderLeaflet({
    data <- switch(input$devicedrop,
                   "nocomputer" = connectivity$nocomputer,
                   "laptop" = connectivity$laptop,
                   "smartphone" = connectivity$smartphone,
                   "tablet" = connectivity$tablet, 
                   "nointernet" = connectivity$nointernet,
                   "satellite" = connectivity$satellite,
                   "cellular" = connectivity$cellular,
                   "broadband" = connectivity$broadband)
    
    device_spec <- switch(input$devicedrop,
                          "nocomputer" = "no computer",
                          "laptop" = "laptop",
                          "smartphone" = "smartphone",
                          "tablet" = "tablet", 
                          "nointernet" = "no internet access",
                          "satellite" = "satellite internet",
                          "cellular" = "cellular internet",
                          "broadband" = "broadband internet")
    
    pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            connectivity$NAME.y,
            "<br />",
            "<strong>% Households with",
            device_spec,
            "access: </strong>",
            round(data, 2)),
      htmltools::HTML
    )
    
    leaflet(data = connectivity, options = leafletOptions(minZoom = 10))%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(data), 
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(data),
                title = "Percent by<br>Quintile Group",
                opacity = 0.7,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
  
  # wifi: done -----------------------------------------------------------
  
  # Iso selector
  output$wifiplot <- renderLeaflet({
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    wifi_iso10 <- switch(input$wifidrop,
                         "Meadows of Dan Elementary School" = wifi_iso_10_1,
                         "Woolwine Elementary School" = wifi_iso_10_2,
                         "Patrick Springs Primary School" = wifi_iso_10_3,
                         "Blue Ridge Elementary School" = wifi_iso_10_4,
                         "Patrick County High School" = wifi_iso_10_5,
                         "Stuart Elementary School" = wifi_iso_10_6,
                         "Patrick County Branch Library" = wifi_iso_10_7,
                         "Hardin Reynolds Memorial School" = wifi_iso_10_8,
                         "Stuart Baptist Church" = wifi_iso_10_9,                       
                         "Patrick Henry Community College Stuart Campus" = wifi_iso_10_10)
    
    wifi_iso15 <- switch(input$wifidrop,
                         "Meadows of Dan Elementary School" = wifi_iso_15_1,
                         "Woolwine Elementary School" = wifi_iso_15_2,
                         "Patrick Springs Primary School" = wifi_iso_15_3,
                         "Blue Ridge Elementary School" = wifi_iso_15_4,
                         "Patrick County High School" = wifi_iso_15_5,
                         "Stuart Elementary School" = wifi_iso_15_6,
                         "Patrick County Branch Library" = wifi_iso_15_7,
                         "Hardin Reynolds Memorial School" = wifi_iso_15_8,
                         "Stuart Baptist Church" = wifi_iso_15_9,                       
                         "Patrick Henry Community College Stuart Campus" = wifi_iso_15_10)
    
    data <- switch(input$wifidrop,
                   "Meadows of Dan Elementary School" = 1,
                   "Woolwine Elementary School" = 2,
                   "Patrick Springs Primary School" = 3,
                   "Blue Ridge Elementary School" = 4,
                   "Patrick County High School" = 5,
                   "Stuart Elementary School" = 6,
                   "Patrick County Branch Library" = 7,
                   "Hardin Reynolds Memorial School" = 8,
                   "Stuart Baptist Church" = 9,                       
                   "Patrick Henry Community College Stuart Campus" = 10)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            wifi_latlong[data, ]$name,
            "<br />",
            "<strong>Address:</strong>",
            wifi_latlong[data, ]$fulladdress,
            "<br />",
            "<strong>Notes:</strong>",
            wifi_latlong[data, ]$notes),
      htmltools::HTML
    )
    
    m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = wifi_iso10, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrone") %>%
      addPolygons(data = wifi_iso15,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrone") %>%
      addMarkers(data = wifi_latlong, ~longitude[data], ~latitude[data],
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("10 Minute Isochrone",
                          "15 Minute Isochrone",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
    m1 
  })
  
  # Coverage table
  output$wifitable <- renderTable({
    data <- switch(input$wifidrop,
                   "Meadows of Dan Elementary School" = 1,
                   "Woolwine Elementary School" = 2,
                   "Patrick Springs Primary School" = 3,
                   "Blue Ridge Elementary School" = 4,
                   "Patrick County High School" = 5,
                   "Stuart Elementary School" = 6,
                   "Patrick County Branch Library" = 7,
                   "Hardin Reynolds Memorial School" = 8,
                   "Stuart Baptist Church" = 9,                       
                   "Patrick Henry Community College Stuart Campus" = 10)
    
    table <- read.csv(paste0("data/isochrones/tables/wifi_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # Wifi deserts
  output$allwifi <- renderLeaflet({
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            wifi_latlong$name,
            "<br />",
            "<strong>Address:</strong>",
            wifi_latlong$fulladdress,
            "<br />",
            "<strong>Notes:</strong>",
            wifi_latlong$notes),
      htmltools::HTML
    )
    
    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .5, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = wifi_iso_10_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_10, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = wifi_latlong,
                 group = "Free Wi-Fi Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("Free Wi-Fi Locations",
                          "Residential Properties"),
        baseGroups = c("10 Minute Isochrones",
                       "15 Minute Isochrones"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  output$allwifitable <- renderTable({
    table <- read.csv("data/isochrones/tables/wifi_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # ems: done ------------------------------------------------------------
  
  output$emsplot <- renderLeaflet({
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    ems_iso8 <- switch(input$emsdrop,
                       "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_1,
                       "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_2,                                                         
                       "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_8_3,                                                                   
                       "VESTA RESCUE SQUAD" = ems_iso_8_4,                                                                                           
                       "ARARAT RESCUE SQUAD" = ems_iso_8_5,                                                                                          
                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_8_6,
                       "JEB STUART RESCUE SQUAD" = ems_iso_8_7,                                                                                      
                       "SMITH RIVER RESCUE SQUAD" = ems_iso_8_8,                                                                                     
                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_8_9)
    
    ems_iso10 <- switch(input$emsdrop,
                        "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_1,
                        "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_2,                                                         
                        "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_10_3,                                                                   
                        "VESTA RESCUE SQUAD" = ems_iso_10_4,                                                                                           
                        "ARARAT RESCUE SQUAD" = ems_iso_10_5,                                                                                          
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_10_6,
                        "JEB STUART RESCUE SQUAD" = ems_iso_10_7,                                                                                      
                        "SMITH RIVER RESCUE SQUAD" = ems_iso_10_8,                                                                                     
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_10_9)
    
    ems_iso12 <- switch(input$emsdrop,
                        "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_1,
                        "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_2,                                                         
                        "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_12_3,                                                                   
                        "VESTA RESCUE SQUAD" = ems_iso_12_4,                                                                                           
                        "ARARAT RESCUE SQUAD" = ems_iso_12_5,                                                                                          
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_12_6,
                        "JEB STUART RESCUE SQUAD" = ems_iso_12_7,                                                                                      
                        "SMITH RIVER RESCUE SQUAD" = ems_iso_12_8,                                                                                     
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_12_9)
    
    data <- switch(input$emsdrop,
                   "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                   "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,                                                         
                   "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,                                                                   
                   "VESTA RESCUE SQUAD" = 4,                                                                                           
                   "ARARAT RESCUE SQUAD" = 5,                                                                                          
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                   "JEB STUART RESCUE SQUAD" = 7,                                                                                      
                   "SMITH RIVER RESCUE SQUAD" = 8,                                                                                     
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            str_to_title(ems[data, ]$NAME),
            "<br />",
            "<strong>Address:</strong>",
            str_to_title(ems[data, ]$ADDRESS), ",", str_to_title(ems[data, ]$CITY), ", VA", ems[data, ]$ZIP,
            "<br />",
            "<strong>Type:</strong>",
            str_to_title(ems[data, ]$NAICSDESCR)),
      htmltools::HTML
    )
    
    m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = ems_iso8, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrone") %>%
      addPolygons(data = ems_iso10,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrone") %>%
      addPolygons(data = ems_iso12,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrone") %>%
      addMarkers(data = ems, ~LONGITUDE[data], ~LATITUDE[data],
                 group = "EMS Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("8 Minute Isochrone",
                          "10 Minute Isochrone",
                          "12 Minute Isochrone",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
    m1 
  })
  
  output$emstable <- renderTable({
    data <- switch(input$emsdrop,
                   "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                   "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,                                                         
                   "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,                                                                   
                   "VESTA RESCUE SQUAD" = 4,                                                                                           
                   "ARARAT RESCUE SQUAD" = 5,                                                                                          
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                   "JEB STUART RESCUE SQUAD" = 7,                                                                                      
                   "SMITH RIVER RESCUE SQUAD" = 8,                                                                                     
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)
    
    
    table <- read.csv(paste0("data/isochrones/tables/ems_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # EMS deserts
  output$allems <- renderLeaflet({
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            str_to_title(ems$NAME),
            "<br />",
            "<strong>Address:</strong>",
            paste0(str_to_title(ems$ADDRESS), ", ", str_to_title(ems$CITY), ", VA ", ems$ZIP),
            "<br />",
            "<strong>Type:</strong>",
            str_to_title(ems$NAICSDESCR)),
      htmltools::HTML
    )
    
    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .5, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = ems_iso_8_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addMarkers(data = ems,
                 group = "EMS Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("8 Minute Isochrones",
                       "10 Minute Isochrones",
                       "12 Minute Isochrones"),
        overlayGroups = c("EMS Locations",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  output$allemstable <- renderTable({
    table <- read.csv("data/isochrones/tables/ems_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  
  # usda - lahunv10share  -----------------------------------------------------------
  var_usda <- reactive({
    input$usdadrop
  })
  output$usdaplot <- renderLeaflet({
    data <- switch(input$usdadrop,
                   "lakids1share" = usda$lakids1share,
                   "lakids10share" = usda$lakids10share,
                   "lalowi1share" = usda$lalowi1share,
                   "lalowi10share" = usda$lalowi10share,
                   "lapop1share" = usda$lapop1share,  
                   "lapop10share" = usda$lapop10share,
                   "laseniors1share" = usda$laseniors1share,
                   "laseniors10share" = usda$laseniors10share)
    
    usda_spec <- switch(input$usdadrop,
                        "lakids1share" = "low food access for children at 1 mile",
                        "lakids10share" = "low food access for children at 10 miles",
                        "lalowi1share" = "low food access for low income population at 1 mile",
                        "lalowi10share" = "low food access for low income population at 10 miles",
                        "lapop1share" = "low food access at 1 mile",  
                        "lapop10share" = "low food access at 10 miles",
                        "laseniors1share" = "low food access for seniors at 1 mile",
                        "laseniors10share" = "low food access for seniors at 10 miles")
    
    pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            usda$NAME.y,
            "<br />",
            "<strong>% Population with",
            usda_spec,
            round(data, 2)),
      htmltools::HTML
    )
    
    leaflet(data = usda, options = leafletOptions(minZoom = 10))%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(data), 
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(data),
                title = "Percent by<br>Quartile Group",
                opacity = 0.7,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
  # grocery --------------------------------------------------------
  
  # Iso selector
  output$grocplot <- renderLeaflet({
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    groc_iso10 <- switch(input$grocdrop,
                         "Mountain Meadow Farm and Craft Market" = grc_iso_10_1,
                         "Lowes Foods of Stuart" = grc_iso_10_2,
                         "Patrick County Local Farmers Market" = grc_iso_10_3,
                         "Stuart Farmers Market" = grc_iso_10_4,                
                         "W & W Produce" = grc_iso_10_5,
                         "Walmart Supercenter" = grc_iso_10_6,
                         "Poor Farmers Farm" = grc_iso_10_7)
    
    groc_iso15 <- switch(input$grocdrop,
                         "Mountain Meadow Farm and Craft Market" = grc_iso_15_1,
                         "Lowes Foods of Stuart" = grc_iso_15_2,
                         "Patrick County Local Farmers Market" = grc_iso_15_3,
                         "Stuart Farmers Market" = grc_iso_15_4,                
                         "W & W Produce" = grc_iso_15_5,
                         "Walmart Supercenter" = grc_iso_15_6,
                         "Poor Farmers Farm" = grc_iso_15_7)
    
    data <- switch(input$grocdrop,
                   "Mountain Meadow Farm and Craft Market" = 1,
                   "Lowes Foods of Stuart" = 2,
                   "Patrick County Local Farmers Market" = 3,
                   "Stuart Farmers Market" = 4,                
                   "W & W Produce" = 5,
                   "Walmart Supercenter" = 6,
                   "Poor Farmers Farm" = 7)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            groceries_latlong[data, ]$name,
            "<br />",
            "<strong>Address:</strong>",
            groceries_latlong[data, ]$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            groceries_latlong[data, ]$type),
      htmltools::HTML
    )
    
    m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = groc_iso10, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrone") %>%
      addPolygons(data = groc_iso15,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrone") %>%
      addMarkers(data = groceries_latlong, ~longitude[data], ~latitude[data],
                 group = "Fresh Food Location",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("15 Minute Isochrone",
                          "10 Minute Isochrone",
                          "Residential Properties",
                          "Fresh Food Location"),
        options = layersControlOptions(collapsed = FALSE))
    m1 
  })
  
  # Grocery table
  output$groctable <- renderTable({
    data <- switch(input$grocdrop,
                   "Mountain Meadow Farm and Craft Market" = 1,
                   "Lowes Foods of Stuart" = 2,
                   "Patrick County Local Farmers Market" = 3,
                   "Stuart Farmers Market" = 4,                
                   "W & W Produce" = 5,
                   "Walmart Supercenter" = 6,
                   "Poor Farmers Farm" = 7)
    
    table <- read.csv(paste0("data/isochrones/tables/grc_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # Food deserts
  output$allgroc <- renderLeaflet({
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            groceries_latlong$name,
            "<br />",
            "<strong>Address:</strong>",
            groceries_latlong$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            groceries_latlong$type),
      htmltools::HTML
    )
    
    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .5, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = grc_iso_10_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = groceries_latlong,
                 group = "Fresh Food Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("10 Minute Isochrones",
                       "15 Minute Isochrones"),
        overlayGroups = c("Residential Properties",
                          "Fresh Food Locations"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  # Other food resources
  output$othermap <- renderLeaflet({
    
    pal <- colorFactor(c("#0E879C", "#D9E12B", "#E6A01D"), domain = otherfood$type)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            otherfood$name,
            "<br />",
            "<strong>Address:</strong>",
            otherfood$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            otherfood$type,
            "<br />",
            "<strong>Open to:</strong>",
            otherfood$audience,
            "<br />",
            "<strong>Notes:</strong>",
            otherfood$notes),
      htmltools::HTML
    )
    
    leaflet(data = otherfood,
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = patrickborder, stroke = T, weight = 2, color = "grey", fillOpacity = 0) %>%
      addCircleMarkers(data = otherfood,
                       stroke = FALSE,
                       fillOpacity = 1,
                       color = ~pal(type),
                       radius = 7,
                       opacity = 1,
                       label = labels,
                       labelOptions = labelOptions(direction = "bottom",
                                                   style = list(
                                                     "font-size" = "12px",
                                                     "border-color" = "rgba(0,0,0,0.5)",
                                                     direction = "auto"))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~type,
                title = "Type",
                opacity = 0.9)
  })
  
  output$allgrctable <- renderTable({
    table <- read.csv("data/isochrones/tables/grc_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
}

shinyApp(ui = ui, server = server)