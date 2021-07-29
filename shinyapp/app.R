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
library(leaflet)
library(leafem)
library(raster)

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
                 tabPanel("Floyd County Case Study", value = "ndvi",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Floyd County: NDVI Predictions"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(7,
                                          h4(strong("What's the deal with NDVI?")),
                                          p("We examined Patrick County population sociodemographic and socioeconomic characteristics to better understand the 
                                            residents that the county serves."),
                                          p("We retrieved American Community Survey (ACS) data to calculate this information at census block group and census 
                                            tract levels. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. We used 
                                            the most recently available 5-year estimates from 2014/18 to compute percent Patrick County residents in a given block group or tract by age, race, ethnicity, 
                                            employment, health insurance coverage, and other relevant characteristics."),
                                          p("Our interactive plots visualize census block-group level sociodemographic characteristics of Patrick County residents.")),
                                   column(5,
                                          h4(strong("Map of NDVI Predictions")),
                                          selectInput("NDVIPredictions", "Select Year:", width = "100%", choices = c(
                                            "2021", "2022", "2023", "2024"
                                          )),
                                          p(strong("NDVI Predictions")),
                                          withSpinner(leafletOutput("NDVIMap"))
                                   ))
                 ),
                 
                 # older -----------------------------------------------------------
                 tabPanel("Landsat 8 Data", value = "landsat",
                          fluidRow(style = "margin: 6px;", align = "center",
                                   h1(strong("Using Landsat 8 Images"), align = "center"),
                                   #p("", style = "padding-top:10px;"),
                          )
                          ,
                          fluidRow(style = "margin: 6px;", align = "center",
                                   column(3),
                                   column(align = "center",6,
                                          
                                          h4(strong("")),
                                          p(style = "text-align: justify;", "Launched in 2013, the Landsat 8 satellite is the latest in a series of Landsat predecessors dating back to the 1970’s. 
                                          The data captured on the Landsat 8 satellite is useful for two reasons: firstly, it uses high-resolution sensors. One pixel 
                                          of the Landsat 8’s color bands corresponds to 30 meters of earth, roughly the size of a baseball diamond as shown below. 
                                          There is also a panchromatic band that takes photographs at the 15m resolution, allowing for even higher-detail interpolation 
                                          of satellite images. "), tags$br()
                                          
                                    
                                          )
                                   
                                   ),
                          fluidRow(
                            column(12, align = "center",
                                   img(src = "Picture1.png", style = "text-align:left;")
                                   )
                          ),
                          fluidRow(
                            column(3),
                            column(6,
                                   tags$br(),
                                   p(style = "text-align: justify;", "The second significant advantage of using Landsat 8 satellite imagery is the diversity of wavelengths of light captured 
                                          in each photograph. The Landsat 8 captures eleven distinct “bands” of light:  "))
                          ),
                          fluidRow(style = "margin: 6px", align = "center",
                            column(12, align = "center",
                                   img(src = "Picture2.png", style = "text-align:right;", width = "550px"),
                                   tags$br())
                          ),
                          fluidRow(
                            column(12, align = "center",
                                   img(src = "Picture3.png", style = "display: inline;")
                                   )
                          ),
                          fluidRow(
                            column(12, align = "center",
                                   img(src = "Picture4.png", style = "display: inline;"))
                          ),
                          fluidRow(style = "margin: 6px", align = "left",
                                   column(3),
                                   column(6, align = "center",
                                          p(style = "text-align: justify;", "The landsat 8 captures images corresponding to roughly 250x250 kilometer sections of earth. The different bands can be 
                                          combined to form all sorts of useful secondary images synthesized from the raw wavelengths. The image below is a true-color 
                                          synthesis of the red, green and blue bands of the Landsat Satellite of Las Angeles, California: "))
                          ),
                          fluidRow(style = "margin: 6px", align = "center",
                                   column(12, align = "center",
                                          img(src = "Picture5.png", style = "display: inline; float: center;", width = "300px")))
                 ),
                 
                 # wifi-----------------------------------------------------------
                 tabPanel("Derived Indices", value = "connectivity",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Derived Indices"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(3),
                                   column(6, align = "center",
                                          #h4(strong("Computing Device Ownership and Internet Access Type")),
                                          p(style = "text-align: justify;", "Oftentimes, measurements of natural phenomena are too complex to be accurately described solely with remote sensing. Calculated indices bridge the gap between satellite imagery and internal vegetative processes. The indices of particular interest to this project are the Normalized Difference Vegetative Index (NDVI) and the Normalized Difference Water Index (NDWI). NDVI is strongly correlated to the overall health of plant foliage. The Normalized Difference Water Index is strongly correlated to the amount of water held in plant leaves. Foliage health and water density are both complex phenomena that would be impossible to calculate for each individual pixel of a satellite image, but with the help of these indicators, we can synthesize a good idea of vegetative health using relatively little input. The different wavelengths of light captured by the Landsat 8 satellite can be used to produce the NDVI and NDWI indices of interest for each individual pixel of a remote sensing image.")       
                                   )
                                   
                          ),
                          fluidRow(
                            column(3),
                            column(3,
                                   p(strong(style = "text-align: justify;","Normalized Difference Vegetative Index")),
                                   p(style = "text-align: justify;","The Normalized Difference Vegetative Index is derived from the Near Infrared light and Red light emitted from plants. It is described in detail in Nathalie Pettorelli’s book “The Normalized Difference Vegetative Index”. The Landsat 8 satellite captures both wavelengths of light and the USGS provides a formula for producing this index:"),
                                   
                                   p(strong(align = "center","NDVI = (NIR - R) / (NIR + R)")),
                                   p(style = "text-align: justify;", "This allows us to create aerial maps of NDVI for a particular region by combining individual pixel values. A Map of the NDVI of Southwest Virginia and Southern West Virginia is shown to the right:"),
                                   
                                   p(style = "text-align: justify;", "From these types of aerial maps of derived indices, conclusions about distribution and trends in the vegetative health over time and throughout the region can be made. The Normalized Difference Vegetative Index has been used in applications such as precision agriculture, drought monitoring, flooding and precipitation patterns. The aim of this project is to predict the NDVI by combining the band data from the Landsat 8 satellite to give an accurate prediction of how the NDVI will change over time.")
                                   ),
                            column(6,
                                   img(src = "NDVI_2016_NRV.png", style = "display: inline", width = "550px"))
                          ),
                          fluidRow(
                            
                            column(6, align = "right",
                                   img(src = "Picture7.png", style = "display: inline;")),
                            column(3,
                                   p(strong(style = "text-align: justify;","Normalized Difference Water Index")),
                                   p(style = "text-align: justify;","The Normalized Difference Water Index (NDWI) is highly correlated with the amount of water stored in the foliage of plants, as described in Bo-cai Gao’s paper, “A normalized difference water index for remote sensing of vegetation liquid water from space”. The NDWI is sometimes described as the Normalized Difference Water Index or NDMI. The USGS also provides a formula to calculate the NDWI by combining the Short-Wave Infrared with the Near Infrared wavelengths of light captured from the Landsat 8 satellite in the following formula:"),
                                   
                                   p(strong(align = "center","NDMI = (NIR – SWIR) / (NIR + SWIR)")),  
                                   
                                   p(style = "text-align: justify;","Like the NDVI, this formula allows for per-pixel calculation of this index to describe the distribution of water in vegetation throughout the new river valley, as shown in the image to the left:"))
                          )
                 ),
                 
                 # ems -----------------------------------------------------------
                 tabPanel("Machine Learning", value = "ml",
                          
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Machine Learning Methodology"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(3),
                                   column(6,
                                          p(style = "text-align: justify;", "The
                                          main goal of this project is to use modern 
                                          data-science techniques to produce a model 
                                          that is able to predict the Normalized 
                                          Difference Vegetative Index and the Normalized 
                                          Difference Water Index to a high degree 
                                          of accuracy at a high resolution using 
                                          existing wavelength data captured by the 
                                          Landsat 8. This opens the door to identify 
                                          trends and areas of concern within a region 
                                          that might otherwise look healthy. The 
                                          tool used to accomplish this process is 
                                          a feed-forward neural network that is trained 
                                          off of existing satellite pairs. The model 
                                          trained in this paper looks at an ability 
                                          to predict vegetative health in two year 
                                          intervals. The area of interest in this 
                                          project is the New River Valley of Southwest 
                                          Virginia and specifically Floyd County, 
                                          Virginia. This is the region that the model 
                                          was initially trained on. Raw satellite 
                                          images were downloaded from the Earth Explorer 
                                          via the United States Geological survey 
                                          as Geotiff images of the different wavelengths 
                                          of light captured. A great deal of data 
                                          cleaning, conditioning and filtering had 
                                          to be performed before the images could 
                                          be fed into a machine learning model. The 
                                            first such problem came when the raw 
                                            images were encoded as 16-bit positive 
                                            integer values, and needed to be converted 
                                            to 64-bit floating point values in order 
                                            to be manipulated. Otherwise, non-natural 
                                            values result in high-values instead 
                                            of negative ones that make the resulting 
                                            and processing useless. The other 
                                            technical nuances encountered when using 
                                            the raw satellite images are described in the following sections.")),
                                   
                                   
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   h4(strong("Introduction")),
                                   h4(strong("Top of Atmosphere Reflectance")),
                                   p(style = "text-align: justify;","When a satellite takes progressive images of the earth, it is rare that the sun is in the same position each time it takes a picture. Therefore, the angle of the sun needs to be taken into account when examining multiple images of the earth over time. The United States Geological Survey employs a formula to convert the raw intensity values captured by the Landsat to Top of Atmosphere reflectance. The values for the Sun Elevation angle and correction values are included in the metadata of each GeoTiff image. The United States Geological Survey Provides the following reference for converting raw values to Top of Atmosphere Reflectance:")
                                   ),
                            column(6,
                                   img(src = "Picture10.png", style = "display: inline"),
                                   p(tags$small("Reference: United States Geological Survey ")))
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   img(src = "Picture11.png", style = "display: inline", width = "400px"),
                                   p(tags$small("A measure of NDVI after the region was filtered with the filtration algorithm. Note the absence of Urban areas such as Roanoke and Blacksburg/Christiansburg, Smith Mountain Lake and a large overhanging Cloud on the Scene.  ")),
                            ),
                            column(3,
                                   h4(strong("Filtering Data")),
                                   p(style = "text-align: justify;", "It is important to note that the focus of this project is on vegetation. As it happens, when a satellite snaps a photograph of the earth, it tends to capture much more than just vegetation. Therefore, the team needed a way to filter out clouds and their shadows, water and non-vegetative urban areas. The team used the 9th Band of the Landsat 8 satellite to filter out clouds and developed an algorithm that searched for shadows based on dips in reflectance around clouds. The team also employed a method to filter out the lakes, ponds and rivers in a scene primarily using the Near Infrared Band (Band 5) in combination with others. This method is described in the paper Identification of Water Bodies in a Landsat 8 OLI Image Using a J48 Decision Tree. Once all of these filtration algorithms were applied, the satellite image of Southwest Virginia looked accordingly: "),
                            )
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   h4(strong("Aligning Photographs")),
                                   p(style = "text-align: justify;", "Because this model seeks to predict vegetative health in 2-year intervals, it is imperative that “before” and “after” data is gathered. Therefore, we chose landsat images taken of the exact same region of Southwest Virginia. The only problem was that the satellite images were virtually identical, but once the team began examining them closely, we realized that they were ever so slightly off. A detailed image of the displacement is shown below: "),
                            ),
                            column(6,
                                   img(src = "Picture12.png", style = "display: inline"))
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   img(src = "Picture13.png", style = "display: inline")),
                            column(3,
                                   p(style = "text-align: justify;", "Therefore, the team had to develop an algorithm to align before and after images so that the training sets would line up properly at the pixel level. We managed to accomplish this by subtracting the Band 5 Near Infrared light reflectance for the two years and examining the change. Because features like water are well-absorbed by the NIR light, disparities in alignment became very apparent: "),
                            )
                            
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   h4(strong("Creating Subsets")),
                                   p(style = "text-align: justify;", "Once the necessary areas had been filtered out so that the learning algorithm could focus on vegetation only, the image was divided into 20x20 pixel squares for each of the 11 distinct wavelengths of light emitted from that particular section. The model is input 11 different 20x20 subsets and forms a prediction of the same 20x20 pixel square of how the NDVI and NDWI changes.  "),
                            ),
                            column(6,
                                   img(src = "Picture14.png", style = "display: inline", width = "460px"))
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   img(src = "Picture15.png", style = "display: inline")),
                            column(3,
                                   h4(strong("Building and Training a Neural Network")),
                                   p(style = "text-align: justify;", "Once all the 20x20 subsets of input and output pairs had been assembled, the team needed to create a neural network to handle the more than 4000 inputs per scene. This meant that in a single satellite image, there were going to be over 260,000,000 data points to train from. The team developed a feed-forward neural network with 5,475 neurons and over 4,000,000 weights connecting the neurons together to accommodate the large volume and diversity of data. The structure of the full neural network is below: "),
                                   p(style = "text-align: justify;", "The first 85,000 input/output scenes were used to train the model and the roughly 16,000 remaining were set aside to test the conditioned model. Over time, scenes from different time periods and areas of Virginia were used to increase the robustness of the model.  "),
                                    
                                   )
                            
                          ),
                          tags$br(),
                          tags$br(),
                          fluidRow(
                            column(3),
                            column(3,
                                   h1(strong("Machine Learning Results"), align = "center"),)
                          ),
                          fluidRow(
                            column(3),
                            column(3,
                                   h4(strong("Testing Accuracy ")),
                                   p(style = "text-align: justify;", "The testing results below are for two different time periods of Southwest Virginia and an Image of Central Virginia. The accuracy percentage is the average percentage correct of predicted results and on the true values. The Loss Function used in this training was Means Absolute Error, which is the absolute value of the distance from the predicted values to the true values. The Loss values in the table are the average losses for all 16,000 test samples.  ")
                                   ),
                            column(6,
                                   tags$br(),
                                   img(src = "Picture16.png", style = "display: inline", width = "550px")),
                            
                          ),
                          fluidRow(
                            column(3),
                            column(3,
                                   img(src = "Picture17.png", style = "display: inline")),
                            column(3,
                                   h4(strong("Visualizing Accuracy ")),
                                   p(style = "text-align: justify;", "For a particular training set of the Southwest Virginia subset from 2014 to 2016, the error over time is shown below:  "),
                            )
                            
                          )
                 ),
                 
                 
                 
                 
                 # contact -----------------------------------------------------------
                 tabPanel("Our Team", value = "team",
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
                 tabPanel("References", value = "references"
                   
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
      floyd <- virginiaCounty[5,] %>% st_transform(crs = "+init=epsg:4326")
      m <- leaflet(options = leafletOptions(minzoom = 19))
        
        
      
      
      geotiffFile = "./www/2021_NN_Predictions.tiff"
      
      my_file = raster(geotiffFile)
      
      my_file[!(my_file > 0)] = NA
      
      pal = colorNumeric(
        palette = 'viridis',
        domain = c(0, 1)
      )
      
      m <- addGeoRaster(m, my_file,
                   opacity = 0.55,
                   autozoom = FALSE,
                   colorOptions = colorOptions(
                     palette = hcl.colors(256, palette = "viridis")
                     , na.color = "transparent"
                   ))
      
      addPolygons(m, data = floyd,
                 fillColor = "Transparent",
                 weight = 4,
                 opacity = 1,
                 color = "white") %>%
        addLegend(pal = pal, values = c(0, 1), opacity = 0.7, title = "Predicted NDVI Value",
                  position = "bottomright") %>%
        setView(m, lng = -80.3, lat = 36.91, zoom = 9.5) %>%
          addProviderTiles("CartoDB")
    }
  })
  
  
  
 
  
}

shinyApp(ui = ui, server = server)