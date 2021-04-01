## Packages required for the function
packages = c("tidyverse", "sp","geosphere","sf","leaflet","docstring")
## Check if the package is there ifnot then install it
package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)

Cluster_Sensors_distance <- function(Sensor_location_data, distance,visualization=F) {
    ### This is done to give the documentation to the pacakge
    #' Clustering of the Sensor based on the distance
    #'
    #' @param Sensor_location_data Sensor_location_data Give the dataframe of the sensors with sensor_name,latitude,longitude
    #' @param distance Distance in m
    #' @param visualization Adding the visualization if false will give the dataframe
    #'
    #' @return
    #' @export
    #'
    
    ## Selecting latitude, longitude and sensor_name
    Sensor_Name_loc <- unique(Sensor_location_data[,c("sensor_name","latitude","longitude")])
    ## Converting the location to Spatial Point data frame
    xy <- sp::SpatialPointsDataFrame(
        matrix(c(Sensor_Name_loc$longitude,Sensor_Name_loc$latitude), ncol=2),
        data.frame(ID=Sensor_Name_loc$sensor_name),
        proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    
    # use the distm function to generate a geodesic distance matrix in meters
    mdist <- geosphere::distm(xy)
    ### Distance which can be used for defining the clusters
    d= distance
    # cluster all points using a hierarchical clustering approach
    hc <- hclust(as.dist(mdist), method="complete")
    
    # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
    xy$clust <- cutree(hc, h=d)
    ## Getting the number of Cluster generated
    Number_cluster <- length(unique(xy$clust))
    ## Creating the dataframe
    Sensor_Clusters <- data.frame(sensor_name =xy$ID, Cluster_Number =xy$clust)
    Sensor_Clusters <- Sensor_Clusters %>% left_join(Sensor_Name_loc,by="sensor_name")
    if(visualization==T) {
        ## Giving the cluster a number like Cluster_1
        Sensor_Clusters$Cluster_Number <- paste0("Cluster_",Sensor_Clusters$Cluster_Number)
        Sensor_Clusters$Cluster_Number <- as.factor(Sensor_Clusters$Cluster_Number)
        ## Generating colors based on number of cluster 
        color_pallete <- rainbow(Number_cluster)
        ## Cluster numbers which we then give to Color factor
        cluster_numbers <- as.vector(unique(Sensor_Clusters$Cluster_Number))
        ## Same color for same cluster number
        p = colorFactor(palette = color_pallete,
                        domain =Sensor_Clusters$Cluster_Number )
        ## Not we will plot the cluster through an interactive map
        plot <- leaflet(Sensor_Clusters) %>%addTiles()%>% 
            addCircleMarkers(color = ~p(Cluster_Number),
                             popup =~as.character(`sensor_name`), 
                             stroke = FALSE, fillOpacity = 0.6,
                             label = Sensor_Clusters$Cluster_Number) %>%
            addLegend(position = "topright",pal = p, values = cluster_numbers,
                      title = "Sensor_Cluster")
        return(plot)
        
    }
    else{return (Sensor_Clusters)}
}


Cluster_Sensor_number <- function( Sensor_location_data,number,visualization=F){
    #' Clustering of the Sensor based on Number of clusters
    #'
    #' @param Sensor_location_data Sensor_location_data Give the dataframe of the sensors with sensor_name,latitude,longitude
    #' @param distance Distance in m
    #' @param visualization Adding the visualization if false will give the dataframe
    #'
    #' @return
    #' @export
    #'
    
    ## Selecting latitude, longitude and sensor_name
    Sensor_Name_loc <- unique(Sensor_location_data[,c("sensor_name","latitude","longitude")])
    km <- kmeans(Sensor_Name_loc[,c('latitude','longitude')], centers=number)
    Sensor_Name_loc$Cluster <- km$cluster
    if(visualization==T) {
        ## Giving the cluster a number like Cluster_1
        Sensor_Name_loc$Cluster_Number <- paste0("Cluster_",Sensor_Name_loc$Cluster)
        Sensor_Name_loc$Cluster_Number <- as.factor(Sensor_Name_loc$Cluster_Number)
        ## Generating colors based on number of cluster 
        color_pallete <- rainbow(number)
        ## Cluster numbers which we then give to Color factor
        cluster_numbers <- as.vector(unique(Sensor_Name_loc$Cluster_Number))
        ## Same color for same cluster number
        p = colorFactor(palette = color_pallete,
                        domain =Sensor_Name_loc$Cluster_Number )
        ## Not we will plot the cluster through an interactive map
        plot <- leaflet(Sensor_Name_loc) %>%addTiles()%>% 
            addCircleMarkers(color = ~p(Cluster_Number),
                             popup =~as.character(`sensor_name`), 
                             stroke = FALSE, fillOpacity = 0.6,
                             label = Sensor_Name_loc$Cluster_Number) %>%
            addLegend(position = "topright",pal = p, values = cluster_numbers,
                      title = "Sensor_Cluster")
        return(plot)
        
    }
    else {return (Sensor_Name_loc)}
    
}

## Testing the Function
Sensor_location <- read.csv("~/Sensors_loc.csv") ## Read the .csv file
## Give the location of the sensors data
## Filtering it for Kanpur city
Sensor_location <- Sensor_location %>% filter(city=="Kanpur")

## Distance Cluster
Cluster_Sensors_distance(Sensor_location,distance=5000,visualization = F)

## Number based Clustering aka K-means
Cluster_Sensor_number(Sensor_location,n=4,visualization = T)

