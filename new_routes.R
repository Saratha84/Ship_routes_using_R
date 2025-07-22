pacman::p_load(
  tidyverse,terra,
  sf,giscoR,ggnewscale
  
)
install.packages("archive")
library(archive)

zipfile <-"C:/Users/sarat/Downloads/shipdensity_global.zip"

# Extract ZIP contents to working directory
archive::archive_extract(archive = zipfile, dir = getwd())

# List files in archive to find the .tif
tif_name <- archive::archive(zipfile) |>
  filter(grepl("\\.tif$", path)) |>
  pull(path)

# Create full path to the extracted .tif file
tif_path <- file.path(getwd(), basename(tif_name))

# Load the raster using terra
shipping <- terra::rast(tif_path)


xmin<-44.806634
ymin<- -7.822420
xmax<-111.779290
ymax<- 31.548014

bounding_box<- sf::st_sfc(
  sf::st_polygon(
    list(
      cbind(
        c(xmin,xmax,xmax,xmin,xmin),
        c(ymin,ymin,ymax,ymax,ymin)
      )
    )
    
  ),crs=4326
)
shipping_traffic<-terra::crop(
  x=shipping,
  y=bounding_box,
  snap="in"
)
shipping_traffic_clean<-terra::ifel(
  shipping_traffic==0,
  NA,
  shipping_traffic
)
url_1<-"https://eogdata.mines.edu/nighttime_light/annual/v22/2022/VNL_v22_npp-j01_2022_global_vcmslcfg_c202303062300.average_masked.dat.tif.gz"
file_name=basename(url_1)
download_dir <- "C:/Users/sarat/Downloads"

# Find the full path to the downloaded .gz file
path_to_nightlight <- list.files(
  path = download_dir,
  pattern = file_name,
  full.names = TRUE
)

# Read using GDAL virtual file system
nightlight <- terra::rast(
  paste0("/vsigzip/", normalizePath(path_to_nightlight, winslash = "/"))
)

nightlight_region<-terra::crop(
  x=nightlight,
  y=bounding_box,
  snap="in"
)

nightlight_resampled<-terra::resample(
  x=nightlight_region,
  y=shipping_traffic_clean,
  method="bilinear"
)
# Copy the raster
nightlight_clean <- nightlight_resampled

# Replace negative values with NA
nightlight_clean[nightlight_clean < 0] <- NA

# Plot the cleaned raster
log_nightlight <- log10(nightlight_clean + 1)  # +1 avoids log(0)
plot(log_nightlight, main = "Nightlight (Log Scale)", col = viridis::viridis(100))

nightlight_cols <- c("#000000", "#FFBF00", "#FFFF00", "#FFFFFF")
nightlight_pal <- colorRampPalette(nightlight_cols, bias = 2)(256)



nightlight_df<-as.data.frame(
  nightlight_resampled,
  xy=TRUE,
  na.rm=TRUE
)
nightlight_df <- as.data.frame(log_nightlight, xy = TRUE, na.rm = TRUE)
names(nightlight_df)[3] <- "nightlight_value"


library(ggplot2)

map <- ggplot() +
  geom_raster(
    data = nightlight_df,
    aes(x = x, y = y, fill = nightlight_value)
  ) +
  scale_fill_gradientn(
    colours = nightlight_pal,
    na.value = "transparent",
    name = "Nightlight Intensity"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.margin = unit(c(-1, -1, -1, -1), "cm")
  )


shipping_traffic_cols <- hcl.colors(
  n = 5,
  palette = "Blues"
)

scales::show_col(
  shipping_traffic_cols,
  ncol=5,
  labels=TRUE
)
shipping_traffic_pal<-colorRampPalette(
  shipping_traffic_cols[1:4]
)(256)



shipping_traffic_df<-as.data.frame(
  shipping_traffic_clean,
  xy=TRUE,
  na.rm=TRUE
)
map <-ggplot()+
  geom_raster(
    data=nightlight_df,
    aes(
      x=x,
      y=y,
      fill=nightlight_value
    )
  )+
  scale_fill_gradientn(
    colors = nightlight_pal
  )+
  ggnewscale::new_scale_fill()+
  geom_raster(
    data=shipping_traffic_df,
    aes(
      x=x,
      y=y,
      fill=shipdensity_global
    )
  )+
  scale_fill_gradientn(
    colors = shipping_traffic_pal
  )+
  theme_void()+
  theme(
    legend.position = "none",
    plot.margin = unit(
      c(
        t=-1,r=-1,
        b=-1,l=-1
      ),"cm"
      )
    )
ggsave(
  filename = "traffic_shipping_preview_latest_new.png",
  plot = map,
  width = 10,          # smaller size
  height = 6,
  dpi = 300            # low resolution (default screen resolution)
)








