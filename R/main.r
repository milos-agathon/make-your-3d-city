#############################################
# 3D buildings using LiDAR
# Milos Popovic 2023/10/31
#############################################

# 1. INSTALL & LOAD PACKAGES

libs <- c(
    "terra", "sf",
    "rayshader"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if (
    any(installed_libraries == F)) {
    install.packages(
        libs[!installed_libraries]
    )
}

invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# 2. GET DATA

lidar_url <- "https://ns_hwh.fundaments.nl/hwh-ahn/ahn4/03a_DSM_0.5m/R_30GZ1.zip"
lidar_file <- basename(lidar_url)
download.file(
    url = lidar_url,
    destfile = lidar_file,
    mode = "wb"
)
unzip(lidar_file)
lidar_rast <- terra::rast(
    paste0(
    gsub(
        "\\..*", "",
        lidar_file
    ), ".tif"
)
)

ortho_rast <- terra::rast(
    "https://geotiles.citg.tudelft.nl/Luchtfoto_2023/RGB_30GZ1.tiff"
)

terra::plotRGB(ortho_rast)

# 2. CROP AREA

# 52.0802976, 4.3111618

coords <- data.frame(
    long = 4.3111618,
    lat = 52.0802976
) |>
sf::st_as_sf(
    coords = c(
        "long", "lat"
    ),
    crs = sf::st_crs(4326)
) |>
sf::st_transform(
    crs = terra::crs(
       ortho_rast 
    )
)

hague_buffer <- terra::buffer(
    terra::vect(coords),
    width = 500
)

lidar_crop <- terra::crop(
    lidar_rast,
    hague_buffer,
    snap = "in",
    mask = T
)

terra::plot(lidar_crop)

ortho_crop <- terra::crop(
    ortho_rast,
    hague_buffer,
    snap = "in",
    mask = T
)

terra::plot(ortho_crop)

# 3. RESAMPLE

ortho_resampled <- terra::resample(
    x = ortho_crop,
    y = lidar_crop,
    method = "bilinear"
)

plot(ortho_resampled)

# 4. SAVE ORTHO AS IMAGE

terra::writeRaster(
    ortho_resampled,
    "binnenhof.png",
    overwrite = T
)

img <- png::readPNG(
    "binnenhof.png"
)

# 5. FILL MISSING VALUES

lidar_crop_predict <- terra::focal(
    lidar_crop,
    w = 9,
    fun = mean,
    na.policy = "only",
    na.rm = T
)

plot(lidar_crop_predict)

lidar_crop_predict <- terra::ifel(
    is.na(lidar_crop_predict),
    -3.4064,
    lidar_crop_predict
)

plot(lidar_crop_predict)

# 6. RENDER

lidar_mat <- rayshader::raster_to_matrix(
    lidar_crop_predict
)

lidar_mat |>
    rayshader::height_shade() |>
    rayshader::add_overlay(
        img,
        alphalayer = 1
    ) |>
    rayshader::plot_3d(
        lidar_mat,
        solid = F,
        zscale = 1,
        zoom = .6,
        phi = 45,
        theta = -30,
        windowsize = 800 
    )

# 7. RENDER OBJECT

rayshader::render_highquality(
    filename = "3d-city-building.png",
    preview = T,
    light = F,
    environment_light = "air_museum_playground_4k.hdr",
    intensity_env = 1,
    rotate_env = 90,
    parallel = T,
    interactive = F,
    width = 4000,
    height = 4000
)
