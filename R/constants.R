# constants
small <- 1.0e-10

# EGM-08 constants
radius_earth <- 6378.1363 |> units::set_units("km")
flatnes_earth <- 1.0 / 298.257223563 |> units::set_units("")
rotation_earth <- 7.292115e-5 |> units::set_units("rad/s")
mu <- units::set_units(398600.4415, "km^3/s^2")

eccentricity_earth <- sqrt(2.0 * flatnes_earth - flatnes_earth^2)
