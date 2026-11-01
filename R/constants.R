# constants
small <- 1.0e-10

# EGM-08 constants
mean_equatorial_radius_earth <- 6378.1363 |> units::set_units("km")
flatness_earth <- 1.0 / 298.257223563 |> units::set_units("")
rotation_earth <- 7.292115e-5 |> units::set_units("rad/s")
gravitational_constant_earth <- units::set_units(3.986004415e14, "m^3/s^2")
eccentricity_earth <- sqrt(
  2.0 * flatness_earth - flatness_earth * flatness_earth
)
squared_eccentricity_earth <- eccentricity_earth * eccentricity_earth
mu <- units::set_units(398600.4415, "km^3/s^2")
