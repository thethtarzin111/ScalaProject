//Team Innova
//Members: Mohammad Amman, Salek Md Peash Been, Thet Htar Zin
/*
Description of the file
This is for the solar planel where we can generate electricity from solar power.
Things to consider here the most are direction of the sun and angle of the panel.
 */
package com.innova.renewableenergy
case class SolarPanel(
                       panelId: String,
                       orientation: Double, //Current angle of the panel in degrees
                       efficiency: Double = 0.9 //Define default efficiency
                     ) extends EnergySource {
  //We extend immutable variables from EnergySource
  val id: String = panelId
  val sourceType: String = "Solar"

  //Simulating solar power output based on degree orientation
  def getOutput(): Double = {
    //"For optimal output, panels should be installed facing south at an angle of 42 degrees." - Caruna.fi
    val optimalAngle = 42.0
    val angleDifference = Math.abs(orientation - optimalAngle)

    //If orientation is worse, we get less output.
    val orientationFactor = Math.max(0.0, 1.0 - (angleDifference / 90.0))

    //Output is calculated in kW.
    //We assume the baseOutput tho this depends on panel size, solar radiation, etc.
    val baseOutput = 2.5 //In kW
    val output = baseOutput * orientationFactor * efficiency

    Math.max(0.0, output) //Should never be a negative value
  }

  //To otimize panel orientation, we track the sun's movement.
  //As the sun moves, the panel will also move.
  def trackSun(sunAngle: Double): SolarPanel = {
    copy(orientation = sunAngle)
  }

  //Status report for monitoring
  def statusReport(): Map[String, String] = {
    Map(
      "id" -> id,
      "type" -> sourceType,
    "orientation" -> f"$orientation%.1f°",
    "efficiency" -> f"${efficiency * 100}%.1f%%",
    "output" -> f"${getOutput()}%.2f kW"
    )
  }
}