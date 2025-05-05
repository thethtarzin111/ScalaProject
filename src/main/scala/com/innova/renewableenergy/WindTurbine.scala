package com.innova.renewableenergy
case class WindTurbine(
                        turbineId: String,
                        direction: Double, //Current angle in degrees
                        windSpeed: Double = 5.0, //Assuming default speed in m/s
                          efficiency: Double = 0.85 //Assuming default efficieny
)extends EnergySource {
  //immutable data types from EnergySource
  val id: String = turbineId
  val sourceType: String = "Wind"

  //Simulating wind power output based on wind direction and speed
  def getOutput(): Double = {
    //This is just simplifed calculation.
    //In reality, we also need calculation for blade length, air density, etc.

    //Threadsholds for speed limit
    // No power output if the windspeed is less than cut-in and greater than cut-out
    //Cut-in is the minimum wind speed at which the turbine starts generating electricity.
    //Cut-out is the max wind speed and if it exceeds, the turbine shut down to avoid any damage.
    if (windSpeed < 3.0 || windSpeed > 25.0) {
      //shudDown()   <--Need to implement some kind of shut down function
      return 0.0
    }

    val speedFactor = if (windSpeed <= 12.0) {
      //Increase wind speed until rated
      Math.min(1.0, Math.pow(windSpeed / 12.0, 3))
    } else {
      //Keep the rated speed but drop to zero at cut-out
      Math.max(0.0, 1.0 - ((windSpeed - 12.0) / 13.0))
    }

    //We assume base capacity in kW.
    val capacity = 25000.0
    capacity * windSpeed * efficiency
  }

  //To optimize turbine facing, we track wind direction
  def trackWind(windDirection: Double): WindTurbine = {
    copy(direction = windDirection)
  }

  //Status report for monitoring
  def statusReport(): Map[String, String] = {
    Map(
      "id" -> id,
      "type" -> sourceType,
    "direction" -> f"$direction%.1f°",
    "windSpeed" -> f"$windSpeed%.1f m/s",
    "efficiency" -> f"${efficiency * 100}%.1f%%",
    "output" -> f"${getOutput()}%.2f kW"
    )
  }
}