//Team Innova
//Members: Mohammad Amman, Salek Md Peash Been, Thet Htar Zin
//AI Declaration: This part of the code was heavily referenced from AI tool called Claude and it was used for debugging as well.
package com.innova.renewableenergy
import java.time.LocalDateTime

// DataCollection object simulates gathering data from energy sources
object DataCollection {

  // Simulate fetching real-time data from energy sources
  def fetchData(sources: Map[String, EnergySource]): Seq[EnergyRecord] = {
    val now = LocalDateTime.now()

    sources.map { case (id, source) =>
      // In a real implementation, this would get actual readings from sensors
      // For this MVP, we'll simulate readings based on the source type

      val (output, status, additionalInfo) = source match {
        case solar: SolarPanel =>
          val output = simulateSolarOutput(solar)
          val status = if (output < 0.5) "warning" else "active"
          val additionalInfo = Map("orientation" -> solar.orientation.toString)
          (output, status, additionalInfo)

        case wind: WindTurbine =>
          val output = simulateWindOutput(wind)
          val status =
            if (wind.windSpeed > 20.0) "critical"
            else if (wind.windSpeed > 18.0) "warning"
            else "active"
          val additionalInfo = Map(
            "windSpeed" -> wind.windSpeed.toString,
            "direction" -> wind.direction.toString
          )
          (output, status, additionalInfo)

        case hydro: HydroPower =>
          val output = simulateHydroOutput(hydro)
          val status =
            if (hydro.flowrate > 48.0) "warning"
            else "active"
          val additionalInfo = Map(
            "flowrate" -> hydro.flowrate.toString,
            "head" -> hydro.head.toString
          )
          (output, status, additionalInfo)

        case _ => (0.0, "unknown", Map.empty[String, String])
      }

      EnergyRecord(
        timestamp = now,
        sourceId = source.id,
        sourceType = source.sourceType,
        outputKw = output,
        status = status,
        efficiency = getEfficiency(source),
        additionalInfo = additionalInfo
      )
    }.toSeq
  }

  // Get the efficiency of an energy source
  private def getEfficiency(source: EnergySource): Double = source match {
    case solar: SolarPanel => solar.efficiency
    case wind: WindTurbine => wind.efficiency
    case hydro: HydroPower => hydro.efficiency
    case _ => 0.0
  }

  // Simulate solar panel output based on orientation and time of day
  private def simulateSolarOutput(solar: SolarPanel): Double = {
    val hour = LocalDateTime.now().getHour
    val solarFactor =
      if (hour < 6 || hour > 18) 0.0
      else 1.0 - Math.abs(12 - hour) / 8.0

    val orientationFactor = Math.max(0.0, 1.0 - Math.abs(solar.orientation - 42.0) / 90.0)
    2.5 * solarFactor * orientationFactor * solar.efficiency
  }

  // Simulate wind turbine output based on wind speed
  private def simulateWindOutput(wind: WindTurbine): Double = {
    if (wind.windSpeed < 3.0 || wind.windSpeed > 25.0) {
      0.0
    } else {
      val speedFactor = Math.min(1.0, Math.pow(wind.windSpeed / 12.0, 3))
      250.0 * speedFactor * wind.efficiency
    }
  }

  // Simulate hydro power output based on flowrate and head
  private def simulateHydroOutput(hydro: HydroPower): Double = {
    val waterDensity = 1000.0
    val gravity = 9.81
    val powerOutput = hydro.efficiency * waterDensity * gravity * hydro.head * hydro.flowrate
    powerOutput / 1000.0 // Convert to kW
  }
}