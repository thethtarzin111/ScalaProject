//Team Innova
//Members: Mohammad Amman, Salek Md Peash Been, Thet Htar Zin
/*
Description of the file
This is for the hydro plant where we can generate electricity from hydropower.
Things to consider here the most are flowrate and head.
 */
package com.innova.renewableenergy
case class HydroPower(
                       unitId: String,
                       flowrate: Double, //Unit in m^3/s
                       //"The head is the distance that a given water source has to fall before the point where power is generated." - Wikipedia
                       head: Double = 10.0, //Unit in meters
                       efficiency: Double = 0.9
                     ) extends EnergySource {

  val id: String = unitId
  val sourceType: String = "Hydro"

  def getOutput(): Double = {
    /*
    Hydropower formula
    P=η×ρ×g×h×Q
    P = power output in watts.
    η  = efficiency of the turbine
    ρ (rho) = water density (1000 kg/m^3)
    g = gravity (9.81 m/s^2)
    h = head height (m)
    Q = discharge of water in m^3/s (flkowrate)
    Reference: Omni Calculator (https://www.omnicalculator.com/ecology/hydroelectric-power)
     */

    val waterDensity = 1000.0
    val gravity = 9.81

    //First we calculate the power in Watts according to hydropower formula, and convert it to kW.
    val powerOutput = efficiency * waterDensity * gravity * head * flowrate
    powerOutput / 1000.0 //To convert to kW
  }

  //We adjust water flow rate to optimize output.
  def trackWaterflow(newFlowrate: Double): HydroPower = {
    copy(flowrate = Math.max(0.0, newFlowrate))
  }

  //Status report for monitoring
  def statusReport(): Map[String, String] = {
    Map(
      "id" -> id,
      "type" -> sourceType,
    "flowrate" -> f"$flowrate%.2f m³/s",
    "headHeight" -> f"$head%.1f m",
    "efficiency" -> f"${efficiency * 100}%.1f%%",
    "output" -> f"${getOutput()}%.2f kW"
    )
  }
}