//Team Innova
//Members: Mohammad Amman, Salek Md Peash Been, Thet Htar Zin
//In this code, we've mainly utilized pure functions as well as Option data type.
//This is mainly developed with reference from lectures and also resorted to AI when things were a bit confusing.

package com.innova.renewableenergy
case class ControlUnit(controlledSources: Map[String, EnergySource]) {

  //Control and adjust solar panel orientation
  def controlSolarOrientation(panelId: String, newOrientation: Double) = {
    controlledSources.get(panelId) match {
      case Some(panel: SolarPanel) => val updatedPanel = panel.trackSun(newOrientation)
        copy(controlledSources = controlledSources.updated(panelId, updatedPanel))

      case _=> this //Source is not a solar panel or is not found.
    }
  }

  //Control and adjust wind turbine direction
  def controlTurbineDirection(turbineId: String, newDirection: Double): ControlUnit = {
    controlledSources.get(turbineId) match {
      case Some(turbine: WindTurbine) => val updatedTurbine = turbine.trackWind(newDirection)
        copy(controlledSources = controlledSources.updated(turbineId, updatedTurbine))

      case _=> this
    }
  }

  //Control and adjust water flow rate
  def controlWaterflowRate(unitId: String, newFlowrate: Double): ControlUnit = {
    controlledSources.get(unitId) match {
      case Some(hydroPlant: HydroPower) => val updatedHydroPlant = hydroPlant.trackWaterflow(newFlowrate)
        copy(controlledSources = controlledSources.updated(unitId, updatedHydroPlant))

      case _=> this
    }
  }

  //Get power generated as total output from all controlled sources
  def getTotalOutput(): Double = {
    controlledSources.values.foldLeft(0.0)((total, source) => total + source.getOutput())
  }

  //We select a source type and get its power output.
  def getOutputByType(): Map[String, Double] = {
    controlledSources.values
      .groupBy(_.sourceType)
      .map {case (sourceType, sources) =>
        (sourceType, sources.foldLeft(0.0)((total, source) => total + source.getOutput()))
      }
  }
}