package com.innova.renewableenergy
object REPS {
  // Main control function to run the plant
  def run(
           sources: Map[String, EnergySource],
           alertSystem: AlertSystem
         ): (ControlUnit, Seq[String]) = {

    // Initialize control unit
    val controlUnit = ControlUnit(sources)

    // Monitor energy sources for issues
    val alerts = monitorSources(sources.values.toSeq, alertSystem)

    // Make appropriate adjustments
    val adjustedControlUnit = adjustOperations(controlUnit, alerts)

    // Return updated control unit and alerts
    (adjustedControlUnit, alerts)
  }

  // Monitor sources and generate alerts
  def monitorSources(
                      sources: Seq[EnergySource],
                      alertSystem: AlertSystem
                    ): Seq[String] = {
    alertSystem.generateAlerts(sources)
  }

  // Adjust plant operations based on alerts and current state
  def adjustOperations(
                        controlUnit: ControlUnit,
                        alerts: Seq[String]
                      ): ControlUnit = {

    // This is where the intelligence of the system would be
    // For now, implementing simple logic to demonstrate functionality

    // Helper function to extract IDs of sources with issues
    def getSourceIdFromAlert(alert: String): Option[String] = {
      val pattern = """\(\w+\)""".r
      pattern.findFirstIn(alert).map(s => s.substring(1, s.length - 1))
    }

    // Update control unit based on each alert
    alerts.foldLeft(controlUnit) { (updatedUnit, alert) =>
      val sourceId = getSourceIdFromAlert(alert).getOrElse("")

      if (sourceId.isEmpty) return updatedUnit

      // Get the source if it exists
      controlUnit.controlledSources.get(sourceId) match {
        case Some(source: SolarPanel) if alert.contains("Suboptimal orientation") =>
          // Adjust solar panel orientation
          updatedUnit.controlSolarOrientation(sourceId, 45.0) // Move to optimal angle

        case Some(source: WindTurbine) if alert.contains("Dangerous wind speed") =>
          // Feather turbine to safe position
          updatedUnit.controlTurbineDirection(sourceId, 0.0) // Face away from wind

        case Some(source: HydroPower) if alert.contains("High water flow rate") =>
          // Reduce flow rate to manageable level
          updatedUnit.controlWaterflowRate(sourceId, 40.0) // Reduce to safe level

        case _ => updatedUnit // No adjustment needed or source not found
      }
    }
  }

  // Analyze historical data to suggest improvements
  def analyzeData(
                   historicalData: Map[String, Seq[Double]],
                   currentSources: Map[String, EnergySource]
                 ): Seq[String] = {

    // This function would usually analyze historical performance data
    // and make suggestions for optimization

    val suggestions = collection.mutable.Buffer[String]()

    historicalData.foreach { case (sourceId, outputs) =>
      if (outputs.nonEmpty) {
        val avgOutput = outputs.sum / outputs.length
        val maxOutput = outputs.max

        // If average output is below 70% of maximum, suggest maintenance
        if (avgOutput < 0.7 * maxOutput) {
          suggestions += s"Source $sourceId: Maintenance recommended. " +
            f"Average output (${avgOutput}%.2f kW) is below 70%% of maximum (${maxOutput}%.2f kW)."
        }
      }
    }

    suggestions.toSeq
  }
}