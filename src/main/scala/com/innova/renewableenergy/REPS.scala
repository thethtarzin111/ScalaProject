//Team Innova
//Members: Mohammad Amman, Salek Md Peash Been, Thet Htar Zin
/*
Description of the file:
This is our main system that connects everything.
There's also a menu base userinterface.
Initially planned to connect with UserInterface.scala but since something went wrong, we just put the menu in here.
AI Declaration: This part of the code was heavily referenced from AI tool called Claude and it was used for debugging as well.
 */
package com.innova.renewableenergy

object REPS {
  // Main control function to run the plant
  def run(
           sources: Map[String, EnergySource],
           alertSystem: AlertSystem
         ): Unit = {

    // Initialize control unit
    var controlUnit = ControlUnit(sources)
    var running = true

    // Main menu loop
    while (running) {
      val choice = UserInterface.showMenu()

      choice match {
        case 1 => // Monitor Energy Sources
          val records = DataCollection.fetchData(controlUnit.controlledSources)
          UserInterface.displaySourceStatuses(records)

          // Generate and display alerts
          val alerts = monitorSources(controlUnit.controlledSources.values.toSeq, alertSystem)
          UserInterface.displayAlerts(alerts)

          // Make automatic adjustments
          controlUnit = adjustOperations(controlUnit, alerts)
          println("\nAutomatic adjustments applied based on alerts.")

        case 2 => // View Energy Data
          val date = UserInterface.getSearchDate()
          val records = DataStorage.searchByDate(date)
          UserInterface.displayEnergyData(records)

        case 3 => // Analyze Energy Production
          val period = UserInterface.showAnalysisOptions()
          val sourceType = UserInterface.showSourceTypes()

          val allRecords = DataStorage.loadData()
          val filteredRecords = DataAnalysis.filterByPeriod(allRecords, period)

          val analysisResults = if (sourceType == "All") {
            Map(
              "Solar" -> DataAnalysis.analyzeOutput(filteredRecords, "Solar"),
              "Wind" -> DataAnalysis.analyzeOutput(filteredRecords, "Wind"),
              "Hydro" -> DataAnalysis.analyzeOutput(filteredRecords, "Hydro")
            )
          } else {
            Map(sourceType -> DataAnalysis.analyzeOutput(filteredRecords, sourceType))
          }

          UserInterface.displayAnalysisResults(analysisResults)

        case 4 => // Adjust Energy Sources
          println("\nManual Energy Source Adjustment")
          println("------------------------------")
          println("1. Adjust Solar Panel Orientation")
          println("2. Adjust Wind Turbine Direction")
          println("3. Adjust Hydro Power Flow Rate")
          print("\nChoose an option (1-3): ")

          val adjustChoice = scala.io.StdIn.readInt()

          adjustChoice match {
            case 1 => // Solar
              print("Enter Solar Panel ID: ")
              val id = scala.io.StdIn.readLine()
              print("Enter new orientation (degrees): ")
              val orientation = scala.io.StdIn.readDouble()
              controlUnit = controlUnit.controlSolarOrientation(id, orientation)
              println(s"Solar panel $id orientation adjusted to $orientation degrees.")

            case 2 => // Wind
              print("Enter Wind Turbine ID: ")
              val id = scala.io.StdIn.readLine()
              print("Enter new direction in degrees: ")
              val direction = scala.io.StdIn.readDouble()
              controlUnit = controlUnit.controlTurbineDirection(id, direction)
              println(s"Wind turbine $id direction adjusted to $direction degrees.")

            case 3 => // Hydro
              print("Enter Hydro Power ID: ")
              val id = scala.io.StdIn.readLine()
              print("Enter new flow rate (m³/s): ")
              val flowrate = scala.io.StdIn.readDouble()
              controlUnit = controlUnit.controlWaterflowRate(id, flowrate)
              println(s"Hydro power $id flow rate adjusted to $flowrate m³/s.")

            case _ =>
              println("Invalid option.")
          }

        case 5 => // Exit
          running = false
          println("Exiting REPS. Goodbye!")

        case _ => // Invalid choice
          println("Invalid option. Please try again.")
      }

      // Pause before showing menu again
      if (running) {
        println("\nPress Enter to continue...")
        scala.io.StdIn.readLine()
      }
    }
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