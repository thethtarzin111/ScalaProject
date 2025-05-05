//Team Innova
//Members: Mohammad Amman, Salek Md Peash Been, Thet Htar Zin
//This part of the code was implemented with AI tool called Claude.
/*
This is the main file or the starting point of the whole system.
 */
package com.innova.renewableenergy

object Main {
  def main(args: Array[String]): Unit = {
    println("Innova's Renewable Energy Plant System")
    println("------------------------------------")

    // Create energy sources
    val energySources = Map(
      "solar1" -> SolarPanel("solar1", 30.0),
      "solar2" -> SolarPanel("solar2", 50.0, 0.75),
      "wind1" -> WindTurbine("wind1", 180.0, 8.0),
      "wind2" -> WindTurbine("wind2", 270.0, 15.0),
      "hydro1" -> HydroPower("hydro1", 30.0),
      "hydro2" -> HydroPower("hydro2", 45.0, 15.0)
    )

    // Create alert system with default thresholds
    val alertSystem = AlertSystem()

    // Display initial data collection
    val records = DataCollection.fetchData(energySources)

    // Store the collected data
    DataStorage.storeData(records)

    // Display current status
    println("\nInitial Energy Source Status:")
    println("-----------------------------")
    println("%-6s | %-10s | %-8s | %-10s | %-10s".format("ID", "Type", "Status", "Output (kW)", "Efficiency"))
    println("-" * 60)

    records.foreach { record =>
      println(f"${record.sourceId}%-6s | ${record.sourceType}%-10s | ${record.status}%-8s | ${record.outputKw}%-10.2f | ${record.efficiency * 100}%-8.1f%%")
    }

    // Create control unit
    val controlUnit = ControlUnit(energySources)

    // Display total output
    println(s"\nTotal plant output: ${controlUnit.getTotalOutput()} kW")

    // Display output by source type
    println("\nOutput by source type:")
    controlUnit.getOutputByType().foreach { case (sourceType, output) =>
      println(f"$sourceType: $output%.2f kW")
    }

    // Display source status reports
    println("\nSource status reports:")
    controlUnit.controlledSources.values.foreach { source =>
      val status = source.statusReport()
      println(s"${status("type")} (${status("id")}): ${status("output")}")
    }

    // Generate and display alerts
    val alerts = alertSystem.generateAlerts(energySources.values.toSeq)

    // Display alerts
    if (alerts.nonEmpty) {
      println("\nAlerts:")
      alerts.foreach(println)
    } else {
      println("\nNo alerts detected.")
    }

    // Start the interactive menu system
    REPS.run(energySources, alertSystem)
  }
}