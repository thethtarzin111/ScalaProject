//Team Innova
//Members: Mohammad Amman, Salek Md Peash Been, Thet Htar Zin
//AI Declaration: This part of the code was heavily referenced from AI tool called Claude and it was used for debugging as well.
package com.innova.renewableenergy
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// Simple command-line interface for the REPS
object UserInterface {

  // Display main menu and get user choice
  def showMenu(): Int = {
    println("\nRenewable Energy Plant System (REPS)")
    println("====================================")
    println("1. Monitor Energy Sources")
    println("2. View Energy Data")
    println("3. Analyze Energy Production")
    println("4. Adjust Energy Sources")
    println("5. Exit")
    print("\nChoose an option (1-5): ")

    try {
      scala.io.StdIn.readInt()
    } catch {
      case _: Exception => 0 // Invalid input
    }
  }

  // Display data analysis options
  def showAnalysisOptions(): String = {
    println("\nSelect Time Period:")
    println("1. Hourly")
    println("2. Daily")
    println("3. Weekly")
    println("4. Monthly")
    print("\nChoose an option (1-4): ")

    val choice = try {
      scala.io.StdIn.readInt()
    } catch {
      case _: Exception => 0 // Invalid input
    }

    choice match {
      case 1 => "hourly"
      case 2 => "daily"
      case 3 => "weekly"
      case 4 => "monthly"
      case _ => "daily" // Default to daily
    }
  }

  // Display source types for analysis
  def showSourceTypes(): String = {
    println("\nSelect Energy Source Type:")
    println("1. Solar")
    println("2. Wind")
    println("3. Hydro")
    println("4. All Sources")
    print("\nChoose an option (1-4): ")

    val choice = try {
      scala.io.StdIn.readInt()
    } catch {
      case _: Exception => 0 // Invalid input
    }

    choice match {
      case 1 => "Solar"
      case 2 => "Wind"
      case 3 => "Hydro"
      case _ => "All" // Default to all
    }
  }

  // Display source statuses
  def displaySourceStatuses(records: Seq[EnergyRecord]): Unit = {
    println("\nCurrent Energy Source Status:")
    println("-----------------------------")
    println("%-6s | %-10s | %-8s | %-10s | %-10s".format("ID", "Type", "Status", "Output (kW)", "Efficiency"))
    println("-" * 60) // Fixed: Using string multiplication instead of repeat

    records.foreach { record =>
      println(f"${record.sourceId}%-6s | ${record.sourceType}%-10s | ${record.status}%-8s | ${record.outputKw}%-10.2f | ${record.efficiency * 100}%-8.1f%%")
    }
  }

  // Display alerts
  def displayAlerts(alerts: Seq[String]): Unit = {
    if (alerts.isEmpty) {
      println("\nNo alerts detected.")
    } else {
      println("\nAlerts:")
      println("-------")
      alerts.foreach(println)
    }
  }

  // Display analysis results
  def displayAnalysisResults(results: Map[String, Map[String, Double]]): Unit = {
    println("\nEnergy Production Analysis:")
    println("--------------------------")

    results.foreach { case (sourceType, stats) =>
      println(s"\n$sourceType Sources:")
      println(f"  Mean: ${stats("mean")}%.2f kW")
      println(f"  Median: ${stats("median")}%.2f kW")
      println(f"  Mode: ${stats("mode")}%.2f kW")
      println(f"  Range: ${stats("range")}%.2f kW")
      println(f"  Midrange: ${stats("midrange")}%.2f kW")
    }
  }

  // Get date from user for data search
  def getSearchDate(): LocalDateTime = {
    println("\nEnter date (YYYY-MM-DD): ")
    val dateStr = scala.io.StdIn.readLine()

    try {
      LocalDateTime.parse(dateStr + " 00:00:00", DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
    } catch {
      case _: Exception =>
        println("Invalid date format! Using current date.")
        LocalDateTime.now()
    }
  }

  // Display energy data
  def displayEnergyData(records: Seq[EnergyRecord]): Unit = {
    if (records.isEmpty) {
      println("\nNo data available for the selected date.")
      return
    }

    println("\nEnergy Production Data:")
    println("----------------------")
    println("%-20s | %-6s | %-10s | %-10s | %-8s".format("Timestamp", "ID", "Type", "Output (kW)", "Status"))
    println("-" * 70) // Fixed: Using string multiplication instead of repeat

    records.foreach { record =>
      val timestamp = record.timestamp.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))
      println(f"$timestamp%-20s | ${record.sourceId}%-6s | ${record.sourceType}%-10s | ${record.outputKw}%-10.2f | ${record.status}%-8s")
    }
  }
}