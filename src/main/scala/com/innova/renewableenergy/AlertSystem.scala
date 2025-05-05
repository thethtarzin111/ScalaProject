package com.innova.renewableenergy
case class AlertSystem(thresholds: Map[String, Double] =
                       Map(
                         "minSolarOutput" -> 0.5,   // kW
                         "minWindOutput" -> 100.0,  // kW
                         "minHydroOutput" -> 200.0, // kW
                         "lowEfficiency" -> 0.5     // 50%
                       )) {

  //We define alert severity levels.
  object AlertLevel extends Enumeration {
    type AlertLevel = Value
    val Info, Warning, Critical = Value
  }
  import AlertLevel._

  //This is data structure for alert.
  case class Alert(
                    sourceId: String,
                    sourceType: String,
                    level: AlertLevel,
                    message: String,
                    timestamp: Long = System.currentTimeMillis()
                  )

  //We need to detect issues with energy sources as well.
  def detectIssues(sources: Seq[EnergySource]): Seq[Alert] = {
    sources.flatMap { source =>
      val output = source.getOutput()

      source match {
        //For solar energy
        case solar: SolarPanel =>
          val alerts = collection.mutable.Buffer[Alert]()

          // Check for low output
          if (output < thresholds("minSolarOutput")) {
            alerts += Alert(
              solar.id,
              "Solar",
              Warning,
              s"Low solar output: ${output.round} kW"
            )
          }

          //We need to check which orientation is optimal.
          //According to Caruna.fi, we use angle of 42 degrees.
          val optimalAngle = 42.0 // simplified
          if (Math.abs(solar.orientation - optimalAngle) > 30.0) {
            alerts += Alert(
              solar.id,
              "Solar",
              Info,
              s"Suboptimal orientation: ${solar.orientation.round}°"
            )
          }

          alerts.toSeq

        //For wind energy
        case wind: WindTurbine =>
          val alerts = collection.mutable.Buffer[Alert]()

          // Check for low output with sufficient wind
          if (output < thresholds("minWindOutput") && wind.windSpeed > 5.0) {
            alerts += Alert(
              wind.id,
              "Wind",
              Warning,
              s"Low wind output despite adequate wind speed: ${output.round} kW"
            )
          }

          // Check for dangerously high wind speed
          if (wind.windSpeed > 20.0) {
            alerts += Alert(
              wind.id,
              "Wind",
              Critical,
              s"Dangerous wind speed: ${wind.windSpeed} m/s"
            )
          }

          alerts.toSeq

        //For hydro power
        case hydro: HydroPower =>
          val alerts = collection.mutable.Buffer[Alert]()

          // Check for low output
          if (output < thresholds("minHydroOutput")) {
            alerts += Alert(
              hydro.id,
              "Hydro",
              Warning,
              s"Low hydro output: ${output.round} kW"
            )
          }

          // Check for high water flow
          if (hydro.flowrate > 50.0) {
            alerts += Alert(
              hydro.id,
              "Hydro",
              Warning,
              s"High water flow rate: ${hydro.flowrate} m³/s"
            )
          }

          alerts.toSeq

        case _ => Seq.empty[Alert]
      }
    }
  }

  // Generate formatted alerts for operators
  def generateAlerts(sources: Seq[EnergySource]): Seq[String] = {
    val alerts = detectIssues(sources)

    // Format alerts based on severity
    alerts.map { alert =>
      val timestamp = java.time.Instant
        .ofEpochMilli(alert.timestamp)
        .atZone(java.time.ZoneId.systemDefault())
        .toLocalDateTime
        .format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))

      val severityMarker = alert.level match {
        case Info => "[INFO]"
        case Warning => "[WARNING]"
        case Critical => "[CRITICAL]"
      }

      f"$severityMarker $timestamp | ${alert.sourceType} (${alert.sourceId}): ${alert.message}"
    }
  }
}