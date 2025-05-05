package com.innova.renewableenergy
// DataAnalysis object provides statistical functions for energy data
object DataAnalysis {

  // Calculate mean (average) of values
  def mean(data: Seq[Double]): Double = {
    if (data.isEmpty) 0.0
    else data.sum / data.length
  }

  // Calculate median (middle value) of sorted data
  def median(data: Seq[Double]): Double = {
    if (data.isEmpty) 0.0
    else {
      val sortedData = data.sorted
      val midIndex = sortedData.length / 2

      if (sortedData.length % 2 == 0) {
        (sortedData(midIndex - 1) + sortedData(midIndex)) / 2.0
      } else {
        sortedData(midIndex)
      }
    }
  }

  // Calculate mode (most frequent value) of data
  def mode(data: Seq[Double]): Seq[Double] = {
    if (data.isEmpty) Seq.empty
    else {
      // Count occurrences of each value
      val valueCounts = data.groupBy(identity).view.mapValues(_.length).toMap

      // Find the maximum frequency
      val maxCount = valueCounts.values.max

      // Find all values with the maximum frequency
      valueCounts.filter(_._2 == maxCount).keys.toSeq
    }
  }

  // Calculate range (difference between max and min)
  def range(data: Seq[Double]): Double = {
    if (data.isEmpty) 0.0
    else data.max - data.min
  }

  // Calculate midrange (average of max and min)
  def midrange(data: Seq[Double]): Double = {
    if (data.isEmpty) 0.0
    else (data.max + data.min) / 2.0
  }

  // Filter data by time period
  def filterByPeriod(records: Seq[EnergyRecord], period: String): Seq[EnergyRecord] = {
    val now = java.time.LocalDateTime.now()

    period match {
      case "hourly" =>
        records.filter(_.timestamp.getHour == now.getHour)

      case "daily" =>
        records.filter(_.timestamp.toLocalDate == now.toLocalDate)

      case "weekly" =>
        val weekStart = now.minusDays(now.getDayOfWeek.getValue - 1)
        records.filter(r => !r.timestamp.toLocalDate.isBefore(weekStart.toLocalDate))

      case "monthly" =>
        records.filter(r =>
          r.timestamp.getMonth == now.getMonth &&
            r.timestamp.getYear == now.getYear
        )

      case _ => records
    }
  }

  // Analyze output data for a specific source type
  def analyzeOutput(records: Seq[EnergyRecord], sourceType: String): Map[String, Double] = {
    val outputs = records
      .filter(_.sourceType == sourceType)
      .map(_.outputKw)

    if (outputs.isEmpty) {
      Map("mean" -> 0.0, "median" -> 0.0, "mode" -> 0.0, "range" -> 0.0, "midrange" -> 0.0)
    } else {
      Map(
        "mean" -> mean(outputs),
        "median" -> median(outputs),
        "mode" -> (if (mode(outputs).isEmpty) 0.0 else mode(outputs).head),
        "range" -> range(outputs),
        "midrange" -> midrange(outputs)
      )
    }
  }
}