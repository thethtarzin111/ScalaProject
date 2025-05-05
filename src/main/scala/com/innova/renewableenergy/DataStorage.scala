package com.innova.renewableenergy
import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// Data record class to store energy source readings
case class EnergyRecord(
                         timestamp: LocalDateTime,
                         sourceId: String,
                         sourceType: String,
                         outputKw: Double,
                         status: String,
                         efficiency: Double,
                         additionalInfo: Map[String, String]
                       )

object DataStorage {
  // Update this path to match your project structure
  val dataFilePath = "src/main/resources/energy_data.csv"
  val dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")

  // Store energy records to CSV file
  def storeData(records: Seq[EnergyRecord]): Unit = {
    val file = new File(dataFilePath)
    val writer = new BufferedWriter(new FileWriter(file, file.exists()))

    // Write header if file is new
    if (!file.exists() || file.length() == 0) {
      writer.write("timestamp,source_id,source_type,output_kw,status,efficiency,additional_info\n")
    }

    // Write each record
    records.foreach { record =>
      val timestampStr = record.timestamp.format(dateTimeFormatter)
      val additionalInfoStr = record.additionalInfo.map { case (k, v) => s"$k=$v" }.mkString(";")

      val line = s"$timestampStr,${record.sourceId},${record.sourceType},${record.outputKw},${record.status},${record.efficiency},$additionalInfoStr\n"
      writer.write(line)
    }

    writer.close()
  }

  // Load all energy records from CSV file
  def loadData(): Seq[EnergyRecord] = {
    val file = new File(dataFilePath)
    if (!file.exists()) return Seq.empty

    val source = Source.fromFile(file)
    val lines = source.getLines().toSeq
    source.close()

    if (lines.isEmpty) return Seq.empty

    // Skip header line
    val dataLines = if (lines.head.contains("timestamp")) lines.tail else lines

    // Parse each line into an EnergyRecord
    dataLines.map { line =>
      val fields = line.split(",")

      // Process additional info into a map
      val additionalInfoMap = if (fields.length > 6) {
        fields(6).split(";").map { item =>
          val kv = item.split("=")
          if (kv.length == 2) kv(0) -> kv(1) else "" -> ""
        }.filter(_._1.nonEmpty).toMap
      } else {
        Map.empty[String, String]
      }

      EnergyRecord(
        timestamp = LocalDateTime.parse(fields(0), dateTimeFormatter),
        sourceId = fields(1),
        sourceType = fields(2),
        outputKw = fields(3).toDouble,
        status = fields(4),
        efficiency = fields(5).toDouble,
        additionalInfo = additionalInfoMap
      )
    }
  }

  // Search and filter data by date
  def searchByDate(date: LocalDateTime): Seq[EnergyRecord] = {
    val allData = loadData()
    allData.filter { record =>
      record.timestamp.toLocalDate == date.toLocalDate
    }
  }

  // Sort data by timestamp or output
  def sortData(data: Seq[EnergyRecord], byOutput: Boolean = false): Seq[EnergyRecord] = {
    if (byOutput) {
      data.sortBy(_.outputKw)
    } else {
      data.sortBy(_.timestamp)
    }
  }
}