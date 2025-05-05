package com.innova.renewableenergy
//Base trait (Parent class) for all energy sources
trait EnergySource {
  //id and type mustn't be changed. Therefore, we declare them as immutable variables.
  val id: String
  val sourceType: String

  //Get current power output
  def getOutput(): Double

  def statusReport(): Map[String, String] //for id and type
}