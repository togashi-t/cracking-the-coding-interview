package chapter7

object Question4 {

  // todo very difficult

  // 駐車場:オブジェクト指向で駐車場を設計してください。

  // 各種条件
  // 駐車場は複数の階層になっていて、各界の駐車スペースは複数列になっている。
  // 駐車可能な車両はオートバイ、自動車、バス
  // 駐車スペースにはオートバイ、普通車両、大型車両用のサイズがある。
  // オートバイはどのスペースにも駐車できる
  // 自動車は普通車両スペースと大型車両スペースのどちらにも駐車できる
  // バスは大型車両スペースにのみ駐車でき、1つの列上で5つ分の大型車両スペースを使う

  sealed trait VehicleSize
  case object MotorcycleSize extends VehicleSize
  case object CompactSize extends VehicleSize
  case object LargeSize extends VehicleSize


  sealed trait Vehicle {
    val spotsNeeded: Int
    val size: VehicleSize
    def canFitInSpot(spot: ParkingSpot): Boolean
    def printChar: Char
  }

  case class Motorcycle() extends Vehicle {
    val spotsNeeded = 1
    val size = MotorcycleSize
    def canFitInSpot(spot: ParkingSpot): Boolean = true
    def printChar: Char = 'M'
  }

  case class Car() extends Vehicle {
    val spotsNeeded = 1
    val size = CompactSize
    def canFitInSpot(spot: ParkingSpot): Boolean =
      spot.size == LargeSize || spot.size == CompactSize
    def printChar: Char = 'C'
  }

  case class Bus() extends Vehicle {
    val spotsNeeded = 5
    val size = LargeSize
    def canFitInSpot(spot: ParkingSpot): Boolean = spot.size == LargeSize
    def printChar: Char = 'B'
  }

  case class ParkingSpot(size: VehicleSize, vehicle: Option[Vehicle]) {
    def isAvailable: Boolean = vehicle.isEmpty
    def park(v: Vehicle): ParkingSpot = if (isAvailable && v.canFitInSpot(this)) copy(vehicle = Some(v)) else this
    def free(): ParkingSpot = copy(vehicle = None)
    def print: Char = vehicle.map(_.printChar).getOrElse(size match {
      case MotorcycleSize => 'm'
      case CompactSize => 'c'
      case LargeSize => 'l'
    })
  }

  case class Level(floor: Int, spots: Vector[ParkingSpot], availableSpots: Int) {
    def parkVehicle(vehicle: Vehicle): Option[Level] = {
      if (availableSpots < vehicle.spotsNeeded) None
      else findAvailableSpots(vehicle).flatMap { spotNumber =>
        val (updatedSpots, success) = parkStartingAtSpot(spotNumber, vehicle)
        if (success) Some(copy(spots = updatedSpots, availableSpots = availableSpots - vehicle.spotsNeeded))
        else None
      }
    }

    private def findAvailableSpots(vehicle: Vehicle): Option[Int] = {
      val spotsNeeded = vehicle.spotsNeeded
      spots.sliding(spotsNeeded, 1).indexWhere { window =>
        window.forall(_.isAvailable) && vehicle.canFitInSpot(window.head)
      } match {
        case -1 => None
        case index => Some(index)
      }
    }

    private def parkStartingAtSpot(spotNumber: Int, vehicle: Vehicle): (Vector[ParkingSpot], Boolean) = {
      val (before, after) = spots.splitAt(spotNumber)
      val (target, rest) = after.splitAt(vehicle.spotsNeeded)
      val updatedTarget = target.map(_.park(vehicle))
      (before ++ updatedTarget ++ rest, updatedTarget.forall(!_.isAvailable))
    }

    def print: String = spots.map(_.print).mkString(" ")
  }

  case class ParkingLot(levels: Vector[Level]) {
    def parkVehicle(vehicle: Vehicle): Option[ParkingLot] = {
      levels.zipWithIndex.collectFirst {
        case (level, index) if level.parkVehicle(vehicle).isDefined =>
          copy(levels = levels.updated(index, level.parkVehicle(vehicle).get))
      }
    }

    def print: Unit = {
      levels.foreach { level =>
        println(s"Level ${level.floor}: ${level.print}")
      }
      println()
    }
  }


}
