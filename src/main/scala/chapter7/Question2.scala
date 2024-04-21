package chapter7

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Question2 {

  // コールセンター:応答者、マネージャ、ディレクター、3段階のレベルの従業員がいるコールセンターをイメージしてください。
  // まず問い合わせがきたら、手の空いている応答者につなぎます。
  // 応答者で対応できない場合はマネージャにつなぎます。
  // マネージャが忙しい場合や対応しきれない場合はディレクターにつなぎます。
  // このような状況についてクラスとデー夕構造を設計してください。
  // 最初につなぐことのできる従業員に問い合わせを割り当てるメソッドdispatchCall()も実装してください。


  // スタッフについて
  // 職位
  abstract class Position
  case object Respondent extends Position
  case object Manager extends Position
  case object Director extends Position

  case class Employee(id: Long, position: Position)


  // 問合せ者
  case class Caller(name: String)
  // 問合せ
  case class Call(id: Long, caller: Caller)




  case class CallCenter(
                         openEmployees: Map[Position, Queue[Employee]], // 手が空いている従業員
                         ongoings: Set[(Call, Employee)], // 対応中の電話および従業員
                         respondentWaitingCalls: Queue[Call], // 当初の電話つながり待ち
                         superiorWaitingCalls: Map[Position, Queue[Call]] // 上役への電話接続待ち
                       )

  object CallCenter {
    // 問合せ者を順番につなぐ。はじめの応対者は必ずRespondentとする
    def dispatchCall(callCenter: CallCenter): CallCenter = {
      (callCenter.respondentWaitingCalls.dequeueOption, callCenter.openEmployees(Respondent).dequeueOption) match {
        case (Some((headCall, tailCall)), Some((headOpenRespondent, tailOpenRespondent))) => // 電話のつながり待ちがあり、かつ手が空いているRespondentがある場合
          callCenter.copy(
            openEmployees = callCenter.openEmployees.updated(Respondent, tailOpenRespondent),
            ongoings = callCenter.ongoings + (headCall, headOpenRespondent),
            respondentWaitingCalls = tailCall
          )
        case _ =>
          callCenter
      }
    }


    // 通話終了
    def endCall(callCenter: CallCenter, callWithEmployee: (Call, Employee)): CallCenter = {
      val (_, employee) = callWithEmployee
      val position = employee.position
      callCenter.copy(
        openEmployees = callCenter.openEmployees.updated(position, callCenter.openEmployees(position).enqueue(employee)),
        ongoings = callCenter.ongoings - callWithEmployee
      )
    }

    // 上役に取り次ぐ
    def passToSuperior(callCenter: CallCenter, callWithEmployee: (Call, Employee)): CallCenter = {
      val (call, employee) = callWithEmployee
      val position = employee.position

      // Respondentの場合、最初にManagerを試み、なければDirectorへ
      // Managerの場合は直接Directorへ
      val nextPositions = position match {
        case Respondent => List(Manager, Director)
        case Manager    => List(Director)
        case _          => throw new Exception("Directorからは取り次げない")
      }

      @tailrec
      def tryAssign(callCenter: CallCenter, positions: List[Position]): CallCenter = positions match {
        case nextPosition :: rest =>
          callCenter.openEmployees.get(nextPosition).flatMap(_.dequeueOption) match {
            case Some((nextEmployee, updatedQueue)) =>
              // 次の従業員に問い合わせを割り当て、キューを更新
              val updatedOngoings = callCenter.ongoings - callWithEmployee + ((call, nextEmployee))
              val updatedEmployees = callCenter.openEmployees.updated(nextPosition, updatedQueue)
              callCenter.copy(openEmployees = updatedEmployees, ongoings = updatedOngoings)

            case None =>
              // 次の役職の従業員が空いていなければ、さらに上の役職を試みる
              tryAssign(callCenter, rest)
          }

        case Nil =>
          // 適切な従業員がいない場合、問い合わせを待機キューに追加
          val lastAttempted = positions.head
          val updatedQueue = callCenter.superiorWaitingCalls.getOrElse(lastAttempted, Queue.empty).enqueue(call)
          callCenter.copy(superiorWaitingCalls = callCenter.superiorWaitingCalls.updated(lastAttempted, updatedQueue))
      }

      tryAssign(callCenter, nextPositions)
    }

  }


}
