package adventOfCode.problems
package year2019

object problem23 extends baseProblem {

  import adventOfCode.utils.intcode._
  import scala.collection.immutable.Queue

  override def solve1(input: Input): Long = {
    val states = runNetwork(input)
    val firstStateWithNatMsg = states.find(_.queues(NatId).nonEmpty).get

    firstStateWithNatMsg.queues(NatId).head.y
  }

  override def solve2(input: Input): Long = {
    import adventOfCode.utils.algorithms.IteratorSlidingTuple

    val states = runNetwork(input)
    val firstYDup = states.filter(_.isIdle).map(_.queues(NatId).last.y).sliding2.find { case (y1, y2) => y1 == y2 }.get._1

    firstYDup
  }

  private def runNetwork(input: Input): Iterator[State] = {
    val memory = parseMemory(input.mkString)

    val networkAddrs = 0L until 50L
    val machines = networkAddrs.map(id => MachineData(id, makeMachine(memory, List(id, -1)))).toList
    val queues = networkAddrs.appended(NatId).map(id => id -> Queue.empty[Message]).toMap

    val initialState = State(machines, queues, false)

    Iterator.iterate(initialState)(runTick)
  }

  private type MachineId = Long
  private type MsgQueue = Queue[Message]
  private type Machines = List[MachineData]

  private case class MachineData(machineId: MachineId, machine: Machine)
  private case class Message(x: Long, y: Long, destination: MachineId)
  private case class State(machines: Machines, queues: Map[MachineId, MsgQueue], isIdle: Boolean)

  private def runTick(state: State): State = {
    val queuesMap =
      if (state.isIdle) state.queues.updated(0, Queue(state.queues(NatId).last))
      else state.queues

    val (updatedMachines, updatedQueue, updatedIdle) =
      state.machines.foldRight((List.empty: Machines, queuesMap, true)) {
        case (MachineData(machineId, machine), (accMachines, accQueueMap, accIdle)) => {
          val (updatedMachine, updatedQueueMap, updatedIdle) =
            if (machine.output.size == 3) {
              val List(y, x, destination) = machine.output: @unchecked
              val message = Message(x, y, destination)
              val queueWithMessage = accQueueMap.updatedWith(destination) {
                case Some(queue) => Some(queue.enqueue(message))
                case None        => sys.error("unexpected")
              }
              (resetOutput(machine), queueWithMessage, false)
            } else if (machine.waitsForInput && machine.input.isEmpty) {
              val msgQueue = accQueueMap(machineId)
              if (msgQueue.isEmpty) {
                (machine, accQueueMap, accIdle)
              } else {
                val (msg, restQueue) = msgQueue.dequeue
                (addInput(machine, List(msg.x, msg.y)), accQueueMap.updated(machineId, restQueue), false)
              }
            } else {
              (calcNextState(machine), accQueueMap, false)
            }

          val updatedMachines = MachineData(machineId, updatedMachine) :: accMachines
          (updatedMachines, updatedQueueMap, updatedIdle)
        }
      }

    State(updatedMachines, updatedQueue, updatedIdle)
  }

  private val NatId: MachineId = 255L

}
