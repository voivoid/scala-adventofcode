package adventOfCode.problems
package year2019

object problem07 extends baseProblem {

  import adventOfCode.utils.intcode._

  override def solve1(input: Input): Signal = {
    solve(input, phaseSettings = 0 to 4, calcSimpleSignal)
  }

  override def solve2(input: Input): Signal = {
    solve(input, phaseSettings = 5 to 9, calcLoopedSignal)
  }

  private type Signal = Int

  private def solve(input: Input, phaseSettings: Range, calcSignal: (IndexedSeq[Int], Memory) => Signal): Signal = {
    val memory = parseMemory(input)
    val settingsPermutations = phaseSettings.permutations.iterator
    val signals = settingsPermutations.map(settings => calcSignal(settings, memory))

    signals.max
  }

  private def calcSimpleSignal(phaseSettings: IndexedSeq[Int], memory: Memory): Signal = {
    val initSignal = 0
    phaseSettings.foldLeft(initSignal) {
      case (outputSignal, phaseSetting) => {
        val machine = run(memory, List(phaseSetting, outputSignal))
        machine.output.head
      }
    }
  }

  private def calcLoopedSignal(phaseSettings: IndexedSeq[Int], memory: Memory): Signal = {
    val amps = phaseSettings.map(setting => {
      val machine = makeMachine(memory, List(setting))
      runMachine(machine)
    })

    val initSignal = 0
    loopAmps(initSignal, amps.toList)
  }

  @scala.annotation.tailrec
  private def loopAmps(initialSignal: Signal, amps: List[Machine]): Signal = {
    val updatedAmps = List.unfold((initialSignal, amps)) {
      case (_, Nil) => None
      case (signal, amp :: restAmps) => {
        val (outputSignal, updatedAmp) = provideSignalAndGetOutput(signal, amp)
        Some((updatedAmp, (outputSignal, restAmps)))
      }
    }

    val lastAmp = updatedAmps.last
    val lastSignal = lastAmp.output.head

    if (lastAmp.waitsForInput) {
      loopAmps(lastSignal, updatedAmps)
    } else {
      lastSignal
    }
  }

  private def provideSignalAndGetOutput(signal: Signal, amp: Machine): (Signal, Machine) = {
    assert(amp.waitsForInput)

    val updatedAmp = resumeMachine(amp, List(signal))
    (updatedAmp.output.head, updatedAmp)
  }

}
