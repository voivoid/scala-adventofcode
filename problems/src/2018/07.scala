package adventOfCode.problems
package year2018

object problem07 extends baseProblem {

  import adventOfCode.utils.algorithms.toposort._
  import scala.collection.immutable.TreeSet

  override def solve1(input: Input): String = {
    val stepsReqs = input.getLines().map(parseStepRequirement).toList
    topologicalSortLexiOrdered(stepsReqs).mkString
  }

  override def solve2(input: Input): Int = {
    solve2(input, maxWorkers = 5, stepBaseTime = 60)
  }

  def solve2(input: Input, maxWorkers: Int, stepBaseTime: Int): Int = {
    import adventOfCode.utils.algorithms.IteratorGroupValuesByTheirLength

    val stepsReqs = input.getLines().map(parseStepRequirement).toList

    val childrenStepsMap = stepsReqs.groupMap(_.parent)(_.child)
    val dependencyCounterMap = stepsReqs.iterator.map(_.child).groupValuesByTheirLength

    val readyForWorkSteps =
      childrenStepsMap.keys.to(TreeSet).diff(dependencyCounterMap.keys.toSet)

    // step states transition: step -> readyStep -> doneStep

    @scala.annotation.tailrec
    def doWork(
      readyStepsOrdered: TreeSet[Step],
      dependencyCounterMap: Map[Step, Int],
      workInProcess: List[Work],
      totalTime: Seconds
    ): Seconds = {
      if (readyStepsOrdered.isEmpty && workInProcess.isEmpty) {
        assert(dependencyCounterMap.isEmpty) // check the graph has no cycles
        totalTime
      } else {
        val (timePassed, (doneWork, busyWork)) = updateWork(workInProcess)

        val doneSteps = doneWork.map(_.step)
        val doneStepsChildren = doneSteps.flatMap(childrenStepsMap.withDefaultValue(List.empty))

        val nextDependencyCounterMap = decreaseDependencyCounters(doneStepsChildren, dependencyCounterMap)

        val stepsBecameReady = doneStepsChildren.filter(!nextDependencyCounterMap.contains(_))

        val freeWorkers = maxWorkers - busyWork.size
        val (stepsStarted, nextSteps) = (readyStepsOrdered ++ stepsBecameReady).splitAt(freeWorkers)
        val nextWorkers = busyWork ++ (stepsStarted.iterator.map(makeWorker(_, stepBaseTime)))

        doWork(nextSteps, nextDependencyCounterMap, nextWorkers, totalTime + timePassed)
      }
    }

    doWork(readyForWorkSteps, dependencyCounterMap, List.empty, 0)
  }

  private type Step = Char
  private type Seconds = Int
  private case class StepReq(override val parent: Step, override val child: Step) extends topoNode[Step]
  private case class Work(timeLeft: Seconds, step: Step)

  private def decreaseDependencyCounters(doneStepsChildren: List[Step], parentCounterStepsMap: Map[Step, Int]): Map[Step, Int] = {
    doneStepsChildren.foldLeft(parentCounterStepsMap) { case (map, child) =>
      map.updatedWith(child)(counter => counter.map(_ - 1).filter(_ != 0))
    }
  }

  private def updateWork(workInProcess: List[Work]): (Seconds, (List[Work], List[Work])) = {
    val timePassed = workInProcess.minByOption(_.timeLeft).map(_.timeLeft).getOrElse(0)

    val workInProcessWithUpdatedTime = workInProcess.map(worker => worker.copy(timeLeft = worker.timeLeft - timePassed))
    val (workDone, busyWorkers) = workInProcessWithUpdatedTime.partition(_.timeLeft == 0)

    (timePassed, (workDone, busyWorkers))
  }

  private def makeWorker(step: Step, stepBaseTime: Seconds): Work = {
    val stepTime = calcStepTime(step, stepBaseTime)
    Work(stepTime, step)
  }

  private def calcStepTime(step: Step, stepBaseTime: Seconds): Seconds = step - 'A' + stepBaseTime + 1

  private def parseStepRequirement(str: String): StepReq = {
    import fastparse._
    import fastparse.SingleLineWhitespace._
    import adventOfCode.utils.parse.{parseValue, alpha}

    def step[_: P] = P(alpha)
    def parser[_: P] = P("Step" ~ step ~ "must" ~ "be" ~ "finished" ~ "before" ~ "step" ~ step ~ "can" ~ "begin" ~ ".")
      .map(StepReq tupled _)

    parseValue(str, parser(_))
  }

}
