package com.vivi.boar

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.{MPFloatVar, MPIntVar, ModelSpec}
import optimus.algebra.Const
import optimus.algebra.Expression

val tactics = Seq(
    "Skirmish",
    "Echelon Assault",
    "Stand Fast",
    "Withdraw",
    "Frontal Assault",
    "Commit Reserves",
    "Turn Flank",
    "Refuse Flank",
    "Expectation"
)

case class Problem(
    leader: Boolean,
    oppLeader: Boolean,
    flank: Boolean = false,
    drm: Double = 0,
    hexVal: Double = 10,
    turnFlank: Boolean = true,
    oppTurnFlank: Boolean = true
) extends ModelSpec(SolverLib.oJSolver) {

    lazy val opposite = Problem(
        leader = oppLeader,
        oppLeader = leader,
        flank = flank,
        drm = if (drm == 0) drm else -drm,
        hexVal = hexVal,
        turnFlank = oppTurnFlank,
        oppTurnFlank = turnFlank
    )

    lazy val solution: Seq[Option[Double]] = {
        val skirmish = MPFloatVar(0, 1)
        val echelon = MPFloatVar(0, 1)
        val stand = MPFloatVar(0, 1)
        val withdraw = MPFloatVar(0, 1)
        val frontal = MPFloatVar(0, 1)
        val reserves = MPFloatVar(0, 1)
        val turn = MPFloatVar(0, 1)
        val refuse = MPFloatVar(0, 1)
        val nc = Const(-drm)
        val hex = Const(hexVal)
        val v = MPFloatVar(-2, 2)
        val all: Expression =
            skirmish + echelon + stand + withdraw + frontal + reserves + turn + refuse

        val choices: Expression =
            if (!leader) skirmish + echelon + stand + withdraw
            else if (!flank) skirmish + echelon + stand + withdraw + frontal + reserves
            else if (!turnFlank) skirmish + echelon + stand + withdraw + frontal + reserves + refuse
            else all

        val zero = Const(0)
        val one = Const(1)
        val two = Const(2)

        add(choices := Const(1))
        add(all := Const(1))

        add(-echelon + stand + (nc - hex) * withdraw + Const(2) * frontal - reserves - turn >:= v)
        add(skirmish - stand - withdraw + turn - refuse >:= v)
        add(-skirmish + echelon + (nc - hex) * withdraw - frontal - reserves + two * turn >:= v)
        add(
            (nc + hex) * skirmish + echelon + (nc + hex) * stand + nc * withdraw + two * frontal + (nc + hex) * refuse >:= v
        )
        if (oppLeader) {
            add(-two * skirmish + stand - two * withdraw + reserves + turn >:= v)
            add(skirmish + stand - frontal - turn - refuse >:= v)
            if (flank) {
                if (oppTurnFlank)
                    add(skirmish - echelon - two * stand - frontal + reserves + two * refuse >:= v)
                add(echelon + (nc - hex) * withdraw + reserves - two * turn >:= v)
            }
        }
        add(v <:= one)

        start()
        release()

        Range(0, 9).map(model.getVarValue(_))
    }

    def printParams =
        s"${leader} | ${oppLeader} | ${flank}| ${drm} |${hexVal} |  ${turnFlank} | ${oppTurnFlank} "

}

def printSolution(vars: Seq[Option[Double]]) = {
    val values = vars.map(_.getOrElse(0))
    val pairs = tactics.zip(values)
    for (p <- pairs) {
        Console.err.println(s"${p._1} = ${p._2}")
    }
    Console.err.println("")
}

object test extends App {

    System.setProperty("logback.configurationFile", "logback-boar.xml")

    val problems =
        for {
            leader <- Seq(false, true)
            oppLeader <- Seq(false, true)
            problem = Problem(leader = leader, oppLeader = oppLeader)
            prob <-
                if (problem == problem.opposite) Seq(problem) else Seq(problem, problem.opposite)
        } yield (prob, prob.solution)

    val drms = Range(1, 7)
    val moreProblems = for {
        leader <- Seq(false, true)
        oppLeader <- Seq(false, true)
        flank <- if (leader || oppLeader) Seq(false, true) else Seq(false)
        hexVal <- Seq(0, 0.5)
        drm <- drms
        problem = Problem(
            leader = leader,
            oppLeader = oppLeader,
            flank = flank,
            turnFlank = true,
            oppTurnFlank = true,
            drm = drm,
            hexVal = hexVal
        )
        prob <- if (problem == problem.opposite) Seq(problem) else Seq(problem, problem.opposite)
    } yield (prob, prob.solution)

    val savannahProblems = for {
        leader <- Seq(false, true)
        oppLeader <- if (leader) Seq(false, true) else Seq(true)
        turnFlank <- if (leader) Seq(true, false) else Seq(false)
        oppTurnFlank <- if (oppLeader && leader && turnFlank) Seq(false) else Seq(true)
        hexVal <- Seq(0, 0.5)
        drm <- Range(0, 7)
        problem = Problem(
            leader = leader,
            oppLeader = oppLeader,
            flank = true,
            turnFlank = turnFlank,
            oppTurnFlank = oppTurnFlank,
            drm = drm,
            hexVal = hexVal
        )
        prob <- if (problem == problem.opposite) Seq(problem) else Seq(problem, problem.opposite)
    } yield (prob, prob.solution)

    val allProblems = problems ++ moreProblems ++ savannahProblems

    Console.err.println(s"${allProblems.size} RESULTS")
    Console.err.println("========")

    for ((prob, sols) <- allProblems) {
        Console.err.println(prob.printParams)
        printSolution(sols)
    }
}
