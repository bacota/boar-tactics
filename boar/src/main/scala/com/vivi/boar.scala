package com.vivi.boar

import optimus.optimization._
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.{MPFloatVar, MPIntVar, ModelSpec}
import optimus.algebra.Const
import optimus.algebra.Expression

case class  Tactic(val name: String)
object Skirmish extends Tactic("Skirmish")
object Echelon extends Tactic("Echelon Assault")
object Stand extends Tactic("Stand Fast")
object Withdraw extends Tactic("Withdraw")
object Frontal extends Tactic("Frontal Assault")
object Reserves extends Tactic("Commit Reserves")
object Turn extends Tactic("Turn Flank")
object Refuse extends Tactic("Refuse Flank")

import Tactic._

val  tactics = Seq(Skirmish,Echelon,Stand,Withdraw,Frontal,Reserves,Turn,Refuse)

val drms: Map[(Tactic,Tactic), Option[Int]] = Map(
    (Skirmish, Skirmish) -> Some(0),
    (Skirmish, Echelon) -> Some(1),
    (Skirmish, Stand) -> Some(-1),
    (Skirmish, Withdraw) -> None,
    (Skirmish, Frontal) -> Some(-2),
    (Skirmish, Reserves) -> Some(1),
    (Skirmish, Turn) -> Some(1),
    (Skirmish, Refuse) -> Some(0),    

    (Echelon, Skirmish) -> Some(-1),
    (Echelon, Echelon) -> Some(0),
    (Echelon, Stand) -> Some(1),
    (Echelon, Withdraw) -> Some(1),
    (Echelon, Frontal) -> Some(0),
    (Echelon, Reserves) -> Some(0),
    (Echelon, Turn) -> Some(-1),
    (Echelon, Refuse) -> Some(1),    

    (Stand, Skirmish) -> Some(1),
    (Stand, Echelon) -> Some(-1),
    (Stand, Stand) -> Some(0),
    (Stand, Withdraw) -> None,
    (Stand, Frontal) -> Some(1),
    (Stand, Reserves) -> Some(1),
    (Stand, Turn) -> Some(-2),
    (Stand, Refuse) -> Some(0),    

    (Withdraw, Skirmish) -> None,
    (Withdraw, Echelon) -> Some(-1),
    (Withdraw, Stand) -> None,
    (Withdraw, Withdraw) -> None,
    (Withdraw, Frontal) -> Some(-2),
    (Withdraw, Reserves) -> Some(0),
    (Withdraw, Turn) -> Some(0),
    (Withdraw, Refuse) -> None,    

    (Frontal, Skirmish) -> Some(2),
    (Frontal, Echelon) -> Some(0),
    (Frontal, Stand) -> Some(-1),
    (Frontal, Withdraw) -> Some(2),
    (Frontal, Frontal) -> Some(0),
    (Frontal, Reserves) -> Some(-1),
    (Frontal, Turn) -> Some(-1),
    (Frontal, Refuse) -> Some(0),    

    (Reserves, Skirmish) -> Some(-1),
    (Reserves, Echelon) -> Some(0),
    (Reserves, Stand) -> Some(-1),
    (Reserves, Withdraw) -> Some(0),
    (Reserves, Frontal) -> Some(1),
    (Reserves, Reserves) -> Some(0),
    (Reserves, Turn) -> Some(1),
    (Reserves, Refuse) -> Some(1),

    (Turn, Skirmish) -> Some(-1),
    (Turn, Echelon) -> Some(1),
    (Turn, Stand) -> Some(2),
    (Turn, Withdraw) -> Some(0),
    (Turn, Frontal) -> Some(1),
    (Turn, Reserves) -> Some(-1),
    (Turn, Turn) -> Some(0),
    (Turn, Refuse) -> Some(-2),    
    
    (Refuse, Skirmish) -> Some(0),
    (Refuse, Echelon) -> Some(-1),
    (Refuse, Stand) -> Some(0),
    (Refuse, Withdraw) -> None,
    (Refuse, Frontal) -> Some(0),
    (Refuse, Reserves) -> Some(-1),
    (Refuse, Turn) -> Some(2),
    (Refuse, Refuse) -> Some(0),    
)

def getDrm(attack: Tactic, defense: Tactic): Option[Int] =
    if (!drms.contains((attack,defense))) { 
            println(s"Unrecognized tactic in '${attack}' or '${defense}'")
            None
    } else {
        drms( (attack,defense) )
    }

case class Problem(
    leader: Boolean,
    oppLeader: Boolean,
    flank: Boolean = false,
    drm: Double = 0,
    hexVal: Double = 10,
    turnFlank: Boolean = true,
    oppTurnFlank: Boolean = true,
    useEchelon: Boolean = true,
    oppEchelon: Boolean = true 
) extends ModelSpec(SolverLib.oJSolver) {

    lazy val opposite = Problem(
        leader = oppLeader,
        oppLeader = leader,
        flank = flank,
        drm = if (drm == 0) drm else -drm,
        hexVal = hexVal,
        turnFlank = oppTurnFlank,
        oppTurnFlank = turnFlank,
        useEchelon = oppEchelon,
        oppEchelon = useEchelon
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

        val zero = Const(0)
        val one = Const(1)
        val two = Const(2)

        add(skirmish + echelon + stand + withdraw + frontal + reserves + turn + refuse  := one)

        if (!turnFlank)
            add(turn := zero)

        if (!useEchelon)
            add(echelon := zero)

        if (!leader) {
            add(frontal := zero)
            add(reserves := zero)
        }

        if (!flank || !leader) {
            add(refuse := zero)                        
            add(turn := zero)            
        }

        add(-echelon + stand + (nc - hex) * withdraw + Const(2) * frontal - reserves - turn >:= v)
        if (oppEchelon)
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
        s"${leader} | ${oppLeader} | ${flank}| ${drm} |${hexVal} |  ${turnFlank} | ${oppTurnFlank}|${useEchelon}|${oppEchelon} "

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


    val chewProblems = for {
        leader <- Seq(false, true)
        oppLeader <- if (leader) Seq(false, true) else Seq(true)
        chew <- Seq(1,2)
        hexVal = 100
        drm = 0
        (turnFlank, useEchelon) = if (chew != 1) (true,true) else (false,false)
        (oppTurnFlank, oppEchelon) = if (chew != 2) (true,true) else (false,false)
        problem = Problem(
            leader = leader,
            oppLeader = oppLeader,
            flank = true,
            turnFlank = turnFlank,
            oppTurnFlank = oppTurnFlank,
            useEchelon = useEchelon,
            oppEchelon = oppEchelon,
            drm = drm,
            hexVal = hexVal
        )
        prob <- if (problem == problem.opposite) Seq(problem) else Seq(problem, problem.opposite)
    } yield (prob, prob.solution)
    
    for ((prob, sols) <- allProblems ++ chewProblems) {
    //for ((prob, sols) <- chewProblems) {    
        Console.err.println(prob.printParams)
        printSolution(sols)
    }


    val special = Problem(
        leader = true,
        oppLeader = true,
        flank = true,
        turnFlank = true,
        oppTurnFlank = false,
        useEchelon = false,
        oppEchelon = true,
        drm = 0,
        hexVal = 0
    )
    printSolution(special.solution)
        printSolution(special.opposite.solution)
}
