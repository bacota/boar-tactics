package com.vivi.boar

import com.amazonaws.services.lambda.runtime.Context
import java.io.{ InputStream, OutputStream, PrintStream }
import scala.io.Source
import upickle.default._
import scala.util.Random


class LambdaHandler {

    implicit val boarRW: ReadWriter[Problem] = upickle.default.macroRW[Problem]

    val random = new Random()

    def handleRequest(input: InputStream, output: OutputStream, context: Context): Unit = {
        val json = Source.fromInputStream(input).mkString
        val problem: Problem = read[Problem](json)
        val stream = new PrintStream(output)
        val solution = problem.solution.map(_.getOrElse(0.0))
        val value =solution.last
        val scan: Seq[Double] = solution.scanLeft(0.0)( (x,y) => x+y)
        val r = random.nextDouble()
        val suggestion = scan.zip(tactics).find(_._1 >= r).get._2
        stream.println(s"Problem  is ${problem}")
        stream.println(s"Value is ${value}")
        stream.println(s"Suggestion  is ${suggestion}")
        val values = tactics.zip(solution).map(p => s"${p._1}: ${p._2}").mkString
        stream.println(values)
        output.close()
    }
}


