package com.vivi.boar

import com.amazonaws.services.lambda.runtime.Context
import java.io.{ InputStream, OutputStream, PrintStream }
import scala.io.Source
import upickle.default._


class LambdaHandler {

    implicit val boarRW: ReadWriter[Problem] = upickle.default.macroRW[Problem]

    def handleRequest(input: InputStream, output: OutputStream, context: Context): Unit = {
        val json = Source.fromInputStream(input).mkString
        val problem: Problem = read[Problem](json)
        val stream = new PrintStream(output)
        stream.print(s"Problem  is ${problem}")
        stream.print(s"Solution  is ${problem.solution}")        
        output.close()
    }
}


