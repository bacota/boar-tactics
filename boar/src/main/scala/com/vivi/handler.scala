package com.vivi.boar

import com.amazonaws.services.lambda.runtime.Context
import java.io.{ InputStream, OutputStream, PrintStream }
import scala.io.Source

class LambdaHandler {
  def handleRequest(input: InputStream, output: OutputStream, context: Context): Unit = {
    val json = Source.fromInputStream(input).mkString
    val stream = new PrintStream(output)
    stream.print(s"Echoing ${json}")
    output.close()
  }
}


