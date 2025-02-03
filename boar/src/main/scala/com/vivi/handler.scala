package com.vivi.boar

import com.amazonaws.services.lambda.runtime.Context
import java.io.{InputStream, OutputStream, PrintStream}
import java.util.Base64
import scala.io.Source
import upickle.default._
import scala.util.Random

case class Response(
    expected: Double,
    suggestion: String,
    values: Map[String, Double]
)


class LambdaHandler {

    System.setProperty("logback.configurationFile", "logback-boar.xml")

    val random = new Random()

    implicit val boarRW: ReadWriter[Problem] = upickle.default.macroRW[Problem]
    implicit val responseRW: ReadWriter[Response] = upickle.default.macroRW[Response]

    def handleRequest(input: InputStream, output: OutputStream, context: Context): Unit = {
        val jsonString = Source.fromInputStream(input).mkString
        val json = ujson.read(jsonString)
        val bodyStr = json("body").str
        val isEncoded = json("isBase64Encoded").bool
        val body = if (!isEncoded) bodyStr else  String(Base64.getDecoder().decode(bodyStr))
        val problem: Problem = read[Problem](body)
        val stream = new PrintStream(output)
        val solution = problem.solution.map(_.getOrElse(0.0))
        val scan: Seq[Double] = solution.scanLeft(0.0)((x, y) => x + y)
        val r = random.nextDouble()
        val response = Response(
            expected =  solution.last + problem.drm,
            suggestion = scan.zip(tactics).reverse.find(_._1 <= r).get._2,
            values = tactics.zip(solution).toMap
        )
        stream.println(write(response))
        output.close()
    }
}
