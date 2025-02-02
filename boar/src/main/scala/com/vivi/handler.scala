package com.vivi.boar

import com.amazonaws.services.lambda.runtime.Context
import java.io.{InputStream, OutputStream, PrintStream}
import java.util.Base64
import scala.io.Source
import upickle.default._
import scala.util.Random

case class ResponseBody(
    value: Double,
    suggestion: String,
    values: Map[String, Double]
)

case class Response(
    statusCode: Int = 200,
    headers: Map[String, String] = Map(),
    body: ResponseBody
)

class LambdaHandler {

    implicit val boarRW: ReadWriter[Problem] = upickle.default.macroRW[Problem]
    implicit val responseBodyRW: ReadWriter[ResponseBody] = upickle.default.macroRW[ResponseBody]
    implicit val responseRW: ReadWriter[Response] = upickle.default.macroRW[Response]

    val random = new Random()

    def handleRequest(input: InputStream, output: OutputStream, context: Context): Unit = {
        val jsonString = Source.fromInputStream(input).mkString
        val json = ujson.read(jsonString)
        val encoded: String = json("body").str
        val body = String(Base64.getDecoder().decode(encoded))
        val problem: Problem = read[Problem](body)
        val stream = new PrintStream(output)
        val solution = problem.solution.map(_.getOrElse(0.0))
        val value = solution.last + problem.drm
        val scan: Seq[Double] = solution.scanLeft(0.0)((x, y) => x + y)
        val r = random.nextDouble()
        val responseBody = ResponseBody(
            value = solution.last + problem.drm,
            suggestion = scan.zip(tactics).reverse.find(_._1 <= r).get._2,
            values = tactics.zip(solution).toMap
        )
        val response = Response(
            body = responseBody
        )
        stream.println(write(response))
        output.close()
    }
}
