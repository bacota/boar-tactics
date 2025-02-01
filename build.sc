import mill._, scalalib._, scalafmt._

object boar extends ScalaModule with ScalafmtModule {
    def scalaVersion = "3.5.2"
    override def ammoniteVersion = "3.0.0-M2"
    // def scalacOptions = Seq("-deprecation", "-explain", "-rewrite", "-source:3.4-migration")

    def scalacOptions = Seq("-deprecation", "-explain", "-rewrite")

    def ivyDeps = Agg(
        ivy"com.github.vagmcs:optimus_3:3.4.5",
        ivy"com.github.vagmcs:optimus-solver-oj_3:3.4.5",
        ivy"com.amazonaws:aws-lambda-java-core:1.2.3",
        ivy"com.amazonaws:aws-lambda-java-events:3.14.0",
        ivy"com.lihaoyi:upickle_3:4.1.0"
    )

    object test extends ScalaTests {
        def ivyDeps = Agg(
            ivy"com.lihaoyi::utest:0.8.3"
        )
        def testFramework = "utest.runner.Framework"
    }
}
