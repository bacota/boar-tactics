import mill._, scalalib._, scalafmt._

object boar extends ScalaModule with ScalafmtModule {
    def scalaVersion = "3.5.2"
    override def ammoniteVersion = "3.0.0-M2"
    // def scalacOptions = Seq("-deprecation", "-explain", "-rewrite", "-source:3.4-migration")

    def scalacOptions = Seq("-deprecation", "-explain", "-rewrite")

    def ivyDeps = Agg(
        ivy"com.github.vagmcs:optimus_3:3.4.5",
    )

    object test extends ScalaTests {
        def ivyDeps = Agg(
            ivy"com.lihaoyi::utest:0.8.3"
        )
        def testFramework = "utest.runner.Framework"
    }
}
