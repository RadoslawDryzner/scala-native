package scala.scalanative

import org.scalatest._

import scala.scalanative.api.CompilationFailedException

class NIRCompilerTest extends FlatSpec with Matchers with Inspectors {

  "The compiler" should "return products of compilation" in {
    val files =
      NIRCompiler { _ compile "class A" }.filter(_.isFile).map(_.getName)
    val expectedNames = Seq("A.class", "A.nir")
    files should contain theSameElementsAs expectedNames
  }

  it should "compile whole directories" in {
    val sources = Map(
      "A.scala" -> "class A",
      "B.scala" -> "class B extends A",
      "C.scala" -> "trait C",
      "D.scala" -> """class D extends B with C
                     |object E""".stripMargin
    )

    NIRCompiler.withSources(sources) {
      case (sourcesDir, compiler) =>
        val nirFiles =
          compiler.compile(sourcesDir) filter (_.isFile) map (_.getName)
        val expectedNames =
          Seq("A.class",
              "A.nir",
              "B.class",
              "B.nir",
              "C.class",
              "C.nir",
              "D.class",
              "D.nir",
              "E$.class",
              "E$.nir",
              "E.class")
        nirFiles should contain theSameElementsAs expectedNames
    }
  }

  it should "report compilation errors" in {
    assertThrows[api.CompilationFailedException] {
      NIRCompiler { _ compile "invalid" }
    }
  }

  it should "compile to a specified directory" in {
    val temporaryDir =
      java.nio.file.Files.createTempDirectory("my-target").toFile()
    val nirFiles =
      NIRCompiler(outDir = temporaryDir) { _ compile "class A" }
        .filter(_.isFile)
    forAll(nirFiles) { _.getParentFile should be(temporaryDir) }
  }

  it should "report error for extern method without result type" in {
    // given
    val code =
      """import scala.scalanative.native.extern
        |
        |@extern
        |object Dummy {
        |  def foo() = extern
        |}""".stripMargin

    // when
    val caught = intercept[CompilationFailedException] {
      NIRCompiler(_.compile(code))
    }

    // then
    caught.getMessage should include("extern method foo needs result type")
  }

}
