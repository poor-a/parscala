package parscala
package file

import java.nio.file.{Path, Paths, Files}
import java.nio.file.attribute.BasicFileAttributes
import java.util.stream.Stream

object DirectoryTraverser {
  private def isScalaSource(p : Path, attr : BasicFileAttributes) : Boolean =
    attr.isRegularFile() && p.getFileName().toString.endsWith(".scala")

  def getScalaSources(dir : String) : Stream[String] = {
    Files.find(Paths.get(dir), Integer.MAX_VALUE, (p : Path, attr : BasicFileAttributes) => isScalaSource(p, attr)).map(_.toString)
  }
}
