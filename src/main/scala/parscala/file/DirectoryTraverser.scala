package parscala
package file

import java.nio.file.{Path, Files}
import java.nio.file.attribute.BasicFileAttributes
import java.util.stream.Stream

object DirectoryTraverser {
  private def isScalaSource(p : Path, attr : BasicFileAttributes) : Boolean =
    attr.isRegularFile() && p.getFileName().toString.endsWith(".scala")

  def getScalaSources(dir : Path) : Stream[Path] = {
    Files.find(dir, Integer.MAX_VALUE, (p : Path, attr : BasicFileAttributes) => isScalaSource(p, attr))
  }
}
