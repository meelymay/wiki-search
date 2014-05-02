import scala.util.Marshal

trait WikiIndex {
  def dumpIndex(index: Index, filename: String) {
      val out = new FileOutputStream(filename)
      out.write(Marshal.dump(index))
      out.close
  }

  def loadIndex(filename: String): Index = {
      val in = new FileInputStream(filename)
      val bytes = Stream.continually(in.read).takeWhile(-1 != _).map(_.toByte).toArray
      Marshal.load[Index](bytes)
  }
}