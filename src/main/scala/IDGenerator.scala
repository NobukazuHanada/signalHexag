package hexasignal

case class Id(id:String) {
  def IdString : String = id
}

object IDGenerator{
  val hostname = java.net.InetAddress.getLocalHost().getHostName()
  var counter = 0

  def generate : Id = {
    val now = java.time.Instant.now().toString()
    counter += 1;
    val text = hostname + now + counter.toString()
    Id(java.security.MessageDigest.getInstance("MD5").digest(text.getBytes).map("%02x".format(_)).mkString)
  }
  
  def generate(text :String) : Id = {
    val now = java.time.Instant.now().toString()
    counter += 1;
    val phrase = text + hostname + now + counter.toString()
    Id(java.security.MessageDigest.getInstance("MD5").digest(phrase.getBytes).map("%02x".format(_)).mkString)
  }
}
