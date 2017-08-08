object Main
{
	def main(args: Array[String]) = {
		for (a <- args)
			println ("decoded : " +java.net.URLDecoder.decode(a));
	}
}
