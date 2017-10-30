class Block(val index: Long, val time: Long, val prehash: String, val data: Seq[String], var key: Long = 0L) {
    val hash: String = calHash
    def strHash: String = String.format("%064x",
        new java.math.BigInteger(1, java.security.MessageDigest.getInstance("SHA-256").digest(
            (infostring + key).getBytes("UTF-8")
        ))
    )
    def calHash: String = {
        var tmphash = strHash
        while (tmphash.take(4) != "0000") { // dificaulty 4
            key += 1
            tmphash = strHash
        }
        tmphash
    }
    def infostring: String = "k=" + key + "&i=" + index + "&t=" + time +
        "&p=" + prehash + "&d=" + data.mkString("\n")
    def stringify: String = "h=" + hash + "&" + infostring
}

class ScalaBlockChain(var chain: Seq[Block] = Seq(new Block(0, System.currentTimeMillis, "", Seq("START")))) {
    val numberOfBlockKeys = 5
    def validate(newchain: Seq[Block] = chain): Boolean = {
        var check = chain.head.data.mkString("\n") == "START"
        var prehash = ""
        var pretime = 0L
        var i = 0
        while(i < chain.size && check) {
            val block = chain(i)
            if (block.prehash == prehash && block.hash == block.calHash && block.time >= pretime) {
                prehash = block.hash
                pretime = block.time
                i += 1
            } else {
                check = false
            }
        }
        check
    }
    def stringify: String = chain.map(_.stringify).mkString("|")
    def parseChain(bcstr: String): Seq[Block] = bcstr.split("|").map(parseBlock(_))
    def parseBlock(bstr: String): Block = {
        val p = bstr.split("&")
        val keymap = p.take(numberOfBlockKeys).map{ s =>
            val kv = s.split("=")
            (kv(0), kv(1))
        }.toMap
        val data = p.drop(numberOfBlockKeys).mkString("").drop(2).split("\n").toSeq
        val block = new Block(keymap("i").toLong, keymap("t").toLong, keymap("p"), data, keymap("k").toLong)
        if (block.hash == keymap("h")) block
        else null: Block
    }
    def addBlock(newdata: Seq[String]): String = {
        val block = chain.last
        val newblock = new Block(block.index + 1, System.currentTimeMillis, block.hash, newdata)
        chain :+= newblock
        newblock.stringify
    }
    def checkChain(newchain: Seq[Block]): Boolean =
        chain.zip(newchain).forall { case (a, b) => a.hash == b.hash }
    def syncChain(newchain: Seq[Block]): Boolean = {
        if (validate(newchain)) {
            if (checkChain(newchain)) {
                if (newchain.size > chain.size) {
                    chain ++= newchain.takeRight(newchain.size - chain.size)
                    true
                } else true
            } else false
        } else false
    }
    def syncBlock(newblock: Block): Boolean = {
        if (newblock.prehash == chain.last.hash && newblock.hash == newblock.calHash) {
            chain :+= newblock
            true
        } else false
    }
}
