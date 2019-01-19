package prism1.block

import org.scalatest._
import examples.commons.{Nonce, SimpleBoxTransactionPrism, Value}
import examples.prism1.HybridNodeViewHolder.log
import examples.prism1.blocks.{PowBlock, PowBlockCompanion}
import examples.prism1.mining.{HybridMiningSettings, HybridSettings}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.core.settings.ScorexSettings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.PublicKey
import scorex.util.encode.Base58

class PowBlockTest extends PropSpec{

  val settingsFilename = "src/main/resources/settings.conf"
  val hybridSettings: HybridSettings = HybridSettings.read(Some(settingsFilename))
  val settings: ScorexSettings = hybridSettings.scorexSettings
  val minerSettings: HybridMiningSettings = hybridSettings.mining
  //  private val validator = new SemanticBlockValidator(Blake2b256)

  def generatePowGenesis: PowBlock = {
    val GenesisAccountsNum = 50
    val GenesisBalance = Value @@ 100000000L

    //propositions with wallet seeds minerNode1 (20accs), minerNode2 (20 accs), minerNode3 (10 accs)
    val icoMembers: IndexedSeq[PublicKey25519Proposition] = IndexedSeq(
      "Gh5ipRV2fzCn5176CPHnW4EVk9MR2HU6m91ZhoPxUwHN",
      "5r37KJVi3DSKsh5atfhdN6CbpvEh6mKwEZvzuCWjtcf1",
      "71bFWP8JFmCiGS9m9b6GZfwmtgFUb1WDHmfk5mb63YEf",
      "7r3XqE1ZvTHzmd6teP3kUBm1KvAi2Kwkfj69VSo7VpPW",
      "Bf4GE6HBLbsHzGex93uYb1nN52HeBKfVC84ZxDraH3ZB",
      "B9sUJG3fUwRf33VaTEae2KxVqLHt7Ft1W69mFd9G5ZTb",
      "8ZSn9YP1rAgy5zRKzXbds8QHPggKEY9zWW7ZWjTvVQYf",
      "DHjxreyiz7ZLW4CH6XDVma4dWidwqRM3coacs6d6vXD4",
      "AmvdYNLLA4ZVj7Wj1FKKi1E9Eve7qnb6peXB8dQThQEz",
      "FBa8ZrF5CBVTZfj1enFvjXUTySJE6cCnrhx27Rct2aFH",
      "5CQtS7mWcNUrbW9TFVafmgP9C2bvUHcFAx6T9f791fVB",
      "BYiAvhAs2ZC7YCuzjez52tfHRVBep6ZmZToUMZCtrV45",
      "Bqd2xjzfaztCv2hLoaedxJ73yWQpbVqRgQobs2BGx3q4",
      "HvqRZ2TANTqFZJ7Qrpg2r6u14f1J7ZULeoM9cCRFq4QZ",
      "2oGZpxT1SceyZSVfs4R2hNYZopNL3LpVbPQ9seQbQpLo",
      "4u3xxr6tNBcY9NSC918xkikYrYC9RxyuTzksaQsbsXkK",
      "6uetbFeCJ4nhe9r1bbMN3D7sdBPJtafqacrLTJ21nfcK",
      "saLQifmdmE7urULqeJht8uWou7xh8qkapmmu3HM3SaT",
      "DqkHG29Rm5YSCahuR1VxytPFJFBqUhQKhAq7kokLakDc",
      "D9KQHUj4zkMJBYqfjoWbkMU2SPiuH6UA16Tq8Ns1zHwT",
      "GQz8mafKfC8Scb11ppCagiPGAHVSzDd3DQhZgsrzHKq8",
      "GBa7NdFDQYjkEsjn4xJvgYBZdwrN6Ds6FHMzcMhgAqFw",
      "Eozk3S7aTZStqAEmN8pLYAcSNpgNtUBBHykeNPqcKbwE",
      "26AZ94vmuVMiruQbxpaArtooeByf4mg7YERm7ASPLtzX",
      "4FLYR7RY2VPwxrk11naQeE2kuHe2sm6gttxFYzMAsipU",
      "B3HzLmPcDribF2csqSvdteTVcQsNkmxCKNFR3xLZ3Rqu",
      "2YE8p31Fr7KfgQTSWdCWK7C1wk4Y3Yb3gzvecHfjGQCS",
      "6haBGvcBz8ZrBza5BBWAGtVghKoSDunp1JXyFjhRL9Pg",
      "8Na86fSM2Cv5LvoezW5aL8h2qaP76Cs7EXVRjPZvY2dG",
      "5TSGDgKxXQmBL7K1UjXJJijA9uEZWYku7hQZjA4YchmJ",
      "6KDfFLDnSxTBQ58NvBWqeXLTTJtbALrw2uNDW2haGkTs",
      "G8vHzNUhbs8LH12p27dexZnXhYXcHa2F5rybLDyRC59y",
      "BjwrFU2FyBBB7x2vn3d5r3P9THG7kJi37A1VcJZj9ngy",
      "BXs7geU54HqMCicgzsuWNeF2CgD7DfQWg2KyJSuu35cj",
      "8r11HX4Ap8V9JsUVD7fivyzLzZ14DG9fSHhXDb2pgoeo",
      "FKroztkLwNbqibtwP6g5GYECuVRoVShT2GyuaATYYWeZ",
      "FUsLAekPGpPPQvvksV1VphYzPJgaEsbwEiBxEh4U9T6p",
      "7FkG9kkU66XQtPJuzgyAEB4Lcw4b78PZHfXwNbFgvohA",
      "ASpaQgkEP49UHUR8hAMrruiG4HpGo6WybvJ88njD5L7B",
      "FRRXWdY6br8kcTWk4VWnwiL7kAsgNvAbRyoXxdAkFqZt",
      "5YgmHSQ9AZpniZ9DMfTkZSfM3A1BJsXKqCAtCSr3Ybkq",
      "7vV4aqkg1YY5VnM34oJ7BRMXZyvULGPva6Tesmng9XvH",
      "45VGbsfFeiXkW2uoC7tDRVUSHjnYhtpfrYN57wTANHsn",
      "8QwTmye6VsHx3fkAvmJqvSgHPjdPCaT3wakEfpujsWM5",
      "6nUtKXw7WFgV2tRuFyYwBrg4kBMYzADekPqLTwnUccxV",
      "3Kw5jix8XMWj2SHsxt7c1w9iiK3s6qc4NMyY6bDUXvTg",
      "EVjrmbKvTkVk7JRzDEaHBL2tpcdAtHgyNhDyPXGcAXLv",
      "GXkCiK2P7khngAtfhG8TSqm4nfPbpMDNFBiG8CF41ZtP",
      "8etCeR343fg5gktxMh5j64zofFvWuyNTwmHAzWbsptoC",
      "AnwYrjV3yb9NuYWz31C758TZGTUCLD7zZdSYubbewygt"
    ).map(s => PublicKey25519Proposition(PublicKey @@ Base58.decode(s).get))
      .ensuring(_.length == GenesisAccountsNum)

    val genesisAccount = PrivateKey25519Companion.generateKeys("genesis".getBytes)
    val genesisAccountPriv = genesisAccount._1
    val genesisTxs = Seq(SimpleBoxTransactionPrism(
      IndexedSeq(genesisAccountPriv -> Nonce @@ 0L),
      icoMembers.map(_ -> GenesisBalance),
      0L,
      0L))
    /**
     * TODO: change txsHash
     * val txsHash = Blake2b256(PowBlockCompanion.txBytes(genesisTxs))
     */
    val txsHash = Array.fill(32)(0: Byte)

    val powGenesis = PowBlock(minerSettings.GenesisParentId,  1481110008516L, 2, genesisAccount._2, genesisTxs, txsHash)
    powGenesis
  }



  ignore("Genesis Block generateKeys should be identical") {
    val k1 = PrivateKey25519Companion.generateKeys("genesis".getBytes)
    val k2 = PrivateKey25519Companion.generateKeys("genesis".getBytes)
    assert(k1 == k2)
  }

  ignore("Genesis Block txsByte should be identical") {
    val gb = generatePowGenesis
    val blocks = Seq(generatePowGenesis, generatePowGenesis, generatePowGenesis)
    blocks.foreach { b: PowBlock =>
      assert(b.txBytes == gb.txBytes)
    }
  }

  property("Genesis Block id should be identical") {
    val gb = generatePowGenesis
    val blocks = Seq(generatePowGenesis, generatePowGenesis, generatePowGenesis)
    blocks.foreach { b: PowBlock =>
      assert(b.id == gb.id)
    }
  }
}
