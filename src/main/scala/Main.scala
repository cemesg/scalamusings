object Main extends App {

  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] =
    {
      network + (person -> network.getOrElse(person, Set()))
    }

  def addFriend(network: Map[String, Set[String]], person1: String, person2: String): Map[String, Set[String]] = {
    val updatedNetwork = for {
      friends1 <- network.get(person1)
      friends2 <- network.get(person2)
    } yield {
      network +
        (person1 -> (friends1 + person2)) +
        (person2 -> (friends2 + person1))
    }

    updatedNetwork.getOrElse({
      println("Error: Both persons should exist in the network.")
      network // Return the network unchanged
    })
  }

  def removeFriend(network: Map[String, Set[String]], person1: String, person2: String): Map[String, Set[String]] = {
    val updatedNetwork = for {
      friends1 <- network.get(person1)
      friends2 <- network.get(person2)
    } yield {
      network +
        (person1 -> (friends1 - person2)) +
        (person2 -> (friends2 - person1))
    }

    updatedNetwork.getOrElse({
      println("Error: Both persons should exist in the network.")
      network // Return the network unchanged
    })
  }

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    val connections = network.getOrElse(person, Set.empty[String])
    network ++ connections.map(entry => entry -> (network.getOrElse(entry, Set.empty[String]) - person))
  }


  val network: Map[String, Set[String]] = Map()
  val updatedNetwork = add(network, "Cem")
  println(updatedNetwork)
  var net = addFriend(updatedNetwork, "Cem", "Orhun")
  println(net)
  net = add(net, "Orhun")
  println(net)
  net = addFriend(net, "Cem", "Orhun")
  println(net)
  net = removeFriend(net, "Cem", "Orhun")
  println(net)
  net = addFriend(net, "Cem", "Orhun")
  println(net)
  net = add(net, "Cem")
  println(net)
  net = remove(net,"Orhun")
  println(net)
  net = remove(net,"hüsamettin")
  println(net)
  net = removeFriend(net, "Cem", "Orhun")
  println(net)




}
