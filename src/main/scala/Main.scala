import scala.annotation.tailrec

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

  def mostFriends(network: Map[String, Set[String]]): String =
    network.maxBy(entry => entry._2.size)._1

  def isConnected(network: Map[String, Set[String]], person1: String, person2: String): Boolean =
    {
    @tailrec
      def bfs(target : String, visited: Set[String], next: Set[String] ) : Boolean = {
      if(next.isEmpty) false
      else {
        val currentPerson = next.head
        if (currentPerson == target) true
        else if (visited.contains(currentPerson)) bfs(target, visited, next.tail)
        else bfs (target, visited + currentPerson, next.tail ++ network(currentPerson))
      }
    }
      bfs(person2, Set(), network(person1))
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
  net = add(net, "Some Dude")
  net = add(net, "Some Gal")
  net = addFriend(net, "Cem", "Some Gal")
  net = addFriend(net, "Cem", "Some Dude")
  println(net)
  println(mostFriends(net))
  println(isConnected(net, "Cem", "Orhun"))
  net = addFriend(net, "Orhun", "Some Gal")
  println(isConnected(net, "Cem","Orhun"))



}
