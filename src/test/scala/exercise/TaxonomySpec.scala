package exercise

import org.scalatest.{Matchers, WordSpec}

import scala.annotation.tailrec

case class Translation(iso: String, value: String)

case class Tag(tag: String, translation: Seq[Translation])

case class Node(id: String, tag: Tag, children: Seq[Node] = Seq.empty[Node]) {

  def findById(idToFind: String): Option[Node] = {
    @tailrec
    def findChildren(c: Seq[Node], id: String): Option[Node] = {
      c match {
        case Nil => None
        case c1 => c1.head.findById(id) match {
          case Some(n) => Some(n)
          case None => findChildren(c1.tail, id)
        }
      }
    }

    if (id == idToFind) Some(this)
    else findChildren(children, idToFind)
  }
}

class TaxonomySpec extends WordSpec with Matchers {

  "findByID" should {
    "return the categories node when ID 'categories'" in new SetUp {
      categories.findById("categories") shouldBe Some(categories)
    }

    "return the shows node when ID 'shows'" in new SetUp {
      categories.findById("shows") shouldBe Some(shows)
    }

    "return the theatre node when ID 'theatre'" in new SetUp {
      categories.findById("theatre") shouldBe Some(theatre)
    }

    "return the films node when ID 'films'" in new SetUp {
      categories.findById("films") shouldBe Some(films)
    }

    "return the films node when ID 'jazz'" in new SetUp {
      categories.findById("jazz") shouldBe Some(jazz)
    }

    "return the films node when ID 'french'" in new SetUp {
      categories.findById("french") shouldBe Some(frenchRestaurant)
    }
  }

  trait SetUp {

    val chineseTag = Tag("chinese", Seq(Translation(iso = "en_GB", value = "Chinese")))
    val comedyTag = Tag("comedy", Seq(Translation(iso = "en_GB", value = "Comedy")))
    val actionTag = Tag("action", Seq(Translation(iso = "en_GB", value = "Action")))
    val filmTag = Tag("film", Seq(Translation(iso = "en_GB", value = "Film")))
    val theatreTag = Tag("theatre", Seq(Translation(iso = "en_GB", value = "Theatre")))
    val showTag = Tag("show", Seq(Translation(iso = "en_GB", value = "Show")))
    val jazzTag = Tag("jazz", Seq(Translation(iso = "en_GB", value = "Jazz")))
    val popTag = Tag("pop", Seq(Translation(iso = "en_GB", value = "Pop")))
    val rockTag = Tag("rock", Seq(Translation(iso = "en_GB", value = "Rock")))
    val musicTag = Tag("music", Seq(Translation(iso = "en_GB", value = "Music")))
    val frenchTag = Tag("french", Seq(Translation(iso = "en_GB", value = "French")))
    val italianTag = Tag("italian", Seq(Translation(iso = "en_GB", value = "Italian")))
    val restaurantTag = Tag("restaurant", Seq(Translation(iso = "en_GB", value = "Restaurant")))
    val categoryTag = Tag("category", Seq(Translation(iso = "en_GB", value = "Category")))

    val chinese = Node("chinese", chineseTag)
    val comedy = Node("comedy", comedyTag)
    val action = Node("action", actionTag)

    val films = Node("films", filmTag, Seq(chinese, comedy, action))
    val theatre = Node("theatre", theatreTag)

    val shows = Node("shows", showTag, Seq(theatre, films))

    val jazz = Node("jazz", jazzTag)
    val pop = Node("pop", popTag)
    val rock = Node("rock", rockTag)

    val music = Node("music", musicTag, Seq(jazz, pop, rock))

    val chineseRestaurant = Node("chinese", chineseTag)
    val frenchRestaurant = Node("french", frenchTag)
    val italianRestaurant = Node("italian", italianTag)

    val restaurants = Node("restaurants", restaurantTag, Seq(chineseRestaurant, frenchRestaurant, italianRestaurant))

    val categories = Node("categories", categoryTag, Seq(shows, music, restaurants))

  }

}
