package exercise

import org.scalatest.{Matchers, WordSpec}

class TaxonomySpec extends WordSpec with Matchers {

  "findByID" should {
    "return the some categories tree when ID 'categories'" in new SetUp {
      categories.findById("categories") shouldBe Some(categories)
    }

    "return the some theatre tree when ID 'theatre'" in new SetUp {
      categories.findById("theatre") shouldBe Some(theatre)
    }

    "return the some films tree when ID 'films'" in new SetUp {
      categories.findById("films") shouldBe Some(films)
    }

    "return the some jazz tree when ID 'jazz'" in new SetUp {
      categories.findById("jazz") shouldBe Some(jazz)
    }

    "return the some french tree when ID 'french'" in new SetUp {
      categories.findById("french") shouldBe Some(frenchRestaurant)
    }

    "return the None when ID 'notExist'" in new SetUp {
      categories.findById("notExist") shouldBe None
    }
  }


  "findByTag" should {
    "return the chinese film tree and chinese restaurant tree when Tag is 'chinese'" in new SetUp {
      categories.findByTag("chinese") shouldBe Stream(chinese, chineseRestaurant)
    }

    "return the restaurant tree when Tag is 'restaurant'" in new SetUp {
      categories.findByTag("restaurant") shouldBe Stream(restaurants)
    }

    "return the action tree when Tag is 'action'" in new SetUp {
      categories.findByTag("action") shouldBe Stream(action)
    }

    "return no tree when Tag is 'notExist'" in new SetUp {
      categories.findByTag("notExist") shouldBe Stream.empty[Tree]
    }
  }

  "findDescendants" should {
    "return the descendants of the tree category" in new SetUp {
      categories.findDescendants shouldBe Stream(
        shows, music, restaurants,
        theatre, films,
        chinese, comedy, action,
        jazz, pop, rock,
        chineseRestaurant, frenchRestaurant, italianRestaurant)
    }

    "return the descendants of the tree music" in new SetUp {
      music.findDescendants shouldBe Stream(jazz, pop, rock)
    }
  }

  "cvsFormat" should {
    "return the tree in a csv format" in new SetUp {
      categories.cvsFormat shouldBe csv
    }
  }

  "csv" should {
    "be converted in a tree" in new SetUp {
      Tree(csv) shouldBe categories
    }

    "be raise an CVSFormatException when the cvs is not well formed" in new SetUp {
      val exception = intercept[CVSFormatException] {
        Tree("""bad format csv""")
      }
      exception.getMessage shouldBe "The format is not correct"
    }

    "be raise an CVSFormatException when the cvs does not contains the records in the right order" in new SetUp {
      val exception = intercept[CVSFormatException] {
        Tree(csvWithBadOrder)
      }
      exception.getMessage shouldBe "Node 'jazz' does not exist"
    }
  }

  trait SetUp {

    import Tree._

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

    val chinese = leaf("chinese", chineseTag)
    val comedy = leaf("comedy", comedyTag)
    val action = leaf("action", actionTag)

    val films = Tree("films", filmTag, Stream(chinese, comedy, action))
    val theatre = leaf("theatre", theatreTag)

    val shows = Tree("shows", showTag, Stream(theatre, films))

    val jazz = leaf("jazz", jazzTag)
    val pop = leaf("pop", popTag)
    val rock = leaf("rock", rockTag)

    val music = Tree("music", musicTag, Stream(jazz, pop, rock))

    val chineseRestaurant = leaf("chinese", chineseTag)
    val frenchRestaurant = leaf("french", frenchTag)
    val italianRestaurant = leaf("italian", italianTag)

    val restaurants = Tree("restaurants", restaurantTag, Stream(chineseRestaurant, frenchRestaurant, italianRestaurant))

    val categories = Tree("categories", categoryTag, Stream(shows, music, restaurants))

    val csv =
      """categories,category,en_GB,Category,"shows,music,restaurants"
        |shows,show,en_GB,Show,"theatre,films"
        |music,music,en_GB,Music,"jazz,pop,rock"
        |restaurants,restaurant,en_GB,Restaurant,"chinese,french,italian"
        |theatre,theatre,en_GB,Theatre,""
        |films,film,en_GB,Film,"chinese,comedy,action"
        |chinese,chinese,en_GB,Chinese,""
        |comedy,comedy,en_GB,Comedy,""
        |action,action,en_GB,Action,""
        |jazz,jazz,en_GB,Jazz,""
        |pop,pop,en_GB,Pop,""
        |rock,rock,en_GB,Rock,""
        |chinese,chinese,en_GB,Chinese,""
        |french,french,en_GB,French,""
        |italian,italian,en_GB,Italian,""""".stripMargin


    val csvWithBadOrder =
      """categories,category,en_GB,Category,"shows,music"
        |shows,show,en_GB,Show,"theatre,films"
        |jazz,jazz,en_GB,Jazz,""
        |music,music,en_GB,Music,"jazz,pop,rock"
        |theatre,theatre,en_GB,Theatre,""
        |films,film,en_GB,Film,"chinese,comedy,action"
        |pop,pop,en_GB,Pop,""
        |rock,rock,en_GB,Rock,""
        |chinese,chinese,en_GB,Chinese,""
        |comedy,comedy,en_GB,Comedy,""
        |action,action,en_GB,Action,""""".stripMargin
  }
}
