package objsets

/**
  * A class to represent tweets.
  */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

/**
  * This represents a set of objects of type `Tweet` in the form of a binary search
  * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
  * invariant which always holds: for every branch `b`, all elements in the left
  * subtree are smaller than the tweet at `b`. The elements in the right subtree are
  * larger.
  *
  * Note that the above structure requires us to be able to compare two tweets (we
  * need to be able to say which of two tweets is larger, or if they are equal). In
  * this implementation, the equality / order of tweets is based on the tweet's text
  * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
  * text from different users.
  *
  *
  * The advantage of representing sets as binary search trees is that the elements
  * of the set can be found quickly. If you want to learn more you can take a look
  * at the Wikipedia page [1], but this is not necessary in order to solve this
  * assignment.
  *
  * [1] http://en.wikipedia.org/wiki/Binary_search_tree
  */
abstract class TweetSet {

  /**
    * This method takes a predicate and returns a subset of all the elements
    * in the original set for which the predicate is true.
    *
    * Question: Can we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    * Yes - concrete method here
    */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty) //add helper method for `filter` params: (predicate, new Empty tweet)

  //abstract methods

  /**
    * This is a helper method for `filter` that propagetes the accumulated tweets.
    */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
    * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def union(that: TweetSet): TweetSet //We should implement in subclass - no implementation here

  /**
    * Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of
    * type `java.util.NoSuchElementException`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses? - - no implementation here
    */
  def mostRetweeted: Tweet ////We should implement in subclass - no implementation here

  /**
    * Returns a list containing all tweets of this set, sorted by retweet count
    * in descending order. In other words, the head of the resulting list should
    * have the highest retweet count.
    *
    * Hint: the method `remove` on TweetSet will be very useful.
    * Question: Should we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def descendingByRetweet: TweetList ////We should implement in subclass - no implementation here

  /**
    * The following methods are already implemented
    */

  /**
    * Returns a new `TweetSet` which contains all elements of this set, and the
    * the new element `tweet` in case it does not already exist in this set.
    *
    * If `this.contains(tweet)`, the current set is returned.
    */
  def incl(tweet: Tweet): TweetSet

  /**
    * Returns a new `TweetSet` which excludes `tweet`.
    */
  def remove(tweet: Tweet): TweetSet

  /**
    * Tests if `tweet` exists in this `TweetSet`.
    */
  def contains(tweet: Tweet): Boolean

  /**
    * This method takes a function and applies it to every element in the set.
    */
  def foreach(f: Tweet => Unit): Unit
}


class Empty extends TweetSet {
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc //body is accumulator whitch matches p , return type must be a TweetSet

  /**
    * The following methods are already implemented
    */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  // override abstract methods in concrete class

  /**
    * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    * We should implement in this subclass
    */
  override def union(that: TweetSet): TweetSet = that //in case of empty set union return only that

  /**
    * Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of
    * type `java.util.NoSuchElementException`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    * We should implement in this subclass
    */
  override def mostRetweeted: Tweet = throw new java.util.NoSuchElementException //in case of empty set, throw Exception

  /**
    * Returns a list containing all tweets of this set, sorted by retweet count
    * in descending order. In other words, the head of the resulting list should
    * have the highest retweet count.
    *
    * Hint: the method `remove` on TweetSet will be very useful.
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    * We should implement in this subclass
    */
  override def descendingByRetweet: TweetList = Nil //empty list is Nil
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    //set of left TweetSet which matches acc
    val leftTwwetSet = left.filterAcc(p, acc)
    //set of right TweetSet which matches acc
    val rightTweetSet = right.filterAcc(p, leftTwwetSet)

    if (p(elem))
      rightTweetSet.incl(elem) // or rightAccumulator incl elem
    else
      rightTweetSet
  }

  /**
    * The following methods are already implemented
    */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  // override abstract methods in concrete class

  /**
    * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    * We should implement in subclass and returns that set of tweets
    */
  override def union(that: TweetSet): TweetSet = {
    left.union(right.union(that.incl(elem))) //left union with (right union with (that include element))
  }

  /**
    * Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of
    * type `java.util.NoSuchElementException`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    * We should implement in subclass and throw Exception for Empty
    */

  override def mostRetweeted: Tweet = {
    //return maxim from tweet and maxim from tweetSet
    def maximTweet(tweet: Tweet, tweetSet: TweetSet): Tweet = {
      if (tweetSet.isInstanceOf[Empty]) tweet
       else {
        val maxim = tweetSet.mostRetweeted
        if (tweet.retweets > maxim.retweets) tweet else maxim
      }
    }
    maximTweet(maximTweet(elem, left), right)
  }

  /**
    * Returns a list containing all tweets of this set, sorted by retweet count
    * in descending order. In other words, the head of the resulting list should
    * have the highest retweet count.
    *
    * Hint: the method `remove` on TweetSet will be very useful.
    * Question: Should we implement this method here, or should it remain abstract
    * and be implemented in the subclasses?
    * We should implement in subclass
    */
  override def descendingByRetweet: TweetList = {
    val retweetCount = mostRetweeted
    new Cons(retweetCount, remove(mostRetweeted).descendingByRetweet)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean

  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  //tweetset with fillteracc on exists google keyword
  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(tweet => google.exists(keyword => tweet.text.contains(keyword)))
  //tweetset with fillteracc on exists apple keyword
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(tweet => apple.exists(keyword => tweet.text.contains(keyword)))

  /**
    * A list of all tweets mentioning a keyword from either apple or google,
    * sorted by the number of retweets.
    */

  //union this whith that
  lazy val trending: TweetList = (googleTweets union appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
