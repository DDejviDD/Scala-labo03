type Word = String
type Sentence = List[Word]
type FingerPrint = String


import scala.collection.immutable.List

val dictionary: List[Word] = List("ate", "eat", "tea", "pot", "top", "sonja", "jason", "normal", "I", "love", "you", "olive")

def fingerPrint(s: Word): FingerPrint = s.sorted
def fingerPrint(s: Sentence): FingerPrint = s.mkString("").toLowerCase().sorted

val matchingWords: Map[FingerPrint, List[Word]] = dictionary.groupBy(a=> fingerPrint(a)).withDefaultValue(List())
def wordAnagrams(word: Word): List[Word] = matchingWords(fingerPrint(word))

println(wordAnagrams("eta"))
println(wordAnagrams("jbdikb"))

def subseqs(fp: FingerPrint): List[FingerPrint] = (for { i <- 0 to fp.length; c <- fp.combinations(i) } yield c).toList.distinct


// Test code with for example:
println(subseqs("abbc"))
List("", "c", "b", "bc", "bb", "bbc", "a", "ac", "ab", "abc", "abb", "abbc")

def subtract(x: FingerPrint, y: FingerPrint): FingerPrint = x diff y
println(subtract("aabbcc", "abc"))
println(subtract("aabbcc", "ac"))


def sentenceAnagrams(sentence: Sentence): List[Sentence] = sentence match {
  case  Nil => List()
  case _ =>
    def toto(fingerPrint: FingerPrint):List[Sentence] = {
      for {
        fps <- subseqs(fingerPrint())
        anagramList <- wordAnagrams(fps)
      } yield anagramList
    }
}

println(sentenceAnagrams(List("eat", "tea")))
println(sentenceAnagrams(List("you", "olive")))
println(sentenceAnagrams(List("I", "love", "you")))
val sentence = List("I", "love", "you")

fingerPrint(List("I", "love", "you"))
fingerPrint(List("you", "love", "olive"))