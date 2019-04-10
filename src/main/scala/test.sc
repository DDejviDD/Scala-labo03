type Word = String
type Sentence = List[Word]
type FingerPrint = String


import scala.collection.immutable.List
import scala.collection.immutable.Stream.Empty

val dictionary: List[Word] = List("ate", "eat", "tea", "pot", "top", "sonja", "jason", "normal", "I", "love", "you", "olive")

def fingerPrint(s: Word): FingerPrint = s.sorted
def fingerPrint(s: Sentence): FingerPrint = s.mkString("").toLowerCase().sorted

val matchingWords: Map[FingerPrint, List[Word]] = dictionary.groupBy(a=> fingerPrint(a)).withDefaultValue(List())
def wordAnagrams(word: Word): List[Word] = matchingWords(fingerPrint(word))

println(wordAnagrams("eta"))
println(wordAnagrams("jbdikb"))

def subseqs(fp: FingerPrint): List[FingerPrint] = fp.inits.flatMap(_.tails).withFilter(!_.isEmpty).toList

// Test code with for example:
println(subseqs("abbc"))
List("", "c", "b", "bc", "bb", "bbc", "a", "ac", "ab", "abc", "abb", "abbc").sorted
