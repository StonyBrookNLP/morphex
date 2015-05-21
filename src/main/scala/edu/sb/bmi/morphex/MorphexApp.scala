package edu.sb.bmi.morphex

import java.io.{PrintWriter, File}

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.io.Source

/**
 * Created by niranjan on 5/6/15.
 */

case class Span(start: Int, end: Int)
case class Token(text: String, span: Span)
case class AnnotatedToken(token: Token, annotation: String)
class Morphex(nameReferences: Map[String, String],
               valueReferences: Map[String, String]) {

  def startEndPosition(token: String, content: String, startIndex:Int = 0): Span = {
    val start:Int = content.indexOf(token)
    val end = start + token.size
    val prevStart = if(start - 1 > 0) start-1 else start
    val nextEnd = if(end+1 < content.length && end+1 >= 0) end + 1 else end
    if(start > 0 && end < content.length) {
      val expandedToken = content.substring(prevStart, nextEnd)
      val pregex = s"""[a-zA-Z]${token}""".r
      val fregex = s"""${token}[a-zA-Z]""".r
      if (pregex.findFirstMatchIn(expandedToken) != None || fregex.findFirstMatchIn(expandedToken) != None)
        new Span(-1, end)
      else
        new Span(start, end)
    } else {
      new Span(-1, -1)
    }
  }

  def export(content: String, featureNameAnnotations: Seq[AnnotatedToken],
             featureValueAnnotations: Seq[AnnotatedToken],
             file: String) = {
    val writer = new PrintWriter(file)
    var tokenid = 1
    var valueid = 1
    var tokenidValueidMap = new HashMap[String, String]
    var tokenidMap = new HashMap[AnnotatedToken, String]
    var valueidMap = new HashMap[AnnotatedToken, String]
    def export(content: String, annotatedTokens: Seq[AnnotatedToken], value: Boolean) = {
      annotatedTokens.foreach {
        annotatedToken =>
          //T1	DiagnosisValue 306 328	fibrillary astrocytoma
          //E1	DiagnosisValue:T1
          //T2	Nuclear_Pleomorphism 354 365	pleomorphic
          //T3	Moderate 343 353	moderately
          //E2	Moderate:T3
          val tokenidString = s"T${tokenid}"
          tokenidMap += (annotatedToken -> tokenidString)
          writer.println(s"${tokenidString}\t${annotatedToken.annotation} ${annotatedToken.token.span.start} ${annotatedToken.token.span.end}\t${annotatedToken.token.text}")
          if(value) {
            val valueidString = s"E${valueid}"
            valueidMap += (annotatedToken -> valueidString)
            writer.println(s"${valueidString}\t${annotatedToken.annotation}:T${tokenid}")
            tokenidValueidMap += (tokenidString -> valueidString)
            valueid = valueid + 1
          }
          tokenid = tokenid + 1
      }
    }
    export(content, featureNameAnnotations, value = false)
    export(content, featureValueAnnotations, value = true)

    def link(nameTokens: Seq[AnnotatedToken], valueTokens: Seq[AnnotatedToken]) = {
      import math._

      def distance(x: Span, y: Span) = min(abs(x.start - y.start), abs(x.end - y.end))
      def nearestValueTokenWithinWindow(name: AnnotatedToken, window: Int = 50) = {
        valueTokens.map( value => (distance(name.token.span, value.token.span), value)).minBy(_._1) match {
          case (x:Int, y:AnnotatedToken) if x <= window  => Some(y)
          case _ => None
        }
      }
      for {
        nameToken <- nameTokens
        valueToken <- nearestValueTokenWithinWindow(nameToken)
      } yield {
        (nameToken, valueToken)
      }
    }
    val linkedTokens: Seq[(AnnotatedToken, AnnotatedToken)] = if(!featureValueAnnotations.isEmpty) {
      link(featureNameAnnotations, featureValueAnnotations)
    } else {
      Seq()
    }

    /**
     * T1	Mitotic_figure 1461 1476	mitotic figures
       T2	Mild 1457 1460	two
       E1	Mild:T2
       R1	Value Arg1:T1 Arg2:E1

     */
    var relationid = 1
    linkedTokens.foreach {
      nameValueTokens =>
        val (name:AnnotatedToken, value: AnnotatedToken) = nameValueTokens
        (tokenidMap.get(name), valueidMap.get(value)) match {
          case (Some(tokenid), Some(valueid)) =>
            writer.println(s"R${relationid}\tValue Arg1:${tokenid} Arg2:${valueid}")
          case _ =>
        }
        relationid = relationid + 1
    }

    writer.close()
  }

  def annotate(content: String, outputFile: String) = {
    val texts = content.split(" ").toSet
    val (featureNameTokens, featureValueTokens) = annotatedTokens(content, texts)
    export(content, featureNameTokens, featureValueTokens, outputFile)
  }

  def annotatedTokens(content: String, texts: Set[String]): (Seq[AnnotatedToken],  Seq[AnnotatedToken]) = {

    def subsumes(x: Span, y: Span) = x.start <= y.start && x.end >= y.end

    def filterSubsumedSpans(tokens: Seq[AnnotatedToken]) = {
      tokens.sortBy(atoken => atoken.token.span.end - atoken.token.span.start)

    }

    def apply(dictionary: Map[String, String]) = {
      val possibleMatches = dictionary.keySet.filter { reference =>
        reference.split(" ").toSet exists texts
      }
      println(s"Possible matches ${possibleMatches}")
      val tokens = for {
        text <- possibleMatches
        annotation <- dictionary.get(text)
        span = startEndPosition(text, content)
        if span.start >=0 && span.end < content.size
      } yield {
        new AnnotatedToken(new Token(text, span), annotation)
      }
      tokens.toSeq.sortBy { _.token.span.start }
    }
    (apply(nameReferences), apply(valueReferences))
  }

}

object MorphexApp extends App {

  val namesFile = args(0)
  val valuesFile = args(1)
  val textFilesDir = new File(args(2))
  val threshold = args(3)
  def recursiveTextFiles(file: File): Array[File] = {
    val these = file.listFiles
    these.filter { _.getName.endsWith("txt") } ++ these.filter(_.isDirectory).flatMap { recursiveTextFiles(_) }
  }
  val files: Array[File] = recursiveTextFiles(textFilesDir)
  //println(s"Recursive text files ${files.mkString("\n")}")
  println(s"# of txt files = ${files.size}")
  val numRegex = s"""([${threshold}-9][0-9]*)""".r
  println(s"Num Regex: " + numRegex.toString())
  def toReferenceMap(file: String) = Source.fromFile(file).getLines().flatMap {
    line =>
      val splits = line.split("\t")
      splits.tail.filter{ numRegex.findFirstMatchIn(_) != None} .map {
        text =>

          val reference = text.replaceAll("""\([0-9]+\)""", "").trim
          (reference -> splits(0))
      }

  }.toMap

  def annotateFile(file: File) = {
    val text = Source.fromFile(file).mkString
    val outputFile = file.getAbsolutePath.replaceAll("txt", "ann")
    println(s"Output file: ${outputFile}")
    morphex.annotate(text, outputFile)
    //println(s"Annotations: ${annotations.mkString("\n")}")
  }
  val namesMap = toReferenceMap(namesFile)
  val valuesMap = toReferenceMap(valuesFile)
  println("Name references = %d".format(namesMap.keys.size))
  println("Value references = %d".format(valuesMap.keys.size))
  val morphex = new Morphex(namesMap, valuesMap)
  files.foreach { annotateFile(_) }
}




class ReferenceAggregator {

  def main(args: Array[String]) {
    val annFile = args(1)
    val txtFile = args(2)

    val annContents = Source.fromFile(annFile).getLines().toSeq

    val txtContents = Source.fromFile(txtFile).mkString

    def getText(content: String, start: Int, end: Int): Option[String] = {
      def validIndex(index: Int) = index >= 0 && index < content.size
      if (validIndex(start) && validIndex(end))
        Some(content.substring(start, end))
      else
        None
    }

    val annotationRe = """(.*?) ([0-9]+) ([0-9]+)""".r
    def getStartAndEnd(annotation: String) = annotation match {
      case annotationRe(name: String, s: String, t: String) =>
        Some((name, s.toInt, t.toInt))
      case _ => None
    }
    val nameValueMap = new mutable.HashMap[String, Set[String]]()
    annContents.foreach { line =>
      val splits = line.split("\t")
      val annotation = splits(1)
      getStartAndEnd(annotation) match {
        case Some((name: String, start: Int, end: Int)) =>
          getText(txtContents, start, end) match {
            case Some(textReference) =>
              println( """Feature Name: %s Feature Text = %s start = %d end = %d""".format(name, textReference, start, end))
              val references: Set[String] = nameValueMap.getOrElse(name, Set[String]()) + textReference
              nameValueMap += (name -> references)
            case None =>
              println("Ignoring annotation: %s".format(annotation))
          }

        case _ => println("Ignoring annotation: %s".format(annotation))
      }
    }

    nameValueMap.keySet.foreach {
      key =>
        println("%s => %s".format(key, nameValueMap.get(key)))
    }
  }
}