package edu.sb.bmi.morphex

import edu.pitt.dbmi.nlp.noble.coder.NobleCoder

import scala.io.Source
import scala.collection.JavaConverters._


/**
 * Created by niranjan on 12/8/15.
 */
object NobleCoderWrapper {

  def main(args: Array[String]) {
    val testFile = args(0) //File containing some text that needs to be annotated via NobleCoder.
    val text = Source.fromFile(testFile).getLines().mkString("\n")
    val coder = new NobleCoder("NCI_Thesaurus")
    val mentions = coder.process(text).asScala
    mentions.foreach(mention => {
      System.out.println(mention.getConcept + "\t" + mention.getText + "\t" + mention.getStartPosition + "\t" + mention.getEndPosition)
    })
  }
}
