package parscala
package controlflow

trait NonLocal[E,X] {
  def entryLabel(implicit evidence : E =:= C) : BLabel
  def successors(implicit evidence : X =:= C) : List[(BLabel, EdgeLabel.TagType)]
  def sLabels : List[SLabel]
}
