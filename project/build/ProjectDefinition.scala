import sbt._

class ProjectDefinition(info: ProjectInfo) extends DefaultProject(info) { 
    
  //Specs testing framework -- use the 2.8.0.RC1 Snapshot
  val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.6"
}