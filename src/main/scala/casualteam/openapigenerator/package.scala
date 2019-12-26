package casualteam

package object openapigenerator {
  //Left = computed, Right = provided by api
  type EntityName = Either[List[String], String]
}
