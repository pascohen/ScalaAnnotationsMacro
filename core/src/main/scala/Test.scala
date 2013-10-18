class Turtle(val name:String) {
  def ask(s:String) = name+" answers to "+s  
}

object Test extends App {
  println("hello")
  
  val I = new Turtle("jj")
  @dsl 
  def foo = {
     println("ask")
     I ask "why" assign_to(name)
     println(name)
     
     1 to 10 map { v =>
         I ask ("why "+v) assign_to(name)
         println(name)
       }
     
     var i = 0
     while(i<3) {
         I ask ("why "+i) assign_to(name)
         println(name)
         i = i+1
     }
     
     var j = 0
     do {
         I ask ("why "+j*10) assign_to(name)
         println(name)
         j = j+1
     }while(j<3)
  }
  
  foo
  
  println("end")
}
