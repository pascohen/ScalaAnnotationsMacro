import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object dslMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._

    // shortcut 
    val myAnnot = annottees(0)

    def transformTree: PartialFunction[Tree, Tree] = {
      case Apply(Select(x, TermName("assign_to")), List(Ident(TermName(name)))) =>
        println("Got " + name + " for " + x); 
        ValDef(Modifiers(), TermName(name), TypeTree(), x)
    }

    def handle(item: Tree): Tree = {
      if (transformTree.isDefinedAt(item)) {
        transformTree(item)
      } else {
        item match {
          
          /*
          LabelDef(a, b, If(
              Apply(), 
              Block(List(), 
                  Apply()), Literal(Constant(()))))

          */
          
          case LabelDef(a, b, c) => LabelDef(a, b, handle(c))
           
          case If(c,Block(t),d) => t match {
            case t: Tuple2[_, _] => If(c,Block(handleList(t._1), handle(t._2)),d)
            case _ => handle(item)
          }

          case Block(t) => t match {
            case t: Tuple2[_, _] => Block(handleList(t._1), handle(t._2))
            case _ => handle(item)
          }
          case Function(a, Block(t)) => t match {
            case t: Tuple2[_, _] => Function(a, Block(handleList(t._1), handle(t._2)))
            case _ => handle(item)
          }
          
          case Apply(a, list) => Apply(a, handleList(list))
          case _ =>
            println("Handling " + showRaw(item))
            item
        }
      }
    }

    def handleList(item: List[Tree]): List[Tree] = {
      println("Handling List Tree" + item)
      item.map(it => handle(it))
    }

    val result = myAnnot.tree match {
      case DefDef(a, b, c, d, e, Block(t)) => {
        t match {
          case t: Tuple2[_, _] => DefDef(a, b, c, d, e, Block(handleList(t._1), handle(t._2)))
          case _ => handle(myAnnot.tree)
        }
      }
      case _ => handle(myAnnot.tree)
    }
    c.Expr[Any](result)
  }
}

class dsl extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro dslMacro.impl
}
