object RomanNumeralConverter {

  val symbolList = List((1000,'M'),(500,'D'),(100,'C'),(50,'L'),(10,'X'),(5,'V'),(1,'I'))
  val symbolMap = Map('M' -> 1000, 'D' -> 500, 'C' -> 100, 'L' -> 50, 'X' -> 10, 'V' -> 5, 'I'->1)
  val repeatableList = List('I', 'X', 'C', 'M')
  val unrepeatableList = List('V', 'L', 'D')

  def arabicToLatin(input:Int):String = {
    a(input, symbolList, "")
  }

  /* First iteration
  private def a(value:Int,list:List[(Int,Char)], genString:String):String = {
    if(list.isEmpty) genString
    else if( value >= list.head._1) a(value - list.head._1, list, genString + list.head._2 )
    else a(value, list.tail, genString )
  }*/

  //4 is IV    ((5,V),(1,I))
 // 9 is IX   ( (10,X), (5,V), (1,I))

  private def a(value:Int,list:List[(Int,Char)], genString:String):String = {
    if(list.isEmpty) genString
      //perform a subtraction to avoid 3 in a row
    else if( list.length > 1 && repeatableList.contains(list.tail.head._2) && value < list.head._1 && value >= 4*list.tail.head._1)
      a(value - (list.head._1 - list.tail.head._1) , list, genString + list.tail.head._2 + list.head._2 )
      //unrepeatable symbols can never be subtracted so go down another level
    else if( list.length > 2 && repeatableList.contains(list.tail.tail.head._2) && value < list.head._1 && value >= (list.head._1 - list.tail.tail.head._1))
      a(value - (list.head._1 - list.tail.tail.head._1) , list, genString + list.tail.tail.head._2 + list.head._2 )
    else if( value >= list.head._1)
      a(value - list.head._1, list, genString + list.head._2 )
      //iterate
    else a(value, list.tail, genString )
  }

  def latinToArabic(input:String):Int = {
    b(input, 0)
  }

  private def b(inputString:String, genValue:Int):Int = {
    if(inputString.isEmpty) genValue
      //If this numeral smaller than next then we have a subtraction
    else if(inputString.length > 1 && (symbolMap.get(inputString.head).get < symbolMap.get(inputString.tail.head).get))
      b(inputString.tail.tail, genValue + symbolMap.get(inputString.tail.head).get - symbolMap.get(inputString.head).get)
    else b(inputString.tail, genValue + symbolMap.get(inputString.head).get)
  }
}