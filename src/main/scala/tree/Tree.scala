package tree

import scala.math.{pow, abs, BigDecimal}


private enum Branch(val condition: Boolean): 
    case Left extends Branch(true)
    case Right extends Branch(false)
      
        
class Tree(val X: Map[String, Seq[Boolean]], val Y: Seq[Double], val name: String = "X"):

    private var _splitFeature: Option[String] = None 
    private var _left: Option[Tree] = None 
    private var _right: Option[Tree] = None      

    /** 
     * @return all features considered 
     */
    def features: List[String] = X.keys.toList

    /**
     * @return name of the feature used in the partition of the root node
     */
    def splitFeature: Option[String] = _splitFeature

    /**
     * @return left subtree 
     */
    def left: Option[Tree] = _left 

    /**
     * @return right subtree 
     */
    def right: Option[Tree] = _right

    /**
     * @return error of the split
     */
    def splitError: Option[Double] = 
        (left, right) match 
            case (Some(l), Some(r)) => Some(l.error + r.error)
            case _ => None

    /**
     * @return square error of the root node 
     */
    def error: Double = Tree.sqError(Y)

    /**
     * @return number of datapoints in the root node
     */ 
    def datapoints: Int = Y.size

    /**
     * @return mean of the datapoints in the root node
     */
    def mean: Double = Tree.mean(Y)

    /**
     * @return height of the tree
     */
    def height: Int = 
        def childHeight(child: Option[Tree]) = 
            child match
                case Some(c) =>  1 + c.height 
                case None => 0  
        
        val leftHeight = childHeight(_left)
        val rightHeight = childHeight(_right)
        if leftHeight > rightHeight then leftHeight else rightHeight

    /**
     * @param id identifier of a subtree (e.g., "XRRL")
     * @return subtree with the corresponding identifier 
     */
    def subTree(id: String): Option[Tree] = 
        id.tail.length match
            case 0 => Some(this)
            case s => (
                id.tail(0) match
                    case 'L' => _left match 
                        case Some(t) => t.subTree(id.tail)
                        case None => None 

                    case 'R' => _right match 
                        case Some(t) => t.subTree(id.tail)
                        case None => None

                    case _ => throw Exception("invalid identifier")
            )

    /**
     * Splits the tree until no further splits are possible 
     */ 
    def splitAll: Unit = 
        var prevHeight: Int = this.height 

        while 
            this.split 
            this.height > prevHeight
        do (prevHeight = this.height)

    /**
     * Split the leaf nodes increasing the height of the tree
     */
    def split: Unit = 
        // split children if they exist
        if _left.isDefined && _right.isDefined then
            _left.get.split
            _right.get.split

        // split this node
        else if Tree.isSplitPossible(X) then
            var minErrorFeature: String = ""
            var minError: Double = Double.MaxValue

            for     
                (fName, values) <- X
                leftY = Tree.selectY(X, Y,fName, Branch.Left)  
                rightY = Tree.selectY(X, Y, fName, Branch.Right)  
                error =  Tree.sqError(leftY) + Tree.sqError(rightY)       
                if values.toSet.size > 1 && error < minError
            do  
                minError = error
                minErrorFeature = fName

            val leftY = Tree.selectY(X, Y,minErrorFeature, Branch.Left)
            val rightY = Tree.selectY(X, Y,minErrorFeature, Branch.Right)
            val leftX = Tree.selectX(X, minErrorFeature, Branch.Left)
            val rightX = Tree.selectX(X, minErrorFeature, Branch.Right)
            _splitFeature = Some(minErrorFeature)
            _left = Some(Tree(leftX, leftY, name + "L"))
            _right = Some(Tree(rightX, rightY, name + "R"))        

    /**
     * @param x a configuration,    pairs (feature name, value)
     * @return predicted performance
     */
    def predict(x: Map[String, Boolean]): Double =
        splitFeature match
            case None => Tree.mean(Y) // no children
            case Some(feature) => (
                x.keys.toList.contains(feature) match 
                    case false => Tree.mean(Y) 
                    case true => (if x(feature) then _left.get.predict(x) else _right.get.predict(x))
            )
        
    /**
     * @return string representation of this Tree
     */
    override def toString: String = 
        val indent = "  " * (name.length-1) 

        val errorSplitString = splitError match
            case Some(err) => s"${indent}error_of_split: ${Tree.roundValue(err)}\n" 
            case None => s"${indent}error_of_split:\n"

        val leftString = _left match
            case Some(left) => s"${indent}successor_left:\n" + left.toString 
            case None => "" 

        val rightString = _right match 
            case Some(right) => s"${indent}successor_right:\n" + right.toString
            case None => ""
        
        s"${indent}datapoints: ${datapoints}\n" +
        errorSplitString + 
        s"${indent}mean: ${Tree.roundValue(mean)}\n" +
        s"${indent}name: ${name}\n" +
        s"${indent}split_by_splitFeature: ${splitFeature.getOrElse("")}\n" +
        leftString +
        rightString
    

object Tree: 

    /**
     * @param n the number to be rounded
     * @param decimals number of decimal places to be conserved
     * @return rounded number
     */
    private def roundValue(n: Double, decimals: Int = 2) = 
        BigDecimal(n).setScale(decimals, BigDecimal.RoundingMode.HALF_UP).toDouble

    /**
     * @param sample list of values
     * @return mean value 
     */
    private def mean(sample: Seq[Double]): Double = 
        sample.foldLeft(0.0)(_ + _) / sample.size

    /**
     * @param sample list of values 
     * @param mean the mean
     * @return square error
     */
    private def sqError(sample: Seq[Double]): Double  =
        sample.map(_ - mean(sample)).map(pow(_, 2)).sum

    /**
     * @return the root node of this tree can be split
     */
    private def isSplitPossible(X: Map[String, Seq[Boolean]]): Boolean = 
        X.size >= 1 &&  // there is at least one feature
        X.map((fName, values) => values.toSet.size > 1).reduce(_ || _) // a division is possible

    /**
     * @param fName feature name
     * @param branch Left or Right
     * @return selection of Y
     */
    private def selectY(X: Map[String, Seq[Boolean]], Y: Seq[Double], feature: String, branch: Branch): Seq[Double] =  
        val x = X(feature)
        Y.zip(x).filter((y, x) => x == branch.condition).map((y,x) => y)  

    /**
     * @param fName feature name
     * @param branch Left or Right
     * @return selection of X
     */
    private def selectX(X: Map[String, Seq[Boolean]], feature: String, branch: Branch): Map[String, Seq[Boolean]] =
        val x = X(feature)
        X.filter((k, vals) => k != feature)
            .map((k, vals) => (k, vals.zip(x).filter((vals, x) => x == branch.condition).map((vals, x) => vals)))


val testX: Map[String, List[Boolean]] = Map(
    "x1" -> List(true,  true,   true, true, true, true, true, true, true, true), 
    "x2" -> List(false, false, true, true, false, true, true, false, true, false),
    "x3" -> List(true,  true,   false, true, false, false, true, true, true, false),
    "x4" -> List(true,  false, true, true, false, false, true, false, false, true),
    "x5" -> List(true,  false, false, true, false, true, false, true, false, false)
)

val testY: List[Double] = List(207, 342, 222, 300, 180, 612, 269, 406, 130, 390)

