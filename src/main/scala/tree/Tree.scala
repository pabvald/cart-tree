package tree

import scala.math.{pow, abs}


private enum Branch(val condition: Double): 
    case Left extends Branch(1.0)
    case Right extends Branch(0.0)
      
        
class Tree(val X: Map[String, Seq[Double]], val Y: Seq[Double], level: Int = 0):
    private var _feature: Option[String] = None 
    private var _splitError: Option[Double] = None
    private var _left: Option[Tree] = None 
    private var _right: Option[Tree] = None      

    /**
     * @param fName feature name
     * @param branch Left or Right
     * @return selection of Y
     */
    private def selectY(feature: String, branch: Branch): Seq[Double] =  
        val x = X(feature)
        Y.zip(x).filter((y,x) => x == branch.condition).map((y,x) => y)  

    /**
     * @param fName feature name
     * @param branch Left or Right
     * @return selection of X
     */
    private def selectX(feature: String, branch: Branch): Map[String, Seq[Double]] =
        val x = X(feature)
        X.filter((fName, values) => fName != feature).map((fName, values) => 
            (fName, values.zip(x).filter(_._2 == branch.condition).map(_._1)))

    /**
     * @return name of the feature used in the partition of the root node
     */
    def feature: Option[String] = _feature

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
    def splitError: Option[Double] = _splitError

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
        _left match
            case  Some(child) =>  1 + child.height 
            case None => 1   

    /**
     * Split the leaf nodes increasing the height of the tree
     */
    def split: Unit = 
        if _left.isDefined && _right.isDefined then
            _left.get.split
            _right.get.split

        else if X.size > 0 && !X.map((fName, values) => values.isEmpty).reduce(_ && _) then
            var minErrorFeature: String = ""
            var minError: Double = Double.MaxValue

            for     
                fName <- X.keys
                leftY = selectY(fName, Branch.Left)  
                rightY = selectY(fName, Branch.Right)  
                error =  Tree.sqError(leftY) + Tree.sqError(rightY)                
                if error < minError
            do  
                minError = error
                minErrorFeature = fName

            val leftY = selectY(minErrorFeature, Branch.Left)
            val rightY = selectY(minErrorFeature, Branch.Right)
            val leftX = selectX(minErrorFeature, Branch.Left)
            val rightX = selectX(minErrorFeature, Branch.Right)
            _splitError = Some(minError)
            _feature = Some(minErrorFeature)
            _left = Some(Tree(leftX, leftY, level + 1))
            _right = Some(Tree(rightX, rightY, level + 1))        

    /**
     * @return string representation of this Tree
     */
    override def toString: String = 
        val indent = "    "*(level) 
        val leftString = _left match
            case Some(left) => left.toString 
            case None => "" 
        val rightString = _right match 
            case Some(right) => right.toString
            case None => ""
        
        s"${indent}level: ${level}\n" +
        s"${indent}datapoints: ${datapoints}\n" +
        s"${indent}error_of_split: ${splitError.getOrElse("")}\n" + 
        s"${indent}mean: ${mean}\n" + 
        s"${indent}split_by_feature: ${feature.getOrElse("")}\n" +
        s"${indent}successor_left:\n" + leftString +
        s"${indent}successor_right:\n" + rightString
    

object Tree: 

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


val X: Map[String, List[Double]] = Map(
    "x1" -> List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
    "x2" -> List(0, 0, 1, 1, 0, 1, 1, 0, 1, 0),
    "x3" -> List(1, 1, 0, 1, 0, 0, 1, 1, 1, 0),
    "x4" -> List(1, 0, 1, 1, 0, 0, 1, 0, 0, 1),
    "x5" -> List(1, 0, 0, 1, 0, 1, 0, 1, 0, 0)
)

val Y: List[Double] = List(207, 342, 222, 300, 180, 612, 269, 406, 130, 390)

