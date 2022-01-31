package load

import org.apache.poi.ss.usermodel.{DataFormatter, WorkbookFactory, Row, Cell}
import java.io.File
import collection.JavaConverters.*
import scala.collection.{mutable}


object Load:

    /* Total number of instances */
    val n: Int = 72

    /** All ids **/ 
    val allIds: List[Int] = (0 until n).toList 

    /** Sample ids */
    val sampleIds: List[Int] = List(
        List(0,12),         // Distance 0
        List(3,6,15,18),    // Distance 1
        List(2,13,24,48),   // Distance 2
        List(7,19,66,51),   // Distance 3
        List(23,37,57,61),  // Distance 4 
        List(40,56,31,52),  // Distance 5
        List(71,59,35,58)   // Distance 6
    ).flatMap(x => x)

    /** No-sample ids **/
    val noSampleIds: List[Int] = (allIds.toSet diff sampleIds.toSet).toList

    /**
     * @param subset list of identifiers to be included
     * @return (features values, performance, distance)
     */
    def load(subset: List[Int]): 
            (Map[String, List[Boolean]], List[Double], List[Int]) = 

        var header: List[String] = List[String]() 
        var ids: List[Int] = List[Int]()
        var distances: List[Int] = List[Int]()
        var X: mutable.Map[String, List[Boolean]] = mutable.Map()
        var Y: List[Double] = List[Double]()

        // Read excel file
        val f = File("src/main/resources/measurements.xlsx")
        val workbook = WorkbookFactory.create(f)
        val sheet = workbook.getSheetAt(0) 

        header= sheet.getRow(0).asScala.map(cell => cell.getStringCellValue).toList        
        header.filter(x => !Seq("Performance", "Distance", "Id").contains(x))
              .map(h => X = X + (h -> List[Boolean]()))
        
        // Fill excel data into structures
        for 
            i <- (1 to n).reverse
            j <- (0 until sheet.getRow(0).getPhysicalNumberOfCells)
        do  
            val value: Double = sheet.getRow(i).getCell(j).getNumericCellValue
            header(j) match 
                case "Id" => ids = value.toInt :: ids
                case "Distance" => distances = value.toInt :: distances
                case "Performance" => Y = value :: Y
                case _ =>  X(header(j)) =  (value match
                    case 0.0 =>  false    
                    case 1.0 =>  true 
                    case _ => throw Exception("feature value must be 1.0 or 0.0")
                ) :: X(header(j))

        // Obtain a filtering of the intances
        val included: Array[Boolean] = ids.map(id => subset.contains(id)).toArray
        
        // Include only the specified subset
        X.map((key, values) => X(key) = 
            values.zip(0 until values.length)
                  .filter((value, i) => included(i))
                  .map((value, i) => value)
                  .toList
        )

        Y = Y.zip(0 until Y.length)
             .filter((y, i) => included(i))
             .map((y, i) => y)
             .toList

        distances = distances.zip(0 until distances.length)
             .filter((d, i) => included(i))
             .map((d, i) => d)
             .toList
        
        return (X.toMap, Y, distances)

    /**
     * @param instance's identifier
     * @return a single instance 
     */
    def loadOne(id: Int): (Map[String, Boolean], Double, Int) =
        val (x, y, distances) = load(List(id))
        (x.map((k,v) => (k -> v(0))), y(0), distances(0))
