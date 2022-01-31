package load

import org.apache.poi.ss.usermodel.{ DataFormatter, WorkbookFactory, Row, Cell}
import java.io.File
import collection.JavaConverters.*
import scala.collection.{mutable}

enum Selection:
    case All, Sample, NotSample

object Load:

    /* Total number of instances */
    val n: Int = 72

    /** Selected sample */
    val sampleIds: List[Int] = List(
        List(0,12),         // Distance 0
        List(3,6,15,18),    // Distance 1
        List(2,13,24,48),   // Distance 2
        List(7,19,66,51),   // Distance 3
        List(23,37,57,61),  // Distance 4 
        List(40,56,31,52),  // Distance 5
        List(71,59,35,58)   // Distance 6
    ).flatMap(x => x)

    /**
     * @param conf 
     * @return (features values, performance)
     */
    def loadMeasurements(conf: Selection = Selection.Sample): 
            (Map[String, List[Boolean]], List[Double], List[Int], List[Int]) = 

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
        val included: Array[Boolean] = conf match 
            case Selection.All => Array.fill(n)(true)
            case Selection.Sample => ids.map(id => sampleIds.contains(id)).toArray
            case Selection.NotSample => ids.map(id => !sampleIds.contains(id)).toArray
        
        // Include only the specified subset (all, sample, or not-sample)
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
        
        return (X.toMap, Y, ids, distances)
