module Actions
open Domain
open System
open System.IO
open System.Text.RegularExpressions
open OfficeOpenXml

//TODO: add error handling for methods accessing files 


/// <summary>Gets the maximum row number for a given worksheet</summary>    
/// <param name="worksheet">The input worksheet.</param>
/// <returns>Maximum row number</returns>
let getMaxRowNumber (worksheet:ExcelWorksheet) = 
   worksheet.Dimension.End.Row 

/// <summary>Gets the maximum column number for a given worksheet</summary>    
/// <param name="worksheet">The input worksheet.</param>
/// <returns>Maximum column number</returns>
let getMaxColNumber (worksheet:ExcelWorksheet) = 
   worksheet.Dimension.End.Column

/// <summary>Gets all the values from given column in a given worksheet in a sequence. </summary>   
/// <param name="colIndex">The column index (starting from 1!).</param> 
/// <param name="worksheet">The input worksheet.</param>
/// <returns>Sequence of cell values</returns>
let getColumn colIndex (worksheet:ExcelWorksheet) = seq { 
   let maxRow = getMaxRowNumber worksheet  
   for i in 1..maxRow do        
       let value = worksheet.Cells.[i,colIndex].Value;
       if  not <| isNull value then
           let content = value.ToString()
           yield content
       else
           yield ""
}
/// Gets all the values from given row in a given worksheet in a sequence. 
let getRow rowIndex (worksheet:ExcelWorksheet) = seq { 
   let maxCol = getMaxColNumber worksheet  
   for i in 1..maxCol do        
       let value = worksheet.Cells.[rowIndex,i].Value
       if not <| isNull value then
           let content = value.ToString()
           yield content
       else
           yield ""
}

/// gets excel doc given a filename. Creates the excelPackage if the the file does not already exist
let getDocument filename = 
     let newFile = FileInfo(filename)        
     let package = new ExcelPackage(newFile)        
     package

/// creates a new excel doc given a file name
let createDocument filename = 
        if (File.Exists(filename)) then
            File.Delete(filename)

        let newFile = FileInfo(filename)        
        let package = new ExcelPackage(newFile)        
        package

/// given an Excel WorkSheet containing labor requirements return a LaborRequirements record 
let extractLaborRequirements ws = 
     let isNonZero str =
          match System.Int32.TryParse str with
          | true, v when v > 0 -> true
          | _ -> false

     let extractDistribution col =
         let distL = col |> Seq.tail |> Seq.toList
         ///TODO convert to TryFind for case all zero
         let start = List.findIndex isNonZero distL
         let distribution = distL |> List.filter isNonZero |> List.map int
         {operationHours = (start,  start + distribution.Length); distribution = distribution}
     {
          LaborRequirements.Monday = getColumn 2 ws |> extractDistribution
          Tuesday = getColumn 3 ws |> extractDistribution

          Wednesday = getColumn 4 ws |> extractDistribution

          Thursday = getColumn 5 ws |> extractDistribution

          Friday = getColumn 6 ws |> extractDistribution

          Saturday = getColumn 7 ws |> extractDistribution

          Sunday = getColumn 8 ws |> extractDistribution
     }
    
let parseTime (str:string) = 
    match str with 
    | s when s.EndsWith "am" ->
          let time = Regex.Replace(s, "[^0-9]", "") |> int
          if time = 12 then 0 else time
    | s when s.EndsWith "pm" ->
          let time = Regex.Replace(s, "[^0-9]", "") |> int
          if time = 12 then 12 else time + 12
    | s -> raise <| ArgumentException("Invaild availibility:" + s)

let formatTime = function
| time when time < 12 ->
     let stringTime = if time = 0 then "12" else time.ToString()
     stringTime + "am"
| time ->
     let stringTime = if time = 12 then "12" else (time - 12).ToString()
     stringTime + "pm"

let parseAvailibility (str: string) =
     match str.ToLower().Replace(" ", "") with 
     | "unavailible" -> Unavailible
     | "open" -> Open
     | "" -> Unavailible
     | str -> 
          match str.Split('-') |> Array.toList with
          | range when range.Length = 2 -> 
               let start = List.head range
               let  stop = List.head <| List.tail range
               Partial (parseTime start, parseTime stop)               
          |  _ -> raise <| ArgumentException("Invaild availibility")

let extractEmployee (row : string list) = 
     let wA = {
          WeeklyAvailibility.Monday = parseAvailibility row.[1]
          Tuesday = parseAvailibility row.[2]
          Wednesday =parseAvailibility row.[3]
          Thursday =parseAvailibility row.[4]
          Friday =parseAvailibility row.[5]
          Saturday=parseAvailibility row.[6]
          Sunday =parseAvailibility row.[7]
     }
     createEmployee (row.[10], row.[0], wA, row.[8] |> int, row.[9] |> int, 5)
 
let extractEmployees w =
     let lastRow = getMaxRowNumber w
     let rows = [for i in 2 .. lastRow -> getRow i w |> Seq.toList]

     List.map extractEmployee rows
     
let updateLaborDistribution ld (str: string) =
     match str.ToLower().Replace(" ", "") with
     | "unavailible" -> ld
     | "" -> ld
     | s -> 
          match s.Split('-') |> Array.toList with
          | range when range.Length = 2 -> 
               let start = List.head range |> parseTime
               let  stop = (List.head <| List.tail range) |> parseTime
               let openTime = fst <| ld.operationHours 
               let ldStart = start - openTime
               let ldStop = stop - start + ldStart
               let distributon = List.mapi (fun i n -> if  i >= ldStart &&  i <= ldStop then n - 1 else n) ld.distribution
               {ld with distribution = distributon }        
          |  _ -> ld


let shiftLength (str: string) = 
    match  str.ToLower().Replace(" ", "") with
    | s when s.Contains "-" ->
          match s.Split('-') |> Array.toList with
          | range when range.Length = 2 -> 
               let start = List.head range |> parseTime
               let  stop = (List.head <| List.tail range) |> parseTime
               stop - start
          |  _ -> 0
    | _ -> 0 

let isValidShiftInput (str: string) =
     let s = str.ToLower().Replace(" ", "")
     s = "unavailible" || s.Contains "-"

let getNewEmpTotal days emp =
   let dayLengths = List.map shiftLength days
   let totalHours = List.sum dayLengths
   let days = List.sumBy (fun x -> if x > 0 then 1 else 0) dayLengths
   (emp.maxHours - totalHours, emp.maxDays - days)

let updateEmployee (rows: string list list) (e: Employee<string>) =

     let row = List.find (fun (r: string list) -> r.[1] = e._id) rows
     let mon = row.[2]
     let tue = row.[3]
     let wed = row.[4]
     let th = row.[5]
     let fri = row.[6]
     let sat = row.[7]
     let sun = row.[8]

     let (newMax, maxDays) = getNewEmpTotal [mon; tue; wed; th; fri; sat; sun;] e
     let availibility = e.availibility

     let wA = {
          WeeklyAvailibility.Monday = if isValidShiftInput mon then Unavailible else availibility.Monday
          Tuesday = if isValidShiftInput tue then Unavailible else availibility.Tuesday
          Wednesday = if isValidShiftInput wed then Unavailible else availibility.Wednesday
          Thursday = if isValidShiftInput th then Unavailible else availibility.Thursday
          Friday = if isValidShiftInput fri then Unavailible else availibility.Friday
          Saturday = if isValidShiftInput sat then Unavailible else availibility.Saturday
          Sunday = if isValidShiftInput sun then Unavailible else availibility.Sunday
     }
     {e with availibility = wA;  maxDays = maxDays; maxHours = if newMax >= 0 then newMax else 0;}

let applyRequests w employees (lr: LaborRequirements) = 

     let lastRow = getMaxRowNumber w
     let rows = [for i in 2 .. lastRow -> getRow i w |> Seq.toList]
     let updatedEmps = List.map (updateEmployee rows) employees

     let cols =  [for i in 3 .. 9 -> getColumn i w |> Seq.toList] |> List.map (fun x -> x |> List.tail)
     let applyReqToLd = List.fold updateLaborDistribution
     let updatedLR = {
          LaborRequirements.Monday = applyReqToLd lr.Monday cols.[0]

          Tuesday = applyReqToLd lr.Tuesday cols.[1]

          Wednesday =  applyReqToLd lr.Wednesday cols.[2]

          Thursday =  applyReqToLd lr.Thursday cols.[3]

          Friday =  applyReqToLd lr.Friday cols.[4]

          Saturday =  applyReqToLd lr.Saturday cols.[5]

          Sunday =  applyReqToLd lr.Sunday cols.[6]
     }

     updatedLR, updatedEmps

let saveDay (ws:ExcelWorksheet) (rows: string list list) workdayOption colIndex =
     match workdayOption with
     | Some workday ->
          printfn "%A" workday
          let shifts = workday.shifts
          List.iter (fun ((idOption: string Option), (start, stop)) -> 
               match idOption with 
               | Some id ->
                    printfn "Id: %s" id
                    let i = List.findIndex (fun (emp: string list) -> emp.[1] = id) rows
                    printfn "Int: %d" i
                    // i + 2 since the headers are not includded in the rows list and the index start at 1 
                    ws.Cells.[i + 2, colIndex].Value <- formatTime start + "-" + formatTime stop
               | None -> ()
          ) shifts
          ()
     | None -> ()

(*
     So lets split the ws into rows to seperate by employees
     then for each day in schedule terate through the shifts and use the id to find the employee in the row and the employees index.
     then set the value for that cell given the index to the value of the shift 
*)
let saveSchedule ws schedule =
     let lastRow = getMaxRowNumber ws
     let rows = [for i in 2 .. lastRow -> getRow i ws |> Seq.toList]
     let save = saveDay ws rows 

     do save schedule.Monday 3
     do save schedule.Tuesday 4
     do save schedule.Wednesday 5
     do save schedule.Thursday 6
     do save schedule.Friday 7
     do save schedule.Saturday 8
     do save schedule.Sunday 9
     ()   

let produceExcelSchedule () =
    let exist = File.Exists("schedule.xlsx")
    let package = getDocument "schedule.xlsx"
    /// if the file does not exist create the excell project with default worksheets 
    if (not exist) then
          let empWS = package.Workbook.Worksheets.Add("Employees")
          empWS.Cells.[1,1].Value <- "Name"
          empWS.Cells.[1,2].Value <- "Monday"
          empWS.Cells.[1,3].Value <- "Tuesday"
          empWS.Cells.[1,4].Value <- "Wednesday"
          empWS.Cells.[1,5].Value <- "Thursday"
          empWS.Cells.[1,6].Value <- "Friday"
          empWS.Cells.[1,7].Value <- "Saturday"
          empWS.Cells.[1,8].Value <- "Sunday"
          empWS.Cells.[1,9].Value <- "Max Hours"
          empWS.Cells.[1,10].Value <- "Seniority"
          empWS.Cells.[1,11].Value <- "Number/email"

          let lrWS = package.Workbook.Worksheets.Add("Labor Requirements")
          lrWS.Cells.[1,2].Value <- "Monday"
          lrWS.Cells.[1,3].Value <- "Tuesday"
          lrWS.Cells.[1,4].Value <- "Wednesday"
          lrWS.Cells.[1,5].Value <- "Thursday"
          lrWS.Cells.[1,6].Value <- "Friday"
          lrWS.Cells.[1,7].Value <- "Saturday"
          lrWS.Cells.[1,8].Value <- "Sunday"
          for i in 0 .. 23 do
               lrWS.Cells.[i + 2, 1].Value <- i
               for j in 2 .. 8 do
                    lrWS.Cells.[i + 2, j].Value <- 0
                
          let sWS = package.Workbook.Worksheets.Add("Schedule")
          sWS.Cells.[1,1].Value <- "Name"
          sWS.Cells.[1,2].Value <- "Number/email"
          sWS.Cells.[1,3].Value <- "Monday"
          sWS.Cells.[1,4].Value <- "Tuesday"
          sWS.Cells.[1,5].Value <- "Wednesday"
          sWS.Cells.[1,6].Value <- "Thursday"
          sWS.Cells.[1,7].Value <- "Friday"
          sWS.Cells.[1,8].Value <- "Saturday"
          sWS.Cells.[1,9].Value <- "Sunday"
          sWS.Cells.[1,10].Value <- "#Hours"
          package.Save()
          printfn "The file 'schedule.xlsx' has been added in this folder.\nFill out the the worksheets and run this applicatin again to create a new schedule."
          
    else
         let employees = extractEmployees <| package.Workbook.Worksheets.["Employees"]
         let lr = extractLaborRequirements <| package.Workbook.Worksheets.["Labor Requirements"]
         let scheduleWS = package.Workbook.Worksheets.Copy("Schedule", "Schedule" + DateTime.Now.ToString()) 

         let (updatedLR, updatedEmp) = applyRequests scheduleWS employees lr
         
         let schedulePayload =  produceWorkWeek updatedLR updatedEmp
         do saveSchedule scheduleWS schedulePayload.schedule
         package.Save()

