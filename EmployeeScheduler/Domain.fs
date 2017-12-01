module Domain
open System

type Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

type Shifts = {
    Monday : (int * int) list  
    Tuesday : (int * int) list 
    Wednesday : (int * int) list 
    Thursday : (int * int) list 
    Friday : (int * int) list 
    Saturday: (int * int) list 
    Sunday : (int * int) list 
} 
           
type Availibility = Open | Unavailible | Partial of (int * int)

type WeeklyAvailibility = {
    Monday : Availibility
    Tuesday : Availibility
    Wednesday : Availibility
    Thursday : Availibility
    Friday : Availibility
    Saturday: Availibility
    Sunday : Availibility
}

type Request<'id> = {
     employeeId : 'id
     date : DateTime
}

type WeeklyRequests<'id> = {
    Monday : Request<'id> list
    Tuesday : Request<'id> list
    Wednesday : Request<'id> list
    Thursday : Request<'id> list
    Friday : Request<'id> list
    Saturday: Request<'id> list
    Sunday : Request<'id> list
}

type Employee<'id> = {
    _id : 'id
    firstName : string
    lastName : string
    availibility : WeeklyAvailibility
    desiredHours : int
    maxHours : int
    maxNumDay : int
    rank : int  
    complexityScore : float 
}


type AggregateEmployeeData<'id> = {
     employee : Employee<'id>
     hourTotal : int 
     daysScheduled : Day list 
}

type Staff<'id> = Employee<'id> list 

type LaborDistribution = {
     operationHours : int * int
     distribution : int list 
}

type  LaborRequirements = {
    Monday : LaborDistribution  
    Tuesday : LaborDistribution 
    Wednesday : LaborDistribution 
    Thursday : LaborDistribution 
    Friday : LaborDistribution 
    Saturday: LaborDistribution 
    Sunday : LaborDistribution 
}



type WorkDay<'id> = {
   laborDistribution : LaborDistribution
   shifts :  ('id Option * (int * int)) list     
}

type Schedule<'id> = {
    Monday : WorkDay<'id> Option 
    Tuesday : WorkDay<'id> Option
    Wednesday : WorkDay<'id> Option
    Thursday : WorkDay<'id> Option
    Friday : WorkDay<'id> Option
    Saturday: WorkDay<'id> Option
    Sunday : WorkDay<'id> Option
}

let private initialS =  {
          Monday = None
          Tuesday = None
          Wednesday = None
          Thursday = None
          Friday = None
          Saturday = None
          Sunday = None
}

type InprogressSchedule<'id> = {
     schedule  : Schedule<'id>
     employeePool : Staff<'id>
}

let rec getLastHour n distribution = 
     match n with
     | _ when (List.length distribution - 1) = n ->
         if distribution.[n] < distribution.[n - 1] then n else n + 1
     | _ when distribution.[n] <= distribution.[n + 1] -> getLastHour (n + 1) distribution
     | _ -> n + 1
     
let rec getShifts laborD shifts = 
     let {operationHours = opening, _; distribution = distribution} = laborD
     if List.forall (fun x -> x = 0) distribution then shifts
     else
          let firstHour = List.findIndex (fun x -> x <> 0) distribution 
          let start = opening + firstHour 
          let lastHour = getLastHour firstHour distribution 
          let stop = opening + lastHour
          let updatedDistribution = List.mapi (fun i x -> 
            if i < lastHour && x - 1 >= 0 then x - 1 else x) distribution
          getShifts {laborD with distribution = updatedDistribution} (shifts @ [start, stop])

//assume that prefilled shifts are removed from labor          
//function that takes LaborRequirements and returns Shifts
// currently not being used
let laborRequirementsToShifts (laborR: LaborRequirements) : Shifts = 
     {
          Monday = getShifts laborR.Monday []
          Tuesday = getShifts laborR.Tuesday []
          Wednesday = getShifts laborR.Wednesday []
          Thursday = getShifts laborR.Thursday []
          Friday = getShifts laborR.Friday []
          Saturday = getShifts laborR.Saturday []
          Sunday = getShifts laborR.Sunday []
     }



let calcAvaComplexity = function
| Open -> 0
| Unavailible -> 1
| Partial (x, y) -> 1 / (y - x + 1)

let (<+>) x y = (calcAvaComplexity x) + (calcAvaComplexity y)

let calcEmployeeComplexity  (wAvailibility: WeeklyAvailibility) (rank: int) = 
   let { 
        WeeklyAvailibility.Monday = mon; Tuesday = tue; Wednesday = wed;
        Thursday = thurs; Friday = fri; Saturday = sat; Sunday = sun
        } = wAvailibility
   (mon <+> tue)  + (wed <+> thurs ) + (fri <+> sat) + (sun <+> Open) + rank
     

let getLaborTotal laborD = 
     List.sum laborD.distribution

let calcDaysAvailibleLabor dayLength availibility =
 
     let folder state a = 
          match a with
          | Open -> dayLength + state
          | Unavailible -> state
          | Partial(x, y) -> y - x + state 

     List.fold folder 0 availibility

let calcDayComplexity laborD availibility = 
     let total = getLaborTotal laborD
     let (x, y) = laborD.operationHours
     let dayLength = y - x
     let availible = calcDaysAvailibleLabor dayLength availibility
     //(total * (availible  +  total))/ availible
     availible - total
let (>%>) l f = List.filter f l

let getEmployeeAvailibility day employee = 
     match day with 
     // if an employee has been scheduled their max amount then consider them unavialible
     | Monday -> employee.availibility.Monday
     | Tuesday -> employee.availibility.Tuesday
     | Wednesday  -> employee.availibility.Wednesday
     | Thursday  -> employee.availibility.Thursday
     | Friday  -> employee.availibility.Friday
     | Saturday  -> employee.availibility.Saturday
     | Sunday -> employee.availibility.Sunday

let updateEmployeeAvailibility day availbility (wAve: WeeklyAvailibility)  =
     match day with 
     | Monday -> {wAve with Monday = availbility}
     | Tuesday -> {wAve with Tuesday = availbility}
     | Wednesday  -> {wAve with Wednesday = availbility}
     | Thursday  -> {wAve with Thursday = availbility}
     | Friday  -> {wAve with Friday = availbility}
     | Saturday  -> {wAve with Saturday = availbility}
     | Sunday -> {wAve with Sunday = availbility}

let isEmployeeAvailible ignoreMaxHours day (start, stop) (employee:Employee<'id>) =
     let shiftLength = stop - start
     let {maxHours = maxHours} = employee
     let availibility = getEmployeeAvailibility day employee

     match shiftLength with
     | l when maxHours >= l || ignoreMaxHours ->
         match availibility with
         | Open -> true
         | Unavailible -> false
         | Partial range -> 
          match range with
          | (x, y) when x <= start && y >= stop -> true
          | _ -> false
     | _ -> false

//if the first option is a none this functions returns the backup option
let (<||>) first backup = 
     match first with 
     | Some _ -> first
     | None -> backup
        

let rec assignShifts day staff unassigned  assigned =
     match unassigned with
     | head :: tail ->
          let maxHoursObserverd = List.tryFind (isEmployeeAvailible false day head ) staff
          let maxHoursIgnored = List.tryFind (isEmployeeAvailible true day head ) staff
          
          match maxHoursObserverd <||> maxHoursIgnored with
          | Some employee ->
               let maxNumDay =  employee.maxNumDay - 1
               let maxHours = employee.maxHours - (snd head - fst head)
               
               let updatedEmployee = {employee with availibility = updateEmployeeAvailibility day Unavailible employee.availibility; maxNumDay = maxNumDay; maxHours = maxHours;}
               let updatedAssigned = assigned @ [(Some employee._id, head)]
              // put employess at the end of the lst once used so that priority is given to employees not scheduled that day yet
               let updatedStaff = (List.filter (fun e -> e._id <> employee._id ) staff) @ [updatedEmployee]
               assignShifts day updatedStaff  tail updatedAssigned
          //if no one is avialible to take the shift
          | None -> assignShifts day staff tail (assigned @ [(None, head)])
     | [] ->
          //once done make all employees unavailible for that day, this is so that this day no longer affects the employees schedule complexity
           (assigned, List.map (fun e -> {e with availibility = updateEmployeeAvailibility day Unavailible e.availibility}) staff)
           
//use shifts and day and staff list to return a tuple of workday, updatedEmployee list
let createWorkDay (l, day, s) staff =
     let orderedStaff = List.sortBy (fun e -> calcEmployeeComplexity e.availibility e.rank) staff
     let (shifts, updatedStaff) = assignShifts day orderedStaff s []
   
     ({laborDistribution = l; shifts = shifts}, updatedStaff)

// given a schedule and a day and a workday return a schedule with the updated workday
// use pattern matching on day type to implemeant this 
let updateSchedule day (schedule: Schedule<'id>) workDay =
     match day with 
     | Monday -> {schedule with Monday = Some workDay}
     | Tuesday -> {schedule with Tuesday = Some workDay}
     | Wednesday  -> {schedule with Wednesday = Some workDay}
     | Thursday  -> {schedule with Thursday = Some workDay}
     | Friday  -> {schedule with Friday = Some workDay}
     | Saturday  -> {schedule with Saturday = Some workDay}
     | Sunday -> {schedule with Sunday = Some workDay}

let updateInProgressSchedule state (l, day, shifts) = 
      let (w, e) = createWorkDay (l, day, shifts) state.employeePool 
      {schedule = updateSchedule day state.schedule w; employeePool = e}

let getAvailibility day employees  =
     List.map (fun e -> getEmployeeAvailibility day e) employees
     
let mutable debugCount = 0;

let rec createSchedule  state data =
     let {schedule = s; employeePool = staff} = state
     // printfn "%i : " debugCount
     // printfn "%A" data
     let sort (l, day, _) = 
          let availbility = getAvailibility day staff
          -1 * (calcDayComplexity l availbility)
     let reOrdered = List.sortBy sort data
     // printfn "%A" reOrdered
     // printfn "\n"
     // debugCount <- (debugCount + 1)
     match reOrdered with
     | head :: tail -> 
          let newState = updateInProgressSchedule state head
          createSchedule newState tail
     | [] -> s

//assumes that request have aready been used to update the availibility of the employess before sending them to this function
let produceWorkWeek (l: LaborRequirements) staff = 
     [
          (l.Monday,  Monday);
          (l.Tuesday,  Tuesday)
          (l.Wednesday,  Wednesday)
          (l.Thursday,  Thursday)
          (l.Friday,  Friday)
          (l.Saturday, Saturday)
          (l.Sunday, Sunday)
     ] 
     |> List.map (fun (l, day) -> (l, day, getShifts l [])) 
     |> createSchedule {schedule = initialS; employeePool = staff;}
     
    