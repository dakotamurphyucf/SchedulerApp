module Domain

/// Days of the week
type Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

/// Each day property contains a list with pairs of int's representing the shifts for the day as a range 
type Shifts = {
    Monday : (int * int) list  
    Tuesday : (int * int) list 
    Wednesday : (int * int) list 
    Thursday : (int * int) list 
    Friday : (int * int) list 
    Saturday: (int * int) list 
    Sunday : (int * int) list 
} 

/// Availibility of Employee for a given day of the week
type Availibility = Open | Unavailible | Partial of (int * int)

/// The weekly Availibility for an Employee
type WeeklyAvailibility = {
    Monday : Availibility
    Tuesday : Availibility
    Wednesday : Availibility
    Thursday : Availibility
    Friday : Availibility
    Saturday: Availibility
    Sunday : Availibility
}

/// Record that represents an Employee.
/// The 'id can be any type. 
/// name is a string to represent the name of the employee
/// availibility is the weekly availbility of the employee
/// maxHours is the maximum number of hours an employee can be scheduled in a week
/// rank is an int 1 - 5 that represents the seniority of the employee. This is used to help prioritize senior employees when making the schedule 
/// maxDays represents the maximum number of days that an employee can be scheduled any given day
type Employee<'id> = {
    _id : 'id
    name : string
    availibility : WeeklyAvailibility
    maxHours : int
    rank : int 
    maxDays : int 
}

/// List of Employees
type Staff<'id> = Employee<'id> list 

/// Record that represents the desired Number of Employees to be scheduled during each hour of operation
/// operationHours represents the start and end time for a workday. 0 - 24 scale
/// distribution is a list of int's that represnts the labor count for each hour
type LaborDistribution = {
     operationHours : int * int
     distribution : int list 
}

/// Record that contains the LaborDistribution for each day of the week
type  LaborRequirements = {
    Monday : LaborDistribution  
    Tuesday : LaborDistribution 
    Wednesday : LaborDistribution 
    Thursday : LaborDistribution 
    Friday : LaborDistribution 
    Saturday: LaborDistribution 
    Sunday : LaborDistribution 
}

/// Record that contains scheduled shifts for a workday
/// shifts is a list of tuples containing an 'id option and the shift. 
/// Note: a shift with None means the shift was not assigned to an employee
type WorkDay<'id> = {
   laborDistribution : LaborDistribution
   shifts :  ('id Option * (int * int)) list     
}

///  Record that contains the weeks schedule. Each day contains a WorkDay Option. 
/// Note: a day with None as the value implies the buisness is closed.
type Schedule<'id> = {
    Monday : WorkDay<'id> Option 
    Tuesday : WorkDay<'id> Option
    Wednesday : WorkDay<'id> Option
    Thursday : WorkDay<'id> Option
    Friday : WorkDay<'id> Option
    Saturday: WorkDay<'id> Option
    Sunday : WorkDay<'id> Option
}

/// Unit Schedule 
let private initialS =  {
          Monday = None
          Tuesday = None
          Wednesday = None
          Thursday = None
          Friday = None
          Saturday = None
          Sunday = None
}

/// return type of produceWorkWeek function. Used to help package data in schedule building logic
type SchedulePayload<'id> = {
     schedule  : Schedule<'id>
     employeePool : Staff<'id>
}

/// construcrts an employee record
let createEmployee (id, name, availibility, maxHours, rank, maxDays) =
     {
          _id = id
          name = name
          availibility = availibility
          maxHours = maxHours
          rank = rank
          maxDays = maxDays
     }  

/// returns the end time of a shift given a distribution and the index representing the start of a shift in the distribution
let rec getShiftEndTime n distribution = 
     match n with
     /// case when n equals the last index in the list
     | _ when (List.length distribution - 1) = n -> n + 1
     ///  if the next hour contains the same number of employees or more return getShiftEndTime with n + 1
     | _ when distribution.[n] <= distribution.[n + 1] -> getShiftEndTime (n + 1) distribution
     /// if the next hour contains less employees than return n + 1 as the end time for the shift
     | _ -> n + 1

/// given a labor distribution  and empty list generatres a list of shifts that realize the given labor distribution
let rec getShifts laborD shifts = 
     let {operationHours = opening, _; distribution = distribution} = laborD
     /// stop condition for recursive logic. Return the shifts when no more labor hours are left in the distribution
     if List.forall (fun x -> x = 0) distribution then shifts
     /// generate the next shift from the distribution
     else
          /// the first index where the value is not zero
          let startHour = List.findIndex (fun x -> x <> 0) distribution 

          /// hour where shift finishes relative to index 0 of distribution list.
          /// If getShiftEndTime results in a shift longer than 8 hours then set stopHour so that the shift is only 8 hours 
          let stopHour = getShiftEndTime startHour distribution |> (fun x -> if x - startHour <= 8 then x else startHour + 8)
          
          /// add the opening value to startHour and stopHour to get the true start and stop times  
          let startTime = opening + startHour
          let stopTime = opening + stopHour

          /// reduce the labor count by 1 for each index that is contained in the generated shift
          let updatedDistribution = List.mapi (fun i x -> 
            if i < stopHour && x - 1 >= 0 then x - 1 else x) distribution
          
          /// append the generated shift to the shifts list and run getShifts with the updated distribution
          getShifts {laborD with distribution = updatedDistribution} (shifts @ [startTime, stopTime])

       
/// function that takes LaborRequirements and returns Shifts
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


/// given an availibility returns a float representing its complexity 
let calcAvaComplexity = function
| Open -> 0.0
| Unavailible -> 1.0
/// this definition was chosen so that as the length of the shift approached zero the value approaches 1.0 , and also bounds its value above 0.0 
| Partial (x, y) -> 1.0 / (float y - float x + 1.0)

/// Uses an employees availibility and  rank to calculate the employees scheduling complexity
/// Note: bigger values represent greater complexity
let calcEmployeeComplexity  (wAvailibility: WeeklyAvailibility) (rank: int) = 
   let { 
        WeeklyAvailibility.Monday = mon; Tuesday = tue; Wednesday = wed;
        Thursday = thurs; Friday = fri; Saturday = sat; Sunday = sun
        } = wAvailibility
   /// this definition uses the sum of each days availbility complexity and the rank to determine complexity
   /// intuitivley we wanted scheduling complexity to increase for more senior employees. Also employeess with less availibility should have increased scheduling complexity
   float rank + List.sumBy calcAvaComplexity [mon; tue; wed; thurs; fri; sat; sun;]
   
let getLaborTotal laborD = 
     List.sum laborD.distribution

/// given the length of operating hours and the days availibility for all employees returns the availble labor hours for the workday
let calcDaysAvailibleLabor dayLength availibility =
 
     let folder state a = 
          match a with
          | Open -> dayLength + state
          | Unavailible -> state
          | Partial(x, y) -> y - x + state 

     List.fold folder 0 availibility

/// returns the scheduling complexity of a work day given a labor distribution and the availibility for all employees that day.
/// Note: smaller values represent greater complexity
let calcDayComplexity laborD availibility = 
     let total = getLaborTotal laborD
     let (x, y) = laborD.operationHours
     let dayLength = y - x
     let availible = calcDaysAvailibleLabor dayLength availibility
     /// this definition is used so that smaller the difference between availible labor and required labor the more complex the scheduling is for the day
     availible - total

/// given a day and an employee returns the employees availibility for that day
let getEmployeeAvailibility day employee = 
     match day with 
     | Monday -> employee.availibility.Monday
     | Tuesday -> employee.availibility.Tuesday
     | Wednesday  -> employee.availibility.Wednesday
     | Thursday  -> employee.availibility.Thursday
     | Friday  -> employee.availibility.Friday
     | Saturday  -> employee.availibility.Saturday
     | Sunday -> employee.availibility.Sunday

/// returns an updated weekly availibility with the the availibility for the given day changed
let updateEmployeeAvailibility day availbility (wAve: WeeklyAvailibility)  =
     match day with 
     | Monday -> {wAve with Monday = availbility}
     | Tuesday -> {wAve with Tuesday = availbility}
     | Wednesday  -> {wAve with Wednesday = availbility}
     | Thursday  -> {wAve with Thursday = availbility}
     | Friday  -> {wAve with Friday = availbility}
     | Saturday  -> {wAve with Saturday = availbility}
     | Sunday -> {wAve with Sunday = availbility}

/// returns a bool indicating if the employee is availible for the given day and shift
let isEmployeeAvailible  day (start, stop) (employee:Employee<'id>) =
     let shiftLength = stop - start
     let {maxHours = maxHours; maxDays = maxDays} = employee
     let availibility = getEmployeeAvailibility day employee
     match shiftLength with
     /// return false when the employee has no more unscheduled days left
     | _ when maxDays <= 0 -> false
     /// when the employee has enough availible hours
     | l when maxHours >= l ->
         match availibility with
         | Open -> true
         | Unavailible -> false
         | Partial range -> 
          match range with
          | (x, y) when x <= start && y >= stop -> true
          | _ -> false
     | _ -> false

/// returns a tuple of assigned shifts and updated list of employees given a day, staff and list of unassigned shifts
let rec assignShifts day staff unassigned  assigned =
     match unassigned with
     | head :: tail ->
          match List.tryFind (isEmployeeAvailible day head ) staff with
          | Some employee ->
               /// reduce employees maxdays and maxhours
               let maxDays =  employee.maxDays - 1
               let maxHours = employee.maxHours - (snd head - fst head)
               
               let updatedEmployee = {
                    employee with 
                         availibility = updateEmployeeAvailibility day Unavailible employee.availibility
                         maxHours = maxHours
                         maxDays = maxDays
               }

               let updatedAssigned = assigned @ [(Some employee._id, head)]
              /// put employess at the end of the lst once used so that priority is given to employees not scheduled that day yet
               let updatedStaff = (List.filter (fun e -> e._id <> employee._id ) staff) @ [updatedEmployee]
               assignShifts day updatedStaff  tail updatedAssigned

          /// if no one is availible to take the shift
          | None -> assignShifts day staff tail (assigned @ [(None, head)])
     | [] ->
          /// once done make all employees unavailible for that day, this is so that this day no longer affects the employees schedule complexity when remaining day are scheduled
           (assigned, List.map (fun e -> {e with availibility = updateEmployeeAvailibility day Unavailible e.availibility}) staff)
           
/// use shifts and day and staff list to return a tuple of workday, updatedEmployee list
let scheduleWorkDay (l, day, s) staff =
     /// sort the staff by most complex scheduling 
     let orderedStaff = List.sortBy (fun e -> -1.0 * calcEmployeeComplexity e.availibility e.rank) staff
     let (shifts, updatedStaff) = assignShifts day orderedStaff s []
   
     ({laborDistribution = l; shifts = shifts}, updatedStaff)

/// given a schedule and a day and a workday return a schedule with the updated workday
let updateSchedule day (schedule: Schedule<'id>) workDay =
     match day with 
     | Monday -> {schedule with Monday = Some workDay}
     | Tuesday -> {schedule with Tuesday = Some workDay}
     | Wednesday  -> {schedule with Wednesday = Some workDay}
     | Thursday  -> {schedule with Thursday = Some workDay}
     | Friday  -> {schedule with Friday = Some workDay}
     | Saturday  -> {schedule with Saturday = Some workDay}
     | Sunday -> {schedule with Sunday = Some workDay}

/// return an updated schedule payload with the given day scheduled 
let updateSchedulePayload state (l, day, shifts) = 
      let (w, e) = scheduleWorkDay (l, day, shifts) state.employeePool 
      {schedule = updateSchedule day state.schedule w; employeePool = e}

/// returns  the availibility of the employees for the given day
let getAvailibility day employees  =
     List.map (fun e -> getEmployeeAvailibility day e) employees

/// returns a schedule payload with the schedule data applied 
let rec createSchedule  state data =
     let {employeePool = staff} = state
     /// returns the scheduling complexity for a work day 
     let sort (l, day, _) = 
          let availbility = getAvailibility day staff
          calcDayComplexity l availbility

     /// sort the schedule data from most to least complex day
     let reOrdered = List.sortBy sort data
   
     match reOrdered with
     | head :: tail -> 
          /// update the payload with the most complex day scheduled
          let newState = updateSchedulePayload state head
          /// run createSchedule with the new state and remaining schedule data
          /// Note: this function is called recursivley with updated employee data so the schedule complexity for employees and days are recalculated with each function call 
          createSchedule newState tail
     /// no more data to be proccessed so return the state
     | [] -> state

/// given labor requirements and staff returns the schedule payload
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
     /// produce the shifts for each workday 
     |> List.map (fun (l, day) -> (l, day, getShifts l [])) 
     |> createSchedule {schedule = initialS; employeePool = staff;}
     
    