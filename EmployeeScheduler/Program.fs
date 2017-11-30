// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
(*
     THIS is an employee scheduler and it is meant to output a schedule given employee availibility, 
     REQUESTED days off, optimal employee count per hour,  take into account individual hour requirements , 
     take into account skill level,  take into account postion of employees 
     algorithm should go from scheduling less availible to more availible 
*)
open Domain 
open MongoDB.Bson
open MongoDB.Driver
open MongoDB.FSharp
Serializers.Register()

let connectionString = "mongodb://localhost"
let client = MongoClient(connectionString)
let db = client.GetDatabase("Scheduler")
let collection = db.GetCollection<Employee<BsonObjectId>> "employees" 
let id = BsonObjectId(ObjectId.GenerateNewId())



let lr: LaborRequirements =  {
          Monday = {operationHours = 8, 23; distribution = [1; 2; 2; 3;4; 4; 3; 3; 5; 6; 6; 6; 5; 5; 5;]}
          Tuesday = {operationHours = 8, 23; distribution = [1; 2; 2; 2;3; 3; 3; 3; 2; 2; 4; 5; 5; 5; 5;]}
          Wednesday = {operationHours = 8, 23; distribution = [1; 2; 2; 2; 4; 4; 3; 3; 2; 2; 5; 5; 4; 4; 4;]}
          Thursday = {operationHours = 8, 23; distribution = [1; 2; 2; 2; 4; 4; 3; 3; 2; 2; 5; 5; 4; 4; 4;]}
          Friday = {operationHours = 8, 23; distribution = [1; 2; 2; 2; 4; 4; 3; 3; 2; 2; 5; 5; 4; 4; 4;]}
          Saturday = {operationHours = 8, 23; distribution = [1; 2; 2; 2;3; 3; 3; 3; 2; 2; 4; 5; 5; 5; 5;]}
          Sunday = {operationHours = 8, 23; distribution = [1; 2; 2; 2;3; 3; 3; 3; 2; 2; 4; 5; 5; 5; 5;]}
     }

let emp = [
     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "George"; lastName = "washington"; availibility = {
            Monday = Partial (8,12)
            Tuesday = Open
            Wednesday = Open
            Thursday = Open
            Friday = Open
            Saturday= Open
            Sunday = Open
        }; desiredHours = 30; maxHours = 40; rank = 10; complexityScore = 0.0; maxNumDay = 7}
     
     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "Carl"; lastName = "wash"; availibility = {
            Monday = Open
            Tuesday = Open
            Wednesday = Open
            Thursday = Open
            Friday = Open
            Saturday= Open
            Sunday = Open
        }; desiredHours = 30; maxHours = 40; rank = 4; complexityScore = 0.0; maxNumDay = 6}

     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "Amanda"; lastName = "ngto"; availibility = {
            Monday = Partial (8,12)
            Tuesday = Open
            Wednesday = Open
            Thursday = Partial (8,15)
            Friday = Unavailible
            Saturday= Open
            Sunday = Partial (12,21)
        }; desiredHours = 30; maxHours = 25; rank = 8; complexityScore = 0.0; maxNumDay = 5}
     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "George"; lastName = "washington"; availibility = {
            Monday = Open
            Tuesday = Open
            Wednesday = Open
            Thursday = Open
            Friday = Open
            Saturday= Open
            Sunday = Open
        }; desiredHours = 30; maxHours = 40; rank = 7; complexityScore = 0.0; maxNumDay = 7}
     
     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "Carl"; lastName = "wash"; availibility = {
            Monday = Open
            Tuesday = Open
            Wednesday = Open
            Thursday = Open
            Friday = Open
            Saturday= Open
            Sunday = Open
        }; desiredHours = 30; maxHours = 40; rank = 4; complexityScore = 0.0; maxNumDay = 6}

     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "Amanda"; lastName = "ngto"; availibility = {
            Monday = Partial (8,12)
            Tuesday = Open
            Wednesday = Open
            Thursday = Partial (8,15)
            Friday = Open
            Saturday= Open
            Sunday = Open
        }; desiredHours = 30; maxHours = 40; rank = 8; complexityScore = 0.0; maxNumDay = 5}
     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "George"; lastName = "washington"; availibility = {
            Monday = Open
            Tuesday = Open
            Wednesday = Open
            Thursday = Open
            Friday = Open
            Saturday= Open
            Sunday = Open
        }; desiredHours = 30; maxHours = 40; rank = 7; complexityScore = 0.0; maxNumDay = 7}
     
     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "Carl"; lastName = "wash"; availibility = {
            Monday = Open
            Tuesday = Open
            Wednesday = Open
            Thursday = Open
            Friday = Open
            Saturday= Open
            Sunday = Open
        }; desiredHours = 30; maxHours = 40; rank = 4; complexityScore = 0.0; maxNumDay = 6}

     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "Amanda"; lastName = "ngto"; availibility = {
            Monday = Partial (8,12)
            Tuesday = Open
            Wednesday = Open
            Thursday = Partial (8,15)
            Friday = Open
            Saturday= Open
            Sunday = Open
        }; desiredHours = 30; maxHours = 40; rank = 8; complexityScore = 0.0; maxNumDay = 5}
     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "Carl"; lastName = "wash"; availibility = {
            Monday = Open
            Tuesday = Open
            Wednesday = Open
            Thursday = Open
            Friday = Open
            Saturday= Open
            Sunday = Open
        }; desiredHours = 30; maxHours = 40; rank = 4; complexityScore = 0.0; maxNumDay = 6}
     { _id = BsonObjectId(ObjectId.GenerateNewId()); firstName = "Carl"; lastName = "wash"; availibility = {
            Monday = Open
            Tuesday = Open
            Wednesday = Open
            Thursday = Open
            Friday = Open
            Saturday= Open
            Sunday = Open
        }; desiredHours = 30; maxHours = 40; rank = 8; complexityScore = 0.0; maxNumDay = 6}
]
[<EntryPoint>]
let main argv = 
     //     async {
     //         do! insertOne collection { _id = id; firstName = "George"; lastName = "washington"; availibility = {
     //             Monday = Partial (8,12)
     //             Tuesday = Open
     //             Wednesday = Open
     //             Thursday = Open
     //             Friday = Open
     //             Saturday= Open
     //             Sunday = Open
     //         }; desiredHours = 30; maxHours = 40; rank = 10; complexityScore = 0.0; maxNumDay = 5}

     //         let! george = findOneById collection id  
     //         printfn "Dakota was here"
     //         printfn "%A" george
     //         return george
     //     } |> Async.RunSynchronously |> ignore
    let s = produceWorkWeek lr emp
    //let shifts = getShifts {operationHours = 8, 13; distribution = [3; 3; 6; 6; 6; 6; 8; 9; 10]} []
    //let shifts = laborRequirementsToShifts lr
    //let hour = getLastHour (0, 0) [3; 3; 6; 6; 2]
    //printfn "%A" shifts
    //printfn "%A" shifts.[0]
    //printfn "%A" shifts.[1]
    printfn "%A" s
    0 // return an integer exit code
