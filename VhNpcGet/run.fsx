#r "System.Net.Http"
#r "Newtonsoft.Json"

open System.Net
open System.Net.Http
open Newtonsoft.Json

type Npc = {
    name: string
}

type Named = {
    name: string
}

let samples = "test1,test2,test3,test4,test5"

let getName (xs : string) =
    let rng = new System.Random()
    let split = xs.Split(',')
    split.[rng.Next(split.Length)]

let Run(req: HttpRequestMessage, log: TraceWriter) = 
    async {
        log.Info(sprintf "F# HTTP Trigger function began processing a request")

        let retName = getName samples

        return req.CreateResponse(HttpStatusCode.OK, { name = retName })
    } |> Async.RunSynchronously

// let Run(req: HttpRequestMessage, log: TraceWriter) =
//     async {
//         log.Info(sprintf 
//             "F# HTTP trigger function processed a request.")

//         // Set name to query string
//         let name =
//             req.GetQueryNameValuePairs()
//             |> Seq.tryFind (fun q -> q.Key = "name")

//         match name with
//         | Some x ->
//             return req.CreateResponse(HttpStatusCode.OK, "Hello " + x.Value);
//         | None ->
//             let! data = req.Content.ReadAsStringAsync() |> Async.AwaitTask

//             if not (String.IsNullOrEmpty(data)) then
//                 let named = JsonConvert.DeserializeObject<Named>(data)
//                 return req.CreateResponse(HttpStatusCode.OK, "Hello " + named.name);
//             else
//                 return req.CreateResponse(HttpStatusCode.BadRequest, "Specify a Name value");
//     } |> Async.RunSynchronously
