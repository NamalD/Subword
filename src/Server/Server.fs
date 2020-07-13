open System.IO
open System.Threading.Tasks
open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared
open Library


let tryGetEnv key = 
    match Environment.GetEnvironmentVariable key with
    | x when String.IsNullOrWhiteSpace x -> None 
    | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let dictionary = Dictionary.read

let webApp = router {
    get "/api/init" (fun next ctx ->
        task {
            return! json "" next ctx
        })

    getf "/api/search/%s"
        (fun text ->
            let cleanText = text.Replace(" ", "")
            (fun next ctx ->
                task {
                    return! json (SubWords.generateSubWords text dictionary) next ctx
                })
        )
}

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
}

run app
