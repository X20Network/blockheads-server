namespace CubeHeadsServer

open System
open System.Web
open System.Net.Http

module Moralis =

    [<CLIMutable>]
    type TimeStamp = {
        iso: date
    }
    
    [<CLIMutable>]
    type MatchEvent = {
        objectId: string    
        log_index: int
        transaction_hash: string
        _created_at: date
        _updated_at: date
        address: string
        block_hash: string
        block_number: int64
        confirmed: bool
        cubeletIndex: int
        cubetrophyIndex: int
        players: int[]
        randomSeed: string
        scoreB: int
        scoreR: int
        transaction_index: int
        userB: string
        userR: string
        block_timestamp: TimeStamp
    }

    [<CLIMutable>]
    type Results<'T> = {
        results: 'T[]
    }

    module private MatchEvent =

        open Data
        
        let toResult (event :MatchEvent) =
            { userB = event.userB
              userR = event.userR
              scoreB = event.scoreB
              scoreR = event.scoreR
              teamB = event.players |> Array.take 4
              teamR = event.players |> Array.skip 4
              trophyIndex = event.cubetrophyIndex
              cubeletIndex = event.cubeletIndex
              date = event.block_timestamp.iso }

    let getResultsForAccount (account :string) =
        task {
            let account = account.ToLower()
            use client = new HttpClient()
            let url = "https://eqvine8lhmho.usemoralis.com:2053/server/classes/matchEvent"
            let where = HttpUtility.UrlEncode(sprintf "{\"$or\": [{\"userR\": \"%s\"}, {\"userB\": \"%s\"}]}" account account)
            let request = new HttpRequestMessage(HttpMethod.Get, url + "?where=" + where)
            request.Headers.Add("X-Parse-Application-Id", "xM24pQrlxdF6qlE6cTNfb1vvztGUCRV98APSyytQ")
            request.Headers.Add("X-Parse-Master-Key", "mMnh4gsodOoq9QTwG8l9uAW2fmcDNHDRbQFiXdqG")
            let! response = client.SendAsync(request)
            let! str = response.Content.ReadAsStringAsync()
            let results = Newtonsoft.Json.JsonConvert.DeserializeObject<Results<MatchEvent>>(str)
            return results.results |> Array.sortByDescending (fun r -> r.block_number) |> Array.map MatchEvent.toResult
        }

    let getResultsAfter (blockNumber :int64) =
        task {
            use client = new HttpClient()
            let url = "https://eqvine8lhmho.usemoralis.com:2053/server/classes/matchEvent"
            let where = HttpUtility.UrlEncode(sprintf "{\"block_number\": {\"$gt\": %i }}" blockNumber)
            let request = new HttpRequestMessage(HttpMethod.Get, url + "?where=" + where)
            request.Headers.Add("X-Parse-Application-Id", "xM24pQrlxdF6qlE6cTNfb1vvztGUCRV98APSyytQ")
            request.Headers.Add("X-Parse-Master-Key", "mMnh4gsodOoq9QTwG8l9uAW2fmcDNHDRbQFiXdqG")
            let! response = client.SendAsync(request)
            let! str = response.Content.ReadAsStringAsync()
            let results = Newtonsoft.Json.JsonConvert.DeserializeObject<Results<MatchEvent>>(str)
            if results.results = null then 
                return 0L, [||]
            else
                let maxbn = 
                    if results.results.Length = 0 then 0L else (results.results |> Array.maxBy (fun r -> r.block_number)).block_number
                return maxbn, results.results |> Array.sortBy (fun r -> r.block_number) |> Array.map MatchEvent.toResult
        }