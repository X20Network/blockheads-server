namespace CubeHeadsServer

module Jobs =

    let revealTeams adminPassword gamePeriod = fun () -> 
        task {
            let! _ = Ethereum.revealTeams adminPassword gamePeriod
            return ()
        } |> Async.AwaitTask |> Async.RunSynchronously

    let processTournament adminPassword revealsLimit stepsLimit matchesLimit = fun () ->
        async {
            // process tournament
            let! _ = Ethereum.processTournament adminPassword revealsLimit stepsLimit matchesLimit |> Async.AwaitTask

            // wait 1 minute
            do! System.Threading.Tasks.Task.Delay 60000 |> Async.AwaitTask

            // sync NFTs
            let blockNumber =
                match Data.getLatestLogEntry Data.SyncNftCache with
                | Some { data = Data.SyncNftCacheData blockNumber } -> blockNumber
                | _ -> 0

            // get results
            let! maxbn, results = Moralis.getResultsAfter blockNumber |> Async.AwaitTask

            let trophyIndices = results |> Array.map (fun r -> r.trophyIndex) |> Array.distinct
            let cubeletIndices = results |> Array.map (fun r -> r. cubeletIndex) |> Array.distinct

            // get the token URIs
            let trophies = Array.zeroCreate trophyIndices.Length
            let cubelets = Array.zeroCreate cubeletIndices.Length

            let adminAccount = Ethereum.adminAccount adminPassword
            let web3 = Ethereum.web3 adminAccount
            let cubeletsContract = Ethereum.cubeletsContract web3
            let cubetrophiesContract = Ethereum.cubetrophiesContract web3

            for i in 0 .. trophies.Length - 1 do
                let trophyIndex = trophyIndices.[i]
                let! trophy = 
                    cubetrophiesContract
                            .GetFunction("tokenURIhidden")
                            .CallAsync(trophyIndex) |> Async.AwaitTask
                trophies.[i] <- trophyIndex, trophy

            for i in 0 .. cubelets.Length - 1 do
                let cubeletIndex = cubeletIndices.[i]
                let! cubelet = 
                    cubeletsContract
                        .GetFunction("tokenURIhidden")
                        .CallAsync(cubeletIndex) |> Async.AwaitTask
                cubelets.[i] <- cubeletIndex, cubelet

            let trophyByIndex = trophies |> Map.ofArray
            let cubeletByIndex = cubelets |> Map.ofArray

            // insert for all users
            try
                // for users
                results |> Array.iter (fun r ->
                   let opr = Data.saveCubeletCache { user = r.userB; date = r.date; tokenIndex = r.cubeletIndex; tokenUri = cubeletByIndex |> Map.find r.cubeletIndex }
                   if opr.HttpStatusCode > 300 then raise (exn())
                   let opr = Data.saveCubeletCache { user = r.userR; date = r.date; tokenIndex = r.cubeletIndex; tokenUri = cubeletByIndex |> Map.find r.cubeletIndex }
                   if opr.HttpStatusCode > 300 then raise (exn())
                   let opr = Data.saveTrophyCache { user = r.userB; date = r.date; tokenIndex = r.trophyIndex; tokenUri = trophyByIndex |> Map.find r.trophyIndex }
                   if opr.HttpStatusCode > 300 then raise (exn())
                   let opr = Data.saveTrophyCache { user = r.userR; date = r.date; tokenIndex = r.trophyIndex; tokenUri = trophyByIndex |> Map.find r.trophyIndex }
                   if opr.HttpStatusCode > 300 then raise (exn())
                   ())
                // nfts
                trophies |> Array.iter (fun (i, u) ->
                    let opr = Data.saveNft { name = "trophy"; index = i; tokenUri = u }
                    if opr.HttpStatusCode > 300 then raise (exn()))
                // cubelets
                cubelets |> Array.iter (fun (i, u) ->
                    let opr = Data.saveNft { name = "cubelet"; index = i; tokenUri = u }
                    if opr.HttpStatusCode > 300 then raise (exn()))
                // insert action
                Data.log Data.SyncNftCache (Data.SyncNftCacheData maxbn)
            with
            | ex -> ()

            return ()
        } |> Async.RunSynchronously