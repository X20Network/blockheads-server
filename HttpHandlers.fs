namespace CubeHeadsServer

module HttpHandlers =

    open Microsoft.AspNetCore.Http
    open FSharp.Control.Tasks
    open System.Threading.Tasks
    open Giraffe
    open Nethereum.Signer
    open Microsoft.Extensions.Configuration
    open System.Net.Http

    let getAdminPassword (config :IConfiguration) = config["AdminWalletPassword"]

    let getGamePeriod (config :IConfiguration) = 
        System.TimeSpan.ParseExact(config["GamePeriod"], "hh\:mm", System.Globalization.CultureInfo.InvariantCulture)

    let getRevealPeriod (config :IConfiguration) =
        System.TimeSpan.ParseExact(config["RevealPeriod"], "hh\:mm", System.Globalization.CultureInfo.InvariantCulture)

    let getRevealsLimit (config :IConfiguration) = System.Int32.Parse(config["RevealsLimit"])

    let getStepsLimit (config :IConfiguration) = System.Int32.Parse(config["StepsLimit"])

    let getMatchesLimit (config :IConfiguration) = System.Int32.Parse(config["MatchesLimit"])

    let handleGetAuthenticationMessage =
        fun (next :HttpFunc) (ctx :HttpContext) ->
            task {
                let account = ctx.TryGetQueryStringValue("account")
                match account with
                | None -> return failwith "account parameter not provided"
                | Some account ->
                    return! text (Authentication.getMessage account) next ctx
            }

    let handleGetTeam =
        fun (next :HttpFunc) (ctx :HttpContext) ->
            task {
                let msg = ctx.TryGetQueryStringValue("message")
                let account = ctx.TryGetQueryStringValue("account")
                match msg, account with
                | Some msg, Some account ->
                    let signer = new EthereumMessageSigner()
                    let maccount = signer.EcRecover(System.Text.Encoding.UTF8.GetBytes(Authentication.getMessage account), msg)
                    if account = maccount then
                        match Data.getLatestTeam account with
                        | None -> return! setStatusCode 404 next ctx
                        | Some team -> return! json team next ctx
                    else return failwith <| "signed message account does not match account: " + maccount + ", " + account
                | _ -> return failwith "no signed message or account supplied"
            }

    let handlePostTeam =
        fun (next :HttpFunc) (ctx :HttpContext) ->
            task {
                let msg = ctx.TryGetQueryStringValue("message")
                let account = ctx.TryGetQueryStringValue("account")
                match msg, account with
                | Some msg, Some account ->
                    let signer = new EthereumMessageSigner()
                    let maccount = signer.EcRecover(System.Text.Encoding.UTF8.GetBytes(Authentication.getMessage account), msg)
                    if account = maccount then
                        let! team = ctx.BindJsonAsync()
                        let gamePeriod = getGamePeriod (ctx.GetService<IConfiguration>())
                        let revealPeriod = getRevealPeriod (ctx.GetService<IConfiguration>())
                        let result = Data.saveTeam gamePeriod revealPeriod team
                        match result.HttpStatusCode with
                        | 204 -> return! setStatusCode 200 next ctx
                        | code -> return failwith <| "an error occurred saving team: " + code.ToString()
                    else return failwith "signed message account does not match account"
                | _ -> return failwith "no signed message or account supplied" 
            }

    let handleGetResults =
        fun (next :HttpFunc) (ctx :HttpContext) ->
            task {
                let account = ctx.TryGetQueryStringValue("account")
                match account with
                | None -> return failwith "no account supplied"
                | Some account ->
                    let! results = Moralis.getResultsForAccount account
                    return! json results next ctx
            }

    let handleGetTrophies =
        fun (next :HttpFunc) (ctx :HttpContext) ->
            task {
                let account = ctx.TryGetQueryStringValue("account")
                match account with
                | None -> return failwith "no account supplied"
                | Some account ->
                    let! results = Data.getTrophiesForUser account |> Async.StartAsTask
                    return! json results next ctx
            }

    let handleGetCubelets =
        fun (next :HttpFunc) (ctx :HttpContext) ->
            task {
                let account = ctx.TryGetQueryStringValue("account")
                match account with
                | None -> return failwith "no account supplied"
                | Some account ->
                    let! results = Data.getCubeletsForUser account |> Async.StartAsTask
                    return! json results next ctx
            }

    let handleGetTrophy =
        fun (next: HttpFunc) (ctx :HttpContext) ->
            task {
                let indexStr = ctx.TryGetQueryStringValue("index")
                match indexStr with
                | None -> return failwith "no index supplied"
                | Some indexStr ->
                    match System.Int32.TryParse indexStr with
                    | (false, _) -> return failwith "index invalid"
                    | (true, index) -> 
                        let! nft = Data.getNft "trophy" index
                        match nft with
                        | None -> return! json null next ctx
                        | Some nft -> return! json nft next ctx 
            }

    let handleGetCubelet =
        fun (next: HttpFunc) (ctx :HttpContext) ->
            task {
                let indexStr = ctx.TryGetQueryStringValue("index")
                match indexStr with
                | None -> return failwith "no index supplied"
                | Some indexStr ->
                    match System.Int32.TryParse indexStr with
                    | (false, _) -> return failwith "index invalid"
                    | (true, index) -> 
                        let! nft = Data.getNft "cubelet" index
                        match nft with
                        | None -> return! json null next ctx
                        | Some nft -> return! json nft next ctx 
            }

    let mutable private revealsTask :(date * Task<(Data.Team * string * Nethereum.RPC.Eth.DTOs.TransactionReceipt option)[]>) Option = None

    let mutable private tournamentTask :Task<unit> Option = None

    let handleGetProcessReveals =
        fun (next :HttpFunc) (ctx :HttpContext) ->
            task {
                let adminPassword = getAdminPassword (ctx.GetService<IConfiguration>())
                let gamePeriod = getGamePeriod (ctx.GetService<IConfiguration>())
                let pt = Ethereum.revealTeams adminPassword gamePeriod
                revealsTask <- Some (Utils.getGameDateForReveals date.UtcNow gamePeriod, pt)
                return! text "Processing" next ctx
            }

    let handleGetRevealState =
        fun (next :HttpFunc) (ctx :HttpContext) ->
            task {
                match revealsTask with
                | None -> return! text "Not called" next ctx
                | Some (date, pt) ->
                    match pt.Status with
                    | TaskStatus.Canceled -> return! text "task cancelled" next ctx
                    | TaskStatus.Faulted -> return! text ("task faulted: " + pt.Exception.Message) next ctx
                    | TaskStatus.Running -> return! text "task running" next ctx
                    | TaskStatus.RanToCompletion ->
                        let results = pt.Result
                        let sb = new System.Text.StringBuilder()
                        sb.AppendLine(date.ToString()) |> ignore
                        results |> Array.iter (fun (t, hash, receipt) ->
                            match receipt with
                            | None -> sb.AppendLine(t.account + ", hash: " + hash) |> ignore
                            | Some receipt -> sb.AppendLine(t.account + ", " + receipt.TransactionHash) |> ignore)
                        return! text (sb.ToString()) next ctx
                    | _ -> return! text ("task status: " + pt.Status.ToString()) next ctx
            }

    let handleGetProcessTournament =
        fun (next :HttpFunc) (ctx :HttpContext) ->
            task {
                let adminPassword = getAdminPassword (ctx.GetService<IConfiguration>())
                let revealsLimit = getRevealsLimit (ctx.GetService<IConfiguration>())
                let stepsLimit = getStepsLimit (ctx.GetService<IConfiguration>())
                let matchesLimit = getMatchesLimit (ctx.GetService<IConfiguration>())
                let pt = Ethereum.processTournament adminPassword revealsLimit stepsLimit matchesLimit
                tournamentTask <- Some pt
                return! text "Processing" next ctx
            }

    let handleGetProcessState =
        fun (next :HttpFunc) (ctx :HttpContext) ->
            task {
                match tournamentTask with
                | None -> return! text "Not called" next ctx
                | Some pt ->
                    match pt.Status with
                    | TaskStatus.Canceled -> return! text "task cancelled" next ctx
                    | TaskStatus.Faulted -> return! text ("task faulted: " + pt.Exception.Message) next ctx
                    | TaskStatus.Running -> return! text "task running" next ctx
                    | TaskStatus.RanToCompletion -> return! text "task completed" next ctx
                    | _ -> return! text ("task status: " + pt.Status.ToString()) next ctx
            }

    let handleGetDates =
        fun (next :HttpFunc) (ctx : HttpContext) ->
            task {
                let gamePeriod = getGamePeriod (ctx.GetService<IConfiguration>())
                let revealPeriod = getRevealPeriod (ctx.GetService<IConfiguration>())
                return! text ("game date for commits: " + (Utils.getGameDateForCommitment date.UtcNow gamePeriod revealPeriod).ToString() + ", game date for reveals" + (Utils.getGameDateForReveals date.UtcNow gamePeriod).ToString()) next ctx
            }