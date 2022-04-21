namespace CubeHeadsServer

module Ethereum =

    open System.IO
    open System.Reflection
    open System.Numerics
    open System.Threading.Tasks
    open Nethereum.Web3
    open Nethereum.Web3.Accounts
    open Nethereum.Hex.HexTypes
    open Nethereum.RPC.Fee1559Suggestions
    open Nethereum.ABI.FunctionEncoding.Attributes
    open Nethereum.RPC.Eth.DTOs
    open Nethereum.RPC.Accounts
    open Nethereum.Contracts
    open Data

    [<Event("Tournament")>]
    type TournamentEventDTO() =
        [<Parameter("uint256", "tournamentId", 1)>]
        member val TournamentId = BigInteger(0) with get, set
        interface IEventDTO

    let infuraUrl = "https://kovan.infura.io/v3/2ba89d75124a4e0baf363346d70820fb"

    let chainId = 42

    let cubeballAbi =  
        let assembly = Assembly.GetExecutingAssembly()
        let name = "CubeHeadsServer.cubeball-abi.json"
        use stream = assembly.GetManifestResourceStream(name)
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    let cubeletsAbi =  
        let assembly = Assembly.GetExecutingAssembly()
        let name = "CubeHeadsServer.cubelets-abi.json"
        use stream = assembly.GetManifestResourceStream(name)
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    let cubetrophiesAbi =  
        let assembly = Assembly.GetExecutingAssembly()
        let name = "CubeHeadsServer.cubetrophies-abi.json"
        use stream = assembly.GetManifestResourceStream(name)
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    let cubeballAddress = "0x2f11d70E8224107Ff341b18Bcd0e11e0401f5E92"

    let cubeletsAddress = "0x458fB8D564aA83BAa5e081D240645f0d35902205"

    let cubetrophiesAddress = "0x7BfA2F8Fce71fD2BC0a559b2BF1C62b366e157bc"

    let private adminAccountJson = File.ReadAllText(Authentication.adminWalletPath)

    let adminAccount password = Account.LoadFromKeyStore(adminAccountJson, password, chainId = BigInteger(chainId))

    let web3 (adminAccount :IAccount) = new Web3(adminAccount, infuraUrl)

    let cubeballContract (web3 :Web3) = web3.Eth.GetContract(cubeballAbi, cubeballAddress)

    let cubeletsContract (web3: Web3) = web3.Eth.GetContract(cubeletsAbi, cubeletsAddress)

    let cubetrophiesContract (web3 :Web3) = web3.Eth.GetContract(cubetrophiesAbi, cubetrophiesAddress)

    let processTxs (web3 :Web3) gasIncreasePeriodMin maxTimeFactor (pollDelay :int) (tinputs :TransactionInput[]) =
        let one = BigInteger(1)
        let rec pollTxs feeFactor (startTime :date) (txHashesAndReceipts :(string * TransactionReceipt option)[])=
            if txHashesAndReceipts |> Array.forall (function | _, Some _ -> true | _ -> false) then
                // if all txs have receipts (ie have been confirmed, then just return them)
                log TransactionsConfirmed (TransactionsConfirmedData(txHashesAndReceipts |> Array.map fst))
                task { return txHashesAndReceipts }
            else
                if (System.DateTime.UtcNow - startTime).TotalMinutes > gasIncreasePeriodMin then
                    // if the time has been longer than gas increase period then process the txs again with higher gas prices
                    processTxs (feeFactor + one)
                else
                    task {
                        // wait for poll delay
                        do! Task.Delay pollDelay 
                        // check for the tx receipts
                        let! receipts = 
                            txHashesAndReceipts 
                                |> Array.map (fun (hash, receipt) ->
                                    match receipt with
                                    | Some _ -> async { return hash, receipt }
                                    | None -> 
                                        async {
                                            let! receipt = web3.Eth.Transactions.GetTransactionReceipt.SendRequestAsync(hash) |> Async.AwaitTask
                                            if receipt = null then
                                                return hash, None
                                            else
                                                return hash, Some receipt })
                                |> Async.Parallel
                                |> Async.StartAsTask
                        return! pollTxs feeFactor startTime receipts }
        and processTxs feeFactor =
            task {
                let feeStrategy = web3.FeeSuggestion.GeTimePreferenceFeeSuggestionStrategy()
                feeStrategy.MaxTimeFactor <- maxTimeFactor
                let! fee = feeStrategy.SuggestFeeAsync()
                fee.MaxFeePerGas <- System.Nullable<BigInteger>((fee.MaxFeePerGas.Value + ((fee.MaxFeePerGas.Value * feeFactor) / BigInteger(10))))
                fee.MaxPriorityFeePerGas <- System.Nullable<BigInteger>((fee.MaxPriorityFeePerGas.Value + ((fee.MaxPriorityFeePerGas.Value * feeFactor) / BigInteger(10))))
                // set the fees of the transactions
                tinputs |> Array.iter (fun tinput ->
                    tinput.MaxPriorityFeePerGas <- HexBigInteger(fee.MaxPriorityFeePerGas.Value)
                    tinput.MaxFeePerGas <- HexBigInteger(fee.MaxFeePerGas.Value))
                // send the transactions and collect the tx hashes
                let! txHashes = 
                    tinputs |> Array.map (fun tinput -> 
                        task {
                            let! signed = web3.TransactionManager.SignTransactionAsync(tinput)
                            return! web3.Eth.Transactions.SendRawTransaction.SendRequestAsync(signed) } |> Async.AwaitTask)
                            |> Async.Parallel
                            |> Async.StartAsTask
                let txHashesAndReceipts = txHashes |> Array.map (fun hash -> hash, None)
                log SentTransactions (SentTransactionsData (feeFactor, txHashes))
                // poll the txs to see if they have been confirmed
                return! pollTxs feeFactor (System.DateTime.UtcNow) txHashesAndReceipts
            }
        processTxs (BigInteger(0))

    let processTournament adminPassword (revealsLimit :int) (stepsLimit :int) (matchesLimit :int)=
        let adminAccount = adminAccount adminPassword
        let web3 = web3 adminAccount
        let cubeballContract = cubeballContract web3
        let rec run (fromBlock :HexBigInteger) =
            // create the tx input
            let tinput = 
                cubeballContract
                    .GetFunction("processTournament")
                    .CreateTransactionInput(adminAccount.Address, revealsLimit, stepsLimit, matchesLimit, 0, 0)
            task {
                let! nonce = adminAccount.NonceService.GetNextNonceAsync()
                tinput.Nonce <- nonce
                //let! gas = cubeballContract.GetFunction("processTournament").EstimateGasAsync(revealsLimit, stepsLimit, matchesLimit, 0, 0)
                let gas = HexBigInteger(BigInteger(5000000))
                tinput.Gas <- gas
                let! receipts = processTxs web3 25.0 100 10000 [|tinput|]
                match receipts with
                | [| _, Some receipt |] -> 
                    let events = receipt.DecodeAllEvents<TournamentEventDTO>()
                    if events.Count >= 1 then
                        let event = events[0].Event
                        log ProcessTournamentEnd (ProcessTournamentEndData event.TournamentId)
                    else
                        return! run fromBlock
                | _ -> return failwith "transactions not processed" }
        task {
            let! currentBlock = web3.Eth.Blocks.GetBlockNumber.SendRequestAsync()
            log ProcessTournament (ProcessTournamentData { startBlock = currentBlock.Value })
            return! run currentBlock
        }

    let revealTeams adminPassword gamePeriod =
        let adminAccount = adminAccount adminPassword
        let web3 = web3 adminAccount
        let cubeballContract = cubeballContract web3
        let gameDate = Utils.getGameDateForReveals date.UtcNow gamePeriod
        let teams = Data.getTeamsForDate gameDate |> Seq.toArray
        let one = BigInteger(1)
        log RevealTeams (RevealTeamsData { gameDate = gameDate; teams = teams })
        task {
            let! nonce = adminAccount.NonceService.GetNextNonceAsync()
            let tinputs, _ = 
                teams |> Array.mapFold (fun (nonce :BigInteger) team -> 
                    let tinput = 
                        cubeballContract
                            .GetFunction("revealTeams")
                            .CreateTransactionInput(adminAccount.Address, team.account, team.team, HexBigInteger(team.salt).ToHexByteArray())
                    tinput.Nonce <- HexBigInteger(nonce)
                    (team, tinput), nonce + one) nonce.Value
            let! _ =
                tinputs |> Array.map (fun (team, tinput) ->
                    task {
                        //let! gas = 
                        //    cubeballContract
                        //        .GetFunction("revealTeams")
                        //        .EstimateGasAsync(team.account, team.team, HexBigInteger(team.salt).ToHexByteArray())
                        let gas = HexBigInteger(BigInteger(5000000))
                        tinput.Gas <- gas
                    } |> Async.AwaitTask)
                        |> Async.Parallel
                        |> Async.StartAsTask
            let tinputs = tinputs |> Array.map snd
            let! receipts = processTxs web3 25.0 100 10000 tinputs
            return Array.zip teams receipts |> Array.map (fun (t, (h, r)) -> t, h, r)
        }

    let getCubeletTokenUri adminPassword (index :int) :Task<int * string> =
        let adminAccount = adminAccount adminPassword
        let web3 = web3 adminAccount
        let cubeletsContract = cubeletsContract web3
        task {
            let! uri = 
                cubeletsContract
                    .GetFunction("tokenURIhidden")
                    .CallAsync(index)
            return (index, uri) }

    let getCubetrophyTokenUri adminPassword (index :int) : Task<int * string> =
        let adminAccount = adminAccount adminPassword
        let web3 = web3 adminAccount
        let cubetrophiesContract = cubeletsContract web3
        task {
            let! uri = 
                cubetrophiesContract
                    .GetFunction("tokenURIhidden")
                    .CallAsync(index)
            return (index, uri) }