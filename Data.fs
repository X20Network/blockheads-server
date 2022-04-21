namespace CubeHeadsServer

open FSharp.Azure.Storage.Table
open Microsoft.Azure.Cosmos.Table
open Nethereum.Hex.HexTypes
open System.Numerics

module Data =

    open System.Text.Json

    type private TeamRaw =
        { [<PartitionKey>] account: string
          [<RowKey>] invertedTicks: string
          teamJson: string
          teamHash: string
          salt: string }

    type Team =
        { date: date
          account: string
          team: int[]
          teamHash: string
          salt: string }

    type Result =
        { userB: string
          userR: string
          scoreB: int
          scoreR: int
          teamB: int[]
          teamR: int[]
          trophyIndex: int
          cubeletIndex: int
          date: date }

    type LogEntryRaw =
        { [<PartitionKey>] action: string
          [<RowKey>] invertedTicks: string
          data: string }

    type TokenCacheRaw =
        { [<PartitionKey>] user: string
          [<RowKey>] invertedTicks: string
          tokenIndex: int
          tokenUri1: string
          tokenUri2: string
          tokenUri3: string
          tokenUri4: string
          tokenUri5: string
          tokenUri6: string
          tokenUri7: string
          tokenUri8: string }

    type NftRaw =
        { [<PartitionKey>] name: string
          [<RowKey>] index: string
          tokenUri1: string
          tokenUri2: string
          tokenUri3: string
          tokenUri4: string
          tokenUri5: string
          tokenUri6: string
          tokenUri7: string
          tokenUri8: string }

    type Nft =
        { index: int
          tokenUri: string
          name: string }

    type TokenCache =
        { date: date
          user: string
          tokenIndex: int
          tokenUri: string }

    type ProcessTournamentData =
        { startBlock: BigInteger }

    type RevealTeamsData =
        { gameDate: date
          teams: Team[] }

    type LogAction =
        | RevealTeams
        | ProcessTournament
        | ProcessTournamentEnd
        | SentTransactions
        | TransactionsConfirmed
        | SyncNftCache

    type LogData =
        | Error of exn
        | RevealTeamsData of RevealTeamsData
        | ProcessTournamentData of ProcessTournamentData
        | ProcessTournamentEndData of BigInteger
        | SentTransactionsData of BigInteger * (string)[]
        | TransactionsConfirmedData of string[]
        | SyncNftCacheData of int64

    type LogEntry =
        { date: date
          action: LogAction
          data: LogData }

    let private account = CloudStorageAccount.Parse "DefaultEndpointsProtocol=https;AccountName=cubeheads;AccountKey=M60tA02Zrs+mx43G9Dua2B/skIl9KRADgPcV4WeHay1TC1e/FA6kTAhZ+HcDByNgKEcRmCaNa4KoeeV1sJ+3KA==;EndpointSuffix=core.windows.net"

    let private tableClient = account.CreateCloudTableClient()

    let private fromTeamTable q = fromTable tableClient "teams" q

    let private inTeamTable team = inTable tableClient "teams" team

    let private fromActionTable q = fromTable tableClient "actions" q

    let private inActionTable action = inTable tableClient "actions" action

    let private fromCubeletsTable q = fromTable tableClient "cubelets" q

    let private fromCubeletsTableAsync q = fromTableAsync tableClient "cubelets" q
    
    let private inCubeletsTable tokenCache = inTable tableClient "cubelets" tokenCache

    let private fromTrophiesTable q = fromTable tableClient "trophies" q

    let private fromTrophiesTableAsync q = fromTableAsync tableClient "trophies" q

    let private inTrophiesTable tokenCache = inTable tableClient "trophies" tokenCache

    let private fromNftsTableAsync q = fromTableAsync tableClient "nfts" q

    let private inNftsTable nft = inTable tableClient "nfts" nft

    let private toInvertedTicks (date :System.DateTime) = System.String.Format("{0:D19}", System.DateTime.MaxValue.Ticks - date.ToUniversalTime().Ticks)
    
    let private fromInvertedTicks ticks = new System.DateTime(System.DateTime.MaxValue.Ticks - System.Int64.Parse(ticks))
    
    let private toTeam (raw :TeamRaw) =
        { date = fromInvertedTicks raw.invertedTicks
          account = raw.account 
          team = JsonSerializer.Deserialize(raw.teamJson)
          teamHash = raw.teamHash
          salt = raw.salt }

    let private fromTeam (team :Team) =
        { invertedTicks = toInvertedTicks team.date
          account = team.account
          teamJson = JsonSerializer.Serialize team.team
          teamHash = team.teamHash
          salt = team.salt }

    let private logActionName logAction =
        match logAction with
        | RevealTeams -> "RevealTeams"
        | ProcessTournament -> "ProcessTournament"
        | ProcessTournamentEnd -> "ProcessTournamentEnd"
        | SentTransactions -> "SentTransactions"
        | TransactionsConfirmed -> "TransactionsConfirmed"
        | SyncNftCache -> "SyncNftCache"

    let private logActionFromName name =
        match name with
        | "RevealTeams" -> RevealTeams
        | "ProcessTournament" -> ProcessTournament
        | "ProcessTournamentEnd" -> ProcessTournamentEnd
        | "SentTransactions" -> SentTransactions
        | "TransactionsConfirmed" -> TransactionsConfirmed
        | "SyncNftCache" -> SyncNftCache

    let private fromLogEntry (logEntry :LogEntry) =
        { invertedTicks = toInvertedTicks logEntry.date
          action = logEntry.action |> logActionName
          data = FSharp.Json.Json.serialize logEntry.data }

    let private toLogEntry (raw :LogEntryRaw) =
        { date = fromInvertedTicks raw.invertedTicks
          action = logActionFromName raw.action
          data = FSharp.Json.Json.deserialize raw.data }

    let private toTokenCache (raw :TokenCacheRaw) =
        { date = fromInvertedTicks raw.invertedTicks
          tokenIndex = raw.tokenIndex
          tokenUri = raw.tokenUri1 + raw.tokenUri2 + raw.tokenUri3 + raw.tokenUri4 + raw.tokenUri5 + raw.tokenUri6 + raw.tokenUri7 + raw.tokenUri8
          user = raw.user }

    let private toNft (raw: NftRaw) =
        { name = raw.name
          index = raw.index |> System.Int32.Parse
          tokenUri = raw.tokenUri1 + raw.tokenUri2 + raw.tokenUri3 + raw.tokenUri4 + raw.tokenUri5 + raw.tokenUri6 + raw.tokenUri7 + raw.tokenUri8 }

    let getLatestTeam account =
        Query.all<TeamRaw>
            |> Query.where <@ fun g s -> s.PartitionKey = account @>
            |> Query.take 1
            |> fromTeamTable
            |> Seq.tryHead
            |> Option.map (fst >> toTeam)

    let getTeam account date =
        Query.all<TeamRaw>
            |> Query.where <@ fun g s -> s.RowKey = (toInvertedTicks date) && s.PartitionKey = account @>
            |> fromTeamTable
            |> Seq.tryHead
            |> Option.map (fst >> toTeam)

    let getTeamsForDate date =
        Query.all<TeamRaw>
            |> Query.where <@ fun g s -> s.RowKey = toInvertedTicks date @>
            |> fromTeamTable
            |> Seq.map (fst >> toTeam)

    let saveTeam gamePeriod revealPeriod (team :Team) =
        let existing = 
            Query.all<TeamRaw>
            |> Query.where <@ fun g s -> s.RowKey = (toInvertedTicks <| Utils.getGameDateForCommitment team.date gamePeriod revealPeriod) && s.PartitionKey = team.account @>
            |> fromTeamTable
            |> Seq.tryHead
        match existing with
        | Some (raw, metadata) ->
            ({ raw with teamJson = JsonSerializer.Serialize team.team }, metadata.Etag) |> Replace |> inTeamTable
        | None ->
            { invertedTicks = toInvertedTicks <| Utils.getGameDateForCommitment team.date gamePeriod revealPeriod
              account = team.account
              teamJson = JsonSerializer.Serialize team.team
              teamHash = team.teamHash
              salt = team.salt } |> Insert |> inTeamTable

    let getLatestLogEntry action =
        let actionStr = action |> logActionName
        Query.all<LogEntryRaw>
            |> Query.where <@ fun g s -> s.PartitionKey = actionStr @>
            |> Query.take 1
            |> fromActionTable
            |> Seq.tryHead
            |> Option.map (fst >> toLogEntry)

    let saveLogEntry logEntry = logEntry |> fromLogEntry |> Insert |> inActionTable

    let log action data =
        { date = date.UtcNow
          data = data
          action = action } |> saveLogEntry |> ignore

    let saveTrophyCache (tokenCache :TokenCache) =
        let split length index =
            let start = length * index
            if tokenCache.tokenUri.Length > start then tokenCache.tokenUri.Substring(start, min (tokenCache.tokenUri.Length - start) length) else ""
        { invertedTicks = toInvertedTicks tokenCache.date
          user = tokenCache.user
          tokenIndex = tokenCache.tokenIndex
          tokenUri1 = split 32000 0
          tokenUri2 = split 32000 1
          tokenUri3 = split 32000 2
          tokenUri4 = split 32000 3
          tokenUri5 = split 32000 4
          tokenUri6 = split 32000 5
          tokenUri7 = split 32000 6
          tokenUri8 = split 32000 7} |> InsertOrReplace |> inTrophiesTable

    let saveCubeletCache (tokenCache :TokenCache) =
        let split length index =
            let start = length * index
            if tokenCache.tokenUri.Length > start then tokenCache.tokenUri.Substring(start, min (tokenCache.tokenUri.Length - start) length) else ""
        { invertedTicks = toInvertedTicks tokenCache.date
          user = tokenCache.user
          tokenIndex = tokenCache.tokenIndex
          tokenUri1 = split 32000 0
          tokenUri2 = split 32000 1
          tokenUri3 = split 32000 2
          tokenUri4 = split 32000 3
          tokenUri5 = split 32000 4
          tokenUri6 = split 32000 5
          tokenUri7 = split 32000 6
          tokenUri8 = split 32000 7 } |> InsertOrReplace |> inCubeletsTable

    let saveNft (nft :Nft) =
        let split length index =
            let start = length * index
            if nft.tokenUri.Length > start then nft.tokenUri.Substring(start, min (nft.tokenUri.Length - start) length) else ""
        { name = nft.name
          index = nft.index.ToString()
          tokenUri1 = split 32000 0
          tokenUri2 = split 32000 1
          tokenUri3 = split 32000 2
          tokenUri4 = split 32000 3
          tokenUri5 = split 32000 4
          tokenUri6 = split 32000 5
          tokenUri7 = split 32000 6
          tokenUri8 = split 32000 7 } |> InsertOrReplace |> inNftsTable

    let getNft name (index :int) =
        async {
            let indexStr = index.ToString()
            let! nfts =
                Query.all<NftRaw>
                    |> Query.where <@ fun g s -> s.PartitionKey = name && s.RowKey = indexStr @>
                    |> Query.take 1
                    |> fromNftsTableAsync
            return nfts |> Seq.tryHead |> Option.map (fst >> toNft) }

    let getTrophiesForUser (user :string) =
        let user = user.ToLower()
        async {
            let! results = 
                Query.all<TokenCacheRaw>
                    |> Query.where <@ fun g s -> s.PartitionKey = user @>
                    |> fromTrophiesTableAsync
            return results
                |> Seq.map (fst >> toTokenCache) }

    let getCubeletsForUser (user :string) =
        let user = user.ToLower()
        async {
            let! results =
                Query.all<TokenCacheRaw>
                |> Query.where <@ fun g s -> s.PartitionKey = user @>
                |> fromCubeletsTableAsync
            return results
                |> Seq.map (fst >> toTokenCache) }