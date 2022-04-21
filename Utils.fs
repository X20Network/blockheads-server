namespace CubeHeadsServer

type date = System.DateTime

module Utils =

    open System

    let getGameDateForCommitment (date :date) gamePeriod revealPeriod =
        let mutable gameDate = date.Date
        while gameDate - revealPeriod < date do
            gameDate <- gameDate + gamePeriod
        gameDate

    let getGameDateForReveals (date :date) gamePeriod =
        let mutable gameDate = date.Date
        while gameDate < date do
            gameDate <- gameDate + gamePeriod
        gameDate