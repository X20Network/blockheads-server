module CubeHeadsServer.App

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Giraffe
open CubeHeadsServer.HttpHandlers
open Microsoft.Extensions.Logging.AzureAppServices
open FluentScheduler

// ---------------------------------
// Web app
// ---------------------------------

let webApp =
    choose [
        subRoute "/api"
            (choose [
                GET >=> choose [
                    route "/authenticationMessage" >=> handleGetAuthenticationMessage
                    route "/team" >=> handleGetTeam
                    route "/reveal" >=> handleGetProcessReveals
                    route "/revealState" >=> handleGetRevealState
                    route "/process" >=> handleGetProcessTournament
                    route "/processState" >=> handleGetProcessState
                    route "/dates" >=> handleGetDates
                    route "/results" >=> handleGetResults
                    route "/trophies" >=> handleGetTrophies
                    route "/cubelets" >=> handleGetCubelets
                    route "/trophy" >=> handleGetTrophy
                    route "/cubelet" >=> handleGetCubelet
                ]
                POST >=> choose [
                    route "/team" >=> handlePostTeam
                ]
            ])
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let getExnMsg (exn :exn) =
    let sb = new System.Text.StringBuilder()
    let rec get (exn :exn) =
        sb.AppendLine(exn.Message) |> ignore
        if exn.InnerException = null then ()
        else get exn.InnerException
    get exn
    sb.ToString()

let errorHandler (ex : Exception) (logger : ILogger) =
    logger.LogError(ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text (getExnMsg ex)

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder : CorsPolicyBuilder) =
    builder
        .WithOrigins(
            "http://localhost:5000",
            "https://localhost:5001",
            "http://localhost:8080",
            "https://localhost:8080")
       .AllowAnyMethod()
       .AllowAnyHeader()
       |> ignore

let configureApp (app : IApplicationBuilder) =
    let env = app.ApplicationServices.GetService<IWebHostEnvironment>()
    (match env.IsDevelopment() with
    | true  ->
        app.UseDeveloperExceptionPage()
    | false ->
        app .UseGiraffeErrorHandler(errorHandler)
            .UseHttpsRedirection())
        .UseCors(configureCors)
        .UseGiraffe(webApp)

let configureServices (services : IServiceCollection) =
    services.AddCors()    |> ignore
    services.AddGiraffe() |> ignore
    services.Configure<AzureBlobLoggerOptions>(fun _ -> ()) |> ignore

let configureLogging (builder : ILoggingBuilder) =
    builder.AddApplicationInsights()
           .AddAzureWebAppDiagnostics()
           .AddConsole()
           .AddDebug() |> ignore

let configureAppConfiguration (context :HostBuilderContext) (config :IConfigurationBuilder) =
    config
        .AddEnvironmentVariables()
        .AddJsonFile("appsettings.json",false,true)
        .AddJsonFile(sprintf "appsettings.%s.json" context.HostingEnvironment.EnvironmentName ,true) |> ignore

[<EntryPoint>]
let main args =
    let host =
        Host.CreateDefaultBuilder(args)
            .ConfigureWebHostDefaults(
                fun webHostBuilder ->
                    webHostBuilder
                        .Configure(Action<IApplicationBuilder> configureApp)
                        .ConfigureLogging(configureLogging)
                        .ConfigureServices(configureServices)
                        |> ignore)
            .ConfigureAppConfiguration(configureAppConfiguration)
            .Build()

    // set up scheduled jobs
    let config = host.Services.GetService<IConfiguration>()
    let adminPassword = getAdminPassword config
    let gamePeriod = getGamePeriod config
    let revealPeriod = getRevealPeriod config
    let revealsLimit = getRevealsLimit config
    let stepsLimit = getStepsLimit config
    let matchesLimit = getMatchesLimit config

    let nextGameDate = Utils.getGameDateForCommitment date.Now gamePeriod revealPeriod
    let nextRevealDate = nextGameDate - revealPeriod
    
    JobManager.Initialize()

    // reveal teams scheduled job
    JobManager.AddJob(Jobs.revealTeams adminPassword gamePeriod, 
        fun (s :Schedule) -> s.ToRunOnceAt(nextRevealDate.AddSeconds(30.0)).AndEvery(int gamePeriod.TotalSeconds).Seconds() |> ignore)

    // process tournaments and sync nfts
    JobManager.AddJob(Jobs.processTournament adminPassword revealsLimit stepsLimit matchesLimit,
        fun (s :Schedule) -> s.ToRunOnceAt(nextGameDate.AddSeconds(30.0)).AndEvery(int gamePeriod.TotalSeconds).Seconds() |> ignore)

    host.Run()
    0