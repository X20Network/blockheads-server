namespace CubeHeadsServer

module Authentication =

#if DEBUG
    let adminWalletPath = @"C:\Wallets\admin.json"
#else
    let adminWalletPath = @"C:\home\data\admin.json"
#endif

    let getMessage account = "I verify that I am the owner of the account"