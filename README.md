# CRN++ Parser and Simulator

## Build and Run

To build the project: 
```shell
dotnet build
```

To publish the CLI to a single executable file:
```shell
cd CRN.CLI
dotnet publish -r win-x64 -c Release -o publish -p:PublishReadyToRun=true -p:PublishSingleFile=true -p:PublishTrimmed=true --self-contained true -p:IncludeNativeLibrariesForSelfExtract=true
```

Replace the ```-r``` value with your runtime (see RID [catalog](https://docs.microsoft.com/en-us/dotnet/core/rid-catalog))

You can now run the ```.exe``` that was placed in the ```CRN.CLI\publish``` folder.