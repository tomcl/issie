.paket\paket.exe install
dotnet nuget locals all --clear 
dotnet restore src\Main\Main.fsproj
dotnet restore src\Renderer\Renderer.fsproj
yarn install