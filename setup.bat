.paket\paket.exe install
dotnet nuget locals all --clear 
dotnet restore src\Main\Main.fsproj
yarn install