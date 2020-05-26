paket install
dotnet nuget locals all --clean
dotnet restore src/Main/Main.fsproj
dotnet restore src/Renderer/Renderer.fsproj
yarn install