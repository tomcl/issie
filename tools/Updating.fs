namespace Tools

/// Module to generate signature and delta files for differential updating
module Updating =
    open FastRsync.Core
    open FastRsync.Delta
    open FastRsync.Diagnostics
    open FastRsync.Signature
    open Fake.IO.FileSystemOperators
    open System.IO
    
    /// Generate a signature file
    let genSigNew (sigName: string) (file: FileInfo) =
        let sigBuilder = new SignatureBuilder()
        use basisStream = new FileStream(file.FullName, FileMode.Open, FileAccess.Read, FileShare.Read)
        use sigStream = new FileStream(file.Directory.FullName @@ sigName, FileMode.Create, FileAccess.Write, FileShare.Read)
        sigBuilder.Build(basisStream, new SignatureWriter(sigStream))

    /// Generate a delta file
    let genDelta (file: FileInfo) (sigFile: FileInfo) =
        let delta = new DeltaBuilder()
        delta.ProgressReport <- new ConsoleProgressReporter()

        use newFileStream = new FileStream(file.FullName, FileMode.Open, FileAccess.Read, FileShare.Read)
        use sigStream = new FileStream(sigFile.FullName, FileMode.Open, FileAccess.Read, FileShare.Read)
        use deltaStream = new FileStream(file.FullName + ".delta", FileMode.Create, FileAccess.Write, FileShare.Read)

        delta.BuildDelta(newFileStream, new SignatureReader(sigStream, delta.ProgressReport), new AggregateCopyOperationsDecorator(new BinaryDeltaWriter(deltaStream)))