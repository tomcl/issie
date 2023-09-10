module WaveGenWorker

open WorkerInterface
open WaveSim
open CommonTypes
open ModelType

let waveGen (msg: {|waveParams: WaveGenParams; index: WaveIndexT; wave: Wave|}) =
    let outWave = generateWaveform msg.waveParams msg.index msg.wave
    postMessage (msg.index, outWave)
    closeWorker()

defineWorkerOnMsg waveGen
