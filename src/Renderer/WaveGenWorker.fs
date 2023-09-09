module WaveGenWorker

open WorkerInterface
open WaveSim
open CommonTypes
open ModelType

let waveGen (msg: {|ws: WaveSimModel; index: WaveIndexT; wave: Wave|}) =
    let outWave = generateWaveform msg.ws msg.index msg.wave
    postMessage outWave
    closeWorker()

defineWorkerOnMsg waveGen
