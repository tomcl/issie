//  IssieStick debugger test script
//    This script tests the debugger functionality of Issie hardware by reading the value of a specified viewer.
//    It works with Issie designs built in debug mode and loaded onto an IceStick or IssieStick with a FT2232H USB interface.
//    
//    Usage: node Tests\hw\IS-uart.js <Viewer Index>
//      <Viewer Index> is the 8-bit reference assigned to a viewer during compilation.
//      The index of the first viewer is the character "0" (0x30)
//    
//    Prerequisites:
//      - IceStick or IssieStick with a FT2232H USB interface
//      - Issie design with at least one viewer, built in debug mode and loaded onto the stick

const usb = require("usb");
const WebUSB = usb.WebUSB;

const Mutex = require('async-mutex').Mutex;

const mutex = new Mutex();

 
//USB device settings for FT2232H channel B
const VID = 0x0403; //0x0403 = 1027
const PID = 0x6010; //0x6010 = 24592
const DEVICE_IF = 1;   
const TX_EP = 4;
const RX_EP = 3;

//UART speed 115200bps
//const BAUD_VALUE = 49256;
//const BAUD_INDEX = 514;

//UART speed 9600bps
const BAUD_VALUE = 1250;
const BAUD_INDEX = 514;

const BAUD_REQ = 3; //Control request number to set baud

const RX_BUF_SIZE = 64;
const RX_PAD_BYTES = 2; //Extra bytes prefixing every transfer in

const TX_CONTINUE = "C"; //Message to start clock
const TX_PAUSE = "P"; //Message to stop clock
const TX_STEP = "S"; //Message to step one clock cycle
const TX_READ = "R";  //Message to read viewer 0
const TX_INTERVAL = 500;

var device
var sendTimer
var kill = false

//Terminate on Ctrl+C
process.on('SIGINT', async function() {
    clearInterval(sendTimer);
    kill = true;
});

function decimalToHex(d, padding) {
    var hex = Number(d).toString(16);
    padding = typeof (padding) === "undefined" || padding === null ? padding = 2 : padding;

    while (hex.length < padding) {
        hex = "0" + hex;
    }

    return hex;
}

export async function stepAndReadAllViewers(n){
    
    const release = await mutex.acquire();
    await device.transferOut(TX_EP, TX_STEP);
    //console.log("Step sent!");
    let viewerValues = [];
    for (var i=0;i<n;i++){
        let check = true;
        await device.transferOut(TX_EP, TX_READ + decimalToHex(i,2));
        while(check){
            var result = await device.transferIn(RX_EP, RX_BUF_SIZE);
            if (result.data.byteLength > RX_PAD_BYTES) {
                //var message = decoder.decode(result.data.buffer.slice(RX_PAD_BYTES)); //Format input as string
                var message = Buffer.from(result.data.buffer.slice(RX_PAD_BYTES)).toString('hex');   //Format input as hex
                //console.log(`Received message: ${message}`);
                check = false;
                viewerValues.push(message);
               }
        }       
    }
    release();
    return viewerValues;
}

export async function readAllViewers(n){
    
    const release = await mutex.acquire();
    let viewerValues = [];
    for (var i=0;i<n;i++){
        let check = true;
        await device.transferOut(TX_EP, TX_READ + decimalToHex(i,2));
        while(check){
            var result = await device.transferIn(RX_EP, RX_BUF_SIZE);
            if (result.data.byteLength > RX_PAD_BYTES) {
                //var message = decoder.decode(result.data.buffer.slice(RX_PAD_BYTES)); //Format input as string
                var message = Buffer.from(result.data.buffer.slice(RX_PAD_BYTES)).toString('hex');   //Format input as hex
                //console.log(`Received message: ${message}`);
                check = false;
                viewerValues.push(message);
               }
        }       
    }
    release();
    return viewerValues;
}


export async function step() {
    await mutex.waitForUnlock();
    await device.transferOut(TX_EP, TX_STEP);
    console.log("Step sent!");
}

export async function pauseOp() {
    await device.transferOut(TX_EP, TX_PAUSE);
    
}

export async function continuedOp() {
    await device.transferOut(TX_EP, TX_CONTINUE);    
}


export async function disconnect() {
    await device.close();    
    console.log("Device Closed");
}


export async function connectAndRead(n) {

    const customWebUSB = new WebUSB({
        // Bypass cheking for authorised devices
        allowAllDevices: true
    });


    
    //Find device with correct VID and PID
    try {
        device = await customWebUSB.requestDevice({
            filters: [{
                vendorId: VID,
                productId: PID
            }]
        });
    }
    catch(err) {
        throw new Error("IceStick or IssieStick device (FT2232H) not found");
    }

    if (device) {
        console.log(`Found ${device.productName} (${device.vendorId},${device.productId})`);
    }
    
    //Open and configure device
    await device.open();
    console.log('Opened:', device.opened);

    await device.claimInterface(DEVICE_IF);

    const result = await device.controlTransferOut({
        requestType: 'vendor',
        recipient: 'device',
        request: BAUD_REQ,
        value: BAUD_VALUE,
        index: BAUD_INDEX,
    });

    console.log(`Set baud = ${result.status}`);

    let values = await readAllViewers(n);

    return values;

}

export async function simpleConnect() {

    const customWebUSB = new WebUSB({
        // Bypass cheking for authorised devices
        allowAllDevices: true
    });


    
    //Find device with correct VID and PID
    try {
        device = await customWebUSB.requestDevice({
            filters: [{
                vendorId: VID,
                productId: PID
            }]
        });
    }
    catch(err) {
        throw new Error("IceStick or IssieStick device (FT2232H) not found");
    }

    if (device) {
        console.log(`Found ${device.productName} (${device.vendorId},${device.productId})`);
    }
    
    //Open and configure device
    await device.open();
    console.log('Opened:', device.opened);

    await device.claimInterface(DEVICE_IF);

    const result = await device.controlTransferOut({
        requestType: 'vendor',
        recipient: 'device',
        request: BAUD_REQ,
        value: BAUD_VALUE,
        index: BAUD_INDEX,
    });

    console.log(`Set baud = ${result.status}`);


}