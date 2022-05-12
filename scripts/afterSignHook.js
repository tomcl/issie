require('dotenv').config();
const fs = require('fs')
const path = require('path')
const getPackageJson = require('./getPackageJson');
const execSync = require("child_process").execSync;

const { build: { appId, productName } } = getPackageJson('build');
const opts = { stdio: 'pipe' };

module.exports = async function (params) {
    if (process.platform !== 'darwin') {
        return
    }

    console.log('afterSign hook triggered', params)

    const appPath = path.join(
        params.outDir,
        `${productName}-darwin.dmg`
    )
    if (!fs.existsSync(appPath)) {
        console.log('appPath not found', appPath);
        return
    }

    console.log(
        `Notarizing ${appId} found at ${appPath} with Apple ID ${process.env.APPLE_ID}.`
    )

    try {
        const res = execSync(`xcrun altool --notarize-app -f ${appPath} --primary-bundle-id ${appId} -u ${process.env.APPLE_ID} -p ${process.env.APPLE_ID_PASSWORD}`, opts).toString();

        if(res.includes("RequestUUID")){
            const requestUUID = res.match(/[0-9a-f]{8}-[0-9a-f]{4}-[1-5][0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}/)[0];

            if(!requestUUID){
                console.log('Could not find the requestUUID');
                return;
            }

            const status = await checkRequestStatus(requestUUID);

            if(status === true){
                const staple = await stapleApp(appPath);

                if(staple){
                    console.log('Stape action success!');
                } else {
                    throw new Error("Staple action failed");
                }
            } else {
                console.log('status', status);
            }

        } else {
            console.log('No RequestUUID in response');
        }

        console.log(`Done notarizing ${appId}`);

    } catch (error) {
        console.error("Notarization failed!")
        // console.error('ERR', error)
    }
}

const checkRequestStatus = (requestUUID) => {
    return new Promise(resolve => {
        let attempts = 0;
        const checkStatus = () => {
            attempts++;
            const res = execSync(`xcrun altool --notarization-info ${requestUUID} -u ${process.env.APPLE_ID} -p ${process.env.APPLE_ID_PASSWORD}`, opts).toString();

            if(res.includes('in progress')){
                if(attempts < 50){
                    console.log(`In progress. Checking again in 15 seconds. Attempts: ${attempts}`);
                    setTimeout(checkStatus, 15000);
                } else {
                    console.log('Request amount exceeded');
                    resolve("AMT_EXCEEDED");
                }

            } else if(res.includes('success')){
                console.log('Notarization success');
                resolve(true)
            } else {
                console.log('No suitable response');
                resolve("INVALID_RES");
            }
        }

        checkStatus();
    });
}

const stapleApp =  async (appPath) => {
    const res = execSync(`xcrun stapler staple ${appPath}`, opts).toString();

    if(res.includes('worked!')){
        return true;
    } else {
        return false;
    }
}