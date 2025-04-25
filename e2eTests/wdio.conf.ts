import type {Options} from '@wdio/types'
import {
    startUpDatabaseContainer,
    startupAppContainer,
    startNetwork,
    startupSelenium,
    startUpS3MockContainer,
    startUpClamAvContainer,
    stripLeadingSlashFromContainerName, isRunningOnDrone
} from "./environment-test-containers.ts"
//@ts-ignore
import {TimelineService} from 'wdio-timeline-reporter/timeline-service.js'
import {RemoteCapability} from "@wdio/types/build/Capabilities";
import { setValue, getValue } from '@wdio/shared-store-service'
import axeUtils, {AxeResultInfo} from "./test/specs/utils/axeUtils.ts";
import lighthouseUtils from "./test/specs/utils/lighthouseUtils.ts";
import urlUtils from "./test/specs/utils/urlUtils.ts";
import accessibilityUtils from "./test/specs/utils/accessibilityUtils.ts";

export interface User {
    username: string,
    password: string
}

export const config: Options.Testrunner = {
    runner: 'local',
    autoCompileOpts: {
        autoCompile: true,
        tsNodeOpts: {
            project: './tsconfig.json',
            transpileOnly: true,
        }
    },
    specs: [
        './test/specs/**/*.spec.ts'
    ],
    maxInstances: 1,
    //
    // Inserts WebdriverIO's globals (e.g. `browser`, `$` and `$$`) into the global environment.
    // If you set to `false`, you should import from `@wdio/globals`. Note: WebdriverIO doesn't
    // handle injection of test framework specific globals.
    //
    injectGlobals: true,
    capabilities: [{
        browserName: 'chrome',
        acceptInsecureCerts : true,
        'goog:chromeOptions': {
            args: [
                'ignore-certificate-errors',
                'allow-running-insecure-content',
                'unsafely-treat-insecure-origin-as-secure=http://e2e-app:8080',
                'remote-debugging-port=9222',
                'remote-debugging-address=0.0.0.0',
                'remote-allow-origins=*'
            ],
        },
    }],
    path: '/wd/hub',
    protocol: 'http',
    logLevel: 'warn',
    // If you only want to run your tests until a specific amount of tests have failed use
    // bail (default is 0 - don't bail, run all tests).
    bail: 0,
    waitforTimeout: 25000,
    connectionRetryTimeout: 120000,
    connectionRetryCount: 3,
    //@ts-ignore
    services: [
        'shared-store',
        //@ts-ignore
        [TimelineService],
        ['firefox-profile', {
            // Required to allow the SAML self posting form submission, as this posts from secure domain (itportal.dev.fivium.co.uk) to insecure docker container
            'security.warn_submit_secure_to_insecure': false
        }],
    ],
    framework: 'mocha',
    reporters: [
        'spec',
        [
            'timeline',
            {
                outputDir: './reports/e2e',
                fileName: 'supply-chain-action-plan-e2e-report.html',
                screenshotStrategy: 'on:error'
            }
        ],
        [
            'video',
            {
                saveAllVideos: true,
                videoSlowdownMultiplier: 3,
                outputDir: './reports/e2e'
            }
        ]
    ],
    // Options to be passed to Mocha.
    // See the full list at http://mochajs.org/
    mochaOpts: {
        ui: 'bdd',
        timeout: 240000
    },
    async onPrepare(_config, _capabilities) {
        await startNetwork();
        // startup selenium before other containers, to reserve consistent host port
        const selenium = await startupSelenium();
        await lighthouseUtils.setupTunnelToExposeChromeDebugPort(selenium);
        const [database, s3Mock, clamAv] = await Promise.all([
            startUpDatabaseContainer(),
            startUpS3MockContainer(),
            startUpClamAvContainer(),
        ]);
        const app = await startupAppContainer(database, s3Mock, clamAv);

        // set values in shared store to access in beforeSession() hook, to complete configuration
        await setValue("seleniumContainerName", stripLeadingSlashFromContainerName(selenium.getName()));
        await setValue("seleniumContainerMappedPort", selenium.getMappedPort(4444));
        await setValue("seleniumContainerMappedChromePort", selenium.getMappedPort(54322));
        await setValue("appContainerName", stripLeadingSlashFromContainerName(app.getName()));
        // to keep track of AxeResultInfo & LighthouseResultInfo objects, to keep track of urls already visited, and for later displaying info on index.html page
        await setValue("axeResultInfos", []);
        await setValue("lighthouseResultInfos", []);
    },
    async beforeSession(config, _capabilities: RemoteCapability, _specs: string[], _cid: string) {
        // Drone runs the tests in a container next to the spawned containers
        // Running the tests locally are not in a container so need to use localhost + mapped ports
        if (await isRunningOnDrone()) {
            config.hostname = String(await getValue("seleniumContainerName"));
            config.port = 4444;
        } else {
            config.hostname = "localhost"
            config.port = Number(await getValue("seleniumContainerMappedPort"));
        }
        config.baseUrl = `http://${String(await getValue("appContainerName"))}:8080/`;
    },

    async afterCommand(commandName, _args, _result, _error) {
        // we need this if block as without it this hook gets called too much resulting in test suite run slowdown
        if (commandName !== "elementSendKeys" && commandName !== "click") {
            return;
        }

        if (!await isRunningOnDrone()) {
            return;
        }

        // axe & lighthouse accessibility tests, only run & generate reports for URLs not yet visited
        const url = await browser.getUrl();
        const sanitisedUrl = urlUtils.sanitiseUrl(url);

        // this excludes fox pages or any other suchlike service pages
        if (!sanitisedUrl.includes(String(await getValue("appContainerName")))) {
            return;
        }

        const alreadyVisitedUrls = (await getValue("axeResultInfos") as AxeResultInfo[])
            .map(axeResultInfo => axeResultInfo.url);

        if (!alreadyVisitedUrls.includes(sanitisedUrl)) {
            alreadyVisitedUrls.push(sanitisedUrl);
            await setValue("alreadyVisitedUrls", alreadyVisitedUrls);
            await axeUtils.generateAxeReport(sanitisedUrl);
            await lighthouseUtils.generateLighthouseReport(url);
        }
    },
    /**
     * Gets executed after all workers have shut down and the process is about to exit.
     * An error thrown in the `onComplete` hook will result in the test run failing.
     * @param {object} exitCode 0 - success, 1 - fail
     * @param {object} config wdio configuration object
     * @param {Array.<Object>} capabilities list of capabilities details
     * @param {<Object>} results object containing test results
     */
    async onComplete(_exitCode, _config, _capabilities, results) {
        // write index.html file with links to all axe accessibility reports (1 per webpage)
        await axeUtils.writeIndexFile();
        // write index.html file with links to all lighthouse accessibility reports (1 per webpage)
        await lighthouseUtils.writeIndexFile();
        if (await isRunningOnDrone()) {
            // write file with contents of axe & lighthouse results, to then use in drone "slack" build step
            await accessibilityUtils.writeSlackAccessibilityResultsFile(results);
        }
    },
}
