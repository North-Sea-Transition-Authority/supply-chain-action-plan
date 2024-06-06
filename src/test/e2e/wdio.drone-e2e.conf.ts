import { deepmergeCustom } from 'deepmerge-ts'
import { config as wdioConf } from './wdio.conf'
//@ts-ignore
import {TimelineService} from 'wdio-timeline-reporter/timeline-service'

const deepmerge = deepmergeCustom({
  mergeArrays: false
})

export const config = deepmerge(wdioConf, {
  // selenium server opts
  hostname: 'e2e-firefox',
  port: 4444,
  path: '/wd/hub',
  protocol: 'http',

  maxInstances: 1,
  capabilities: [{
    browserName: 'firefox'
  }],
  services: [
    'shared-store',
    [TimelineService],
    ['firefox-profile', {
      // Required to allow the SAML self posting form submission, as this posts from secure domain (itportal.dev.fivium.co.uk) to insecure (e2e-app)
      'security.warn_submit_secure_to_insecure': false
    }]
  ],
  reporters: [
    'spec',
    //@ts-ignore
    ['timeline', {
      outputDir: './reports/e2e',
      fileName: 'scap-e2e-report.html',
      screenshotStrategy: 'on:error'
    }],
    // Add for debugging
    // ['video', {
    //   saveAllVideos: true,
    //   videoSlowdownMultiplier: 3,
    //   outputDir: './reports/e2e'
    // }]
  ],
  baseUrl: 'http://e2e-app:8080'
})

console.log(config);