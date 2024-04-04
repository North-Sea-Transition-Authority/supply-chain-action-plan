import { deepmerge } from 'deepmerge-ts'
import { config as wdioConf } from './wdio.conf'

export const config = deepmerge(wdioConf, {
  // selenium server opts
  hostname: 'e2e-chrome',
  port: 4444,
  path: '/wd/hub',
  protocol: 'http',

  maxInstances: 1,
  capabilities: [{
    browserName: 'firefox'
  }],
  baseUrl: 'http://e2e-app:8080',
}, { clone: false })