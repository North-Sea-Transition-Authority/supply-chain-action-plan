import { deepmergeCustom } from 'deepmerge-ts'
import { config as wdioConf } from './wdio.drone-e2e.conf'

const deepmerge = deepmergeCustom({
  mergeArrays: false
})

export const config = deepmerge(wdioConf, {
  // running selenium docker image locally in WSL2
  hostname: 'localhost',
  baseUrl: 'http://host.docker.internal:8080'
})

console.log(config);