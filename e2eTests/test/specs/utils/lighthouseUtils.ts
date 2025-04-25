import {writeFileSync, mkdirSync} from "fs";
import lighthouse, {Flags, RunnerResult} from 'lighthouse';
import {getValue, setValue} from "@wdio/shared-store-service";
import {convertContainerHostnameToIpAddress, isRunningOnDrone} from "../../../environment-test-containers.ts";
import {StartedTestContainer} from "testcontainers";
import urlUtils from "./urlUtils.ts";

interface CategoryAndScore {
    category: string,
    score: number
}

export interface LighthouseResultInfo {
    url: string,
    categoriesToScores: CategoryAndScore[]
}

export const CategoryToEmoji: {[index: string]:string} = {
    accessibility: "🎨",
    performance: "🚀",
}

class LighthouseUtils {
    public async setupTunnelToExposeChromeDebugPort(selenium: StartedTestContainer) {
        const aptGetUpdateResult = await selenium.exec("apt-get update");
        if (aptGetUpdateResult.exitCode != 0) {
            console.log(aptGetUpdateResult);
            throw new Error(`Failed to run "apt-get update"`);
        }
        const aptGetInstallNcatResult = await selenium.exec("apt-get install ncat -y");
        if (aptGetInstallNcatResult.exitCode != 0) {
            console.log(aptGetInstallNcatResult);
            throw new Error(`Failed to run "apt-get install ncat -y"`);
        }
        // this forwards packets inside the container, from 0.0.0.0:54322 => localhost:9222, so that we can hit chrome debug server (from outside the container) which is bound to localhost inside the container.
        const ncatResult = await selenium.exec(["sh", "-c", `ncat -l "54322" --keep-open --sh-exec "ncat localhost 9222" & pid2=$!`]);
        if (ncatResult.exitCode != 0) {
            console.log(ncatResult)
            throw new Error("Failed to run ncat");
        }
    }

    public async generateLighthouseReport(url: string) {
        const seleniumContainerMappedChromePort = Number(await getValue("seleniumContainerMappedChromePort"));
        const options: Flags = {
            logLevel: "warn",
            output: "html",
            hostname: "localhost",
            port: seleniumContainerMappedChromePort,
            // debugNavigation: true,
            onlyCategories: [
                'performance',
                'accessibility'
            ],
            disableStorageReset: true,
        };

        // if running on drone, wdio runs in its own container on our docker network, so we hit selenium container directly
        if (await isRunningOnDrone()) {
            const seleniumContainerName = await getValue("seleniumContainerName") as string;
            // map container name to its IP address, we do this to avoid chrome debug server throwing "Host header is specified and is not an IP address or localhost" error when making a http request to it
            options.hostname = await convertContainerHostnameToIpAddress(seleniumContainerName);
            options.port = 54322;
        }

        const runnerResult = await lighthouse(url, options);

        if (!runnerResult) {
            console.log(`Lighthouse accessibility audit failed for URL: ${url}`);
            return;
        }

        await this.writeLighthouseResultInfoToStore(url, runnerResult);
        this.writeLighthouseReportToFile(url, runnerResult.report);
    }

    public async getAllLighthouseResultInfosFromStore() {
        return await getValue("lighthouseResultInfos") as LighthouseResultInfo[];
    }

    private async writeLighthouseResultInfoToStore(url: string, lighthouseRunnerResult: RunnerResult) {
        const lighthouseResultInfos = await getValue("lighthouseResultInfos") as LighthouseResultInfo[];
        const {categories} = lighthouseRunnerResult.lhr;
        const lighthouseResultInfo: LighthouseResultInfo = {
            url: urlUtils.sanitiseUrl(url),
            categoriesToScores: []
        };
        for (const category of Object.values(categories)) {
            lighthouseResultInfo.categoriesToScores.push({
                category: category.title,
                score: category.score || 0,
            })
        }
        lighthouseResultInfos.push(lighthouseResultInfo);
        await setValue("lighthouseResultInfos", lighthouseResultInfos);
    }

    private writeLighthouseReportToFile(url: string, lighthouseRunnerResultReport: string | string[]) {
        const path = `reports/lighthouse/${urlUtils.sanitiseUrl(url)}.html`;
        mkdirSync("reports/lighthouse", {
            recursive: true
        });
        // @ts-ignore
        writeFileSync(path, lighthouseRunnerResultReport);
        console.log(`Lighthouse HTML report was saved in the following directory: ${path}`);
    }

    public async writeIndexFile() {
        // sort for consistent <th> to <tr> mappings for each lighthouse audit category (i.e., accessibility, performance, pwa)
        const lighthouseResultInfos = (await this.getAllLighthouseResultInfosFromStore())
            .map(lighthouseResultInfo => {
                lighthouseResultInfo.categoriesToScores.sort((a, b) => a.category.localeCompare(b.category));
                return lighthouseResultInfo;
            });

        if (lighthouseResultInfos.length === 0) {
            console.log("No lighthouse accessibility reports conducted");
            return;
        }

        const fileContentHtmlString =
            "<head>" +
                "<title>Lighthouse accessibility reports index</title>" +
                `<meta charset="UTF-8">` +
            "</head>" +
            `<body><table style="font-size:20px; text-align:left; margin-left:10px; margin-top:10px">` +
            `<tr style="margin-bottom:10px">` +
                "<th>Links to Lighthouse reports</th>" +
                lighthouseResultInfos[0].categoriesToScores
                    .map(categoryAndScore =>
                        `<th>${categoryAndScore.category} ${CategoryToEmoji[categoryAndScore.category.toLowerCase()]}</th>`)
                    .join("") +
            "</tr>" +
            lighthouseResultInfos.map(lighthouseResultInfo => {
                const url = lighthouseResultInfo.url;
                const scoresPerCategoryStrings = [];
                const lighthouseResultInfoValues = Object.values(lighthouseResultInfo.categoriesToScores);
                for (const {category, score} of lighthouseResultInfoValues) {
                    scoresPerCategoryStrings.push(`${category} score: ${score} / 1 ${score === 1 ? "✅" : "❌"}`);
                }
                return "<tr>" +
                         `<td style="margin-right:10px"><a href="${url}.html">${url}</a></td>` +
                          scoresPerCategoryStrings.map(scorePerCategoryString => `<td>${scorePerCategoryString}</td>`
                          ).join("") +
                       "</tr>"
            }).join("") +
            "</table></body>";

        writeFileSync("reports/lighthouse/index.html", fileContentHtmlString);
    }
}

export default new LighthouseUtils();
