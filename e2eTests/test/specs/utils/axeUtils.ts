import axe from "axe-core";
import {createHtmlReport} from "axe-html-reporter";
import AxeBuilder from "@axe-core/webdriverio";
import {getValue, setValue} from "@wdio/shared-store-service";
import {writeFileSync} from "fs";

export interface AxeResultInfo {
    url: string,
    numberOfTestsPassed: number,
    numberOfTestsFailed: number
}

class AxeUtils {
    public async generateAxeReport(url: string) {
        const axeResult = await new AxeBuilder({client: browser}).analyze();
        await this.writeAxeResultInfoToStore(axeResult, url);
        await this.writeAxeReportToFile(axeResult, url);
    }

    public async getAllAxeResultInfosFromStore() {
        return await getValue("axeResultInfos") as AxeResultInfo[];
    }

    private async writeAxeResultInfoToStore(axeResult: axe.AxeResults, url: string) {
        const axeResultInfos = await getValue("axeResultInfos") as AxeResultInfo[];
        // we do this to avoid "ERROR @wdio/utils:shim: Error: Payload Too Large" issue when writing large objects to the store
        const { passes, violations } = axeResult;
        const axeResultInfo: AxeResultInfo = {
            url,
            numberOfTestsPassed: passes.length,
            numberOfTestsFailed: violations.length
        };
        axeResultInfos.push(axeResultInfo);
        await setValue("axeResultInfos", axeResultInfos);
    }

    private async writeAxeReportToFile(axeResult: axe.AxeResults, url: string) {
        createHtmlReport({
            results: axeResult,
            options: {
                projectKey: url,
                doNotCreateReportFile: false,
                outputDirPath: "./reports",
                outputDir: "axe",
                reportFileName: `${url}.html`
            },
        });
    }

    public async writeIndexFile() {
        const axeResultInfos = await this.getAllAxeResultInfosFromStore();

        if (axeResultInfos.length === 0) {
            console.log("No axe accessibility reports conducted");
            return;
        }

        const fileContentHtmlString =
            "<head>" +
                "<title>Axe accessibility reports index</title>" +
                `<meta charset="UTF-8">` +
            "</head>" +
            `<body><table style="font-size:20px; text-align:left; margin-left:10px; margin-top:10px">` +
            `<tr style="margin-bottom:10px">` +
                "<th>Links to axe reports</th>" +
                "<th>Results</th>" +
            "</tr>" +
            axeResultInfos.map(axeResultInfo => {
                const url = axeResultInfo.url;
                const totalPassed = axeResultInfo.numberOfTestsPassed;
                const totalFailed = axeResultInfo.numberOfTestsFailed;
                const total = totalPassed + totalFailed;
                const resultsString = totalPassed === total
                    ? `${totalPassed} / ${total} passed ✅`
                    : `${totalPassed} / ${total} passed ❌`;
                return "<tr>" +
                    `<td style="margin-right:10px"><a href="${url}.html">${url}</a></td>` +
                    `<td>${resultsString}</td>` +
                    "</tr>"
            }).join("") +
            "</table></body>";

        writeFileSync("reports/axe/index.html", fileContentHtmlString);
    }
}

export default new AxeUtils();
