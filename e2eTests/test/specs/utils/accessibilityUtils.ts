import lighthouseUtils, {CategoryToEmoji, LighthouseResultInfo} from "./lighthouseUtils.ts";
import axeUtils, {AxeResultInfo} from "./axeUtils.ts";
import {writeFileSync} from "fs";
import {resolve, dirname} from "path";
import { fileURLToPath } from 'url';

export interface WdioResults {
    finished: number,
    passed: number,
    retries: number,
    failed: number,
}

class AccessibilityUtils {
    public async writeSlackAccessibilityResultsFile(results: WdioResults) {
        const targetFilePath = resolve(dirname(fileURLToPath(import.meta.url)), "../../../../e2eTests/slack.txt");
        const droneResults = `*{{build.status}}* <{{build.link}}|Commit {{truncate build.commit 7}} on {{build.branch}} by ${process.env.DRONE_COMMIT_AUTHOR_NAME}>\nReports published to: http://drone-assets.fivium.local:9090/supply-chain-action-plan/{{build.number}}/`;
        writeFileSync(targetFilePath, `${droneResults}\n${await this.getSlackAccessibilityResultsMessage(results)}`);
        console.log(`Written slack message to ${targetFilePath}`);
    }

    private async getSlackAccessibilityResultsMessage(results: WdioResults) {
        const axeResultInfos = await axeUtils.getAllAxeResultInfosFromStore();
        const lighthouseResultInfos = await lighthouseUtils.getAllLighthouseResultInfosFromStore();

        const wdioResultMessage = `🤖 : ${results.passed}/${results.finished} E2E ${this.pluraliseString(results.finished, "Test")} passed`;
        let axeResultMessage = this.generateAxeScorePerPageString(axeResultInfos);
        let lighthouseResultMessage = this.generateLighthouseScorePerCategoryAndPageStrings(lighthouseResultInfos);

        return `${wdioResultMessage}\n${axeResultMessage}\n${lighthouseResultMessage}`;
    }

    private generateAxeScorePerPageString(axeResultInfos: AxeResultInfo[]) {
        const numberOfPagesPassed = axeResultInfos
            .filter(axeResultInfo => axeResultInfo.numberOfTestsFailed === 0).length;
        const numberOfPages = axeResultInfos.length;
        return `🪓 : ${numberOfPagesPassed}/${numberOfPages} ${this.pluraliseString(axeResultInfos.length, "page")} passed Axe testing`;
    }

    private generateLighthouseScorePerCategoryAndPageStrings(lighthouseResultInfos: LighthouseResultInfo[]) {
        const accessibilityScoreString = `🗽 ${CategoryToEmoji["accessibility"]} : ${this.getLighthouseScoreString("accessibility", lighthouseResultInfos)} passed Lighthouse accessibility`;
        const performanceScoreString = `🗽 ${CategoryToEmoji["performance"]} : ${this.getLighthouseScoreString("performance", lighthouseResultInfos)} passed Lighthouse performance`;
        return `${accessibilityScoreString}\n${performanceScoreString}`;
    }

    private getLighthouseScoreString(category: string, lighthouseResultInfos: LighthouseResultInfo[]) {
        const categoriesToScores = lighthouseResultInfos
            .flatMap(lighthouseResultInfo => lighthouseResultInfo.categoriesToScores);
        const totalPagesPassed = categoriesToScores
            .filter(categoryToScore =>
                categoryToScore.category.trim().toLowerCase().includes(category.trim().toLowerCase()))
            .filter(categoryToScore => categoryToScore.score === 1)
            .length;
        const totalPages = lighthouseResultInfos.length;
        return `${totalPagesPassed}/${totalPages}`;
    }


    private pluraliseString(count: number, str: string) {
        return count === 0 ? str : `${str}s`;
    }
}

export default new AccessibilityUtils();
