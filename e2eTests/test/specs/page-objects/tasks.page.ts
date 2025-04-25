import {Page} from './page.ts'
import {FdsLink} from "../../../test-library/page-objects/components/FdsLink.ts";

export class TasksPage extends Page {
    async goToProjectDetails() {
        return FdsLink.clickLinkWithText("Project details");
    }

    async goToPlannedTenderActivity() {
        return FdsLink.clickLinkWithText("Planned tender activity");
    }

    async goToRelatedPathfinderProjects() {
        return FdsLink.clickLinkWithText("Related Pathfinder projects");
    }

    async goToActualTenderActivityLink() {
        return FdsLink.clickLinkWithText("Actual tender activity");
    }

    async goToContractingPerformance() {
        return FdsLink.clickLinkWithText("Contracting performance");
    }

    async goToProjectPerformance() {
        return FdsLink.clickLinkWithText("Project performance and close-out");
    }

    async goToReviewAndSubmit() {
        return FdsLink.clickLinkWithText("Review and submit");
    }

}