import {Page} from "./page.ts";
import {FdsTextarea} from "../../../test-library/page-objects/components/FdsTextarea.ts";
import {FdsDateInput} from "../../../test-library/page-objects/components/FdsDateInput.ts";

export class RejectOrUpdateScapPage extends Page {
    public async selectDecisionApproveFromDropdown() {
        await $(`button=Decisions`).click();
        await $(`button=SCAP has no objection`).click();
    }

    public async requestFurtherInformation() {
        await $(`button=Request further information`).click();
        await FdsTextarea.enterTextWithLabel("What information needs to be expanded upon?", "Automated request - Please update field");
        await $('[name="INFO_REQUESTED"]').click();
    }

    public async setNextRequiredUpdate() {
        await $(`button=Set next required update`).click();
        await FdsDateInput.setDateInputWithLabel("When is the next required update?", "25", "3", "2033");
        await FdsTextarea.enterTextWithLabel("What information needs to be updated?", "Automated request - Please update tender activity");
        await $('[name="UPDATE_REQUESTED"]').click();
    }

}