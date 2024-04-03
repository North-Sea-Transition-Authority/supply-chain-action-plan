import {Page} from "./page";
import {FdsTextarea} from "../../test-library/page-objects/components/FdsTextarea";
import {FdsDateInput} from "../../test-library/page-objects/components/FdsDateInput";

export class RejectOrUpdateScapPage extends Page {
    public async selectDecisionApproveFromDropdown() {
        await $(`button=Decisions`).click();
        await $(`button=Approve SCAP`).click();
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