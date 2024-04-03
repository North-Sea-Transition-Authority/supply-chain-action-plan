import {Page} from "./page";
import {FdsTextarea} from "../../test-library/page-objects/components/FdsTextarea";
import {FdsRadio} from "../../test-library/page-objects/components/FdsRadio";

export class ApproveScapPage extends Page {
    public async selectDecisionApproveFromDropdown() {
        await $(`button=Decisions`).click();
        await $(`button=Approve SCAP`).click();
    }

    public async approveScapInSidePannel() {
        await FdsTextarea.enterTextWithLabel("Approval comments", "Approved");
        await FdsRadio.selectRadioItemWithText("Has the SCAP been fully completed?", "No");
        await $('[name="APPROVED"]').click(); //There are two Approve buttons on this page with the only difference being their name attr, so we can't use FdsButton here.
    }

    public async getPageTitle(): Promise<string> {
        return $("//h3").getText();
    }

}