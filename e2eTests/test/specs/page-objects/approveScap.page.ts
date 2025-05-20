import { Page } from "./page.ts";
import {FdsTextarea} from "../../../test-library/page-objects/components/FdsTextarea.ts";
import {FdsRadio} from "../../../test-library/page-objects/components/FdsRadio.ts";
import fdsUtils from "./fdsUtils.ts";

export class ApproveScapPage extends Page {
    public async selectDecisionApproveFromDropdown() {
        await $(`button=Decisions`).click();
        await $(`button=No objection`).click();
    }

    public async approveScapInSidePannel() {
        await FdsTextarea.enterTextWithLabel("No objection comments", "Approved");
        await FdsTextarea.enterTextWithLabel("Summary of decision rationale", "rationale");
        await FdsRadio.selectRadioItemWithText("Has the SCAP been fully completed?", "No");
        await fdsUtils.selectCheckboxWithLabelText("I understand what no objection means");
        await $('[name="APPROVED"]').click(); //There are two Approve buttons on this page with the only difference being their name attr, so we can't use FdsButton here.
    }

    public async getPageTitle(): Promise<string> {
        return $("//h3").getText();
    }
}