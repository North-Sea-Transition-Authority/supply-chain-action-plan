import {Page} from "./page";
import {FdsTextarea} from "../../test-library/page-objects/components/FdsTextarea";
import {FdsRadio} from "../../test-library/page-objects/components/FdsRadio";

export class ApproveScapPage extends Page {
    public async selectDecisionApproveFromDropdown() {
        await $(`button=Decisions`).click();
        await $(`button=No objection`).click();
    }

    public async approveScapInSidePannel() {
        await FdsTextarea.enterTextWithLabel("No objection comments", "Approved");
        await FdsTextarea.enterTextWithLabel("Summary of decision rationale", "rationale");
        await FdsRadio.selectRadioItemWithText("Has the SCAP been fully completed?", "No");
        await this.selectCheckboxWithLabelText("I understand what no objection means");
        await $('[name="APPROVED"]').click(); //There are two Approve buttons on this page with the only difference being their name attr, so we can't use FdsButton here.
    }

    public async getPageTitle(): Promise<string> {
        return $("//h3").getText();
    }

    /**
     * Find a checkbox by its value label, ignoring the group to which it belongs, and click it.
     * Cant use the FDS checkbox util methods as they rely on the group question text, which doesnt exist for some scenarios.
     * @param labelText
     */
    private async selectCheckboxWithLabelText(labelText: string) {
        const checkboxItem = await $(`//div[contains(@class, 'govuk-checkboxes__item')]//label[normalize-space(text())='${labelText}']`)
        await checkboxItem.click();
        return checkboxItem;
    }
}