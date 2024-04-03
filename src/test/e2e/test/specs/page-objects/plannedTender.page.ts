import {Page} from './page'
import {FdsTextInput} from "../../test-library/page-objects/components/FdsTextInput";
import {FdsTextarea} from "../../test-library/page-objects/components/FdsTextarea";
import {FdsRadio} from "../../test-library/page-objects/components/FdsRadio";
import {FdsDateInput} from "../../test-library/page-objects/components/FdsDateInput";
import {FdsButton} from "../../test-library/page-objects/components/FdsButton";
export class PlannedTenderPage extends Page {
    public async noPlannedTender() {
        await FdsRadio.selectRadioItemWithText("Does this SCAP have any planned tender activities?", "No");
        await FdsButton.clickButtonWithText("Save and continue");
    }

    public async yesPlannedTenderPage() {
        await FdsRadio.selectRadioItemWithText("Does this SCAP have any planned tender activities?", "Yes");
        await FdsButton.clickButtonWithText("Save and continue");
    }

    public async noAddtionalPlannedTenderActivity() {
        await FdsRadio.selectRadioItemWithText("Do you want to add another planned tender activity?", "No, I have added all the planned tender activities I need to")
        await FdsButton.clickButtonWithText("Save and continue");
    }

    public async updateCompletePlannedTenderPage(){
        await FdsTextarea.enterTextWithLabel("Scope description", "This is an automated updated to SCAP");
        await FdsTextInput.enterTextWithLabel("Estimated value", "10");
        await FdsRadio.selectRadioItemWithText("Remuneration model", "Lump sum");
        await FdsTextarea.enterTextWithLabel("Award rationale", "This is an automated update to award rationale");
        await FdsDateInput.setDateInputWithLabel("Indicative actual tender start date", "30", "12", "2025");
        await FdsDateInput.setDateInputWithLabel("Indicative contract award date", "31", "12", "2025");
        await FdsButton.clickButtonWithText("Save and continue");
        await FdsRadio.selectRadioItemWithText("Do you want to add another planned tender activity?", "No, I have added all the planned tender activities I need to");
        await FdsButton.clickButtonWithText("Save and continue");
    }
}