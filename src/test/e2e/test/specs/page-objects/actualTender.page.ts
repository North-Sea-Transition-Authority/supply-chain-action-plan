import {Page} from './page'
import {FdsRadio} from "../../test-library/page-objects/components/FdsRadio";
import {FdsButton} from "../../test-library/page-objects/components/FdsButton";
import {FdsTextarea} from "../../test-library/page-objects/components/FdsTextarea";
import {FdsTextInput} from "../../test-library/page-objects/components/FdsTextInput";
import {FdsDateInput} from "../../test-library/page-objects/components/FdsDateInput";
import {FdsSearchSelector} from "../../test-library/page-objects/components/FdsSearchSelector";
import {FdsCheckbox} from "../../test-library/page-objects/components/FdsCheckbox";
import {FdsSelect} from "../../test-library/page-objects/components/FdsSelect";

export class ActualTenderPage extends Page {
    public async noActualTender() {
        await FdsRadio.selectRadioItemWithText("Does this SCAP have any actual tender activity?", "No");
        await FdsButton.clickButtonWithText("Save and continue");
    }

    public async yesActualTender(){
        await FdsRadio.selectRadioItemWithText("Does this SCAP have any actual tender activity?", "Yes");
        await FdsButton.clickButtonWithText("Save and continue");
    }

    public async updateCompleteActualTenderPage() {
    await FdsTextInput.enterTextWithLabel("Scope title", "Automated update to SCAP actual tender");
    await FdsTextarea.enterTextWithLabel("Scope description", "this is an Automated actual tender activity update");
    await FdsRadio.selectRadioItemWithText("Remuneration model", "Target cost");
    await FdsSearchSelector.searchAndSelectOption("Enter the invitation to tender recipients", "TESTER");
    await FdsRadio.selectRadioItemWithText("Contract stage", "Contract has been awarded");
    await FdsButton.clickButtonWithText("Save and continue");
    await FdsCheckbox.selectCheckboxItemWithText("Bid participants", "TESTER");
    await FdsButton.clickButtonWithText("Save and continue");
    await FdsSelect.selectItemWithText("Preferred bidder","TESTER");
    await FdsTextInput.enterTextWithLabel("Award value", "20");
    await FdsTextarea.enterTextWithLabel("Award rationale","This is an automated Award for testing!");
    await FdsSearchSelector.searchAndSelectOption("Country of preferred bidder","United Kingdom");
    await FdsDateInput.setDateInputWithLabel("Contract award date", "3", "6", "2023");
    await FdsRadio.selectRadioItemWithText("What are the payment terms for this contract?", "60 days");
    await FdsDateInput.setDateInputWithLabel("Forecast execution start date", "2", "7", "2024");
    await FdsDateInput.setDateInputWithLabel("Forecast execution completion date", "30", "12", "2025");
    await FdsButton.clickButtonWithText("Save and continue");
    await FdsRadio.selectRadioItemWithText("Do you want to add another actual tender activity?", "No, I have added all the actual tender activities I need to");
    await FdsButton.clickButtonWithText("Save and complete");
    }

    public async additionalActualTenderActivity() {
        await FdsRadio.selectRadioItemWithText("Do you want to add another actual tender activity?", "Yes, I want to add one now");
        await FdsButton.clickButtonWithText("Save and complete");
    }

    public async completeAdditonalActualTenderPage() {
        await FdsTextInput.enterTextWithLabel("Scope title", "Automated update to SCAP actual tender second");
        await FdsTextarea.enterTextWithLabel("Scope description", "this is an Automated further update toactual tender activity");
        await FdsRadio.selectRadioItemWithText("Remuneration model", "Target cost");
        await FdsSearchSelector.searchAndSelectOption("Enter the invitation to tender recipients", "TESTER");
        await FdsRadio.selectRadioItemWithText("Contract stage", "Contract has been awarded");
        await FdsButton.clickButtonWithText("Save and continue");
        await FdsCheckbox.selectCheckboxItemWithText("Bid participants", "TESTER");
        await FdsButton.clickButtonWithText("Save and continue");
        await FdsSelect.selectItemWithText("Preferred bidder","TESTER");
        await FdsTextInput.enterTextWithLabel("Award value", "20");
        await FdsTextarea.enterTextWithLabel("Award rationale","This is an automated Award for testing!");
        await FdsSearchSelector.searchAndSelectOption("Country of preferred bidder","United Kingdom");
        await FdsDateInput.setDateInputWithLabel("Contract award date", "3", "7", "2023");
        await FdsRadio.selectRadioItemWithText("What are the payment terms for this contract?", "60 days");
        await FdsDateInput.setDateInputWithLabel("Forecast execution start date", "2", "7", "2025");
        await FdsDateInput.setDateInputWithLabel("Forecast execution completion date", "30", "12", "2026");
        await FdsButton.clickButtonWithText("Save and continue");
        await FdsRadio.selectRadioItemWithText("Do you want to add another actual tender activity?", "No, I have added all the actual tender activities I need to");
        await FdsButton.clickButtonWithText("Save and complete");
    }

}