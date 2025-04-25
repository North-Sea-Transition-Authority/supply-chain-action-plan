import {Page} from './page.ts'
import {FdsRadio} from "../../../test-library/page-objects/components/FdsRadio.ts";
import {FdsButton} from "../../../test-library/page-objects/components/FdsButton.ts";
import {FdsTextInput} from "../../../test-library/page-objects/components/FdsTextInput.ts";
import {FdsSelect} from "../../../test-library/page-objects/components/FdsSelect.ts";

export class ContractingPerformancePage extends Page {
    public async contractingPerformanceNotFullyClosedOut() {
        await FdsRadio.selectRadioItemWithText("Have any of the awarded contracts been fully closed out?", "No");
        await FdsButton.clickButtonWithText("Save and continue");
    }

    public async updateContractingPerformance(){
        await FdsRadio.selectRadioItemWithText("Have any of the awarded contracts been fully closed out?", "Yes")
        await FdsButton.clickButtonWithText("Save and continue");
        await FdsSelect.selectItemWithText("Scope title","Automated update to SCAP actual tender");
        await FdsTextInput.enterTextWithLabel("Outturn cost", "20");
        await FdsButton.clickButtonWithText("Save and continue");
        await FdsRadio.selectRadioItemWithText("Do you want to add further contracting performance?", "No, I have added all the contracting performance I need to");
        await FdsButton.clickButtonWithText("Save and complete");
        }

}