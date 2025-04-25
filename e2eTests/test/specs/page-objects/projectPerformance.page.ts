import {Page} from './page.ts'
import {FdsTextInput} from "../../../test-library/page-objects/components/FdsTextInput.ts";
import {FdsDateInput} from "../../../test-library/page-objects/components/FdsDateInput.ts";
import {FdsRadio} from "../../../test-library/page-objects/components/FdsRadio.ts";
import {FdsButton} from "../../../test-library/page-objects/components/FdsButton.ts";

export class ProjectPerformancePage extends Page {
    public async projectNotComplete() {
        await FdsRadio.selectRadioItemWithText("Has the full project been completed?", "No");
        await FdsButton.clickButtonWithText("Save and complete");
    }

    public async projectComplete(){
        await FdsRadio.selectRadioItemWithText("Has the full project been completed?", "Yes")
        await FdsDateInput.setDateInputWithLabel("Actual execution start date", "1", "7", "2023");
        await FdsDateInput.setDateInputWithLabel("Actual commissioning or completion date", "13", "7", "2023");
        await FdsTextInput.enterTextWithLabel("Project outturn cost", "17");
        await FdsButton.clickButtonWithText("Save and complete");
    }
}