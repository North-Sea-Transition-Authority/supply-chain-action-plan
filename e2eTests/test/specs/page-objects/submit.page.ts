import {Page} from './page.ts'
import {FdsCheckbox} from "../../../test-library/page-objects/components/FdsCheckbox.ts";
import {FdsButton} from "../../../test-library/page-objects/components/FdsButton.ts";

export class SubmitPage extends Page {
    public async reviewAndSubmit() {
        await FdsCheckbox.selectCheckboxItemWithPartialText("Acknowledgements", "I confirm that this SCAP has been checked");
        await FdsButton.clickButtonWithText("Submit");
    }

    public async getErrorText(): Promise<string> {
        return $("div.govuk-error-summary p.govuk-error-message").getText();
    }

}