import {Page} from './page.ts'
import {FdsButton} from "../../../test-library/page-objects/components/FdsButton.ts";
import fdsUtils from "./fdsUtils.ts";

export class SubmitPage extends Page {
    public async reviewAndSubmit() {
        await fdsUtils.selectCheckboxWithLabelText("I confirm that this SCAP has been checked, reviewed and approved by all of our internal stakeholders");
        await FdsButton.clickButtonWithText("Submit");
    }

    public async getErrorText(): Promise<string> {
        return $("div.govuk-error-summary p.govuk-error-message").getText();
    }

}