import {Page} from './page'
import {FdsButton} from "../../test-library/page-objects/components/FdsButton.ts";

export class OrganisationGroupPage extends Page {
        public async selectOperator(operatorName: string) {
            await $("#organisationGroupId").addValue(operatorName);
            await browser.keys("Enter");
        }

        public async notTierOneContractor() {
            await this.radioNo.click();
        }

        private get radioNo() {
            return $("#isTierOneContractor-no");
        }

        public async saveAndContinue() {
            return FdsButton.clickButtonWithText("Save and continue");
        }
}