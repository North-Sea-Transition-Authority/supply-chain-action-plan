import {Page} from "../page.ts";
import {FdsButton} from "../../../../test-library/page-objects/components/FdsButton.ts";
import {FdsCheckbox} from "../../../../test-library/page-objects/components/FdsCheckbox.ts";

class UserRole extends Page {
    public async saveUserRoles() {
        await FdsButton.clickButtonWithText("Save and continue");
    }

    public async tickRole(role : string) {
        await FdsCheckbox.selectCheckboxItemWithPartialText("What actions does", role)
    }

    public async tickRoleWithText(role : string, question : string) {
        await FdsCheckbox.selectCheckboxItemWithPartialText(question, role)
    }
}

export default new UserRole();