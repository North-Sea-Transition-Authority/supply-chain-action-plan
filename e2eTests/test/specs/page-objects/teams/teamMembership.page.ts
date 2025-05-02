import {Page} from "../page.ts";
import {FdsButton} from "../../../../test-library/page-objects/components/FdsButton.ts";
import {FdsTextInput} from "../../../../test-library/page-objects/components/FdsTextInput.ts";

class TeamMembership extends Page {
    public async editUserInTeam(email : string) {
        const tableRow = await $(`//tr[.//li[normalize-space(text()) = '${email}']]`);
        const editLink = await tableRow.$(`.//a[normalize-space(text()) = 'Edit']`)
        await editLink.click();
    }

    public async removeUserInTeam(email : string) {
        const tableRow = await $(`//tr[.//li[normalize-space(text()) = '${email}']]`);
        const removeLink = await tableRow.$(`.//a[normalize-space(text()) = 'Remove']`)
        await removeLink.click();
    }

    public async clickRemove() {
        FdsButton.clickButtonWithText("Remove");
    }

    public async addNewUser() {
        await FdsButton.clickButtonWithText('Add user');
    }

    public async enterUserEmailAndContinue(email : string) {
        const formGroupElement = await $(`//label[contains(@class, 'govuk-label')][../label[normalize-space(text()) = 'What is the Energy Portal username of the user?']]`)
            .$(`./ancestor::div[contains(@class,'govuk-form-group')][1]`);
        const textInputFormField = new FdsTextInput(formGroupElement);
        const textInput = await textInputFormField.getTextInputElement();
        await textInput.setValue(email);
        await FdsButton.clickButtonWithText("Continue");
    }
}

export default new TeamMembership();