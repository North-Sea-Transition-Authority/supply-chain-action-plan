import {Page} from "../page.ts";

class IndustryTeamMembership extends Page {
    public async clickCreateOrganisationButton() {
        const linkElement = await $(`//a[contains(@class, 'govuk-button')][normalize-space(text()) = "Create organisation group team"]`);
        await linkElement.click();
    }
}

export default new IndustryTeamMembership();