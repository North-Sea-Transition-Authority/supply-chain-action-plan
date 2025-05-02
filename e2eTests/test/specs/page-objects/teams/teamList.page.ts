import {Page} from "../page.ts";

class TeamListPage extends Page {
    public async navigateToTeamsPage() {
        await expect(browser).toHaveTitle('Work area - Supply Chain Action Plan');
        const linkElement = await $(`//a[contains(@class, 'govuk-service-navigation__link')][normalize-space(text()) = 'Manage teams']`);
        await linkElement.click();
    }

    public async editTeam(teamName: string) {
        const linkElement = await $(`//a[contains(@class, 'govuk-link')][normalize-space(text()) = 'Manage'][span[normalize-space(text()) = '${teamName}']]`)
        await linkElement.click();
    }
}

export default new TeamListPage();