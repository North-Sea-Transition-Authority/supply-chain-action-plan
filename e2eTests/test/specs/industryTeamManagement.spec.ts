import scapUtils from "./page-objects/scapUtils.ts";
import loginPage, {industryScapSubmitter} from "./page-objects/login.page.ts"
import teamListPage from "./page-objects/teams/teamList.page.ts";
import teamMembershipPage from "./page-objects/teams/teamMembership.page.ts";
import userRolePage from "./page-objects/teams/userRole.page.ts";
import industryTeamMembershipPage from "./page-objects/teams/IndustryTeamMembership.page.ts";

const industryAccessManager = "IndustryAccessManager@scap.co.uk";

describe('Manage industry team', () => {
    it('should successfully edit a users permission', async () => {
        await loginPage.open();
        await loginPage.loginRegulatorScapCaseOfficer();
        await teamListPage.navigateToTeamsPage();
        await teamListPage.editTeam("CENTRICA");
        await teamMembershipPage.editUserInTeam(industryScapSubmitter);
        await userRolePage.tickRoleWithText("Can add, remove and update members of this team (Access manager)", "Dr Industry SCAP Submitter")
        await userRolePage.saveUserRoles();
        const successMessage = await $("//h3").getText();
        await expect(successMessage).toEqual(`Updated roles for Dr Industry SCAP Submitter`);
        await scapUtils.signOut();
    });
    it('should successfully add a user', async () => {
        await loginPage.open();
        await loginPage.loginRegulatorScapCaseOfficer();
        await teamListPage.navigateToTeamsPage();
        await teamListPage.editTeam("CENTRICA");
        await teamMembershipPage.addNewUser();
        await teamMembershipPage.enterUserEmailAndContinue(industryAccessManager);
        await userRolePage.tickRole("Can view an open SCAP (SCAP viewer)")
        await userRolePage.saveUserRoles();
        const successMessage = await $("//h3").getText();
        await expect(successMessage).toEqual(`Industry Access Manager has been added to the team`);
        await scapUtils.signOut();
    });
    it('should successfully remove a user', async () => {
        await loginPage.open();
        await loginPage.loginRegulatorScapCaseOfficer();
        await teamListPage.navigateToTeamsPage();
        await teamListPage.editTeam("CENTRICA");
        await teamMembershipPage.removeUserInTeam(industryAccessManager);
        await teamMembershipPage.clickRemove();
        const successMessage = await $("//h3").getText();
        await expect(successMessage).toEqual(`Dr Industry Access Manager has been removed from the team`);
        await scapUtils.signOut();
    });
    it('should successfully access the add organisation team page', async () => {
        await loginPage.open();
        await loginPage.loginRegulatorScapCaseOfficer();
        await teamListPage.navigateToTeamsPage();
        await industryTeamMembershipPage.clickCreateOrganisationButton();
        await scapUtils.clickBackButton();
        await scapUtils.signOut();
    });
})