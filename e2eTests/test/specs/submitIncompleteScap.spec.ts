import loginPage from "./page-objects/login.page.ts"
import {WorkAreaPage} from "./page-objects/workArea.page.ts";
import {FdsButton} from "../../test-library/page-objects/components/FdsButton.ts";
import {OrganisationGroupPage} from "./page-objects/organisationGroup.page.ts";
import {TasksPage} from "./page-objects/tasks.page.ts";
import scapUtils from "./page-objects/scapUtils.ts";
import {SubmitPage} from "./page-objects/submit.page.ts";

const workAreaPage = new WorkAreaPage();
const organisationGroupPage = new OrganisationGroupPage();
const taskPage = new TasksPage();
const submitPage = new SubmitPage();

describe('Create Invalid SCAP', () => {
    it('invalid SCAP should not be submittable', async () => {
        await loginPage.open();
        await loginPage.loginIndustryScapSubmitter();
        await expect(browser).toHaveTitle(expect.stringContaining('Work area - Supply Chain Action Plan'));
        await workAreaPage.startNewScapButton.click();
        await FdsButton.clickButtonWithPartialText("Start now");
        await organisationGroupPage.selectOperator("CENTRICA");
        await organisationGroupPage.notTierOneContractor();
        await organisationGroupPage.saveAndContinue();
        await taskPage.goToReviewAndSubmit();
        const errorMessage = await submitPage.getErrorText();
        await expect(errorMessage).toEqual(`You cannot submit a SCAP until all sections shown on the task list are completed`);
        await scapUtils.signOut();
    });
});