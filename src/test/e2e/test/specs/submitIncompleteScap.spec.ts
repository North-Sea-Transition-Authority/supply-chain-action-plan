import {LoginPage} from "./page-objects/login.page"
import {WorkAreaPage} from "./page-objects/workArea.page";
import {FdsButton} from "../test-library/page-objects/components/FdsButton"
import {OrganisationGroupPage} from "./page-objects/organisationGroup.page";
import {TasksPage} from "./page-objects/tasks.page";
import {ScapUtils} from "./page-objects/scapUtils";
import {SubmitPage} from "./page-objects/submit.page.ts";

const loginPage = new LoginPage();
const workAreaPage = new WorkAreaPage();
const organisationGroupPage = new OrganisationGroupPage();
const taskPage = new TasksPage();
const submitPage = new SubmitPage();
const scapUtils = new ScapUtils();

describe('Create Invalid SCAP', () => {
    it('invalid SCAP should not be submittable', async () => {
        await loginPage.open();
        await loginPage.loginIndustryScapSubmitter();
        await expect(browser).toHaveTitleContaining('Work area - Supply Chain Action Plan');
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