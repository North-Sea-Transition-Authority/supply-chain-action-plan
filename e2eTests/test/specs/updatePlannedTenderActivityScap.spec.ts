import loginPage from "./page-objects/login.page.ts"
import {TasksPage} from "./page-objects/tasks.page.ts";
import {PlannedTenderPage} from "./page-objects/plannedTender.page.ts";
import {ActualTenderPage} from "./page-objects/actualTender.page.ts";
import {ContractingPerformancePage} from "./page-objects/contractingPerformance.page.ts";
import {ProjectPerformancePage} from "./page-objects/projectPerformance.page.ts";
import {SubmitPage} from "./page-objects/submit.page.ts";
import {SubmitSuccessPage} from "./page-objects/submitSuccess.page.ts";
import {FdsLink} from "../../test-library/page-objects/components/FdsLink.ts";
import {ApproveScapPage} from "./page-objects/approveScap.page.ts";
import {RejectOrUpdateScapPage} from "./page-objects/rejectOrUpdateScap.page.ts";
import scapUtils from "./page-objects/scapUtils.ts";

const taskPage = new TasksPage();
const plannedTenderPage = new PlannedTenderPage();
const actualTenderPage = new ActualTenderPage();
const contractingPerformancePage = new ContractingPerformancePage();
const projectPerformancePage = new ProjectPerformancePage();
const submitPage = new SubmitPage();
const submitSuccessPage = new SubmitSuccessPage();
const approveScapPage = new ApproveScapPage();
const rejectUpdateScapPage = new RejectOrUpdateScapPage();

const SHARED_STORE_SCAP_REF = "scapRef";
describe('Submit and update Planned Tender SCAP', () => {
    it('should successfully submit a new SCAP', async () => {
        await scapUtils.submitScap();
        await scapUtils.signOut();
    });
    it('should request further information', async () => {
        await loginPage.open();
        await loginPage.loginRegulatorScapCaseOfficer();
        const scapRef = await browser.sharedStore.get(SHARED_STORE_SCAP_REF) as string;
        await FdsLink.clickLinkWithText(scapRef);
        await rejectUpdateScapPage.requestFurtherInformation();
        const successMessage = await approveScapPage.getPageTitle();
        await expect(successMessage).toEqual(`Requested further information on ${scapRef}`);
        await scapUtils.signOut();
    });
    it('should successfully update the Planned Tender SCAP', async () =>{
        await loginPage.open();
        await loginPage.loginIndustryScapSubmitter();
        await expect(browser).toHaveTitleContaining('Work area - Supply Chain Action Plan');
        const scapRef = await browser.sharedStore.get(SHARED_STORE_SCAP_REF) as string;
        await FdsLink.clickLinkWithText(scapRef);
        await $(`button=Update SCAP`).click();
        await taskPage.goToPlannedTenderActivity();
        await plannedTenderPage.yesPlannedTenderPage();
        await plannedTenderPage.updateCompletePlannedTenderPage();
        await taskPage.goToActualTenderActivityLink();
        await actualTenderPage.noActualTender();
        await taskPage.goToContractingPerformance();
        await contractingPerformancePage.contractingPerformanceNotFullyClosedOut();
        await taskPage.goToProjectPerformance();
        await projectPerformancePage.projectNotComplete();
        await taskPage.goToReviewAndSubmit();
        await submitPage.reviewAndSubmit();
        const successMessage = await submitSuccessPage.getPageTitle();
        await expect(successMessage).toEqual("SCAP submitted");
        await browser.sharedStore.set(SHARED_STORE_SCAP_REF, scapRef);
        console.log(scapRef);
        await scapUtils.signOut();
    })
    it('should succesfully approve the Planned Tender SCAP', async () =>{
        await scapUtils.approveScap();
        await scapUtils.signOut();
    })
});