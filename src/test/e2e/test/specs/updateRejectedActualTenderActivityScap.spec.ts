import {LoginPage} from "./page-objects/login.page";
import {TasksPage} from "./page-objects/tasks.page";
import {ProjectDetailsPage} from "./page-objects/projectDetails.page";
import {PlannedTenderPage} from "./page-objects/plannedTender.page";
import {ActualTenderPage} from "./page-objects/actualTender.page";
import {ContractingPerformancePage} from "./page-objects/contractingPerformance.page";
import {ProjectPerformancePage} from "./page-objects/projectPerformance.page";
import {SubmitPage} from "./page-objects/submit.page";
import {SubmitSuccessPage} from "./page-objects/submitSuccess.page";
import {FdsLink} from "../test-library/page-objects/components/FdsLink";
import {ApproveScapPage} from "./page-objects/approveScap.page";
import {RejectOrUpdateScapPage} from "./page-objects/rejectOrUpdateScap.page";
import {ScapUtils} from "./page-objects/scapUtils";

const loginPage = new LoginPage();
const taskPage = new TasksPage();
const projectDetailsPage = new ProjectDetailsPage();
const plannedTenderPage = new PlannedTenderPage();
const actualTenderPage = new ActualTenderPage();
const contractingPerformancePage = new ContractingPerformancePage();
const projectPerformancePage = new ProjectPerformancePage();
const submitPage = new SubmitPage();
const submitSuccessPage = new SubmitSuccessPage();
const approveScapPage = new ApproveScapPage();
const rejectUpdateScapPage = new RejectOrUpdateScapPage();
const scapUtils = new ScapUtils();

const SHARED_STORE_SCAP_REF = "scapRef";
describe('Submit and update Rejected Actual Tender SCAP', () => {
    it('should successfully submit a new SCAP', async () => {
        await scapUtils.submitScap();
        await scapUtils.signOut();
    });
    it('should request further information', async () => {
        await scapUtils.updateFurtherInformation();
        await scapUtils.signOut();
    });
    it('should successfully update the Rejected Actual Tender SCAP', async () =>{
        await loginPage.open();
        await loginPage.loginIndustryScapSubmitter();
        await expect(browser).toHaveTitleContaining('Work area - Supply Chain Action Plan');
        const scapRef = await browser.sharedStore.get(SHARED_STORE_SCAP_REF) as string;
        await FdsLink.clickLinkWithText(scapRef);
        await $(`button=Update SCAP`).click();
        await taskPage.goToProjectDetails();
        await projectDetailsPage.updateProjectDetails();
        await taskPage.goToPlannedTenderActivity();
        await plannedTenderPage.yesPlannedTenderPage();
        await plannedTenderPage.updateCompletePlannedTenderPage();
        await taskPage.goToActualTenderActivityLink();
        await actualTenderPage.yesActualTender();
        await actualTenderPage.updateCompleteActualTenderPage();
        await taskPage.goToContractingPerformance();
        await contractingPerformancePage.contractingPerformanceNotFullyClosedOut();
        await taskPage.goToProjectPerformance();
        await projectPerformancePage.projectNotComplete();
        await taskPage.goToReviewAndSubmit();
        await submitPage.reviewAndSubmit();
        const successMessage = await submitSuccessPage.getPageTitle();
        await expect(successMessage).toEqual("SCAP submitted");
        await browser.sharedStore.set(SHARED_STORE_SCAP_REF, scapRef);
        browser.sharedStore
        console.log(scapRef);
        await scapUtils.signOut();
    })
    it('should succesfully approve the Rejected Actual Tender SCAP', async () => {
        await loginPage.open();
        await loginPage.loginRegulatorScapCaseOfficer();
        const scapRef = await browser.sharedStore.get(SHARED_STORE_SCAP_REF) as string;
        await FdsLink.clickLinkWithText(scapRef);
        await approveScapPage.selectDecisionApproveFromDropdown();
        await approveScapPage.approveScapInSidePannel();
        const successMessage = await approveScapPage.getPageTitle();
        await expect(successMessage).toEqual(`SCAP: ${scapRef} has no objection`);
    })
    it('should succesfully set the next required update', async () =>{
        await rejectUpdateScapPage.setNextRequiredUpdate();
        const scapRef = await browser.sharedStore.get(SHARED_STORE_SCAP_REF) as string;
        const successMessage = await approveScapPage.getPageTitle();
        await expect(successMessage).toEqual(`Set date of next update on ${scapRef}`);
        await scapUtils.signOut();
    })
    it('should successfully update the Rejected Actual Tender SCAP again', async () =>{
        await loginPage.open();
        await loginPage.loginIndustryScapSubmitter();
        await expect(browser).toHaveTitleContaining('Work area - Supply Chain Action Plan');
        const scapRef = await browser.sharedStore.get(SHARED_STORE_SCAP_REF) as string;
        await FdsLink.clickLinkWithText(scapRef);
        await $(`button=Update SCAP`).click();
        await taskPage.goToProjectDetails();
        await projectDetailsPage.updateProjectDetails();
        await taskPage.goToPlannedTenderActivity();
        await plannedTenderPage.noAddtionalPlannedTenderActivity();
        await taskPage.goToActualTenderActivityLink();
        await actualTenderPage.additionalActualTenderActivity();
        await actualTenderPage.completeAdditonalActualTenderPage();
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
    it('should succesfully approve the Rejected Actual Tender SCAP', async () =>{
        await scapUtils.approveScap();
        await scapUtils.signOut();
    })
});