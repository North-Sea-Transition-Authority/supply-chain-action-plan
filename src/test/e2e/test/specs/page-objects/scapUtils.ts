import {Page} from './page'
import {LoginPage} from "./login.page";
import {FdsLink} from "../../test-library/page-objects/components/FdsLink";
import {ApproveScapPage} from "./approveScap.page";
import {RejectOrUpdateScapPage} from "./rejectOrUpdateScap.page";
import {FdsButton} from "../../test-library/page-objects/components/FdsButton";
import {WorkAreaPage} from "./workArea.page"
import {OrganisationGroupPage} from "./organisationGroup.page";
import {TasksPage} from "./tasks.page";
import {ProjectDetailsPage} from "./projectDetails.page";
import {PlannedTenderPage} from "./plannedTender.page";
import {PathfinderProjectsPage} from "./pathfinderProjects.page";
import {ActualTenderPage} from "./actualTender.page";
import {ContractingPerformancePage} from "./contractingPerformance.page";
import {ProjectPerformancePage} from "./projectPerformance.page";
import {SubmitPage} from "./submit.page";
import {SubmitSuccessPage} from "./submitSuccess.page";


const workAreaPage = new WorkAreaPage();
const organisationGroupPage = new OrganisationGroupPage();
const taskPage = new TasksPage();
const projectDetailsPage = new ProjectDetailsPage();
const plannedTenderPage = new PlannedTenderPage();
const pathfinderProjectsPage = new PathfinderProjectsPage();
const actualTenderPage = new ActualTenderPage();
const contractingPerformancePage = new ContractingPerformancePage();
const projectPerformancePage = new ProjectPerformancePage();
const submitPage = new SubmitPage();
const submitSuccessPage = new SubmitSuccessPage();
const loginPage = new LoginPage();
const approveScapPage = new ApproveScapPage();
const rejectUpdateScapPage = new RejectOrUpdateScapPage();

const SHARED_STORE_SCAP_REF = "scapRef";
export class ScapUtils extends Page {


    public async updateFurtherInformation () {
        await loginPage.open();
        await loginPage.loginRegulatorScapCaseOfficer();
        const scapRef = await browser.sharedStore.get(SHARED_STORE_SCAP_REF) as string;
        await FdsLink.clickLinkWithText(scapRef);
        await rejectUpdateScapPage.requestFurtherInformation();
        const successMessage = await approveScapPage.getPageTitle();
        await expect(successMessage).toEqual(`Requested further information on ${scapRef}`);
    }

    public async submitScap() {
        await loginPage.open();
        await loginPage.loginIndustryScapSubmitter();
        await expect(browser).toHaveTitleContaining('Work area - Supply Chain Action Plan');
        await workAreaPage.startNewScapButton.click();
        await FdsButton.clickButtonWithPartialText("Start now");
        await organisationGroupPage.selectOperator("CENTRICA");
        await organisationGroupPage.notTierOneContractor();
        await organisationGroupPage.saveAndContinue();
        await taskPage.goToProjectDetails();
        await projectDetailsPage.enterProjectDetails();
        await taskPage.goToPlannedTenderActivity();
        await plannedTenderPage.noPlannedTender();
        await taskPage.goToRelatedPathfinderProjects();
        await pathfinderProjectsPage.noPathfinderProjects();
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
        const scapRef = await submitSuccessPage.getScapRef();
        await browser.sharedStore.set(SHARED_STORE_SCAP_REF, scapRef);
        console.log(scapRef);
    }

    public async approveScap() {
        await loginPage.open();
        await loginPage.loginRegulatorScapCaseOfficer();
        const scapRef = await browser.sharedStore.get(SHARED_STORE_SCAP_REF) as string;
        await FdsLink.clickLinkWithText(scapRef);
        await approveScapPage.selectDecisionApproveFromDropdown();
        await approveScapPage.approveScapInSidePannel();
        const successMessage = await approveScapPage.getPageTitle();
        await expect(successMessage).toEqual(`SCAP: ${scapRef} has been approved`);
    }

    public async signOut () {
        await $(`button=Sign out`).click();
    }
}