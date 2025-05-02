import {Page} from './page.ts'

export const industryScapSubmitter = "IndustryScapSubmitter@scap.co.uk"
export const regulatorScapCaseOfficer = "RegulatorScapCaseOfficer@scap.co.uk";
const password = "dev";

/**
 * Page model for the Fox SAML login screen
 */
export class LoginPage extends Page {

    get usernameInput () {
        return $('input[type="text"]');
    }

    get passwordInput () {
        return $('input[type="password"]');
    }

    get submitButton () {
        return $('input[type="submit"]');
    }

    get returnToScapPortalButton(){
        return $('input[type="button"]');
    }

    public async login (username: string, password: string) {
        await this.usernameInput.setValue(username);
        await this.passwordInput.setValue(password);
        await this.submitButton.click();
    }

    open () {
        return super.open('scap/work-area');
    }

    async loginRegulatorScapCaseOfficer() {
        await this.login(regulatorScapCaseOfficer, password);
    }

    async loginIndustryScapSubmitter() {
        await this.login(industryScapSubmitter, password);
    }

}

export default new LoginPage();