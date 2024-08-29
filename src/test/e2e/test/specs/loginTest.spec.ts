import {LoginPage} from "./page-objects/login.page"
import {ScapUtils} from "./page-objects/scapUtils";

const loginPage = new LoginPage();
const scapUtils = new ScapUtils();
const username = "IndustryAccessManager@scap.co.uk";
const invalidUsername = "invalid@scap.co.uk";
const password = "dev";
const invalidPassword = "invalidPassword";

describe('SCAP Login', () => {
    it('should not login with invalid username', async () => {
        await loginPage.open();
        await loginPage.login(invalidUsername, password);
        const invalidMessage = await $('div*=Invalid username or password');
        await expect(invalidMessage.isDisplayed());
    });
    it('should not login with invalid password', async () => {
        await loginPage.open();
        await loginPage.login(username, invalidPassword);
        const invalidMessage = await $('div*=Invalid username or password');
        await expect(invalidMessage.isDisplayed);
    });
    it('should log in with valid username and password', async () => {
        await loginPage.open();
        await loginPage.login(username, password);
        await expect(browser).toHaveTitleContaining('Work area - Supply Chain Action Plan');
        await scapUtils.signOut();
        await $(`input[value="Return to the Supply Chain Action Plan login page"]`).click();
    });

});