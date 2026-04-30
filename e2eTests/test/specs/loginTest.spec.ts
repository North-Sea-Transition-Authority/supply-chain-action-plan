import loginPage from "./page-objects/login.page.ts"
import scapUtils from "./page-objects/scapUtils.ts";

const email = "industryScapSubmitter@scap.co.uk";
const invalidEmail = "invalid@scap.co.uk";
const password = "dev";
const invalidPassword = "invalidPassword";

describe('SCAP Login', () => {
    it('should not login with invalid email', async () => {
        await loginPage.open();
        await loginPage.login(invalidEmail, password);
        const invalidMessage = await $('div*=Invalid username or password');
        await expect(invalidMessage.isDisplayed());
    });
    it('should not login with invalid password', async () => {
        await loginPage.open();
        await loginPage.login(email, invalidPassword);
        const invalidMessage = await $('div*=Invalid username or password');
        await expect(invalidMessage.isDisplayed);
    });
    it('should log in with valid email and password', async () => {
        await loginPage.open();
        await loginPage.login(email, password);
        await expect(browser).toHaveTitle('Work area - Supply Chain Action Plan');
        await scapUtils.signOut();
        await $(`input[value="Return to the Supply Chain Action Plan login page"]`).click();
    });

});
