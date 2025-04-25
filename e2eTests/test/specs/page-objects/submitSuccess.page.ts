import {Page} from "./page.ts";

export class SubmitSuccessPage extends Page {
    public async getPageTitle(): Promise<string> {
        return $("//h1").getText();
    }

    public async getScapRef() {
        return $("//*[contains(text(), 'SCAP')]/../strong").getText();
    }
}