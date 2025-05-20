import {Page} from "./page.ts";

export class FdsUtils extends Page {

    /**
     * Find a checkbox by its value label, ignoring the group to which it belongs, and click it.
     * Cant use the FDS checkbox util methods as they rely on the group question text, which doesnt exist for some scenarios.
     * @param labelText
     */
    public async selectCheckboxWithLabelText(labelText: string) {
        const checkboxItem = await $(`//div[contains(@class, 'govuk-checkboxes__item')]//label[normalize-space(text())='${labelText}']`)
        await checkboxItem.click();
        return checkboxItem;
    }

}

export default new FdsUtils();