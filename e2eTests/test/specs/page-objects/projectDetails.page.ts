import {Page} from './page.ts'
import {FdsRadio} from "../../../test-library/page-objects/components/FdsRadio.ts";
import {FdsButton} from "../../../test-library/page-objects/components/FdsButton.ts";
import {FdsTextarea} from "../../../test-library/page-objects/components/FdsTextarea.ts";
import {FdsTextInput} from "../../../test-library/page-objects/components/FdsTextInput.ts";
import {FdsDateInput} from "../../../test-library/page-objects/components/FdsDateInput.ts";
import {FdsSearchSelector} from "../../../test-library/page-objects/components/FdsSearchSelector.ts";
import {FdsCheckbox} from "../../../test-library/page-objects/components/FdsCheckbox.ts";
import fdsUtils from "./fdsUtils.ts";

export class ProjectDetailsPage extends Page {

    public async enterProjectDetails() {
        await FdsTextInput.enterTextWithLabel("Project name", "Automation Test Project Name");
        await FdsTextarea.enterTextWithLabel("Project summary", "Automation Test Project Summary Text");
        await FdsCheckbox.selectCheckboxItemWithText("Project type", "Field development plan");
        await FdsTextInput.enterTextWithLabel("Project cost estimate", "15");
        await fdsUtils.selectCheckboxWithLabelText("I confirm that I am aware of the industry voluntary commitment to achieving 50% UK content on all related new energy and decommissioning projects");
        await FdsRadio.selectRadioItemWithText("Do you anticipate that this project will meet the 50% target?", "Yes");
        await FdsSearchSelector.searchAndSelectOption("What fields are related to this project?", "test");
        await FdsRadio.selectRadioItemWithText("Are any installations or subsea infrastructure related to this project?", "No");
        await FdsDateInput.setDateInputWithLabel("Indicative planned execution start date", "31", "3", "2023");
        await FdsDateInput.setDateInputWithLabel("Indicative planned commissioning or completion date", "31", "3", "2031");
        await FdsButton.clickButtonWithText("Save and complete");
    }

    public async updateProjectDetails() {
        await FdsTextInput.enterTextWithLabel("Project name", "Automation Test Project Name - UPDATED");
        await FdsTextarea.enterTextWithLabel("Project summary", "Automation Test Project Summary Text - UPDATED");
        await FdsCheckbox.selectCheckboxItemWithText("Project type", "Field development plan addendum");
        await FdsTextInput.enterTextWithLabel("Project cost estimate", "25");
        await FdsRadio.selectRadioItemWithText("Do you anticipate that this project will meet the 50% target?", "Yes");
        await FdsSearchSelector.searchAndSelectOption("What fields are related to this project?", "Pathfinder test field");
        await FdsRadio.selectRadioItemWithText("Are any installations or subsea infrastructure related to this project?", "No");
        await FdsDateInput.setDateInputWithLabel("Indicative planned execution start date", "31", "3", "2024");
        await FdsDateInput.setDateInputWithLabel("Indicative planned commissioning or completion date", "31", "3", "2032");
        await FdsButton.clickButtonWithText("Save and complete");
    }
}