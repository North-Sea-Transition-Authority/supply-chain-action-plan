import {Page} from './page'
import {FdsTextInput} from "../../test-library/page-objects/components/FdsTextInput";
import {FdsTextarea} from "../../test-library/page-objects/components/FdsTextarea";
import {FdsCheckbox} from "../../test-library/page-objects/components/FdsCheckbox";
import {FdsRadio} from "../../test-library/page-objects/components/FdsRadio";
import {FdsSearchSelector} from "../../test-library/page-objects/components/FdsSearchSelector";
import {FdsDateInput} from "../../test-library/page-objects/components/FdsDateInput";
import {FdsButton} from "../../test-library/page-objects/components/FdsButton";
export class ProjectDetailsPage extends Page {

    public async enterProjectDetails() {
        await FdsTextInput.enterTextWithLabel("Project name", "Automation Test Project Name");
        await FdsTextarea.enterTextWithLabel("Project summary", "Automation Test Project Summary Text");
        await FdsCheckbox.selectCheckboxItemWithText("Project type", "Field development plan");
        await FdsTextInput.enterTextWithLabel("Project cost estimate", "15");
        await FdsCheckbox.selectCheckboxItemWithPartialText("North Sea Transition Deal commitments", "I confirm that I am aware of the industry voluntary commitment to achieving 50% UK");
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