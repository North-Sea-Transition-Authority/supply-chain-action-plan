import {Page} from './page.ts'
import {FdsRadio} from "../../../test-library/page-objects/components/FdsRadio.ts";
import {FdsButton} from "../../../test-library/page-objects/components/FdsButton.ts";
import {FdsTextarea} from "../../../test-library/page-objects/components/FdsTextarea.ts";
import {FdsSearchSelector} from "../../../test-library/page-objects/components/FdsSearchSelector.ts";

export class PathfinderProjectsPage extends Page {
    public async noPathfinderProjects() {
        await FdsRadio.selectRadioItemWithText("Is this SCAP related to any Pathfinder projects?", "No");
        await FdsTextarea.enterTextWithLabel("Provide a rationale for not publishing this information on Pathfinder", "Automation Test Rational for not publishing information on pathfinder");
        await FdsButton.clickButtonWithText("Save and complete");
    }

    public async yesPathfinderProjectPage(){
        await FdsRadio.selectRadioItemWithText("Is this SCAP related to any Pathfinder projects?", "Yes");
        await FdsSearchSelector.searchAndSelectOption("What Pathfinder projects are related to this SCAP?","CENTRICA Pathfinder Project Test");
        await FdsButton.clickButtonWithText("Save and complete");
    }
}