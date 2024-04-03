import {Page} from './page'
import {FdsRadio} from "../../test-library/page-objects/components/FdsRadio";
import {FdsButton} from "../../test-library/page-objects/components/FdsButton";
import {FdsTextarea} from "../../test-library/page-objects/components/FdsTextarea";
import {FdsSearchSelector} from "../../test-library/page-objects/components/FdsSearchSelector";

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