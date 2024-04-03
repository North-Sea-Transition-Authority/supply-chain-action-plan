import {Page} from './page'
export class WorkAreaPage extends Page {
    get startNewScapButton () {
        return $("//*[contains(text(), 'Start new SCAP')]/../a");
    }
}