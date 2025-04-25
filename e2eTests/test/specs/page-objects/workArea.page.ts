import {Page} from './page.ts'
export class WorkAreaPage extends Page {
    get startNewScapButton () {
        return $("//*[contains(text(), 'Start new SCAP')]/../a");
    }
}