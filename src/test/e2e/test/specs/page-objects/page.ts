/**
 * Main page object containing all methods, selectors and functionality
 * that is shared across all page objects on SCAP
 */
export class Page {
    /**
     * Opens a sub page of the page
     * @param path path of the sub page (e.g. /path/to/page.html)
     */
    async open (path: string) {
        return browser.url(`./${path}`)
    }
}