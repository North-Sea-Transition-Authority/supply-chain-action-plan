import {ScapUtils} from "./page-objects/scapUtils";

const scapUtils = new ScapUtils();

describe('Submit and Approve SCAP', () => {
    it('should successfully submit and approve a new SCAP', async () => {
        await scapUtils.submitScap();
        await scapUtils.signOut();
        await scapUtils.approveScap();
        await scapUtils.signOut();
    });
})