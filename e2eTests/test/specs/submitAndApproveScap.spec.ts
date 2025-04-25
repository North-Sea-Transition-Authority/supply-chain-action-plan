import scapUtils from "./page-objects/scapUtils.ts";

describe('Submit and Approve SCAP', () => {
    it('should successfully submit and approve a new SCAP', async () => {
        await scapUtils.submitScap();
        await scapUtils.signOut();
        await scapUtils.approveScap();
        await scapUtils.signOut();
    });
})