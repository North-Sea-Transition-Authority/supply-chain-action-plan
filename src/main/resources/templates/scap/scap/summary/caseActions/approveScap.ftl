<@fdsSlideOutPanel.slideOutPanel panelId="Approve-scap-Panel" headingText="Approve SCAP">
    <@fdsForm.htmlForm actionUrl=springUrl(approvalFormSubmitUrl)>
        <@fdsTextarea.textarea
        path="scapApprovalForm.approvalComments.inputValue"
        labelText="Approval comments"/>

        <@fdsAction.button buttonText="Update SCAP" buttonName="APPROVED"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>