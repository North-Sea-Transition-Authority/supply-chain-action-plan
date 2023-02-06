<#include '../../../layout/layout.ftl'>
<@fdsSlideOutPanel.slideOutPanel panelId="Approve-scap-Panel" headingText="Approve SCAP">
    <@fdsForm.htmlForm actionUrl=springUrl(approvalFormSubmitUrl)>
        <@fdsTextarea.textarea
        path="scapApprovalForm.approvalComments.inputValue"
        labelText="Approval comments"/>
        <@fdsRadio.radioGroup path="scapApprovalForm.projectClosedOut" labelText="Has the SCAP been fully completed?">
            <@fdsRadio.radioItem itemMap={"YES":"Yes"} path="scapApprovalForm.projectClosedOut" itemHintText="It will not be possible to update this SCAP once fully completed"/>
            <@fdsRadio.radioItem itemMap={"NO":"No"} path="scapApprovalForm.projectClosedOut"/>
        </@fdsRadio.radioGroup>
        <@fdsAction.button buttonText="Update SCAP" buttonName="APPROVED"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>