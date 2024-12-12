<#include '../../../layout/layout.ftl'>
<#import '../../../scap/macros/noObjectionGuidance.ftl' as noObjectionGuidance>

<@fdsSlideOutPanel.slideOutPanel panelId="No-Objection-scap-Panel" headingText="No objection">
    <@fdsForm.htmlForm actionUrl=springUrl(approvalFormSubmitUrl)>
        <@fdsTextarea.textarea
        path="scapApprovalForm.approvalComments.inputValue"
        labelText="No objection comments"/>
        <@noObjectionGuidance.noObjectionGuidance/>
        <@fdsTextarea.textarea
          path="scapApprovalForm.decisionRationale.inputValue"
          labelText="Summary of decision rationale"
        />
        <@fdsRadio.radioGroup path="scapApprovalForm.projectClosedOut" labelText="Has the SCAP been fully completed?">
            <@fdsRadio.radioItem itemMap={"YES":"Yes"} path="scapApprovalForm.projectClosedOut" itemHintText="It will not be possible to update this SCAP once fully completed"/>
            <@fdsRadio.radioItem itemMap={"NO":"No"} path="scapApprovalForm.projectClosedOut"/>
        </@fdsRadio.radioGroup>
        <@fdsFieldset.fieldset
        legendHeading="Supporting no objection document"
        legendHeadingSize="h2"
        legendHeadingClass="govuk-fieldset__legend--m"
        optionalLabel=true
        >
          <@fdsFileUpload.fileUpload
          id="approvalDocuments"
          formName="scapApprovalForm"
          path="scapApprovalForm.approvalDocuments"
          downloadUrl=approvalDocumentsTemplate.downloadUrl()
          uploadUrl=approvalDocumentsTemplate.uploadUrl()
          deleteUrl=approvalDocumentsTemplate.deleteUrl()
          maxAllowedSize=approvalDocumentsTemplate.maxAllowedSize()
          allowedExtensions=approvalDocumentsTemplate.allowedExtensions()
          dropzoneLinkScreenReaderText="Choose a file to upload"
          existingFiles=approvalDocumentUploads
          multiFile=false
          />
        </@fdsFieldset.fieldset>
    <#-- Using deprecated checkbox macro as checkboxItem macro is not displaying validation errors FDS-486 -->
        <@fdsCheckbox.checkbox
          path="scapApprovalForm.understandGuidance"
          labelText="I understand what no objection means"
        />
        <hr class="govuk-section-break govuk-section-break--m">
        <@fdsAction.button buttonText="SCAP has no objection" buttonName="APPROVED"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>
