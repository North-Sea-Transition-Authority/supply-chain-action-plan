<@fdsSlideOutPanel.slideOutPanel panelId="Consultation-Response-Panel" headingText="Consultation response">
    <@fdsForm.htmlForm actionUrl=springUrl(consultationResponseSubmitUrl)>
        <@fdsTextarea.textarea
        path="consultationResponseForm.responseComments.inputValue"
        optionalLabel=false
        labelText="What information did you get from the consultation?"/>

        <@fdsFieldset.fieldset
        legendHeading="Consultation Report"
        legendHeadingSize="h2"
        legendHeadingClass="govuk-fieldset__legend--m"
        optionalLabel=true
        >
            <@fdsFileUpload.fileUpload
            id="supportingDocuments"
            formName="consultationResponseForm"
            path="consultationResponseForm.supportingDocuments"
            downloadUrl=supportingDocumentsTemplate.downloadUrl()
            uploadUrl=supportingDocumentsTemplate.uploadUrl()
            deleteUrl=supportingDocumentsTemplate.deleteUrl()
            maxAllowedSize=supportingDocumentsTemplate.maxAllowedSize()
            allowedExtensions=supportingDocumentsTemplate.allowedExtensions()
            dropzoneLinkScreenReaderText="Choose a file to upload"
            existingFiles=supportingDocumentsUploads
            multiFile=false
            />
        </@fdsFieldset.fieldset>

        <@fdsAction.button buttonText="Update case" buttonName="CONSULTATION_RESPONSE"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>
