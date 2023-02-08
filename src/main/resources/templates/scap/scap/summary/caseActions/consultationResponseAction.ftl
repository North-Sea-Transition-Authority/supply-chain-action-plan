<@fdsSlideOutPanel.slideOutPanel panelId="Consultation-Response-Panel" headingText="Consultation response">
    <@fdsForm.htmlForm actionUrl=springUrl(consultationResponseSubmitUrl)>
        <@fdsTextarea.textarea
        path="form.responseComments.inputValue"
        optionalLabel=false
        labelText="Comments"/>

        <@fdsFieldset.fieldset
        legendHeading="Supporting SCAP documents"
        legendHeadingSize="h2"
        legendHeadingClass="govuk-fieldset__legend--m"
        optionalLabel=true
        >
            <@fdsFileUpload.fileUpload
            id="supportingDocuments"
            path="form.supportingDocuments"
            downloadUrl=supportingDocumentsTemplate.downloadUrl()
            uploadUrl=supportingDocumentsTemplate.uploadUrl()
            deleteUrl=supportingDocumentsTemplate.deleteUrl()
            maxAllowedSize=supportingDocumentsTemplate.maxAllowedSize()
            allowedExtensions=supportingDocumentsTemplate.allowedExtensions()
            dropzoneLinkScreenReaderText="Choose a file to upload"
            existingFiles=supportingDocumentsUploads
            multiFile=true
            />
        </@fdsFieldset.fieldset>

        <@fdsAction.button buttonText="Update SCAP" buttonName="CONSULTATION_RESPONSE"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>