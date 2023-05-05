<@fdsSlideOutPanel.slideOutPanel panelId="Info-Response-Panel" headingText="Respond to further information request">
    <@fdsForm.htmlForm actionUrl=springUrl(infoResponseSubmitUrl) >
        <@fdsTextarea.textarea
        path="infoResponseForm.infoResponse.inputValue"
        labelText="What additional information have you recieved?"
        hintText=""/>
        <@fdsFieldset.fieldset
        legendHeading="Supporting Response document"
        legendHeadingSize="h2"
        legendHeadingClass="govuk-fieldset__legend--m"
        optionalLabel=true
        >
          <@fdsFileUpload.fileUpload
          id="infoResponseDocuments"
          formName="infoResponseForm"
          path="infoResponseForm.infoResponseDocuments"
          downloadUrl=furtherInfoDocumentTemplate.downloadUrl()
          uploadUrl=furtherInfoDocumentTemplate.uploadUrl()
          deleteUrl=furtherInfoDocumentTemplate.deleteUrl()
          maxAllowedSize=furtherInfoDocumentTemplate.maxAllowedSize()
          allowedExtensions=furtherInfoDocumentTemplate.allowedExtensions()
          dropzoneLinkScreenReaderText="Choose a file to upload"
          existingFiles=furtherInfoDocumentUploads
          multiFile=false
          />
        </@fdsFieldset.fieldset>
        <@fdsAction.button buttonText="Update case" buttonName="INFO_RESPONSE"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>
