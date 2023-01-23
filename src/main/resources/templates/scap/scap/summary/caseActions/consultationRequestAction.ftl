<@fdsSlideOutPanel.slideOutPanel panelId="Consultation-Request-Panel" headingText="Request consultation">
    <@fdsForm.htmlForm actionUrl=springUrl(consultationRequestSubmitUrl)>
        <@fdsTextarea.textarea
        path="consultationRequestForm.requestComments.inputValue"
        optionalLabel=true
        labelText="Comments"/>

        <@fdsAction.button buttonText="Update SCAP" buttonName="CONSULTATION_REQUESTED"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>