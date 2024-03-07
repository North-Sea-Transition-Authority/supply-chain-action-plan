<@fdsSlideOutPanel.slideOutPanel panelId="Consultation-Request-Panel" headingText="Request consultation">
    <@fdsForm.htmlForm actionUrl=springUrl(consultationRequestSubmitUrl)>
        <@fdsTextarea.textarea
        path="consultationRequestForm.requestComments.inputValue"
        optionalLabel=true
        labelText="What information do you need to get consultation on?"/>

        <@fdsAction.button buttonText="Update case" buttonName="CONSULTATION_REQUESTED"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>
