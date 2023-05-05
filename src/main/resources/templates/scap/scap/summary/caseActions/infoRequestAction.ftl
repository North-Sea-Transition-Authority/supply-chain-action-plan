<@fdsSlideOutPanel.slideOutPanel panelId="Info-Request-Panel" headingText="Request further information">
    <@fdsForm.htmlForm actionUrl=springUrl(infoRequestSubmitUrl) >
        <@fdsTextarea.textarea
        path="infoRequestForm.infoRequest.inputValue"
        labelText="What information needs to be expanded upon?"
        hintText=""/>

        <@fdsAction.button buttonText="Update case" buttonName="INFO_REQUESTED"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>
