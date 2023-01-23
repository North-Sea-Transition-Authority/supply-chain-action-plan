<@fdsSlideOutPanel.slideOutPanel panelId="Info-Request-Panel" headingText="Request further information">
    <@fdsError.errorSummary errorItems=errorList![]/>
    <@fdsForm.htmlForm actionUrl=springUrl(infoRequestSubmitUrl) >
        <@fdsTextarea.textarea
        path="infoRequestForm.infoRequest.inputValue"
        labelText=""
        hintText=""/>

        <@fdsAction.button buttonText="Update SCAP" buttonName="INFO_REQUESTED"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>