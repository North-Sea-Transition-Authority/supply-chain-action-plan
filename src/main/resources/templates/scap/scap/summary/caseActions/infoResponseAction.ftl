<@fdsSlideOutPanel.slideOutPanel panelId="Info-Response-Panel" headingText="Respond to further information request">
    <@fdsForm.htmlForm actionUrl=springUrl(infoResponseSubmitUrl) >
        <@fdsTextarea.textarea
        path="infoResponseForm.infoResponse.inputValue"
        labelText=""
        hintText=""/>

        <@fdsAction.button buttonText="Update SCAP" buttonName="INFO_RESPONSE"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>