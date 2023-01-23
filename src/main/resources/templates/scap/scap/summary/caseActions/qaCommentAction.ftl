<@fdsSlideOutPanel.slideOutPanel panelId="Qa-Panel" headingText="Complete QA checks">
    <@fdsForm.htmlForm actionUrl=springUrl(qaFormSubmitUrl)>
        <@fdsTextarea.textarea
        path="qaForm.qaComments.inputValue"
        optionalLabel=true
        labelText="QA comments"/>

        <@fdsAction.button buttonText="Update SCAP" buttonName="QA"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>