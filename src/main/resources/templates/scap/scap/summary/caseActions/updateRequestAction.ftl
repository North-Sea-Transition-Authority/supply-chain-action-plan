<@fdsSlideOutPanel.slideOutPanel panelId="Update-Request-Panel" headingText="Next required update">
    <@fdsForm.htmlForm actionUrl=springUrl(updateRequestSubmitUrl) >
        <@fdsDateInput.dateInput
        formId="dueDate"
        dayPath="updateRequestForm.dueDate.dayInput.inputValue"
        monthPath="updateRequestForm.dueDate.monthInput.inputValue"
        yearPath="updateRequestForm.dueDate.yearInput.inputValue"
        labelText="When is the next required update?"
        hintText="For example, 31 3 2024"
        />
        <@fdsTextarea.textarea
        path="updateRequestForm.infoRequest.inputValue"
        labelText="What information needs to be updated? "
        hintText=""/>
        <@fdsAction.button buttonText="Set required update" buttonName="UPDATE_REQUESTED"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>
