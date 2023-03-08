<@fdsSlideOutPanel.slideOutPanel panelId="Update-Request-Panel" headingText="Request application update">
    <@fdsForm.htmlForm actionUrl=springUrl(updateRequestSubmitUrl) >
        <@fdsTextarea.textarea
        path="updateRequestForm.infoRequest.inputValue"
        labelText="Reason for update request"
        hintText=""/>

        <@fdsDateInput.dateInput
        formId="dueDate"
        dayPath="updateRequestForm.dueDate.dayInput.inputValue"
        monthPath="updateRequestForm.dueDate.monthInput.inputValue"
        yearPath="updateRequestForm.dueDate.yearInput.inputValue"
        labelText="Update required by"
        hintText="For example, 31 3 2024"
        />
        <@fdsAction.button buttonText="Request update for SCAP" buttonName="UPDATE_REQUESTED"/>
    </@fdsForm.htmlForm>
</@fdsSlideOutPanel.slideOutPanel>