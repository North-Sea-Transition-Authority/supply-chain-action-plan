<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<#assign pageTitle = "Project details" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  errorItems=errorList
  backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsTextInput.textInput
      path="form.projectName.inputValue"
      labelText="Project name"
      inputClass="govuk-!-width-two-thirds"
    />
    <@fdsTextarea.textarea
      path="form.projectSummary.inputValue"
      labelText="Project summary"
      hintText=" Please provide a brief summary description of the project in question –
      this should capture any key project information not captured in the questions below."
    />
    <@fdsCheckbox.checkboxes
      fieldsetHeadingText="Project type"
      path="form.projectTypes"
      checkboxes=projectTypesMap
    />
    <@fdsTextInput.textInput
      labelText="Project cost estimate"
      path="form.projectCostEstimate.inputValue"
      prefix="£"
      suffix="million"
      inputClass="govuk-input--width-5"
    />
    <@fdsDetails.summaryDetails summaryTitle="What costs should be included in my estimates?">
      The project cost estimate should apply only to the Capital Expenditure
      (CapEx) project phase of FDP / FDPa / Carbon Storage projects,
      or the Abandonment and Decommissioning Expenditure (AbEx) for the decommissioning of assets.
    </@fdsDetails.summaryDetails>
    <@fdsCheckbox.checkbox
      fieldsetHeadingText="North Sea Transition Deal commitments"
      path="form.awareOfLocalContentCommitment"
      labelText="I confirm that I am aware of the industry voluntary commitment to achieving 50% UK content on all related new energy and decommissioning projects"
    />
    <@fdsDetails.summaryDetails summaryTitle="Where can I get information on the voluntary commitment">
      Information on the industry voluntary commitment to achieving 50% UK content on all related new energy and
      decommissioning projects can be found in the
        <@fdsAction.link
          linkText="North Sea Transition Deal"
          linkUrl="https://www.gov.uk/government/publications/north-sea-transition-deal/north-sea-transition-deal-accessible-webpage"
        />
      .
    </@fdsDetails.summaryDetails>
    <@fdsRadio.radioGroup
      path="form.expectsToMeetLocalContentCommitment"
      labelText="Do you anticipate that this project will meet the 50% target?"
      hiddenContent=true
    >
      <@fdsRadio.radioYes
        path="form.expectsToMeetLocalContentCommitment"
      />
      <@fdsRadio.radioNo
        path="form.expectsToMeetLocalContentCommitment"
      >
        <@fdsTextarea.textarea
          path="form.willMissLocalContentCommitmentRationale.inputValue"
          labelText="Provide a rationale on why you do not expect the target will be reached"
          nestingPath="form.expectsToMeetLocalContentCommitment"
        />
      </@fdsRadio.radioNo>

    </@fdsRadio.radioGroup>

    <@fdsAddToList.addToList
      addToListId="project-details-field-selector"
      pathForList="form.fieldIds"
      pathForSelector="form.fieldSelector"
      restUrl=springUrl(fieldSearchRestUrl)
      itemName="Fields"
      selectorInputClass="govuk-!-width-two-thirds"
      alreadyAdded=preselectedFields
      selectorMinInputLength=2
      noItemText="There are no selected fields"
      selectorLabelText="What fields are related to this project?"
    />

    <@fdsRadio.radioGroup
      path="form.hasPlatforms"
      labelText="Are any installations or subsea infrastructure related to this project?"
      hiddenContent=true
    >
        <@fdsRadio.radioYes path="form.hasPlatforms">
          <@fdsAddToList.addToList
            addToListId="project-details-installation-selector"
            pathForList="form.installationIds"
            pathForSelector="form.installationSelector"
            alreadyAdded=preselectedFacilities
            itemName="Installations"
            selectorNestingPath="form.hasPlatforms"
            restUrl=springUrl(facilitiesSearchRestUrl)
            selectorMinInputLength=2
            selectorInputClass="govuk-!-width-two-thirds"
            noItemText="There are no selected installations"
          />
        </@fdsRadio.radioYes>
        <@fdsRadio.radioNo path="form.hasPlatforms"/>
    </@fdsRadio.radioGroup>

    <@fdsDateInput.dateInput
      formId="startDate"
      dayPath="form.startDay.inputValue"
      monthPath="form.startMonth.inputValue"
      yearPath="form.startYear.inputValue"
      labelText="Indicative planned execution start date"
      hintText="For example, 31 3 2023"
    />
    <@fdsDateInput.dateInput
      formId="endDate"
      dayPath="form.endDay.inputValue"
      monthPath="form.endMonth.inputValue"
      yearPath="form.endYear.inputValue"
      labelText="Indicative planned commissioning or completion date"
      hintText="For example, 31 3 2024"
      />

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
          existingFiles=supportingDocumentsUploads
          multiFile=true
        />
      </@fdsFieldset.fieldset>
    <@fdsAction.button buttonText="Save and complete"/>
  </@fdsForm.htmlForm>
</@defaultPage>

<#macro addInstallations>

</#macro>
