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
    <@fdsTextInput.textInput
      labelText="Estimated value local content"
      path="form.estimatedValueLocalContent.inputValue"
      prefix="£"
      suffix="million"
      inputClass="govuk-input--width-5"
    />
    <#assign localContentText>
      Local content is defined as a UK registered corporate entity employing a UK workforce, paying UK taxes.

      This includes companies who may have an overseas headquarters or Parent Company but are performing the contracted
      scope through a UK registered corporate entity with UK workforce, paying UK taxes.
    </#assign>
    <@fdsDetails.details
      detailsTitle="How is 'local content' defined?"
      detailsText=localContentText
    />

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
      <#assign isFirstItem = true />
      <#list hasInstallationsMap as value, text >
        <@fdsRadio.radioItem
          path="form.hasPlatforms"
          itemMap={value:text}
          isFirstItem=isFirstItem
        >
          <#if value == "YES">
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
          </#if>
        </@fdsRadio.radioItem>
      </#list>
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
