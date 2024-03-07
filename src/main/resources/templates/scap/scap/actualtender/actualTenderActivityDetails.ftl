<#include '../../layout/layout.ftl'>

<#assign pageTitle = "Actual tender activity"/>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
errorItems=errorList
backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsTextInput.textInput
      path="form.scopeTitle.inputValue"
      labelText="Scope title"
      inputClass="govuk-!-width-two-thirds"
      maxCharacterLength=scopeTitleMaxLength
    />
    <@fdsTextarea.textarea path="form.scopeDescription.inputValue" labelText="Scope description"/>
    <@fdsDetails.summaryDetails summaryTitle="What if my activity is a single source contract?">
      <p class="govuk-body">
        Add the contractor as an "Invitation to Tender recipient" and
        add text detailing the "award rationale" as single source.
      </p>
    </@fdsDetails.summaryDetails>
    <@fdsRadio.radioGroup
      path="form.remunerationModel"
      labelText="Remuneration model"
      hiddenContent=true
      hintText="If multiple apply, pick 'Other' and specify in description">
      <#assign firstItem=true/>
      <#list remunerationModels as key, value>
        <@fdsRadio.radioItem
          path="form.remunerationModel"
          itemMap={key : value} isFirstItem=firstItem>
          <#if key == "OTHER">
            <@fdsTextarea.textarea
              path="form.remunerationModelName.inputValue"
              labelText="Provide the remuneration model"
              nestingPath="form.remunerationModel"
            />
          </#if>
          <#assign firstItem=false/>
        </@fdsRadio.radioItem>
      </#list>
    </@fdsRadio.radioGroup>

    <h2 class="govuk-heading-m">Invitation to tender recipients</h2>
    <@fdsAddToList.addToList
      pathForList="form.invitationToTenderParticipants"
      pathForSelector="form.ittParticipantsSelector"
      alreadyAdded=preselectedIttParticipants![]
      restUrl=springUrl(organisationUnitSearchUrl)
      selectorHintText="To add a new company, type the full company name in the search box and press enter"
      itemName="Invitation to tender recipient"
      selectorLabelText="Enter the invitation to tender recipients"
    />

    <@fdsRadio.radio
      path="form.contractStage"
      labelText="Contract stage"
      radioItems=contractStages
    />

    <#if contractingPerformanceWarning?has_content>
      <@fdsWarning.warning>
        ${contractingPerformanceWarning}
      </@fdsWarning.warning>
    </#if>
    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>
</@defaultPage>
