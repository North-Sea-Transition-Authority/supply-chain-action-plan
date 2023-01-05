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

    <@fdsRadio.radioGroup path="form.remunerationModel" labelText="Remuneration model" hiddenContent=true>
      <#assign firstItem=true/>
      <#list remunerationModels as key, value>
        <@fdsRadio.radioItem path="form.remunerationModel" itemMap={key : value} isFirstItem=firstItem>
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

    <@fdsRadio.radio
      path="form.contractStage"
      labelText="Contract stage"
      radioItems=contractStages
    />

    <#if form.getInvitationToTenderParticipants()?has_content>
      <#assign fieldListSize = form.getInvitationToTenderParticipants()?size>
    <#else>
      <#assign fieldListSize = 0>
    </#if>
    <@fdsAddAField.addAField
      path="form.invitationToTenderParticipants"
      fieldListSize=fieldListSize
      fieldLabelText="Invitation to tender participants"
      actionLinkText="Add another invitation to tender participant"
    />

    <#if contractingPerformanceWarning?has_content>
        <@fdsWarning.warning>
            ${contractingPerformanceWarning}
        </@fdsWarning.warning>
    </#if>
    <@fdsAction.button buttonText="Save and continue"/>
  </@fdsForm.htmlForm>
</@defaultPage>
