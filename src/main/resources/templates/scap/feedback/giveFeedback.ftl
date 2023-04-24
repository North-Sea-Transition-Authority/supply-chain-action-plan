<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->
<#-- @ftlvariable name="scapId" type="uk.co.nstauthority.scap.scap.scap.ScapId" -->

<#assign pageTitle = "Give feedback on ${serviceName}" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  phaseBanner=false
  errorItems=errorList
>

  <@fdsForm.htmlForm>

    <@fdsFieldset.fieldset legendHeading="Satisfaction survey">

      <#if scapId?has_content>
        <input type="hidden" name="applicationId" value="${scapId.scapId()}"/>
      </#if>

      <@fdsRadio.radioGroup path="form.satisfactionRating" labelText="Overall, how satisfied were you with this service?">
        <#list satisfactionRadioItems as value, displayText>
          <@fdsRadio.radioItem path="form.satisfactionRating" itemMap={value: displayText} />
        </#list>
      </@fdsRadio.radioGroup>

      <@fdsFieldset.fieldset
        legendHeading="How could we improve this service?"
        legendHeadingSize="h2"
        legendHeadingClass="govuk-fieldset__legend--s"
      >
        <@fdsTextarea.textarea path="form.comments.inputValue" optionalLabel=true />
      </@fdsFieldset.fieldset>

    </@fdsFieldset.fieldset>

    <@fdsAction.button buttonText="Submit feedback"/>

  </@fdsForm.htmlForm>

</@defaultPage>
