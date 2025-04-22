<#include '../layout/layout.ftl'>

<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->

<#assign pageTitle = "Related Pathfinder projects" />

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  pageSize=PageSize.TWO_THIRDS_COLUMN
  errorItems=errorList
  backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsForm.htmlForm>
    <@fdsRadio.radioGroup
      path="form.hasPathfinderProjects"
      labelText="Is this SCAP related to any Pathfinder projects?"
      hiddenContent=true
    >
      <@fdsRadio.radioYes path="form.hasPathfinderProjects">
        <@fdsAddToList.addToList
          selectorLabelText="What Pathfinder projects are related to this SCAP?"
          pathForList="form.pathfinderProjectIds"
          pathForSelector="form.pathfinderProjectSelector"
          alreadyAdded=preselectedProjects
          restUrl=springUrl(pathfinderSearchRestUrl)
          itemName="Pathfinder project"
          selectorMinInputLength=3
          selectorNestingPath="form.hasPathfinderProjects"
        />
      </@fdsRadio.radioYes>
      <@fdsRadio.radioNo path="form.hasPathfinderProjects">
        <@fdsTextarea.textarea
          labelText="Provide a rationale for not publishing this information on Pathfinder"
          path="form.noPathfinderProjectRationale.inputValue"
          nestingPath="form.hasPathfinderProjects"
        />
      </@fdsRadio.radioNo>
    </@fdsRadio.radioGroup>
    <@fdsAction.button buttonText="Save and complete"/>
  </@fdsForm.htmlForm>
</@defaultPage>
