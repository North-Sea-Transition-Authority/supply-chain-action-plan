<#include '../layout/layout.ftl'>
<#import  '../macros/genericTaskList.ftl' as taskList>

<#-- @ftlvariable name="taskListSections" type="java.util.List<uk.co.nstauthority.scap.tasklist.TaskListSection>" -->

<#assign pageTitle = "Submit a SCAP" />

<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsAction.link
    linkText=deleteActionText
    linkUrl=springUrl(deleteScapUrl)
    linkClass="govuk-button govuk-button--secondary"
    role=true
  />

  <@standardTaskList taskListSections=taskListSections />
</@defaultPage>


<#macro standardTaskList taskListSections>
  <@taskList.taskList sections=taskListSections />
</#macro>
