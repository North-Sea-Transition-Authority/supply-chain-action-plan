<#include '../layout/layout.ftl'>
<#-- @ftlvariable name="taskListSections" type="java.util.List<uk.co.nstauthority.scap.tasklist.TaskListSectionView>" -->
<#assign pageTitle = "Submit a SCAP" />
<@defaultPage
htmlTitle=pageTitle
pageHeading=pageTitle
pageSize=PageSize.TWO_THIRDS_COLUMN
backLinkUrl=springUrl(backLinkUrl)
>
  <@fdsAction.link
    linkText="Delete SCAP"
    linkUrl=springUrl(deleteScapUrl)
    linkClass="govuk-button govuk-button--secondary"
    role=true
  />

  <@standardTaskList taskListSections=taskListSections />
</@defaultPage>


<#macro standardTaskList taskListSections>
  <@fdsTaskList.taskList>
    <#list taskListSections as section>
      <@fdsTaskList.taskListSection
      sectionNumber="${section?index + 1}"
      sectionHeadingText=section.sectionName()
      warningText=section.sectionWarningText()
      >
        <#list section.taskListItemViews() as item>
          <@fdsTaskList.taskListItem
          itemText=item.displayName
          itemUrl=springUrl(item.actionUrl)
          showTag=item.showTaskListLabels()
          completed=item.isItemValid()
          tagClass=(item.customTaskListLabel.labelType().cssClassName)!""
          tagText=(item.customTaskListLabel.labelText())!""
          useNotCompletedLabels=item.showNotCompletedLabel()
          />
        </#list>
      </@fdsTaskList.taskListSection>
    </#list>
  </@fdsTaskList.taskList>
</#macro>
