<#include '../layout/layout.ftl'>

<#macro taskList sections>
  <@fdsTaskList.taskList>
    <#list sections as section>
      <@fdsTaskList.taskListSection sectionHeadingText=section.displayName() sectionNumber="${section?counter}">
        <#list section.items() as item>
          <#if item.label()??>
              <#if item.actionUrl()??>
                <#assign itemUrl = springUrl(item.actionUrl())/>
              <#else>
                <#assign itemUrl = ""/>
              </#if>
              <#if item.label().name() == 'NOT_STARTED'>
                <#assign tagText="Not started"/>
                <#assign tagClass="govuk-tag--grey"/>
              <#elseif item.label().name() == 'IN_PROGRESS'>
                <#assign tagText="In progress"/>
                <#assign tagClass="govuk-tag--blue"/>
              <#else>
                <#assign tagText=""/>
                <#assign tagClass=""/>
              </#if>
            <@fdsTaskList.taskListItem
              itemText=item.displayName()
              itemUrl=itemUrl
              completed=item.label().name() == 'COMPLETED'
              useNotCompletedLabels=item.label().name() == 'NOT_COMPLETED'
              tagText=tagText
              tagClass=tagClass
            >
              <#if item.labelHint()??>
                <div class="govuk-hint govuk-!-margin-bottom-0">
                  ${item.labelHint()}
                </div>
              </#if>
            </@fdsTaskList.taskListItem>
          <#else>
            <@fdsTaskList.taskListItem
              itemText=item.displayName()
              itemUrl=springUrl(item.actionUrl())
              showTag=false
            />
          </#if>
        </#list>
      </@fdsTaskList.taskListSection>
    </#list>
  </@fdsTaskList.taskList>
</#macro>