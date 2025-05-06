<#include '../layout/layout.ftl'>
<#import '../layout/serviceContact.ftl' as serviceContact>


<#-- @ftlvariable name="pageHeading" type="java.lang.String" -->
<#-- @ftlvariable name="serviceBranding" type="uk.co.nstauthority.scap.branding.ServiceConfigurationProperties" -->
<#-- @ftlvariable name="customerBranding" type="uk.co.nstauthority.scap.branding.CustomerConfigurationProperties" -->
<#-- @ftlvariable name="technicalSupport" type="uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfigurationProperties" -->

<#assign pageHeading = "Accessibility statement" />

<@defaultPage htmlTitle=pageHeading pageHeading=pageHeading topNavigation=false backLink=true>

    <#assign customerName = customerBranding.name() />

  <h2 class="govuk-heading-m" id="using-website">Using this website</h2>
  <p class="govuk-body">
    This website is run by the ${customerName}. We want as many people as possible to be able to use this website.
  </p>
  <p class="govuk-body">
    We have also made the website text as simple as possible to understand.
  </p>
  <p class="govuk-body">
      <@fdsAction.link linkText="AbilityNet" linkUrl="https://mcmw.abilitynet.org.uk/" openInNewTab=true /> has advice on
    making your device easier to use if you have a disability.
  </p>

  <h2 class="govuk-heading-m" id="accessibility-coverage">How accessible is this website</h2>
  <p class="govuk-body">
    We know some parts of this website are not fully accessible. We've listed the issues we know about
    in the <a href="#non-accessible-content" class="govuk-link">non-accessible content</a> section.
  </p>
  <p class="govuk-body">
    For users of voice dictation software you may have to use in built features in order to input information.
  </p>

  <h2 class="govuk-heading-m" id="reporting-accessibility-problem">Reporting accessibility problems with this website</h2>
  <p class="govuk-body">
    We are always looking to improve the accessibility of this website. If you need information on this website in a
    different format like accessible PDF, large print, easy read, audio recording or braille or if you find any problems
    that are not listed on this page or think we are not meeting the requirements of the accessibility regulations, contact:

      <@serviceContact.serviceContact serviceContactInfo=technicalSupport />
  </p>
  <p class="govuk-body">
    We will consider your request and get back to you in 5 working days.
  </p>

  <h2 class="govuk-heading-m" id="technical-info">Technical information about this website’s accessibility</h2>
  <p class="govuk-body">
    The ${customerName} is committed to making this website accessible, in accordance with the Public Sector Bodies
    (Websites and Mobile Applications) (No.2) Accessibility Regulations 2018.
  </p>

  <h2 class="govuk-heading-m" id="compliance-status">Compliance status</h2>
  <p class="govuk-body">
    This website is partially compliant with the
      <@fdsAction.link
      linkText="Web Content Accessibility Guidelines version 2.2"
      linkUrl="https://www.w3.org/TR/WCAG22/"
      openInNewTab=true
      /> AA standard, due to the non-compliances listed below.
  </p>

  <h2 class="govuk-heading-m" id="non-accessible-content">Non-accessible content</h2>
  <p class="govuk-body">The content listed below is non-accessible for the following reasons.</p>

  <h3 class="govuk-heading-s">Non-compliance with the accessibility regulations 4 August 2023</h3>
  <ul class="govuk-list govuk-list--bullet">
    <li>
      users are not always notified when conditionally revealed content associated with a radio button or checkbox is
      expanded or collapsed. This fails WCAG 2.2 success criterion 4.1.3 (Status Messages).
    </li>
    <li>
      breadcrumb navigation links are not identified by ARIA landmarks. This fails WCAG 2.2 success criterion
      1.3.1 (Info and Relationships).
    </li>
    <li>
      when uploading a file on macOS with VoiceOver enabled, the user is unable to select the ‘Choose a file’ via
      keyboard only. This fails WCAG 2.2 success criterion 2.2.1 (Keyboard).
    </li>
  </ul>

  <h2 class="govuk-heading-m" id="preparation-statement">Preparation of this accessibility statement</h2>
  <p class="govuk-body">
    This statement was prepared on 9 March 2023. It was last reviewed on 9 March 2023.
  </p>
  <p class="govuk-body">
    This website was last tested on 4 August 2023. The test was carried out by Fivium Ltd.
  </p>
  <p class="govuk-body">
    The ${serviceBranding.name()} service has been developed using the Energy Portal Design System. The Design System was last
    accessibility tested on 4 August 2023. All features on ${serviceBranding.name()} were accessibility tested using automated
    tools as part of the quality assurance process.
  </p>
</@defaultPage>
