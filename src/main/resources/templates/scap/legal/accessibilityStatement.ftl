<#include '../layout/layout.ftl'>
<#import '../layout/serviceContact.ftl' as serviceContact>


<#-- @ftlvariable name="pageHeading" type="java.lang.String" -->
<#-- @ftlvariable name="serviceBranding" type="uk.co.nstauthority.scap.branding.ServiceConfigurationProperties" -->
<#-- @ftlvariable name="customerBranding" type="uk.co.nstauthority.scap.branding.CustomerConfigurationProperties" -->
<#-- @ftlvariable name="technicalSupport" type="uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfigurationProperties" -->

<#assign pageHeading = "Accessibility statement" />

<@defaultPage htmlTitle=pageHeading pageHeading=pageHeading topNavigation=false backLink=true>

    <#assign customerName = customerBranding.name() />

  <p class="govuk-body">
    This accessibility statement applies to the ${serviceBranding.name()} service.
  </p>
  <p class="govuk-body">
    This website is run by the ${customerName}. We want as many people as possible to be able to use this website. For example, that means you should be able to:
  </p>
  <ul class="govuk-list govuk-list--bullet">
    <li>change colours, contrast levels and fonts using browser or device settings</li>
    <li>zoom in up to 400% without the text spilling off the screen</li>
    <li>navigate most of the website using a keyboard or speech recognition software</li>
    <li>listen to most of the website using a screen reader (including the most recent versions of JAWS, NVDA and VoiceOver)</li>
  </ul>
  <p class="govuk-body">
    We have also made the website text as simple as possible to understand.
  </p>
  <p class="govuk-body">
    <@fdsAction.link linkText="AbilityNet" linkUrl="https://mcmw.abilitynet.org.uk/" /> has advice on
    making your device easier to use if you have a disability.
  </p>

  <h2 class="govuk-heading-l" id="accessibility-coverage">How accessible this website is</h2>
  <p class="govuk-body">
    We know some parts of this website are not fully accessible. We've listed the issues we know about
    in the <a href="#non-accessible-content" class="govuk-link">non-accessible content</a> section.
  </p>

  <h2 class="govuk-heading-l" id="reporting-accessibility-problem">Feedback and contact information</h2>
  <p class="govuk-body">
    We are always looking to improve the accessibility of this website. If you need information on this website in a
    different format like accessible PDF, large print, easy read, audio recording or braille or if you find any problems
    that are not listed on this page or think we are not meeting the requirements of the accessibility regulations, contact:

      <@serviceContact.serviceContact serviceContactInfo=technicalSupport />
  </p>
  <p class="govuk-body">
    We will consider your request and get back to you in 5 working days.
  </p>

  <h2 class="govuk-heading-l" id="enforcement-procedure">Enforcement procedure</h2>
  <p class="govuk-body">
    The Equality and Human Rights Commission (EHRC) is responsible for enforcing the Public Sector Bodies (Websites and
    Mobile Applications) (No. 2) Accessibility Regulations 2018 (the ‘accessibility regulations’). If you’re not happy
    with how we respond to your complaint,
      <@fdsAction.link 
      linkText="contact the Equality Advisory and Support Service (EASS)" 
      linkUrl="https://www.equalityadvisoryservice.com/"
      />.
  </p>

  <h2 class="govuk-heading-l" id="technical-info">Technical information about this website’s accessibility</h2>
  <p class="govuk-body">
    The ${customerName} is committed to making this website accessible, in accordance with the Public Sector Bodies
    (Websites and Mobile Applications) (No.2) Accessibility Regulations 2018.
  </p>

  <h2 class="govuk-heading-l" id="compliance-status">Compliance status</h2>
  <p class="govuk-body">
    The website has been tested against the Web Content Accessibility Guidelines (WCAG) 2.2 AA standard.
  </p>
  <p class="govuk-body">
    This website is partially compliant with the
      <@fdsAction.link
      linkText="Web Content Accessibility Guidelines version 2.2"
      linkUrl="https://www.w3.org/TR/WCAG22/"
      /> AA standard, due to the non-compliances listed below.
  </p>

  <h2 class="govuk-heading-l" id="non-accessible-content">Non-accessible content</h2>
  <p class="govuk-body">The content listed below is non-accessible for the following reasons.</p>

  <h3 class="govuk-heading-m">Non-compliance with the accessibility regulations</h3>
  <p class="govuk-body">  
    Some sections of the page are not identified by ARIA landmarks. This fails WCAG 2.2 success criterion 1.3.1 (Info and 
    Relationships).
  </p>
  <p class="govuk-body">  
    Radio buttons which reveal more content when selected use invalid ARIA attributes. This fails WCAG 2.2 success criterion 
    4.1.2 (Name, Role, Value).
  </p>
  <p class="govuk-body">  
    Users of Dragon voice control software may not be able to focus on some expandable elements with the 'Show links' command. 
    This fails WCAG 2.2 success criterion 4.1.2 (Name, Role, Value).
  </p>

  <h2 class="govuk-heading-l" id="preparation-statement">Preparation of this accessibility statement</h2>
  <p class="govuk-body">
    This statement was prepared on 2 June 2025. It was last reviewed on 2 June 2025.
  </p>
  <p class="govuk-body">
    This website was last tested on 29 May 2025 against the WCAG 2.2 AA standard.
  </p>
  <p class="govuk-body">
    The test was carried out by Zoonou. The most viewed pages were tested using automated and manual testing methods.
  </p>
</@defaultPage>
