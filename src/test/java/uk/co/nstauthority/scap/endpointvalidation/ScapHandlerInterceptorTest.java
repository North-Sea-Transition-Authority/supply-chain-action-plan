package uk.co.nstauthority.scap.endpointvalidation;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.HandlerMapping;
import uk.co.nstauthority.scap.authentication.TestUserProvider;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
class ScapHandlerInterceptorTest {

  @Mock
  ScapService scapService;

  @Mock
  ScapDetailService scapDetailService;

  @Mock
  UserDetailService userDetailService;

  @Mock
  HttpServletRequest request;

  @Mock
  HttpServletResponse response;

  @Mock
  HandlerMethod handlerMethod;

  @Mock
  ScapSecurityRule securityRule;

  private Map<String, String> uriVariables;

  private static final ScapId SCAP_ID = new ScapId(1000);

  private ScapHandlerInterceptor scapHandlerInterceptor;

  @BeforeEach
  void setup() {
    uriVariables = new HashMap<>();

    scapHandlerInterceptor = new ScapHandlerInterceptor(
        scapService, userDetailService, Collections.singletonList(securityRule)
    );
  }

  @Test
  @DisplayName("Assert preHandle returns false when any security rule fails and has no failure status or redirect url")
  void preHandle_WhenRuleFails_NullStatusNullUrl() throws IOException, NoSuchMethodException {
    var scap = mock(Scap.class);
    var scapDetail = mock(ScapDetail.class);
    var user = TestUserProvider.getUser();
    var method = TestController.class.getDeclaredMethod("get", ScapId.class);

    uriVariables.put("scapId", SCAP_ID.toString());
    when(request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE)).thenReturn(uriVariables);

    doReturn(ExampleAnnotation.class).when(securityRule).supports();
    doReturn(scap).when(scapService).getScapById(SCAP_ID.scapId());
    doReturn(user).when(userDetailService).getUserDetail();
    doReturn(method).when(handlerMethod).getMethod();
    doReturn(new SecurityRuleResult(false, null, null))
        .when(securityRule).check(any(), eq(request), eq(response), eq(user), eq(scap));

    var interceptorResult = scapHandlerInterceptor.preHandle(
        request,
        response,
        handlerMethod
    );

    assertFalse(interceptorResult);
  }

  @Test
  @DisplayName("Assert preHandle returns false when any security rule fails and has redirect url")
  void preHandle_WhenRuleFails_NonNullUrl() throws IOException, NoSuchMethodException {
    var scap = mock(Scap.class);
    var scapDetail = mock(ScapDetail.class);
    var user = TestUserProvider.getUser();
    var method = TestController.class.getDeclaredMethod("get", ScapId.class);
    var redirectUrl = "redirect-url";

    uriVariables.put("scapId", SCAP_ID.toString());
    when(request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE)).thenReturn(uriVariables);

    doReturn(ExampleAnnotation.class).when(securityRule).supports();
    doReturn(scap).when(scapService).getScapById(SCAP_ID.scapId());
    doReturn(user).when(userDetailService).getUserDetail();
    doReturn(method).when(handlerMethod).getMethod();
    doReturn(new SecurityRuleResult(false, null, redirectUrl))
        .when(securityRule).check(any(), eq(request), eq(response), eq(user), eq(scap));

    var interceptorResult = scapHandlerInterceptor.preHandle(
        request,
        response,
        handlerMethod
    );

    verify(response).sendRedirect(redirectUrl);
    assertFalse(interceptorResult);
  }

  @Test
  @DisplayName("Assert preHandle returns true when security rules pass")
  void preHandle_WhenRulePasses_assertTrue() throws IOException, NoSuchMethodException {
    var scap = mock(Scap.class);
    var scapDetail = mock(ScapDetail.class);
    var user = TestUserProvider.getUser();
    var method = TestController.class.getDeclaredMethod("get", ScapId.class);

    uriVariables.put("scapId", SCAP_ID.toString());
    when(request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE)).thenReturn(uriVariables);

    doReturn(ExampleAnnotation.class).when(securityRule).supports();
    doReturn(scap).when(scapService).getScapById(SCAP_ID.scapId());
    doReturn(user).when(userDetailService).getUserDetail();
    doReturn(method).when(handlerMethod).getMethod();
    doReturn(new SecurityRuleResult(true, null, null))
        .when(securityRule).check(any(), eq(request), eq(response), eq(user), eq(scap));

    var interceptorResult = scapHandlerInterceptor.preHandle(
        request,
        response,
        handlerMethod
    );

    assertTrue(interceptorResult);
  }

  @Test
  @DisplayName("Assert preHandle returns true when security rules pass with method-level annotation")
  void preHandle_WhenRulePassesMethodLevel_assertTrue() throws IOException, NoSuchMethodException {
    var scap = mock(Scap.class);
    var scapDetail = mock(ScapDetail.class);
    var user = TestUserProvider.getUser();
    var method = TestController.class.getDeclaredMethod("getWithMethodLevelAnnotation", ScapId.class);

    uriVariables.put("scapId", SCAP_ID.toString());
    when(request.getAttribute(HandlerMapping.URI_TEMPLATE_VARIABLES_ATTRIBUTE)).thenReturn(uriVariables);

    doReturn(ExampleAnnotation.class).when(securityRule).supports();
    doReturn(scap).when(scapService).getScapById(SCAP_ID.scapId());
    doReturn(user).when(userDetailService).getUserDetail();
    doReturn(method).when(handlerMethod).getMethod();
    doReturn(new SecurityRuleResult(true, null, null))
        .when(securityRule).check(any(), eq(request), eq(response), eq(user), eq(scap));

    var interceptorResult = scapHandlerInterceptor.preHandle(
        request,
        response,
        handlerMethod
    );

    assertTrue(interceptorResult);
  }

  @Controller
  @RequestMapping("/route/{scapId}")
  @ExampleAnnotation(false)
  private static class TestController {
    private static final String ENDPOINT_DATA = "Hello world!";

    @GetMapping
    String get(@PathVariable("scapId") ScapId scapId) {
      return ENDPOINT_DATA;
    }

    @GetMapping
    @ExampleAnnotation(false)
    String getWithMethodLevelAnnotation(@PathVariable("scapId") ScapId scapId) {
      return ENDPOINT_DATA;
    }

    @GetMapping
    String get(@PathVariable String scapId) {
      return ENDPOINT_DATA;
    }
  }
}
