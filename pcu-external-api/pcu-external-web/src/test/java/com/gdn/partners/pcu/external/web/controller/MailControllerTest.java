package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.MailApiPath;
import com.gdn.partners.pcu.external.service.MailService;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.Matchers.is;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class MailControllerTest extends TestHelper {

  @InjectMocks
  private MailController mailController;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private MailService mailService;


  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(mailController).build();
  }

  @Test
  public void sendMailTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.doNothing().when(mailService).sendMail(Mockito.eq(Constants.BUSINESS_PARTNER_CODE));

    MockHttpServletRequestBuilder requestBuilder = get(MailApiPath.BASE_PATH + MailApiPath.SEND_MAIL).
        contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mailService).sendMail(Mockito.eq(Constants.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void notifyMailVisibilityOptionTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    when(mandatoryParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.when(mailService.notifyMailVisibilityOptionForProductWip(Mockito.eq(Constants.BUSINESS_PARTNER_CODE)))
        .thenReturn(Boolean.TRUE);

    MockHttpServletRequestBuilder requestBuilder =
        get(MailApiPath.BASE_PATH + MailApiPath.NOTIFY_MAIL_VISIBILITY_OPTION).
            contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));

    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
    verify(mailService).notifyMailVisibilityOptionForProductWip(Mockito.eq(Constants.BUSINESS_PARTNER_CODE));
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(mailService);
    verifyNoMoreInteractions(mandatoryParameterHelper);
  }
}
