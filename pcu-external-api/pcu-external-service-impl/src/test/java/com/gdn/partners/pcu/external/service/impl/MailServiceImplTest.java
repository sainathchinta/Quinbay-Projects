package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class MailServiceImplTest {

  private static final String PRODUCT_BUSINESS_PARTNER_ID = "PRODUCT_BUSINESS_PARTNER_ID";
  private static final String REQUEST_ID = "requestId";

  @Mock
  private PBPFeign pbpFeign;

  @InjectMocks
  private MailServiceImpl mailService;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void notifyMailVisibilityOptionForProductWipTest() {
    when(this.pbpFeign.notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID))
        .thenReturn(new GdnRestSimpleResponse<Boolean>(REQUEST_ID, Boolean.TRUE));
    boolean response = this.mailService.notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID);
    verify(this.pbpFeign).notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID);
    Assertions.assertTrue(response);
  }

  @Test
  public void notifyMailVisibilityOptionForProductWipExceptionTest() {
    when(this.pbpFeign.notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID))
        .thenReturn(new GdnRestSimpleResponse<Boolean>(null, null, false, REQUEST_ID, null));
    try {
      this.mailService.notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID);
    }
    catch(Exception e) {
      verify(this.pbpFeign).notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID);
    }
  }

  @Test
  public void sendMailNotificationTrueTest() {
    when(this.pbpFeign.notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID))
        .thenReturn(new GdnRestSimpleResponse<Boolean>(null, null, true,REQUEST_ID, Boolean.TRUE));
    when(this.pbpFeign.sendEmailForExceededActivation(PRODUCT_BUSINESS_PARTNER_ID))
        .thenReturn(new GdnBaseRestResponse(Boolean.TRUE));
    this.mailService.sendMail(PRODUCT_BUSINESS_PARTNER_ID);
    verify(this.pbpFeign).notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID);
    verify(this.pbpFeign).sendEmailForExceededActivation(PRODUCT_BUSINESS_PARTNER_ID);
  }

  @Test
  public void sendMailNotificationFalseTest() {
    when(this.pbpFeign.notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID))
        .thenReturn(new GdnRestSimpleResponse<Boolean>(null, null, true,REQUEST_ID, Boolean.FALSE));
    this.mailService.sendMail(PRODUCT_BUSINESS_PARTNER_ID);
    verify(this.pbpFeign).notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID);
    verify(this.pbpFeign, times(0)).sendEmailForExceededActivation(Mockito.anyString());
  }

  @Test
  public void sendMailNotificationExceptionTest() {
    when(this.pbpFeign.notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID))
        .thenReturn(new GdnRestSimpleResponse<Boolean>(null, null, true, REQUEST_ID, Boolean.TRUE));
    when(this.pbpFeign.sendEmailForExceededActivation(PRODUCT_BUSINESS_PARTNER_ID))
        .thenReturn(new GdnBaseRestResponse(Boolean.FALSE));
    try {
      this.mailService.sendMail(PRODUCT_BUSINESS_PARTNER_ID);
    } catch (Exception e) {
      verify(this.pbpFeign).notifyMailVisibilityOptionForProductWip(PRODUCT_BUSINESS_PARTNER_ID);
      verify(this.pbpFeign).sendEmailForExceededActivation(PRODUCT_BUSINESS_PARTNER_ID);
    }
  }
}
