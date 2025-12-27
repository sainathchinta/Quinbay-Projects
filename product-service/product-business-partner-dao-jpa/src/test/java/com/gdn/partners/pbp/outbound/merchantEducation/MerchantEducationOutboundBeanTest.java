package com.gdn.partners.pbp.outbound.merchantEducation;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.merchantEducation.feign.MerchantEducationFeign;


public class MerchantEducationOutboundBeanTest {

  @Mock
  private MerchantEducationFeign merchantEducationFeign;

  @InjectMocks
  private MerchantEducationOutboundBean merchantEducationOutboundBean;

  private SingleBaseResponse<NotificationSettings> response;

  private static final String USER_NAME = "userName";
  private static final String STORED_ID = "storeId";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    NotificationSettings notificationSettings = new NotificationSettings();
    response = new SingleBaseResponse<>();
    response.setSuccess(true);
    response.setValue(notificationSettings);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(merchantEducationFeign);
  }

  @Test
  public void findByUsernameAndStoreCode() {
    Mockito.when(merchantEducationFeign
        .findByUsernameAndStoreCode(Constants.STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.REQUIRED_ID, Constants.DEFAULT_USERNAME, USER_NAME, STORED_ID)).thenReturn(response);
    merchantEducationOutboundBean.findByUsernameAndStoreCode(Constants.STORE_ID, USER_NAME, STORED_ID);
    Mockito.verify(merchantEducationFeign)
        .findByUsernameAndStoreCode(Constants.STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.REQUIRED_ID, Constants.DEFAULT_USERNAME, USER_NAME, STORED_ID);
  }

  @Test
  public void findByUsernameAndStoreCodeException() {
    response.setSuccess(false);
    Mockito.when(merchantEducationFeign.findByUsernameAndStoreCode(Constants.STORE_ID,
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, Constants.REQUIRED_ID,
        Constants.DEFAULT_USERNAME, USER_NAME, STORED_ID)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        merchantEducationOutboundBean.findByUsernameAndStoreCode(Constants.STORE_ID, USER_NAME, STORED_ID);
      });
    } finally {
      Mockito.verify(merchantEducationFeign)
          .findByUsernameAndStoreCode(Constants.STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.REQUIRED_ID, Constants.DEFAULT_USERNAME, USER_NAME, STORED_ID);
    }
  }

  @Test
  public void findByUsernameAndStoreCodeExceptionResponseNull() {
    response.setValue(null);
    Mockito.when(merchantEducationFeign
        .findByUsernameAndStoreCode(Constants.STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.REQUIRED_ID, Constants.DEFAULT_USERNAME, USER_NAME, STORED_ID)).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        merchantEducationOutboundBean.findByUsernameAndStoreCode(Constants.STORE_ID, USER_NAME, STORED_ID);
      });
    } finally {
      Mockito.verify(merchantEducationFeign)
          .findByUsernameAndStoreCode(Constants.STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.REQUIRED_ID, Constants.DEFAULT_USERNAME, USER_NAME, STORED_ID);
    }
  }
}