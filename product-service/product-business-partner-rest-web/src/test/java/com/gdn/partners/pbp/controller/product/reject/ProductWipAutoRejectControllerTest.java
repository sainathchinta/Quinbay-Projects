package com.gdn.partners.pbp.controller.product.reject;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.workflow.product.ProductWipAutoRejectService;

public class ProductWipAutoRejectControllerTest {

  private static final String STORE_ID = "10001";
  private static final String CHANNEL_ID = "1";
  private static final String CLIENT_ID = "1";
  private static final String REQUEST_ID = "12345";
  private static final String USERNAME = "TEST";

  @Mock
  private ProductWipAutoRejectService productWipAutoRejectService;

  @InjectMocks
  private ProductWipAutoRejectController productWipAutoRejectController;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.doNothing().when(this.productWipAutoRejectService)
        .autoRejectProductWipNeedCorrectionExpired(Mockito.anyString());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productWipAutoRejectService);
  }

  @Test
  public void autoRejectProductWipNeedCorrectionExpiredTest() throws Exception {
    this.productWipAutoRejectController.autoRejectProductWipNeedCorrection(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID,
        USERNAME);
    Mockito.verify(this.productWipAutoRejectService).autoRejectProductWipNeedCorrectionExpired(Mockito.eq(STORE_ID));
  }

}
