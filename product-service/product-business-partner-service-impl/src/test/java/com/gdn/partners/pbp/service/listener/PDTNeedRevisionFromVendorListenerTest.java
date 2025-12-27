package com.gdn.partners.pbp.service.listener;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.service.NeedCorrectionService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTNeedRevisionEventModel;

public class PDTNeedRevisionFromVendorListenerTest {

  private static final String PRODUCT_CODE = "productCode";

  private ObjectMapper mapper = new ObjectMapper();
  private PDTNeedRevisionEventModel pdtNeedRevisionEventModel = new PDTNeedRevisionEventModel();

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private NeedCorrectionService needCorrectionService;

  @Mock
  private ProductService productService;

  @InjectMocks
  private PDTNeedRevisionFromVendorListener pdtNeedRevisionFromVendorListener;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    pdtNeedRevisionEventModel.setProductCode(PRODUCT_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(needCorrectionService);
    verifyNoMoreInteractions(productService);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = mapper.writeValueAsString(pdtNeedRevisionEventModel);
    when(this.objectMapper.readValue(message, PDTNeedRevisionEventModel.class)).thenReturn(pdtNeedRevisionEventModel);
    pdtNeedRevisionFromVendorListener.onDomainEventConsumed(message);
    verify(this.objectMapper).readValue(message, PDTNeedRevisionEventModel.class);
    verify(this.needCorrectionService).sendForNeedCorrectionByProductCode(PRODUCT_CODE, false, null, true);
    Mockito.verify(productService).publishProductStatusEventByProductCode(PRODUCT_CODE,
        ProductStatus.NEED_CORRECTION, StringUtils.EMPTY);
  }

  @Test
  public void onDomainEventConsumed_emptyProductCodeTest() throws Exception {
    String message = mapper.writeValueAsString(pdtNeedRevisionEventModel);
    pdtNeedRevisionEventModel.setProductCode(StringUtils.EMPTY);
    when(this.objectMapper.readValue(message, PDTNeedRevisionEventModel.class)).thenReturn(pdtNeedRevisionEventModel);
    try {
      pdtNeedRevisionFromVendorListener.onDomainEventConsumed(message);
    } finally {
      verify(this.objectMapper).readValue(message, PDTNeedRevisionEventModel.class);
    }
  }

  @Test
  public void onDomainEventConsumed_nullTest() throws Exception {
    String message = mapper.writeValueAsString(pdtNeedRevisionEventModel);
    when(this.objectMapper.readValue(message, PDTNeedRevisionEventModel.class)).thenReturn(null);
    try {
      pdtNeedRevisionFromVendorListener.onDomainEventConsumed(message);
    } finally {
      verify(this.objectMapper).readValue(message, PDTNeedRevisionEventModel.class);
    }
  }
}