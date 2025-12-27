package com.gdn.mta.bulk.service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.feignConfig.InventoryFeign;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.dto.common.ListRequestDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class InventoryOutboundServiceTest {

  private ListRequestDTO listRequestDTO = new ListRequestDTO();

  @Mock
  private InventoryFeign inventoryFeign;

  @InjectMocks
  private InventoryOutboundServiceBean inventoryOutboundServiceBean;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.inventoryFeign);
  }

  @Test
  public void findDetailByWebMerchantCodeAndWebItemSkuTest() {
    Mockito.when(this.inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(Constant.STORE_ID,
      Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
      listRequestDTO)).thenReturn(new GdnRestListResponse(null, null, true, null, null, null));
    this.inventoryOutboundServiceBean.findDetailByWebMerchantCodeAndWebItemSku(listRequestDTO);
    Mockito.verify(this.inventoryFeign)
      .findDetailByWebMerchantCodeAndWebItemSku(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, listRequestDTO);
  }

  @Test
  public void findDetailByWebMerchantCodeAndWebItemSku_successFalseTest() {
    Mockito.when(this.inventoryFeign.findDetailByWebMerchantCodeAndWebItemSku(Constant.STORE_ID,
      Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
      listRequestDTO)).thenReturn(new GdnRestListResponse(null, null, false, null, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.inventoryOutboundServiceBean.findDetailByWebMerchantCodeAndWebItemSku(listRequestDTO));
    } finally {
      Mockito.verify(this.inventoryFeign)
        .findDetailByWebMerchantCodeAndWebItemSku(Constant.STORE_ID, Constant.CHANNEL_ID,
          Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, listRequestDTO);
    }
  }
}
