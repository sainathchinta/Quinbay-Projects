package com.gdn.partners.pcu.internal.service.impl;

import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.gda.mta.product.dto.response.ProductSkuResponseList;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;

@ExtendWith(MockitoExtension.class)
class PBPOutboundServiceImplTest {

  private static final String PRODUCT_SKU = "product_sku";
  private static final String PRODUCT_CODE = "product_code";
  private static final String REQUEST_ID = "request-id";

  @InjectMocks
  private PBPOutboundServiceImpl pbpOutboundService;

  @Mock
  private PBPFeign pbpFeign;

  @BeforeEach
  public void setup() {
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(pbpFeign);
  }

  @Test
  public void getProductSkuByProductCodeTest() {
    ProductSkuResponseList productSkuResponseList = new ProductSkuResponseList();
    productSkuResponseList.setProductSkuList(List.of(PRODUCT_SKU));
    Mockito.when(pbpFeign.getProductSkuByProductCode(PRODUCT_CODE))
        .thenReturn(new GdnRestSimpleResponse<>(REQUEST_ID, productSkuResponseList));
    String productSku = pbpOutboundService.getProductSkuByProductCode(PRODUCT_CODE);
    Mockito.verify(pbpFeign).getProductSkuByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_SKU, productSku);
  }

}
