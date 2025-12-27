package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.web.model.request.ProductSizeChartUpdateWebRequest;
import com.gdn.x.product.rest.web.model.request.ProductSizeChartUpdateRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.Set;

public class SizeChartServiceImplTest {

  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String SIZE_CHART_CODE = "SIZE_CHART_CODE";
  private static final String PRODUCT_SKU_1 = "PRODUCT_SKU_1";
  private static final String PRODUCT_SKU_2 = "PRODUCT_SKU_2";

  private ProductSizeChartUpdateWebRequest productSizeChartUpdateWebRequest;

  @InjectMocks
  private SizeChartServiceImpl sizeChartServiceImpl;

  @Mock
  private XProductFeign xProductFeign;

  @Captor
  private ArgumentCaptor<ProductSizeChartUpdateRequest> productSizeChartUpdateRequestArgumentCaptor;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    productSizeChartUpdateWebRequest =
        new ProductSizeChartUpdateWebRequest(Set.of(PRODUCT_SKU_1), Set.of(PRODUCT_SKU_2));
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(xProductFeign);
  }

  @Test
  public void updateProductSizeChartTest() {
    Mockito.when(xProductFeign.updateProductSizeChart(Mockito.eq(SIZE_CHART_CODE),
            productSizeChartUpdateRequestArgumentCaptor.capture()))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    sizeChartServiceImpl.updateProductSizeChart(SIZE_CHART_CODE, productSizeChartUpdateWebRequest);
    Mockito.verify(xProductFeign)
        .updateProductSizeChart(Mockito.eq(SIZE_CHART_CODE), Mockito.eq(productSizeChartUpdateRequestArgumentCaptor.getValue()));
    Assertions.assertEquals(Set.of(PRODUCT_SKU_1),
        productSizeChartUpdateRequestArgumentCaptor.getValue().getAddSizeChartSkus());
    Assertions.assertEquals(Set.of(PRODUCT_SKU_2),
        productSizeChartUpdateRequestArgumentCaptor.getValue().getRemoveSizeChartSkus());
  }
}