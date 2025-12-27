package com.gdn.partners.pcu.internal.service.impl;

import java.util.Arrays;

import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.domain.Page;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.web.model.request.DistributionFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductVendorWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.request.ProductDistributionTaskRequest;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DistributionListServiceImplTest {

  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String REQUEST_ID = "requestId";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String KEYWORD = "keyword";
  private static final String SORT_DIRECTION = "ASC";
  private static final String PRODUCT_CODE1 = "PRODUCT_CODE1";
  private static final String PRODUCT_CODE2 = "PRODUCT_CODE2";

  private DistributionFilterWebRequest distributionFilterWebRequest;
  private ProductVendorWebRequest productVendorWebRequest;

  @Mock
  private PDTFeign pdtFeign;

  @InjectMocks
  private DistributionListServiceImpl distributionListService;

  @Captor
  private ArgumentCaptor<DistributionTaskMultipleFilterRequest> distributionTaskMultipleFilterRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductDistributionTaskRequest> productDistributionTaskRequestArgumentCaptor;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    distributionFilterWebRequest = new DistributionFilterWebRequest();
    distributionFilterWebRequest.setVendorCodes(Arrays.asList(VENDOR_CODE));
    distributionFilterWebRequest.setRejectedList(Arrays.asList(1));
    distributionFilterWebRequest.setCategoryCode(CATEGORY_CODE);
    distributionFilterWebRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    distributionFilterWebRequest.setKeyword(KEYWORD);
    distributionFilterWebRequest.setStatusList(Arrays.asList(WorkflowState.PASSED.name()));
    distributionFilterWebRequest.setSortBy(SORT_DIRECTION);

    productVendorWebRequest = new ProductVendorWebRequest();
    productVendorWebRequest.setVendorCode(VENDOR_CODE);
    productVendorWebRequest.setProductCodes(Arrays.asList(PRODUCT_CODE1, PRODUCT_CODE2));
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(this.pdtFeign);
  }

  @Test
  public void getSummaryByMultipleFilterTest() {
    Mockito.when(this.pdtFeign
        .getSummaryByMultipleFilter(Mockito.eq(PAGE), Mockito.eq(SIZE), Mockito.eq(SORT_DIRECTION),
            Mockito.any(DistributionTaskMultipleFilterRequest.class))).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(new DistributionProductResponse()), new PageMetaData(PAGE, SIZE, 1),
            REQUEST_ID));
    Page<DistributionProductWebResponse> responses =
        this.distributionListService.getSummaryByMultipleFilter(PAGE, SIZE, distributionFilterWebRequest);
    Mockito.verify(this.pdtFeign)
        .getSummaryByMultipleFilter(Mockito.eq(PAGE), Mockito.eq(SIZE), Mockito.eq(SORT_DIRECTION),
            distributionTaskMultipleFilterRequestArgumentCaptor.capture());
    Assertions.assertNotNull(responses);
    Assertions.assertEquals(1,
      distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getStatusList().size());
    Assertions.assertEquals(WorkflowState.PASSED.name(),
        distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getStatusList().get(0));
    Assertions.assertEquals(1, distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getVendorCodes().size());
    Assertions.assertEquals(VENDOR_CODE,
        distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getVendorCodes().get(0));
    Assertions.assertEquals(BUSINESS_PARTNER_CODE,
        distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getBusinessPartnerCode());
    Assertions.assertEquals(CATEGORY_CODE, distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getCategoryCode());
    Assertions.assertEquals(KEYWORD, distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getProductName());
    Assertions.assertEquals(1, distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getRejectedList().size());
    Assertions.assertEquals(1,
        distributionTaskMultipleFilterRequestArgumentCaptor.getValue().getRejectedList().get(0).longValue());
  }

  @Test
  public void saveProductVendorMappingTest() {
    Mockito.when(this.pdtFeign.saveProductDistributionTask(Mockito.any(ProductDistributionTaskRequest.class)))
        .thenReturn(new GdnBaseRestResponse(REQUEST_ID));
    this.distributionListService.saveProductVendorMapping(productVendorWebRequest);
    Mockito.verify(this.pdtFeign).saveProductDistributionTask(productDistributionTaskRequestArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE1, productDistributionTaskRequestArgumentCaptor.getValue().getProductCodes().get(0));
    Assertions.assertEquals(PRODUCT_CODE2, productDistributionTaskRequestArgumentCaptor.getValue().getProductCodes().get(1));
    Assertions.assertEquals(VENDOR_CODE, productDistributionTaskRequestArgumentCaptor.getValue().getVendorCode());
  }
}
