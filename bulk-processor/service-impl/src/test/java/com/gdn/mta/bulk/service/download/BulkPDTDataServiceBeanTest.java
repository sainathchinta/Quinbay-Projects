package com.gdn.mta.bulk.service.download;

import java.util.ArrayList;
import java.util.List;

import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.web.param.PageableHelper;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.download.ProductVendorDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductVendorResponse;
import com.gdn.mta.bulk.repository.download.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.google.common.collect.ImmutableList;

public class BulkPDTDataServiceBeanTest {

  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";

  Pageable page1 = PageRequest.of(0, 25);
  Pageable page2 = PageRequest.of(1, 25);

  private static final List<String> PRODUCT_CODE_1 = ImmutableList.of("product-code-1", "product-code-2");
  private static final List<String> PRODUCT_CODE_2 = ImmutableList.of("product-code-2", "product-code-2", "product-code-3", "product-code-4");
  private static final List<String> PRODUCT_CODE_3 = ImmutableList.of("product-code-1", "product-code-2", "product-code-3", "product-code-4");

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @InjectMocks
  private BulkPDTDataServiceBean bulkPDTDataServiceBean;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.when(productDistributionTaskRepository
        .getProductsForVendor(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.eq(page1),
            Mockito.any())).thenReturn(generateDistributionProductResponse(PRODUCT_CODE_1));
    Mockito.when(productDistributionTaskRepository
        .getProductsForVendor(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.eq(page2),
            Mockito.any())).thenReturn(generateDistributionProductResponse(PRODUCT_CODE_2));
    Mockito.when(productDistributionTaskRepository
        .getProductsForVendor(Mockito.eq(REQUEST_ID), Mockito.eq(USERNAME), Mockito.any(),
            Mockito.any())).thenReturn(generateDistributionProductResponse(PRODUCT_CODE_3));

    ReflectionTestUtils.setField(bulkPDTDataServiceBean, "vendorBulkDownloadOldSize", 25);
  }

  private Page<DistributionProductResponse> generateDistributionProductResponse(
      List<String> productCodes) throws Exception {
    List<DistributionProductResponse> distributionProductResponses = new ArrayList<>();
    for (String productCode : productCodes) {
      DistributionProductResponse distributionProductResponse = new DistributionProductResponse();
      distributionProductResponse.setProductCode(productCode);
      distributionProductResponses.add(distributionProductResponse);
    }
    return new PageImpl<>(distributionProductResponses, page1,
        distributionProductResponses.size());
  }

  private ProductVendorDownloadRequest generateProductVendorRequest() throws Exception {
    ProductVendorDownloadRequest.ProductVendorDownloadBuilder builder =
        new ProductVendorDownloadRequest.ProductVendorDownloadBuilder();
    builder.bulkProcessType(BulkProcessEntity.PRODUCT_VENDOR);
    builder.directDownload(true);
    builder.downloadType(DownloadType.ALL);
    builder.filename("test.xlsx");
    builder.request(REQUEST_ID);
    builder.productListRequest(new ProductListRequest());
    builder.productSize(50);
    builder.username(USERNAME);
    return builder.build();
  }

  @Test
  public void getDataTest() throws Exception {
    Page<DistributionProductResponse> expectedResponse =
        generateDistributionProductResponse(PRODUCT_CODE_3);
    BulkProductVendorResponse response =
        (BulkProductVendorResponse) bulkPDTDataServiceBean.getData(generateProductVendorRequest());
    Mockito.verify(productDistributionTaskRepository, Mockito.times(2))
        .getProductsForVendor(Mockito.anyString(), Mockito.anyString(), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(getProductCodes(response.getResponseList()),
        getProductCodes(expectedResponse.getContent()));
  }

  private List<String> getProductCodes(List<DistributionProductResponse> sourceList) {
    List<String> productCodes = new ArrayList<>();
    for (DistributionProductResponse response : sourceList) {
      String productCode = response.getProductCode();
      productCodes.add(productCode);
    }
    return productCodes;
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(productDistributionTaskRepository);
  }
}
