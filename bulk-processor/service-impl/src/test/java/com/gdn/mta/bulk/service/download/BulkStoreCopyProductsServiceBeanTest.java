package com.gdn.mta.bulk.service.download;


import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.StoreCopyDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.StoreCopyProductResponse;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import org.springframework.test.util.ReflectionTestUtils;

public class BulkStoreCopyProductsServiceBeanTest {

  private static final int MAX_PRODUCT_SIZE = 50;
  private static final String PRODUCT_SKU = "productSku";
  private static final String BUSINESS_PARTNER_CODE = "m-123";
  private Map<String, String> exceptionMap = new HashMap<>();
  private ProductLevel3SummaryResponse productLevel3SummaryResponse =
      new ProductLevel3SummaryResponse();
  private BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response =
      new BulkDownloadProductLevel3Response();
  private StoreCopyDownloadRequest storeCopyDownloadRequest;
  private ProductL3SummaryResponse productL3SummaryResponse = new ProductL3SummaryResponse();

  @InjectMocks
  private BulkStoreCopyProductsServiceBean bulkStoreCopyProductsServiceBean;

  @Mock
  private ProductLevel3Repository mockProductLevel3Repository;

  @Mock
  private BulkProductDataServiceBean bulkProductDataServiceBean;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    bulkDownloadProductLevel3Response =
        new BulkDownloadProductLevel3Response(Arrays.asList(productLevel3SummaryResponse),
            exceptionMap, new PageMetaData(0, 10, 1));
    StoreCopyDownloadRequest.StoreCopyDownloadBuilder productBuilder =
        new StoreCopyDownloadRequest.StoreCopyDownloadBuilder();
    storeCopyDownloadRequest =
        (StoreCopyDownloadRequest) productBuilder.archived(false).bulkProcessType(BulkProcessEntity.STORE_COPY_PRODUCTS)
            .merchant(BUSINESS_PARTNER_CODE).build();
    productL3SummaryResponse.setProductSku(PRODUCT_SKU);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(mockProductLevel3Repository);
  }

  @Test
  public void testGetData() throws Exception {
    Mockito.when(mockProductLevel3Repository.findSummaryByFilterForBulkDownload(eq(BUSINESS_PARTNER_CODE),
            Mockito.any(Pageable.class), Mockito.any(ProductLevel3SummaryRequest.class)))
        .thenReturn(bulkDownloadProductLevel3Response);
    StoreCopyProductResponse data =
        (StoreCopyProductResponse) bulkStoreCopyProductsServiceBean.getData(storeCopyDownloadRequest);
    Mockito.verify(
        mockProductLevel3Repository).findSummaryByFilterForBulkDownload(eq(BUSINESS_PARTNER_CODE), Mockito.any(),
            Mockito.any(ProductLevel3SummaryRequest.class));
    Assertions.assertEquals(1, data.getProductLevel3SummaryResponses().size());
  }

  @Test
  public void testGetDataMorethan50() throws Exception {
    bulkDownloadProductLevel3Response.getPageMetaData().setTotalRecords(100);
    Mockito.when(mockProductLevel3Repository.findSummaryByFilterForBulkDownload(eq(BUSINESS_PARTNER_CODE),
            Mockito.any(Pageable.class), Mockito.any(ProductLevel3SummaryRequest.class)))
        .thenReturn(bulkDownloadProductLevel3Response);
    StoreCopyProductResponse data =
        (StoreCopyProductResponse) bulkStoreCopyProductsServiceBean.getData(storeCopyDownloadRequest);
    Mockito.verify(mockProductLevel3Repository, Mockito.times(2))
        .findSummaryByFilterForBulkDownload(eq(BUSINESS_PARTNER_CODE), Mockito.any(),
            Mockito.any(ProductLevel3SummaryRequest.class));
    Assertions.assertEquals(2, data.getProductLevel3SummaryResponses().size());
  }

  @Test
  public void testGetDataExceptionMorethan50() throws Exception {
    bulkDownloadProductLevel3Response.getPageMetaData().setTotalRecords(100);
    Mockito.when(mockProductLevel3Repository.findSummaryByFilterForBulkDownload(eq(BUSINESS_PARTNER_CODE),
            Mockito.any(Pageable.class), Mockito.any(ProductLevel3SummaryRequest.class)))
        .thenReturn(bulkDownloadProductLevel3Response)
        .thenThrow(Exception.class);
    StoreCopyProductResponse data =
        (StoreCopyProductResponse) bulkStoreCopyProductsServiceBean.getData(storeCopyDownloadRequest);
    Mockito.verify(mockProductLevel3Repository, Mockito.times(2))
        .findSummaryByFilterForBulkDownload(eq(BUSINESS_PARTNER_CODE), Mockito.any(),
            Mockito.any(ProductLevel3SummaryRequest.class));
    Assertions.assertEquals(1, data.getProductLevel3SummaryResponses().size());
  }

  @Test
  public void testGetDataExceptionTest() throws Exception {
    Mockito.when(mockProductLevel3Repository.findSummaryByFilterForBulkDownload(eq(BUSINESS_PARTNER_CODE),
            Mockito.any(Pageable.class), Mockito.any(ProductLevel3SummaryRequest.class)))
        .thenThrow(Exception.class);
    StoreCopyProductResponse data = null;
    try {
       data = (StoreCopyProductResponse) bulkStoreCopyProductsServiceBean.getData(storeCopyDownloadRequest);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(mockProductLevel3Repository)
          .findSummaryByFilterForBulkDownload(eq(BUSINESS_PARTNER_CODE), Mockito.any(), Mockito.any(ProductLevel3SummaryRequest.class));
      Assertions.assertNull(data);
    }
  }


  @Test
  public void testGetActiveProductsByMerchantCode_WhenStoreCopyNewFlowEnabledTrue() throws Exception {
    // given
    ReflectionTestUtils.setField(bulkStoreCopyProductsServiceBean, "storeCopyNewFlowEnabled", true);
    ReflectionTestUtils.setField(bulkStoreCopyProductsServiceBean, "maxL4SizeForStoreCopy", 1000);

    ProductL3SummaryResponse productL3SummaryResponse = new ProductL3SummaryResponse();
    productL3SummaryResponse.setProductSku(PRODUCT_SKU);

    List<ProductL3SummaryResponse> productL3SummaryResponseList = Arrays.asList(productL3SummaryResponse);

    // Mock: fetchListOfProductSkusForExcel
    Mockito.when(bulkProductDataServiceBean.fetchListOfProductSkusForExcel(
            Mockito.any(ProductSummaryRequest.class),
            Mockito.anyString(),
            Mockito.anyString()))
        .thenReturn(productL3SummaryResponseList);

    // Mock: bulkDownloadSummary
    Mockito.when(productBusinessPartnerRepository.bulkDownloadSummary(
            Mockito.any(ProductLevel3SummaryRequest.class),
            Mockito.eq(BUSINESS_PARTNER_CODE),
            Mockito.eq(Constants.ALL)))
        .thenReturn(bulkDownloadProductLevel3Response);

    // when
    StoreCopyProductResponse data =
        (StoreCopyProductResponse) bulkStoreCopyProductsServiceBean.getData(storeCopyDownloadRequest);

    // then
    Mockito.verify(bulkProductDataServiceBean, Mockito.times(1))
        .fetchListOfProductSkusForExcel(Mockito.any(), Mockito.anyString(), Mockito.anyString());

    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1))
        .bulkDownloadSummary(Mockito.any(ProductLevel3SummaryRequest.class),
            Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.eq(Constants.ALL));

    Assertions.assertNotNull(data);
    Assertions.assertEquals(1, data.getProductLevel3SummaryResponses().size());
  }

  @Test
  public void testGetActiveProductsByMerchantCode_WhenStoreCopyNewFlowEnabledTrueMaxL4LimitReached() throws Exception {
    // given
    ReflectionTestUtils.setField(bulkStoreCopyProductsServiceBean, "storeCopyNewFlowEnabled", true);
    ReflectionTestUtils.setField(bulkStoreCopyProductsServiceBean, "maxL4SizeForStoreCopy", 0);

    ProductL3SummaryResponse productL3SummaryResponse = new ProductL3SummaryResponse();
    productL3SummaryResponse.setProductSku(PRODUCT_SKU);

    List<ProductL3SummaryResponse> productL3SummaryResponseList = Arrays.asList(productL3SummaryResponse);

    // Mock: fetchListOfProductSkusForExcel
    Mockito.when(bulkProductDataServiceBean.fetchListOfProductSkusForExcel(
            Mockito.any(ProductSummaryRequest.class),
            Mockito.anyString(),
            Mockito.anyString()))
        .thenReturn(productL3SummaryResponseList);

    // Mock: bulkDownloadSummary
    Mockito.when(productBusinessPartnerRepository.bulkDownloadSummary(
            Mockito.any(ProductLevel3SummaryRequest.class),
            Mockito.eq(BUSINESS_PARTNER_CODE),
            Mockito.eq(Constants.ALL)))
        .thenReturn(bulkDownloadProductLevel3Response);

    // when
    StoreCopyProductResponse data =
        (StoreCopyProductResponse) bulkStoreCopyProductsServiceBean.getData(storeCopyDownloadRequest);

    // then
    Mockito.verify(bulkProductDataServiceBean, Mockito.times(1))
        .fetchListOfProductSkusForExcel(Mockito.any(), Mockito.anyString(), Mockito.anyString());

    Mockito.verify(productBusinessPartnerRepository, Mockito.times(1))
        .bulkDownloadSummary(Mockito.any(ProductLevel3SummaryRequest.class),
            Mockito.eq(BUSINESS_PARTNER_CODE), Mockito.eq(Constants.ALL));

    Assertions.assertNotNull(data);
    Assertions.assertEquals(0, data.getProductLevel3SummaryResponses().size());
  }

}