package com.gdn.mta.bulk.service.download;

import com.gda.mta.product.dto.BulkDownloadProductLevel3Response;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.download.ProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.service.PickupPointService;
import com.gdn.mta.bulk.service.XProductOutboundService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

/**
 * Created by keshashah on 14/11/16.
 */
public class BulkProductDataServiceBeanTest {

  private static final int MAX_PRODUCT_SIZE = 50;
  private static final String PRODUCT_SKU = "productSku";
  private static final String PRODUCT_SKU_2 = "productSku2";
  private static final String PRODUCT_NAME = "productname";
  private static final String PRODUCT_NAME_2 = "productname2";
  private static final String ITEM_NAME = "itemName";
  private static final String EMAIL_CC = "emailCC";
  private static final String EMAIL_TO = "emailTo";
  private static final String FILE_NAME = "test.xlsx";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String BUSINESS_PARTNER_CODE = "m-123";
  private static final String BUSINESS_PARTNER_CODE_1 = "M-123";
  private static final String PICKUP_POINT_CODE = "PICKUP_POINT_CODE";

  private ProductLevel3SummaryResponse productLevel3SummaryResponse =
      new ProductLevel3SummaryResponse();
  private ProductDownloadRequest productDownloadRequest;
  private BulkDownloadProductLevel3Response bulkDownloadProductLevel3Response =
      new BulkDownloadProductLevel3Response();
  private Map<String, String> exceptionMap = new HashMap<>();
  private List<List<String>> rowData = new ArrayList<>();
  private ProductL3SummaryResponse productL3SummaryResponse = new ProductL3SummaryResponse();
  private ProductL3SummaryResponse productL3SummaryResponse2 = new ProductL3SummaryResponse();
  private ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
  private List<String> productSkuList = new ArrayList<>();
  private Map<String, String> productSkuAndNameMap = new HashMap<>();
  private List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
  private PickupPointResponse pickupPointResponse;

  @Mock
  private BusinessPartnerRepository mockBusinessPartnerRepository;

  @Mock
  private BulkDownloadServiceBeanUtil mockBulkDownloadServiceBeanUtil;

  @Mock
  private ProductLevel3Repository mockProductLevel3Repository;
  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @InjectMocks
  private BulkProductDataServiceBean bulkProductDataServiceBean;

  @Captor
  private ArgumentCaptor<ProductLevel3SummaryRequest> productLevel3SummaryRequestArgumentCaptor;

  @Mock
  private PickupPointService pickupPointService;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    ProductDownloadRequest.ProductBuilder productBuilder =
        new ProductDownloadRequest.ProductBuilder();
    productBuilder.privilegedMap(new HashMap<String, Boolean>());
    productBuilder.productSize(1);
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productBuilder.emailCC(EMAIL_CC);
    productBuilder.emailTo(EMAIL_TO);
    productBuilder.filename(FILE_NAME);
    productBuilder.downloadType(DownloadType.ALL);
    productBuilder.merchant(BUSINESS_PARTNER_CODE);
    productBuilder.request(REQUEST_ID);
    productBuilder.username(USERNAME);
    productBuilder.privilegedMap(new HashMap<>());
    productBuilder.productSize(1);
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productSkuList.add(PRODUCT_SKU);
    productLevel3SummaryRequest.setProductSkuList(productSkuList);
    productLevel3SummaryResponse.setProductName(PRODUCT_NAME);
    productSkuAndNameMap.put(PRODUCT_SKU,PRODUCT_NAME);
    productDownloadRequest = productBuilder.build();
    productDownloadRequest.setProductSummaryRequest(new ProductSummaryRequest());
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    bulkDownloadProductLevel3Response.setProductLevel3SummaryResponses(productLevel3SummaryResponses);

    exceptionMap.put("test", "exception occured");

    bulkDownloadProductLevel3Response =
        new BulkDownloadProductLevel3Response(Arrays.asList(productLevel3SummaryResponse),
            exceptionMap);

    rowData.add(Arrays.asList("data"));
    productL3SummaryResponse.setProductSku(PRODUCT_SKU);
    productL3SummaryResponse.setProductName(PRODUCT_NAME);

    productL3SummaryResponse2.setProductSku(PRODUCT_SKU_2);

    ReflectionTestUtils.setField(bulkProductDataServiceBean, "maxProductSize",
      MAX_PRODUCT_SIZE);

    ReflectionTestUtils.setField(bulkProductDataServiceBean, "maxL5DownloadSize",
        100);

    pickupPointResponse = new PickupPointResponse();
    pickupPointResponse.setCode(PICKUP_POINT_CODE);

    Mockito.when(pickupPointService.getPickupPointSummaryFilter(Mockito.anyInt(),Mockito.any(
      PickupPointFilterRequest.class))).thenReturn(Collections.singletonList(pickupPointResponse));

  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(mockBusinessPartnerRepository);
    verifyNoMoreInteractions(mockProductLevel3Repository);
    verifyNoMoreInteractions(mockBulkDownloadServiceBeanUtil);
    verifyNoMoreInteractions(xProductOutboundService);
  }

  @Test
  public void getData_anyServiceThrowException_propagateException() throws Exception {
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenThrow(new Exception());
    ProductDownloadRequest.ProductBuilder productBuilder =
        new ProductDownloadRequest.ProductBuilder();
    productBuilder.privilegedMap(new HashMap<String, Boolean>());
    productBuilder.productSize(1);
    productBuilder.bulkProcessType(BulkProcessEntity.PRODUCT);
    productBuilder.emailCC("kesha@xyz.com");
    productBuilder.emailTo("kesha@xyz.com");
    productBuilder.filename("test.xlsx");
    productBuilder.downloadType(DownloadType.ALL);
    productBuilder.merchant("m-123");
    productBuilder.request("123");
    ProductDownloadRequest request = productBuilder.build();
    try {
      bulkProductDataServiceBean.getData(request);
    } catch (Exception e) {
      verify(mockBusinessPartnerRepository)
          .filterByBusinessPartnerCodeV2(anyString(), anyString());
    }

  }

  @Test
  public void getData_dataExist_success() throws Exception {
    ProfileResponse profileResponse = getProfile();
    profileResponse.getCompany().setInventoryFulfillment("BP");
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class), any(
            ProductLevel3SummaryRequest.class)))
        .thenReturn(bulkDownloadProductLevel3Response);
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 1));
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(), anyBoolean(),
      any(ProfileResponse.class), Mockito.anyList()))
        .thenReturn(rowData);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository)
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowData.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getData_dataExist_fromDB_success() throws Exception {
    ProfileResponse profileResponse = getProfile();
    profileResponse.getCompany().setInventoryFulfillment("BP");
    ReflectionTestUtils.setField(bulkProductDataServiceBean, "productDownloadFetchFromDb",
        true);
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(productBusinessPartnerRepository.bulkDownloadSummary(any(ProductLevel3SummaryRequest.class), anyString(),
        anyString()))
        .thenReturn(bulkDownloadProductLevel3Response);
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse,productL3SummaryResponse2),
            PageRequest.of(0, 2), 2));
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(), anyBoolean(),
      any(ProfileResponse.class), Mockito.anyList()))
        .thenReturn(rowData);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(productBusinessPartnerRepository).bulkDownloadSummary(productLevel3SummaryRequestArgumentCaptor.capture(),
        eq(BUSINESS_PARTNER_CODE_1), anyString());
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    Assertions.assertEquals(PRODUCT_SKU, productLevel3SummaryRequestArgumentCaptor.getValue().getProductSkuList().get(0));
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowData.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getData_dataNotExist_success() throws Exception {
    ProfileResponse profileResponse = getProfile();
    bulkDownloadProductLevel3Response.setProductLevel3SummaryResponses(Collections.EMPTY_LIST);
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class))).thenReturn(bulkDownloadProductLevel3Response);
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList())).thenReturn(rowData);
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 1));
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository)
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowData.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }


  @Test
  public void getData_failedToGet_success() throws Exception {
    ProfileResponse profileResponse = getProfile();
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class))).thenThrow(new Exception());
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList())).thenReturn(rowData);
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 1));
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository)
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowData.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getData_getNullResponse_success() throws Exception {
    ProfileResponse profileResponse = getProfile();
    bulkDownloadProductLevel3Response.setProductLevel3SummaryResponses(null);
    List<ProductLevel3SummaryResponse> responseList = new ArrayList<>();
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class))).thenReturn(null);
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 1));
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class), any(
            ProductLevel3SummaryRequest.class)))
        .thenReturn(bulkDownloadProductLevel3Response);
    when(mockBulkDownloadServiceBeanUtil
        .getAllProductValues(anyMap(), anyBoolean(), anyList(), anyBoolean(), any(ProfileResponse.class), Mockito.anyList())).thenReturn(rowData);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository)
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowData.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getData_getExceptionMap_success() throws Exception {
    ProfileResponse profileResponse = getProfile();
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 1));
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class))).thenReturn(bulkDownloadProductLevel3Response);
    List<List<String>> rowList = new ArrayList<>();
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList())).thenReturn(rowList);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository)
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowList.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }


  public ProfileResponse getProfile() {
    ProfileResponse response = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setOfflineToOnlineFlag(false);
    companyDTO.setInventoryFulfillment("BL");
    List<PickupPointDTO> pickupPointDTOList = new ArrayList<>();
    PickupPointDTO pickupPointDTO = new PickupPointDTO();
    pickupPointDTO.setName("pickup point1");
    pickupPointDTO.setCode("123");
    pickupPointDTOList.add(pickupPointDTO);
    response.setCompany(companyDTO);
    response.setPickupPoints(pickupPointDTOList);
    response.setBusinessPartnerCode("M-123");
    return response;
  }

  public ProductDownloadRequest getProductDownloadRequest(
      ProductDownloadRequest productDownloadRequest, String pickUpPoint) {
    productDownloadRequest.setProductSummaryRequest(new ProductSummaryRequest());
    productDownloadRequest.getProductSummaryRequest().setPickupPointCodes(new ArrayList<>(Arrays.asList("PP-123456")));
    return productDownloadRequest;
  }

  @Test public void testGetExceptionString() throws Exception {
    Map<String, String> map = new HashMap<>();
    Assertions.assertEquals("", BulkProductDataServiceBean.getExceptionMsgString(map, 10, "in"));
    map.put("0", "test0");
    Assertions.assertNotEquals("", BulkProductDataServiceBean.getExceptionMsgString(map, 10, "in"));
    for (int i = 1; i < 15; i++) {
      map.put(String.valueOf(i), "test" + String.valueOf(i));
    }
    String prefix = "produk gagal yang diberikan di bawah :";
    String sufix = "Jumlah total Produk:";

    Assertions.assertTrue(
        StringUtils.contains(BulkProductDataServiceBean.getExceptionMsgString(map, 20, "in"), prefix));
    Assertions.assertTrue(
        StringUtils.contains(BulkProductDataServiceBean.getExceptionMsgString(map, 20, "in"), sufix));
  }

  @Test
  public void getData_nullL4Data() throws Exception {
    ProfileResponse profileResponse = getProfile();
    profileResponse.getCompany().setInventoryFulfillment("BP");
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class), any(
            ProductLevel3SummaryRequest.class)))
        .thenReturn(null);
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 1));
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(), anyBoolean(),
      any(ProfileResponse.class), Mockito.anyList()))
        .thenReturn(rowData);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository)
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowData.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getData_emptyResponseL4() throws Exception {
    bulkDownloadProductLevel3Response.setProductLevel3SummaryResponses(Collections.EMPTY_LIST);
    bulkDownloadProductLevel3Response.setExceptionMap(Collections.EMPTY_MAP);
    ProfileResponse profileResponse = getProfile();
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 1));
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class))).thenReturn(bulkDownloadProductLevel3Response);
    List<List<String>> rowList = new ArrayList<>();
    when(mockBulkDownloadServiceBeanUtil
        .getAllProductValues(anyMap(), anyBoolean(), anyList(), anyBoolean(),
          any(ProfileResponse.class), Mockito.anyList())).thenReturn(rowList);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository)
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowList.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getData_moreThanOnePageL4Response() throws Exception {
    bulkDownloadProductLevel3Response.getPageMetaData().setTotalRecords(51);
    ProfileResponse profileResponse = getProfile();
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 1));
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class))).thenReturn(bulkDownloadProductLevel3Response);
    List<List<String>> rowList = new ArrayList<>();
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList())).thenReturn(rowList);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository, times(2))
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowList.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getData_dataExist_fromDB_EmptyTest() throws Exception {
    ProfileResponse profileResponse = getProfile();
    profileResponse.getCompany().setInventoryFulfillment("BP");
    ReflectionTestUtils.setField(bulkProductDataServiceBean, "productDownloadFetchFromDb",
      true);
    when(mockBusinessPartnerRepository
      .filterByBusinessPartnerCodeV2(anyString(), anyString()))
      .thenReturn(profileResponse);
    when(this.xProductOutboundService
      .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
        eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
      new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 1));
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(), anyBoolean(),
      any(ProfileResponse.class), Mockito.anyList()))
      .thenReturn(rowData);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    verify(mockBusinessPartnerRepository)
      .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
      .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
        eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowData.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getDataFromDBSyncTrueProductTest() throws Exception {
    ItemL4SummaryResponse itemL4SummaryResponse = new ItemL4SummaryResponse();
    ItemL4SummaryResponse itemL4SummaryResponse2 = new ItemL4SummaryResponse();
    itemL4SummaryResponse2.setGeneratedItemName(ITEM_NAME);
    productL3SummaryResponse.setSynchronized(false);
    productL3SummaryResponse2.setSynchronized(false);
    productL3SummaryResponse.setItemL4SummaryResponse(itemL4SummaryResponse);
    productL3SummaryResponse2.setItemL4SummaryResponse(itemL4SummaryResponse2);
    ProfileResponse profileResponse = getProfile();
    profileResponse.getCompany().setInventoryFulfillment("BP");
    ReflectionTestUtils.setField(bulkProductDataServiceBean, "productDownloadFetchFromDb",
      true);
    when(mockBusinessPartnerRepository
      .filterByBusinessPartnerCodeV2(anyString(), anyString()))
      .thenReturn(profileResponse);
    when(productBusinessPartnerRepository.bulkDownloadSummary(any(ProductLevel3SummaryRequest.class), anyString(),
        anyString()))
      .thenReturn(bulkDownloadProductLevel3Response);
    when(this.xProductOutboundService
      .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
        eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
      new PageImpl<>(Arrays.asList(productL3SummaryResponse,productL3SummaryResponse2),
          PageRequest.of(0, 2), 2));
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(), anyBoolean(),
      any(ProfileResponse.class), Mockito.anyList()))
      .thenReturn(rowData);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    verify(mockBusinessPartnerRepository)
      .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(productBusinessPartnerRepository).bulkDownloadSummary(productLevel3SummaryRequestArgumentCaptor.capture(),
      eq(BUSINESS_PARTNER_CODE_1), anyString());
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
      .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
        eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    Assertions.assertEquals(PRODUCT_SKU, productLevel3SummaryRequestArgumentCaptor.getValue().getProductSkuList().get(0));
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowData.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getData_emptyResponseL4Test() throws Exception {
    ReflectionTestUtils.setField(bulkProductDataServiceBean, "maxProductSize",
      1);
    bulkDownloadProductLevel3Response.setProductLevel3SummaryResponses(Collections.EMPTY_LIST);
    bulkDownloadProductLevel3Response.setExceptionMap(Collections.EMPTY_MAP);
    ProfileResponse profileResponse = getProfile();
    when(this.xProductOutboundService
      .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
        Mockito.anyInt(), Mockito.anyString(), Mockito.anyString())).thenReturn(
      new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 2));
    when(mockBusinessPartnerRepository
      .filterByBusinessPartnerCodeV2(anyString(), anyString()))
      .thenReturn(profileResponse);
    when(mockProductLevel3Repository
      .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
        any(ProductLevel3SummaryRequest.class))).thenReturn(bulkDownloadProductLevel3Response);
    List<List<String>> rowList = new ArrayList<>();
    when(mockBulkDownloadServiceBeanUtil
      .getAllProductValues(anyMap(), anyBoolean(), anyList(), anyBoolean(),
        any(ProfileResponse.class), Mockito.anyList())).thenReturn(rowList);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    verify(mockBusinessPartnerRepository)
      .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository, times(2))
      .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
        any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService, times(2))
      .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
        Mockito.anyInt(), Mockito.anyString(), Mockito.anyString());
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowList.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getDataFromDBSyncTrueFalseProductTest() throws Exception {
    ItemL4SummaryResponse itemL4SummaryResponse = new ItemL4SummaryResponse();
    ItemL4SummaryResponse itemL4SummaryResponse2 = new ItemL4SummaryResponse();
    itemL4SummaryResponse2.setGeneratedItemName(ITEM_NAME);
    itemL4SummaryResponse.setGeneratedItemName(ITEM_NAME);
    productL3SummaryResponse.setSynchronized(false);
    productL3SummaryResponse2.setSynchronized(true);
    productL3SummaryResponse.setItemL4SummaryResponse(itemL4SummaryResponse);
    productL3SummaryResponse2.setProductName(PRODUCT_NAME);
    ProfileResponse profileResponse = getProfile();
    profileResponse.getCompany().setInventoryFulfillment("BP");
    ReflectionTestUtils.setField(bulkProductDataServiceBean, "productDownloadFetchFromDb",
      true);
    when(mockBusinessPartnerRepository
      .filterByBusinessPartnerCodeV2(anyString(), anyString()))
      .thenReturn(profileResponse);
    when(productBusinessPartnerRepository.bulkDownloadSummary(any(ProductLevel3SummaryRequest.class), anyString(),
        anyString()))
      .thenReturn(bulkDownloadProductLevel3Response);
    when(this.xProductOutboundService
      .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
        eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
      new PageImpl<>(Arrays.asList(productL3SummaryResponse,productL3SummaryResponse2),
          PageRequest.of(0, 2), 2));
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList())).thenReturn(rowData);
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    verify(mockBusinessPartnerRepository)
      .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(productBusinessPartnerRepository).bulkDownloadSummary(productLevel3SummaryRequestArgumentCaptor.capture(),
      eq(BUSINESS_PARTNER_CODE_1), anyString());
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
      .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
        eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    Assertions.assertEquals(PRODUCT_SKU, productLevel3SummaryRequestArgumentCaptor.getValue().getProductSkuList().get(0));
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowData.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getData_dataExist_success_pickupPointservice() throws Exception {
    ProfileResponse profileResponse = getProfile();
    profileResponse.getCompany().setInventoryFulfillment("BP");
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class), any(
            ProductLevel3SummaryRequest.class)))
        .thenReturn(bulkDownloadProductLevel3Response);
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 1));
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList()))
        .thenReturn(rowData);
    productDownloadRequest = getProductDownloadRequest(productDownloadRequest,"PP-123456");
    productDownloadRequest.setAccessiblePickupPoints(Collections.singleton("PP-123456"));
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode("m-123");
    pickupPointFilterRequest.getCodes().add("PP-123456");
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    verify(pickupPointService).getPickupPointSummaryFilter(eq(0),eq(pickupPointFilterRequest));
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository)
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(rowData.size(), productResponse.getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
  }

  @Test
  public void getData_maxL5SwitchOn() throws Exception {
    ReflectionTestUtils.setField(bulkProductDataServiceBean, "maxL5DownloadSize",
        0);
    ProfileResponse profileResponse = getProfile();
    profileResponse.getCompany().setInventoryFulfillment("BP");
    when(mockBusinessPartnerRepository
        .filterByBusinessPartnerCodeV2(anyString(), anyString()))
        .thenReturn(profileResponse);
    when(mockProductLevel3Repository
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class), any(
            ProductLevel3SummaryRequest.class)))
        .thenReturn(bulkDownloadProductLevel3Response);
    when(this.xProductOutboundService
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME))).thenReturn(
        new PageImpl<>(Arrays.asList(productL3SummaryResponse), PageRequest.of(0, 1), 1));
    when(mockBulkDownloadServiceBeanUtil.getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList()))
        .thenReturn(rowData);
    productDownloadRequest = getProductDownloadRequest(productDownloadRequest,"PP-123456");
    productDownloadRequest.setAccessiblePickupPoints(Collections.singleton("PP-123456"));
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode("m-123");
    pickupPointFilterRequest.getCodes().add("PP-123456");
    BulkDataResponse response = bulkProductDataServiceBean.getData(productDownloadRequest);
    verify(pickupPointService).getPickupPointSummaryFilter(eq(0),eq(pickupPointFilterRequest));
    Assertions.assertTrue(response instanceof BulkProductResponse);
    BulkProductResponse productResponse = (BulkProductResponse) response;
    verify(mockBusinessPartnerRepository)
        .filterByBusinessPartnerCodeV2(anyString(), anyString());
    verify(mockProductLevel3Repository)
        .findSummaryByFilterForBulkDownload(anyString(), any(Pageable.class),
            any(ProductLevel3SummaryRequest.class));
    verify(mockBulkDownloadServiceBeanUtil).getAllProductValues(anyMap(), anyBoolean(), anyList(),
        anyBoolean(), any(ProfileResponse.class), Mockito.anyList());
    verify(this.xProductOutboundService)
        .getProductL3SummaryResponse(Mockito.any(ProductSummaryRequest.class), Mockito.anyInt(),
            eq(MAX_PRODUCT_SIZE), eq(REQUEST_ID), eq(USERNAME));
    assertNotNull(productResponse.getProductContentList());
    Assertions.assertEquals(1, ((BulkProductResponse) response).getProductContentList().size());
    Assertions.assertEquals(profileResponse.getPickupPoints().size(), productResponse.getPickupPoint().size());
    Assertions.assertTrue(((BulkProductResponse) response).isPartialDownload());
  }
}
