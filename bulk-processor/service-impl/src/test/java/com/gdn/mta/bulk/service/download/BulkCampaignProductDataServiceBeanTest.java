package com.gdn.mta.bulk.service.download;

import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkCampaignProductResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.repository.pcb.ProductCategoryBaseRepository;
import com.gdn.mta.bulk.service.PBPOutboundService;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.mta.bulk.service.XProductOutboundService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
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
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

public class BulkCampaignProductDataServiceBeanTest {

  private static final String FOO = "FOO";
  private static final String BAR = "BAR";
  private static final String REQUEST_ID = "REQUEST-ID";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String ITEM_SKU = "item-sku";
  private static final String PROMO_TYPE = "promo_type";
  private static final String KEYWORD = "keyword";
  private static final int PAGE_SIZE = 50;
  private static final int ONE_PAGE_SIZE = 1;
  private static final int TWO = 2;
  private static final String PRODUCT_SKU = "productSku";

  private SystemParameterConfig systemParameterConfig;
  private CampaignProductDownloadRequest campaignProductDownloadRequest =
    new CampaignProductDownloadRequest();
  private ProductSkuSummaryResponse productSkuSummaryResponse = new ProductSkuSummaryResponse();
  private ProductLevel3SummaryResponse productLevel3SummaryResponse =
    new ProductLevel3SummaryResponse();

  @InjectMocks
  private BulkCampaignProductDataServiceBean bulkCampaignProductDataServiceBean;

  @Mock
  private ProductCategoryBaseRepository productCategoryBaseRepository;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Captor
  private ArgumentCaptor<List<String>> stringListCaptor;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private XProductOutboundService xProductOutboundService;

  @Mock
  private PBPOutboundService pbpOutboundService;

  @Test
  public void getDataTest() throws Exception {
    CampaignProductDownloadRequest request = createCampaignProductDownloadRequest();
    request.setSize(PAGE_SIZE);
    List<String> childCategories = Lists.newArrayList(FOO, BAR, FOO);
    doReturn(childCategories).when(productCategoryBaseRepository)
        .getAllChildCategoriesFromC1CategoryCode(eq(REQUEST_ID), eq(null), anyList());
    doReturn(createProductLevel3SummaryResponse()).when(productLevel3Repository)
        .findSummaryByCategoryAndBrand(0, PAGE_SIZE, (CampaignProductDownloadRequest) request);
    doReturn(createProductLevel3SummaryResponse()).when(productLevel3Repository)
        .findSummaryByCategoryAndBrand(1, PAGE_SIZE, (CampaignProductDownloadRequest) request);
    doReturn(Lists.newArrayList()).when(productLevel3Repository).findSummaryByCategoryAndBrand(2, PAGE_SIZE, request);
    BulkDataResponse actual = bulkCampaignProductDataServiceBean.getData(request);
    verify(productCategoryBaseRepository, times(3))
        .getAllChildCategoriesFromC1CategoryCode(eq(REQUEST_ID), eq(null), stringListCaptor.capture());
    stringListCaptor.getAllValues().stream().flatMap(list -> list.stream()).collect(Collectors.toList())
        .equals(request.getCampaignItemSummaryRequest().getCategories());
    verify(productLevel3Repository, times(3)).findSummaryByCategoryAndBrand(0, 0, request);
    BulkCampaignProductResponse actual1 = (BulkCampaignProductResponse) actual;
    Assertions.assertEquals(0, actual1.getProductLevel3SummaryResponses().size());
  }

  @Test
  public void getDataTest_withSize() throws Exception {
    CampaignProductDownloadRequest request = createCampaignProductDownloadRequest();
    request.setSize(20);
    List<String> childCategories = Lists.newArrayList(FOO, BAR, FOO);
    doReturn(childCategories).when(productCategoryBaseRepository)
        .getAllChildCategoriesFromC1CategoryCode(eq(REQUEST_ID), eq(null), anyList());
    doReturn(createProductLevel3SummaryResponseWithData()).when(productLevel3Repository)
        .findSummaryByCategoryAndBrand(0, 0, (CampaignProductDownloadRequest) request);
    doReturn(createProductLevel3SummaryResponseWithData()).when(productLevel3Repository)
        .findSummaryByCategoryAndBrand(1, 0, (CampaignProductDownloadRequest) request);
    doReturn(Lists.newArrayList()).when(productLevel3Repository).findSummaryByCategoryAndBrand(2, PAGE_SIZE, request);
    BulkDataResponse actual = bulkCampaignProductDataServiceBean.getData(request);
    verify(productCategoryBaseRepository, times(3))
        .getAllChildCategoriesFromC1CategoryCode(eq(REQUEST_ID), eq(null), stringListCaptor.capture());
    stringListCaptor.getAllValues().stream().flatMap(list -> list.stream()).collect(Collectors.toList())
        .equals(request.getCampaignItemSummaryRequest().getCategories());
    verify(productLevel3Repository, times(3)).findSummaryByCategoryAndBrand(0, 0, request);
    verify(productLevel3Repository, times(3)).findSummaryByCategoryAndBrand(1, 0, request);
    verify(productLevel3Repository, times(3)).findSummaryByCategoryAndBrand(2, 0, request);
    BulkCampaignProductResponse actual1 = (BulkCampaignProductResponse) actual;
  }

  @Test
  public void getDataTest_WhenEmptyCategoryScope() throws Exception {
    CampaignProductDownloadRequest request = createCampaignProductDownloadRequest();
    request.getCampaignItemSummaryRequest().setCategories(Collections.emptyList());
    doReturn(createProductLevel3SummaryResponse()).when(productLevel3Repository)
        .findSummaryByCategoryAndBrand(0, PAGE_SIZE, request);
    doReturn(createProductLevel3SummaryResponse()).when(productLevel3Repository)
        .findSummaryByCategoryAndBrand(1, PAGE_SIZE, request);
    doReturn(Lists.newArrayList()).when(productLevel3Repository).findSummaryByCategoryAndBrand(2, PAGE_SIZE, request);
    BulkDataResponse actual = bulkCampaignProductDataServiceBean.getData(request);
    verify(productLevel3Repository).findSummaryByCategoryAndBrand(0, 0, request);
    Assertions.assertEquals(Collections.emptyList(), request.getCampaignItemSummaryRequest().getCategories());
    BulkCampaignProductResponse actual1 = (BulkCampaignProductResponse) actual;
    Assertions.assertEquals(0, actual1.getProductLevel3SummaryResponses().size());
  }

  @Test
  public void getDataTest_WhenNoChildCategory() throws Exception {
    CampaignProductDownloadRequest request = createCampaignProductDownloadRequest();
    List<String> categoryCodes = Collections.singletonList(FOO);
    request.getCampaignItemSummaryRequest().setCategories(categoryCodes);
    doReturn(null).when(productCategoryBaseRepository)
        .getAllChildCategoriesFromC1CategoryCode(eq(REQUEST_ID), eq(null), anyList());
    doReturn(createProductLevel3SummaryResponse()).when(productLevel3Repository)
        .findSummaryByCategoryAndBrand(0, PAGE_SIZE, (CampaignProductDownloadRequest) request);
    doReturn(createProductLevel3SummaryResponse()).when(productLevel3Repository)
        .findSummaryByCategoryAndBrand(1, PAGE_SIZE, (CampaignProductDownloadRequest) request);
    doReturn(Lists.newArrayList()).when(productLevel3Repository).findSummaryByCategoryAndBrand(2, PAGE_SIZE, request);
    BulkDataResponse actual = bulkCampaignProductDataServiceBean.getData(request);
    verify(productCategoryBaseRepository)
        .getAllChildCategoriesFromC1CategoryCode(eq(REQUEST_ID), eq(null), stringListCaptor.capture());
    Assertions.assertEquals(stringListCaptor.getValue(), categoryCodes);
    verify(productLevel3Repository).findSummaryByCategoryAndBrand(0, 0, request);
    Assertions.assertEquals(request.getCampaignItemSummaryRequest().getCategories(), categoryCodes);
    BulkCampaignProductResponse actual1 = (BulkCampaignProductResponse) actual;
    Assertions.assertEquals(0, actual1.getProductLevel3SummaryResponses().size());
  }

  private CampaignProductDownloadRequest createCampaignProductDownloadRequest() {
    CampaignItemSummaryRequest campaignItemSummaryRequest = new CampaignItemSummaryRequest();
    campaignItemSummaryRequest.setBrands(Lists.newArrayList(FOO, FOO, FOO));
    campaignItemSummaryRequest.setCategories(Lists.newArrayList(BAR, FOO, BAR));
    campaignItemSummaryRequest.setMerchantCode(MERCHANT_CODE);
    campaignItemSummaryRequest.setItemSku(ITEM_SKU);
    campaignItemSummaryRequest.setKeyword(KEYWORD);
    CampaignProductDownloadRequest campaignProductDownloadRequest = new CampaignProductDownloadRequest();
    campaignProductDownloadRequest.setCampaignItemSummaryRequest(campaignItemSummaryRequest);
    campaignProductDownloadRequest.setPrivilegedMap(Maps.newHashMap());
    campaignProductDownloadRequest.setPromoType(PROMO_TYPE);
    campaignProductDownloadRequest.setRequestId(REQUEST_ID);
    return campaignProductDownloadRequest;
  }

  private List<ProductLevel3SummaryResponse> createProductLevel3SummaryResponse() {
    return Lists.newArrayList(new ProductLevel3SummaryResponse(), new ProductLevel3SummaryResponse(),
        new ProductLevel3SummaryResponse(), new ProductLevel3SummaryResponse());
  }

  private List<ProductLevel3SummaryResponse> createProductLevel3SummaryResponseWithData() {
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    productLevel3SummaryResponse.setItemSku(ITEM_SKU);
    return Arrays
        .asList(productLevel3SummaryResponse, new ProductLevel3SummaryResponse(), new ProductLevel3SummaryResponse(),
            new ProductLevel3SummaryResponse());
  }

  @Test
  public void getData_mppSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(bulkCampaignProductDataServiceBean, "bulkGetAllChildCategorySize",
      PAGE_SIZE);
    ReflectionTestUtils.setField(bulkCampaignProductDataServiceBean, "multiPickupPointEnabled",
      true);
    Mockito.when(
      xProductOutboundService.getProductSkuSummaryResponse(eq(MERCHANT_CODE), eq(0), eq(PAGE_SIZE),
        Mockito.any(ProductSkuSummaryRequest.class))).thenReturn(
      new PageImpl<>(Collections.singletonList(productSkuSummaryResponse), PageRequest.of(0, 1), PAGE_SIZE));
    Mockito.when(
      pbpOutboundService.fetchProductLevel3SummaryByProductSkuList(0, PAGE_SIZE, MERCHANT_CODE,
        true, Collections.singletonList(PRODUCT_SKU))).thenReturn(
      new PageImpl<>(Collections.singletonList(productLevel3SummaryResponse), PageRequest.of(0, 1), PAGE_SIZE));
    bulkCampaignProductDataServiceBean.getData(campaignProductDownloadRequest);
    Mockito.verify(xProductOutboundService)
      .getProductSkuSummaryResponse(eq(MERCHANT_CODE), eq(0), eq(PAGE_SIZE),
        Mockito.any(ProductSkuSummaryRequest.class));
    Mockito.verify(pbpOutboundService)
      .fetchProductLevel3SummaryByProductSkuList(0, PAGE_SIZE, MERCHANT_CODE, true,
        Collections.singletonList(PRODUCT_SKU));
  }

  @Test
  public void getData_mppSwitchOn_2PageL3Test() throws Exception {
    ReflectionTestUtils.setField(bulkCampaignProductDataServiceBean, "bulkGetAllChildCategorySize",
      ONE_PAGE_SIZE);
    ReflectionTestUtils.setField(bulkCampaignProductDataServiceBean, "multiPickupPointEnabled",
      true);
    Mockito.when(xProductOutboundService.getProductSkuSummaryResponse(eq(MERCHANT_CODE), eq(0),
        eq(ONE_PAGE_SIZE), Mockito.any(ProductSkuSummaryRequest.class)))
      .thenReturn(new PageImpl<>(Collections.singletonList(productSkuSummaryResponse), PageRequest.of(0, 1), TWO));
    Mockito.when(xProductOutboundService.getProductSkuSummaryResponse(eq(MERCHANT_CODE), eq(1),
        eq(ONE_PAGE_SIZE), Mockito.any(ProductSkuSummaryRequest.class)))
      .thenReturn(new PageImpl<>(Collections.singletonList(productSkuSummaryResponse), PageRequest.of(0, 1), TWO));
    Mockito.when(
      pbpOutboundService.fetchProductLevel3SummaryByProductSkuList(0, ONE_PAGE_SIZE, MERCHANT_CODE,
        true, Arrays.asList(PRODUCT_SKU, PRODUCT_SKU))).thenReturn(
      new PageImpl<>(Collections.singletonList(productLevel3SummaryResponse), PageRequest.of(0, 1), ONE_PAGE_SIZE));
    bulkCampaignProductDataServiceBean.getData(campaignProductDownloadRequest);
    Mockito.verify(xProductOutboundService)
      .getProductSkuSummaryResponse(eq(MERCHANT_CODE), eq(0), eq(ONE_PAGE_SIZE),
        Mockito.any(ProductSkuSummaryRequest.class));
    Mockito.verify(xProductOutboundService)
      .getProductSkuSummaryResponse(eq(MERCHANT_CODE), eq(1), eq(ONE_PAGE_SIZE),
        Mockito.any(ProductSkuSummaryRequest.class));
    Mockito.verify(pbpOutboundService)
      .fetchProductLevel3SummaryByProductSkuList(0, ONE_PAGE_SIZE, MERCHANT_CODE, true,
        Arrays.asList(PRODUCT_SKU, PRODUCT_SKU));
  }

  @Test
  public void getData_mppSwitchOn_2PageL4Test() throws Exception {
    ReflectionTestUtils.setField(bulkCampaignProductDataServiceBean, "bulkGetAllChildCategorySize",
      ONE_PAGE_SIZE);
    ReflectionTestUtils.setField(bulkCampaignProductDataServiceBean, "multiPickupPointEnabled",
      true);
    Mockito.when(xProductOutboundService.getProductSkuSummaryResponse(eq(MERCHANT_CODE), eq(0),
      eq(ONE_PAGE_SIZE), Mockito.any(ProductSkuSummaryRequest.class))).thenReturn(
      new PageImpl<>(Collections.singletonList(productSkuSummaryResponse), PageRequest.of(0, 1), ONE_PAGE_SIZE));
    Mockito.when(
      pbpOutboundService.fetchProductLevel3SummaryByProductSkuList(0, ONE_PAGE_SIZE, MERCHANT_CODE,
        true, Collections.singletonList(PRODUCT_SKU))).thenReturn(
      new PageImpl<>(Collections.singletonList(productLevel3SummaryResponse), PageRequest.of(0, 1), TWO));
    Mockito.when(
      pbpOutboundService.fetchProductLevel3SummaryByProductSkuList(ONE_PAGE_SIZE, ONE_PAGE_SIZE,
        MERCHANT_CODE, true, Collections.singletonList(PRODUCT_SKU))).thenReturn(
      new PageImpl<>(Collections.singletonList(productLevel3SummaryResponse), PageRequest.of(0, 1), TWO));
    bulkCampaignProductDataServiceBean.getData(campaignProductDownloadRequest);
    Mockito.verify(xProductOutboundService)
      .getProductSkuSummaryResponse(eq(MERCHANT_CODE), eq(0), eq(ONE_PAGE_SIZE),
        Mockito.any(ProductSkuSummaryRequest.class));
    Mockito.verify(pbpOutboundService)
      .fetchProductLevel3SummaryByProductSkuList(0, ONE_PAGE_SIZE, MERCHANT_CODE, true,
        Collections.singletonList(PRODUCT_SKU));
    Mockito.verify(pbpOutboundService)
      .fetchProductLevel3SummaryByProductSkuList(ONE_PAGE_SIZE, ONE_PAGE_SIZE, MERCHANT_CODE, true,
        Collections.singletonList(PRODUCT_SKU));
  }

  @BeforeEach
  public void setUp() {
    initMocks(this);
    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("100");
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constant.CATEGORY_CODE_PARTITION_SIZE))
        .thenReturn(systemParameterConfig);
    ReflectionTestUtils.setField(bulkCampaignProductDataServiceBean, "multiPickupPointEnabled",
      false);
    campaignProductDownloadRequest.setCampaignItemSummaryRequest(new CampaignItemSummaryRequest());
    campaignProductDownloadRequest.getCampaignItemSummaryRequest().setMerchantCode(MERCHANT_CODE);
    productSkuSummaryResponse.setProductSku(PRODUCT_SKU);
    Mockito.when(systemParameterConfigService
      .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterConfigNames.PRODUCT_PARTITION_SIZE)).thenReturn(systemParameterConfig);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productCategoryBaseRepository, productLevel3Repository);
    verifyNoMoreInteractions(pbpOutboundService);
    verifyNoMoreInteractions(xProductOutboundService);
  }
}
