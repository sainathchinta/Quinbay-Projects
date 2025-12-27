package com.gdn.partners.pbp.service.productlevel3;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import com.gdn.mta.product.service.ProductAppealService;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.ItemSkuPickupPointSyncStockDto;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.AvailableToCopyProductDetailsResponse;
import com.gda.mta.product.dto.response.ProductBusinessPartnerAndItemViewConfigDto;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ResignSellerDomainEvent;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemSyncStatus;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductLevel3ImageBundle;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.enums.ProductSyncStatus;
import com.gdn.mta.product.enums.SuspensionStatus;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.repository.ProductStockAlertRepository;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductItemBusinessPartnerService;
import com.gdn.mta.product.service.ProductItemSyncService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3ItemSearch;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.sysparam.SystemParameterService;
import com.gdn.x.businesspartner.commons.enums.InventoryFulfillment;
import com.gdn.x.businesspartner.commons.enums.PurchaseTerms;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryCountWebItemSkuOOSResponseDTO;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemImagesListResponse;
import com.gdn.x.product.rest.web.model.response.ItemImagesResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.SimpleMapStringResponse;
import com.gdn.x.productcategorybase.domain.event.model.VatUpdateHistoryDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.gdn.partners.pbp.outbound.productPricing.ProductPricingOutbound;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.mta.product.service.util.MapperUtil;

public class ProductLevel3ServiceBeanTest {

  private static final String STORE_ID = "STORE_ID";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String LINKED_PARTNER_CODE = "linkedBusinessPartnerCode";
  private static final String GDNSKU = "TEC-12354-1234455";
  private static final String GDNSKU_2 = "TEC-12354-1234455-2";
  private static final String PATH = "path";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRE_ORDER_TYPE = "DATE";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String PRODUCT_SKU = "productSku";
  private static final String REQUEST_ID = "requestId";
  private static final String PRODUCT_ITEM_ID = "productItemId";
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_NAME = "itemName";
  private static final String PRODUCT_NAME = "productName";
  private static final String UPDATED_BY = "updatedBy";

  private static final String CATEGORY_CODE = "category-code-1";

  private static final String SEARCH_KEY = "TOQ-19301-00002";

  private static final long COUNT = 1;
  private static boolean SYNC_STOCK = true;

  private static final String UPDATE_ARCHIVED_ERROR =
      "Can not process invalid input data :Tidak berhasil update  product yang telah diarsipkan";

  private static final String PICKUP_POINT_CODE = "JKT10";
  private static final String PICKUP_POINT_CODE_2 = "JKT10_2";
  private static final String WHOLESALE_RULES = "{\"rules\":[{\"quantity\":10,\"price\":100}]}";
  private static final String ITEM_PICKUP_POINT_ID = "TEC-12354-1234455-JKT10";
  private static final int WHOLESALE_PRICE_FETCH_BATCH_SIZE = 100;
  private static final String SKU_STATUS_ACTIVE = "ACTIVE";
  private static final String SKU_STATUS_INACTIVE = "INACTIVE";

  @Mock
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Mock
  ProductLevel3Repository productLevel3Repository;

  @Mock
  private ProductLevel3Converter productLevel3Converter;

  @Mock
  SystemParameterService systemParameterService;

  @Mock
  private ProductAppealService productAppealService;

  @Mock
  ProductLevel3AggregatorServiceOld productLevel3DirectAggregatorService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductStockAlertRepository productStockAlertRepository;

  @Mock
  private ProductItemSyncService productItemSyncService;

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private ProductOutbound productOutbound;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Mock
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Mock
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @InjectMocks
  private ProductLevel3ServiceBean productLevel3ServiceBean;

  @Mock
  private ProductBusinessPartnerService productBusinessPartnerService;


  @Captor
  private ArgumentCaptor<ProductBusinessPartner> productBusinessPartnerArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ProductItemWholesalePrice>> productItemWholesalePricesArgumentCaptor;

  private WebInventoryCountWebItemSkuOOSResponseDTO webInventoryCountWebItemSkuOOSResponseDTO;

  private static final ProductLevel3SummaryFilter SUMMARY_FILTER = new ProductLevel3SummaryFilter();
  private static final ItemSummaryRequest ITEM_SUMMARY_FILTER = new ItemSummaryRequest();
  private static final Pageable PAGEABLE = PageRequest.of(0, 10);
  private static final SortOrder SORT = new SortOrder("", "");

  private ProfileResponse profileResponse;
  private ItemSummaryRequest itemSummaryRequest;
  private ItemSummaryResponse itemSummaryResponse;
  private List<ItemSummaryResponse> itemSummaryResponseList;
  private Page<ItemSummaryResponse> itemSummaryResponsePage;
  private ProductLevel3Item productLevel3Item;
  private ProductLevel3ItemSearch productLevel3ItemSearch;
  private ProductSystemParameter productSystemParameter;
  private ItemImagesListResponse itemImagesListResponse;
  private VatUpdateHistoryDomainEventModel vatUpdateHistoryDomainEventModel;
  private ProductItemBusinessPartner productItemBusinessPartner;
  private ProductItemBusinessPartner productItemBusinessPartner1;
  private ItemSummaryDetailResponse itemSummaryDetailResponse;
  private ProductL3Response productL3Response;
  private PreOrderDTO preOrderDTO;
  private ProductBusinessPartnerCounter productBusinessPartnerCounter;

  @Mock
  private ProductPricingOutbound productPricingOutbound;

  @Mock
  private MapperUtil mapperUtil;

  @BeforeEach
  public void __initialize() throws Exception {
    initMocks(this);

    this.webInventoryCountWebItemSkuOOSResponseDTO =
        new WebInventoryCountWebItemSkuOOSResponseDTO(ProductLevel3ServiceBeanTest.COUNT);
    this.profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setInventoryFulfillment(InventoryFulfillment.BLIBLI.getName());
    company.setPurchaseTerm(PurchaseTerms.TRADING.getName());
    this.profileResponse.setCompany(company);
    this.itemSummaryRequest = new ItemSummaryRequest();
    this.itemSummaryResponse = new ItemSummaryResponse();
    this.itemSummaryResponseList = new ArrayList<>();
    this.itemSummaryResponseList.add(this.itemSummaryResponse);
    this.itemSummaryResponsePage = new PageImpl<>(this.itemSummaryResponseList);
    this.productLevel3Item = new ProductLevel3Item();
    this.productLevel3Item.setMerchantPromoDiscount(true);
    this.productLevel3Item.setMerchantPromoDiscountActivated(true);
    this.productLevel3ItemSearch = new ProductLevel3ItemSearch();
    this.productLevel3ItemSearch.setBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE);
    when(this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE))
        .thenReturn(this.profileResponse);
    when(this.productLevel3Converter.convertProductLevel3ItemSearchToItemSummaryRequest(this.productLevel3ItemSearch))
        .thenReturn(this.itemSummaryRequest);
    when(this.productLevel3Converter.convertItemSummaryResponseToProductLevel3Item(this.itemSummaryResponse))
        .thenReturn(this.productLevel3Item);
    when(this.productLevel3Repository
        .findSummaryByFilter(this.itemSummaryRequest, ProductLevel3ServiceBeanTest.PAGEABLE,
            ProductLevel3ServiceBeanTest.SORT)).thenReturn(this.itemSummaryResponsePage);

    productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setValue(String.valueOf(false));

    itemImagesListResponse = new ItemImagesListResponse();
    itemImagesListResponse.setItemSku(GDNSKU);
    ItemImagesResponse itemImagesResponse = new ItemImagesResponse();
    itemImagesResponse.setLocationPath(PATH);
    itemImagesResponse.setMainImage(true);
    itemImagesResponse.setSequence(1);
    itemImagesListResponse.setImagesResponseList(Collections.singletonList(itemImagesResponse));

    vatUpdateHistoryDomainEventModel =
        new VatUpdateHistoryDomainEventModel(REQUEST_ID, STORE_ID, PRODUCT_ITEM_ID, ITEM_CODE, ITEM_NAME, UPDATED_BY,
            Boolean.FALSE.toString(), Boolean.TRUE.toString());

    productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setMarkForDelete(false);
    productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setMarkForDelete(true);
    itemSummaryDetailResponse = new ItemSummaryDetailResponse();

    productL3Response = new ProductL3Response();
    productL3Response.setForceReview(true);
    preOrderDTO = new PreOrderDTO();

    productBusinessPartnerCounter = new ProductBusinessPartnerCounter();
    productBusinessPartnerCounter.setAppealedProductCount(10);
    productBusinessPartnerCounter.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @AfterEach
  public void _finalize() {
    verifyNoMoreInteractions(this.productLevel3InventoryService);
    Mockito.verifyNoMoreInteractions(this.productStockAlertRepository);
    Mockito.verifyNoMoreInteractions(this.productItemSyncService);
    Mockito.verifyNoMoreInteractions(this.applicationProperties);
    Mockito.verifyNoMoreInteractions(this.productOutbound);
    Mockito.verifyNoMoreInteractions(this.productCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.kafkaProducer);
    Mockito.verifyNoMoreInteractions(this.productPricingOutbound);
    Mockito.verifyNoMoreInteractions(this.mapperUtil);
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCode() throws Exception {
    this.productLevel3ServiceBean
        .updateSyncStockByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE,
            ProductLevel3ServiceBeanTest.SYNC_STOCK);
    verify(this.productLevel3InventoryService)
        .updateSyncStockByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE,
            ProductLevel3ServiceBeanTest.SYNC_STOCK);
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCodeAndGdnSku() throws Exception {
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setArchived(false);
    Mockito.when(productLevel3Repository.findSummaryByGdnSku(GDNSKU)).thenReturn(itemSummaryResponse);
    productLevel3ServiceBean.updateSyncStockByBusinessPartnerCodeAndGdnSku(BUSINESS_PARTNER_CODE, GDNSKU, true);
    Mockito.verify(productLevel3InventoryService)
        .updateSyncStockByBusinessPartnerCodeAndGdnSku(BUSINESS_PARTNER_CODE, GDNSKU, true, Collections
            .singletonList(new ItemSkuPickupPointSyncStockDto(GDNSKU, itemSummaryResponse.getPickupPointCode(), true)));
  }

  @Test
  public void testUpdateSyncStockByBusinessPartnerCodeAndGdnSkuArchivedProduct() throws Exception {
    try {
      ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
      itemSummaryResponse.setArchived(true);
      Mockito.when(productLevel3Repository.findSummaryByGdnSku(GDNSKU)).thenReturn(itemSummaryResponse);
      productLevel3ServiceBean.updateSyncStockByBusinessPartnerCodeAndGdnSku(BUSINESS_PARTNER_CODE, GDNSKU, true);
      Mockito.verify(productLevel3InventoryService)
          .updateSyncStockByBusinessPartnerCodeAndGdnSku(BUSINESS_PARTNER_CODE, GDNSKU, true, new ArrayList<>());
    } catch (Exception e) {
      Assertions.assertEquals(UPDATE_ARCHIVED_ERROR, e.getMessage());
    }
  }


  @Test
  public void findSummaryByFilter_WithDirectAggregator() throws Exception {
    Mockito.when(systemParameterService.getParameter(Mockito.anyString())).thenReturn("false");
    productLevel3ServiceBean.findSummaryByFilter(SUMMARY_FILTER, PAGEABLE, SORT);
    Mockito.verify(productLevel3DirectAggregatorService)
        .aggregateProductLevel3Summary(eq(SUMMARY_FILTER), eq(PAGEABLE), eq(SORT));
  }

  @Test
  public void findSummaryMinifiedByFilter_WithDirectAggregator() throws Exception {
    Mockito.when(systemParameterService.getParameter(Mockito.anyString())).thenReturn("false");
    productLevel3ServiceBean.findSummaryMinifiedByFilter(SUMMARY_FILTER, PAGEABLE, SORT);
    Mockito.verify(productLevel3DirectAggregatorService)
        .aggregateProductLevel3SummaryMinified(eq(SUMMARY_FILTER), eq(PAGEABLE), eq(SORT));
  }

  @Test
  public void findImageBundleByFilterTest() throws Exception {
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setMasterDataItemImages(Collections.singletonList(new MasterDataItemImageDTO()));
    List<ItemSummaryResponse> itemSummaryResponses = Collections.singletonList(itemSummaryResponse);
    when(this.productLevel3Repository.findSummaryByFilter(ITEM_SUMMARY_FILTER, PAGEABLE, SORT))
        .thenReturn(new PageImpl<>(itemSummaryResponses));
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.IMAGE_BUNDLE_SWITCH))
        .thenReturn(productSystemParameter);
    productLevel3ServiceBean.findImageBundleByFilter(ITEM_SUMMARY_FILTER, PAGEABLE, SORT);
    verify(this.productLevel3Repository).findSummaryByFilter(ITEM_SUMMARY_FILTER, PAGEABLE, SORT);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.IMAGE_BUNDLE_SWITCH);
  }

  @Test
  public void findImageBundleByFilterSwitchTest() throws Exception {
    productSystemParameter.setValue(String.valueOf(true));
    ITEM_SUMMARY_FILTER.setItemSkus(Collections.singletonList(GDNSKU));
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.IMAGE_BUNDLE_SWITCH))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound
        .getListOfImagesByItemSkus(new SimpleSetStringRequest(new HashSet<>(Collections.singletonList(GDNSKU)))))
        .thenReturn(Collections.singletonList(itemImagesListResponse));
    Page<ProductLevel3ImageBundle> imageBundleByFilter =
        productLevel3ServiceBean.findImageBundleByFilter(ITEM_SUMMARY_FILTER, PAGEABLE, SORT);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.IMAGE_BUNDLE_SWITCH);
    verify(xProductOutbound)
        .getListOfImagesByItemSkus(new SimpleSetStringRequest(new HashSet<>(Collections.singletonList(GDNSKU))));
    Assertions.assertEquals(GDNSKU, imageBundleByFilter.getContent().get(0).getGdnSku());
    Assertions.assertEquals(PATH, imageBundleByFilter.getContent().get(0).getImages().get(0).getLocationPath());
    Assertions.assertEquals(1, imageBundleByFilter.getContent().get(0).getImages().get(0).getSequence(), 0);
    Assertions.assertTrue(imageBundleByFilter.getContent().get(0).getImages().get(0).getMainImage());
  }

  @Test
  public void updateResignBusinessPartnerItemsTest() throws Exception {
    productSystemParameter.setValue(String.valueOf(Boolean.FALSE));
    Mockito.when(this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.RESIGN_SELLER_SWITCH)).thenReturn(productSystemParameter);
    this.productLevel3ServiceBean.updateResignBusinessPartnerItems(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(this.productLevel3Repository).updateResignBusinessPartnerItems(BUSINESS_PARTNER_CODE);
    Mockito.verify(this.productSystemParameterService)
      .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.RESIGN_SELLER_SWITCH);
  }

  @Test
  public void updateResignBusinessPartnerItems_switchOnTest() throws Exception {
    productSystemParameter.setValue(String.valueOf(Boolean.TRUE));
    Mockito.when(this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID,
      SystemParameterConstants.RESIGN_SELLER_SWITCH)).thenReturn(productSystemParameter);
    this.productLevel3ServiceBean.updateResignBusinessPartnerItems(STORE_ID, BUSINESS_PARTNER_CODE);
    verify(this.kafkaProducer).send(DomainEventName.RESIGN_SELLER_EVENT,
      ResignSellerDomainEvent.builder().storeId(STORE_ID).businessPartnerCode(BUSINESS_PARTNER_CODE)
        .build());
  }

  @Test
  public void findItemBySearchTest_includeAllTradingProductFalse() throws Exception {
    Page<ProductLevel3Item> result = this.productLevel3ServiceBean
        .findItemBySearch(this.productLevel3ItemSearch, ProductLevel3ServiceBeanTest.PAGEABLE,
            ProductLevel3ServiceBeanTest.SORT);
    assertNotNull(result);
    assertThat(result.getContent().size(), equalTo(this.itemSummaryResponsePage.getContent().size()));
    assertThat(result.getContent().get(0), equalTo(this.productLevel3Item));
    verify(this.productLevel3Converter)
        .convertProductLevel3ItemSearchToItemSummaryRequest(this.productLevel3ItemSearch);
    verify(this.productLevel3Converter).convertItemSummaryResponseToProductLevel3Item(this.itemSummaryResponse);
    verify(this.productLevel3Repository)
        .findSummaryByFilter(this.itemSummaryRequest, ProductLevel3ServiceBeanTest.PAGEABLE,
            ProductLevel3ServiceBeanTest.SORT);
  }

  @Test
  public void findItemBySearchTest_includeAllTradingProductTrue_isBlibliTradingFalse1() throws Exception {
    this.profileResponse.getCompany().setPurchaseTerm(PurchaseTerms.COMMISSION.getName());
    this.productLevel3ItemSearch.setIncludeAllTradingProduct(true);
    Page<ProductLevel3Item> result = this.productLevel3ServiceBean
        .findItemBySearch(this.productLevel3ItemSearch, ProductLevel3ServiceBeanTest.PAGEABLE,
            ProductLevel3ServiceBeanTest.SORT);
    assertNotNull(result);
    assertThat(result.getContent().size(), equalTo(this.itemSummaryResponsePage.getContent().size()));
    assertThat(result.getContent().get(0), equalTo(this.productLevel3Item));
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE);
    verify(this.productLevel3Converter)
        .convertProductLevel3ItemSearchToItemSummaryRequest(this.productLevel3ItemSearch);
    verify(this.productLevel3Converter).convertItemSummaryResponseToProductLevel3Item(this.itemSummaryResponse);
    verify(this.productLevel3Repository)
        .findSummaryByFilter(this.itemSummaryRequest, ProductLevel3ServiceBeanTest.PAGEABLE,
            ProductLevel3ServiceBeanTest.SORT);
  }

  @Test
  public void findItemBySearchTest_includeAllTradingProductTrue_isBlibliTradingFalse2() throws Exception {
    this.profileResponse.getCompany().setInventoryFulfillment(InventoryFulfillment.BUSINESS_PARTNER.getName());
    this.productLevel3ItemSearch.setIncludeAllTradingProduct(true);
    Page<ProductLevel3Item> result = this.productLevel3ServiceBean
        .findItemBySearch(this.productLevel3ItemSearch, ProductLevel3ServiceBeanTest.PAGEABLE,
            ProductLevel3ServiceBeanTest.SORT);
    assertNotNull(result);
    assertThat(result.getContent().size(), equalTo(this.itemSummaryResponsePage.getContent().size()));
    assertThat(result.getContent().get(0), equalTo(this.productLevel3Item));
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE);
    verify(this.productLevel3Converter)
        .convertProductLevel3ItemSearchToItemSummaryRequest(this.productLevel3ItemSearch);
    verify(this.productLevel3Converter).convertItemSummaryResponseToProductLevel3Item(this.itemSummaryResponse);
    verify(this.productLevel3Repository)
        .findSummaryByFilter(this.itemSummaryRequest, ProductLevel3ServiceBeanTest.PAGEABLE,
            ProductLevel3ServiceBeanTest.SORT);
  }

  @Test
  public void findItemBySearchTest_includeAllTradingProductTrue_isBlibliTradingTrue() throws Exception {
    this.productLevel3ItemSearch.setIncludeAllTradingProduct(true);
    Page<ProductLevel3Item> result = this.productLevel3ServiceBean
        .findItemBySearch(this.productLevel3ItemSearch, ProductLevel3ServiceBeanTest.PAGEABLE,
            ProductLevel3ServiceBeanTest.SORT);
    assertNotNull(result);
    assertThat(result.getContent().size(), equalTo(this.itemSummaryResponsePage.getContent().size()));
    assertThat(result.getContent().get(0), equalTo(this.productLevel3Item));
    verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE);
    verify(this.productLevel3Converter)
        .convertProductLevel3ItemSearchToItemSummaryRequest(this.productLevel3ItemSearch);
    verify(this.productLevel3Converter).convertItemSummaryResponseToProductLevel3Item(this.itemSummaryResponse);
    verify(this.productLevel3Repository)
        .findSummaryByFilter(this.itemSummaryRequest, ProductLevel3ServiceBeanTest.PAGEABLE,
            ProductLevel3ServiceBeanTest.SORT);
  }

  @Test
  public void checkAvailableStockTest() throws Exception {
    Mockito.when(this.productStockAlertRepository
        .findAvailableStockByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE)).thenReturn(100);
    Boolean result =
        this.productLevel3ServiceBean.checkAvailableStock(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(result);
    Mockito.verify(this.productStockAlertRepository, Mockito.times(1))
        .findAvailableStockByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE);
  }

  @Test
  public void checkAvailableStockTest_withZeroAvailableStock() throws Exception {
    Mockito.when(this.productStockAlertRepository
        .findAvailableStockByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE)).thenReturn(0);
    Boolean result =
        this.productLevel3ServiceBean.checkAvailableStock(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(result);
    Mockito.verify(this.productStockAlertRepository, Mockito.times(1))
        .findAvailableStockByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE);
  }

  @Test
  public void checkAvailableStockTest_withNoProducts() throws Exception {
    Mockito.when(this.productStockAlertRepository
        .findAvailableStockByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE)).thenReturn(null);
    Boolean result =
        this.productLevel3ServiceBean.checkAvailableStock(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(result);
    Mockito.verify(this.productStockAlertRepository, Mockito.times(1))
        .findAvailableStockByBusinessPartnerCode(ProductLevel3ServiceBeanTest.BUSINESS_PARTNER_CODE);
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenItemsFoundInProgressStatus() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(StringUtils.EMPTY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-11111");
    activeProductResponse_1.setProductSku("TOQ-19301-00001");
    activeProductResponse_1.setProductName("product-name-1");
    activeProductResponse_1.setItemCount(2);

    ActiveProductResponse activeProductResponse_2 = new ActiveProductResponse();
    activeProductResponse_2.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_2.setProductCode("MTA-22222");
    activeProductResponse_2.setProductSku("TOQ-19301-00002");
    activeProductResponse_2.setProductName("product-name-2");
    activeProductResponse_2.setItemCount(3);

    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<>(Arrays.asList(activeProductResponse_1, activeProductResponse_2), pageable1, 2);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    ProductCollection productCollection_1 = new ProductCollection();
    productCollection_1.setProductCode("MTA-11111");
    productCollection_1.setReviewPending(false);

    ProductCollection productCollection_2 = new ProductCollection();
    productCollection_2.setProductCode("MTA-22222");
    productCollection_2.setReviewPending(false);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111", "MTA-22222")))
        .thenReturn(Arrays.asList(productCollection_1, productCollection_2));

    PageRequest pageRequest_1 = PageRequest.of(0, 5);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00001", "TOQ-19301-00002"));

    ItemSummaryResponse itemSummaryResponse_1 = new ItemSummaryResponse();
    itemSummaryResponse_1.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_1.setItemSku("TOQ-19301-00001-00001");
    itemSummaryResponse_1.setCategoryName("category-name-1");

    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode("category-code-1");
    masterCatalogDTO.setCategory(categoryDTO);
    itemSummaryResponse_1.setMasterCatalog(masterCatalogDTO);

    ItemSummaryResponse itemSummaryResponse_2 = new ItemSummaryResponse();
    itemSummaryResponse_2.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_2.setItemSku("TOQ-19301-00001-00002");
    itemSummaryResponse_2.setCategoryName("category-name-1");

    ItemSummaryResponse itemSummaryResponse_3 = new ItemSummaryResponse();
    itemSummaryResponse_3.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_3.setItemSku("TOQ-19301-00002-00001");
    itemSummaryResponse_3.setCategoryName("category-name-2");

    MasterCatalogDTO masterCatalogDTO_2 = new MasterCatalogDTO();
    CategoryDTO categoryDTO_2 = new CategoryDTO();
    categoryDTO_2.setCategoryCode("category-code-2");
    masterCatalogDTO_2.setCategory(categoryDTO_2);
    itemSummaryResponse_3.setMasterCatalog(masterCatalogDTO_2);

    ItemSummaryResponse itemSummaryResponse_4 = new ItemSummaryResponse();
    itemSummaryResponse_4.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_4.setItemSku("TOQ-19301-00002-00002");
    itemSummaryResponse_4.setCategoryName("category-name-2");

    ItemSummaryResponse itemSummaryResponse_5 = new ItemSummaryResponse();
    itemSummaryResponse_5.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_5.setItemSku("TOQ-19301-00002-00003");
    itemSummaryResponse_5.setCategoryName("category-name-2");

    Pageable pageable2 = PageRequest.of(0, 5);
    Page<ItemSummaryResponse> itemSummaryResponses = new PageImpl<>(Arrays
        .asList(itemSummaryResponse_1, itemSummaryResponse_2, itemSummaryResponse_3, itemSummaryResponse_4,
            itemSummaryResponse_5), pageable2, 5);
    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenReturn(itemSummaryResponses);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setName("category-name-1");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-1")).thenReturn(categoryDetailResponse);

    CategoryDetailResponse categoryDetailResponse_2 = new CategoryDetailResponse();
    categoryDetailResponse_2.setName("category-name-2");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-2")).thenReturn(categoryDetailResponse);

    ProductItemSyncStatus productItemSyncStatus = new ProductItemSyncStatus();
    productItemSyncStatus.setGdnItemSku("TOQ-19301-00001-00001");
    productItemSyncStatus.setProductSyncStatus(ProductSyncStatus.IN_PROGRESS);
    Mockito.when(productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"))).thenReturn(Arrays.asList(productItemSyncStatus));
    when(applicationProperties.getProductDetailPageUrlPrefix()).thenReturn("http://blibli.com/product-detail");

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(2, summaries.getTotalElements());

    Assertions.assertEquals("TOQ-19301-00001", summaries.getContent().get(0).getProductSku());
    Assertions.assertEquals(2, summaries.getContent().get(0).getTotalItemSkuCount());
    Assertions.assertEquals("IN_PROGRESS", summaries.getContent().get(0).getStatus());
    Assertions.assertEquals(3, summaries.getContent().get(1).getTotalItemSkuCount());
    Assertions.assertEquals(null, summaries.getContent().get(1).getStatus());
    Assertions.assertEquals("http://blibli.com/product-detail-TOQ.19301.00001.html",
        summaries.getContent().get(0).getItemDetails().get(0).getProductDetailPageLink());
    Assertions.assertEquals("http://blibli.com/product-detail-TOQ.19301.00002.html",
        summaries.getContent().get(1).getItemDetails().get(0).getProductDetailPageLink());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false);
    verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
    Mockito.verify(productItemSyncService)
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"));
    verify(applicationProperties, times(5)).getProductDetailPageUrlPrefix();
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-1");
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-2");
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111", "MTA-22222"));
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenXProductServiceFailsByFetchingProductSkuDetails() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(StringUtils.EMPTY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    Mockito.when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenThrow(Exception.class);

    try {
      this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable);
    } catch (Exception e) {
      Mockito.verify(productLevel3Repository)
          .getAllProducts(summaryFilterRequest, pageable, Collections.emptyList(), StringUtils.EMPTY, false);
    }
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenXProductServiceFailsByFetchingItemSkuDetails() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(StringUtils.EMPTY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-19301");
    activeProductResponse_1.setProductSku("TOQ-19301-00002");
    activeProductResponse_1.setProductName("product-name-2");
    activeProductResponse_1.setItemCount(3);

    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<>(Arrays.asList(activeProductResponse_1), pageable, 1);

    Mockito.when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    PageRequest pageRequest_1 = PageRequest.of(0, 3);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00002"));

    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenThrow(Exception.class);
    ProductCollection productCollection = new ProductCollection();
    productCollection.setReviewPending(false);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-19301")))
        .thenReturn(Arrays.asList(productCollection));

    try {
      this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable);
    } catch (Exception e) {
      Mockito.verify(productLevel3Repository)
          .getAllProducts(summaryFilterRequest, pageable, Collections.emptyList(), StringUtils.EMPTY, false);
      verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
      verify(productCollectionRepository)
          .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-19301"));
    }
  }

  @Test
  public void productsAvailableToBeCopiedTest_withEmptyResponse_whenRemoveAlreadyCopiedItemSku() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(StringUtils.EMPTY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-11111");
    activeProductResponse_1.setProductSku("TOQ-19301-00001");
    activeProductResponse_1.setProductName("product-name-1");
    activeProductResponse_1.setItemCount(2);

    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<>(Arrays.asList(activeProductResponse_1), pageable1, 1);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    ProductCollection productCollection_1 = new ProductCollection();
    productCollection_1.setProductCode("MTA-11111");
    productCollection_1.setReviewPending(false);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111")))
        .thenReturn(Arrays.asList(productCollection_1));

    PageRequest pageRequest_1 = PageRequest.of(0, 2);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00001"));

    ItemSummaryResponse itemSummaryResponse_1 = new ItemSummaryResponse();
    itemSummaryResponse_1.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_1.setItemSku("TOQ-19301-00001-00001");
    itemSummaryResponse_1.setCategoryName("category-name-1");

    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode("category-code-1");
    masterCatalogDTO.setCategory(categoryDTO);
    itemSummaryResponse_1.setMasterCatalog(masterCatalogDTO);

    ItemSummaryResponse itemSummaryResponse_2 = new ItemSummaryResponse();
    itemSummaryResponse_2.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_2.setItemSku("TOQ-19301-00001-00002");
    itemSummaryResponse_2.setCategoryName("category-name-1");

    Pageable pageable2 = PageRequest.of(0, 2);
    Page<ItemSummaryResponse> itemSummaryResponses =
        new PageImpl<>(Arrays.asList(itemSummaryResponse_1, itemSummaryResponse_2), pageable2, 2);
    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenReturn(itemSummaryResponses);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setName("category-name-1");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-1")).thenReturn(categoryDetailResponse);

    ProductItemSyncStatus productItemSyncStatus_1 = new ProductItemSyncStatus();
    productItemSyncStatus_1.setGdnItemSku("TOQ-19301-00001-00001");
    productItemSyncStatus_1.setProductSyncStatus(ProductSyncStatus.SUCCESS);
    ProductItemSyncStatus productItemSyncStatus_2 = new ProductItemSyncStatus();
    productItemSyncStatus_2.setGdnItemSku("TOQ-19301-00001-00002");
    productItemSyncStatus_2.setProductSyncStatus(ProductSyncStatus.SUCCESS);

    Mockito.when(productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE,
            Arrays.asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002")))
        .thenReturn(Arrays.asList(productItemSyncStatus_1, productItemSyncStatus_2));
    when(applicationProperties.getProductDetailPageUrlPrefix()).thenReturn("http://blibli.com/product-detail");

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(1, summaries.getTotalElements());
    Assertions.assertEquals(Collections.emptyList(), summaries.getContent());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false);
    verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
    Mockito.verify(productItemSyncService)
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE,
            Arrays.asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002"));
    verify(applicationProperties, times(2)).getProductDetailPageUrlPrefix();
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-1");
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111"));
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenFilterApplyOnProductSku() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .searchKey("TOQ-19301-00002").build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword("TOQ-19301-00002")
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-19301");
    activeProductResponse_1.setProductSku("TOQ-19301-00002");
    activeProductResponse_1.setProductName("product-name-2");
    activeProductResponse_1.setItemCount(3);

    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<>(Arrays.asList(activeProductResponse_1), pageable1, 1);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    ProductCollection productCollection = new ProductCollection();
    productCollection.setReviewPending(false);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-19301")))
        .thenReturn(Arrays.asList(productCollection));

    PageRequest pageRequest_1 = PageRequest.of(0, 3);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00002"));

    ItemSummaryResponse itemSummaryResponse_1 = new ItemSummaryResponse();
    itemSummaryResponse_1.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_1.setItemSku("TOQ-19301-00002-00001");
    itemSummaryResponse_1.setCategoryName("category-name-2");

    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode("category-code-2");
    masterCatalogDTO.setCategory(categoryDTO);
    itemSummaryResponse_1.setMasterCatalog(masterCatalogDTO);

    ItemSummaryResponse itemSummaryResponse_2 = new ItemSummaryResponse();
    itemSummaryResponse_2.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_2.setItemSku("TOQ-19301-00002-00002");
    itemSummaryResponse_2.setCategoryName("category-name-2");

    ItemSummaryResponse itemSummaryResponse_3 = new ItemSummaryResponse();
    itemSummaryResponse_3.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_3.setItemSku("TOQ-19301-00002-00003");
    itemSummaryResponse_3.setCategoryName("category-name-2");

    Pageable pageable3 = PageRequest.of(0, 3);
    Page<ItemSummaryResponse> itemSummaryResponses_2 =
        new PageImpl<>(Arrays.asList(itemSummaryResponse_1, itemSummaryResponse_2, itemSummaryResponse_3), pageable3,
            3);
    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenReturn(itemSummaryResponses_2);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setName("category-name-2");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-2")).thenReturn(categoryDetailResponse);

    Mockito.when(productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE,
            Arrays.asList("TOQ-19301-00002-00001", "TOQ-19301-00002-00002", "TOQ-19301-00002-00003")))
        .thenReturn(Collections.emptyList());
    when(applicationProperties.getProductDetailPageUrlPrefix()).thenReturn("http://blibli.com/product-detail");

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(1, summaries.getTotalElements());

    Assertions.assertEquals("TOQ-19301-00002", summaries.getContent().get(0).getProductSku());
    Assertions.assertEquals(3, summaries.getContent().get(0).getTotalItemSkuCount());
    Assertions.assertEquals("product-name-2", summaries.getContent().get(0).getProductName());
    Assertions.assertEquals("category-name-2", summaries.getContent().get(0).getCategoryName());
    Assertions.assertEquals(null, summaries.getContent().get(0).getStatus());
    Assertions.assertEquals("TOQ-19301-00002-00001", summaries.getContent().get(0).getItemDetails().get(0).getItemSku());
    Assertions.assertEquals("TOQ-19301-00002-00002", summaries.getContent().get(0).getItemDetails().get(1).getItemSku());
    Assertions.assertEquals("TOQ-19301-00002-00003", summaries.getContent().get(0).getItemDetails().get(2).getItemSku());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false);
    verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
    Mockito.verify(productItemSyncService)
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE,
            Arrays.asList("TOQ-19301-00002-00001", "TOQ-19301-00002-00002", "TOQ-19301-00002-00003"));
    verify(applicationProperties, times(3)).getProductDetailPageUrlPrefix();
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-2");
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-19301"));
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenRemoveAlreadyCopiedItemSku() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(StringUtils.EMPTY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-11111");
    activeProductResponse_1.setProductSku("TOQ-19301-00001");
    activeProductResponse_1.setProductName("product-name-1");
    activeProductResponse_1.setItemCount(2);

    ActiveProductResponse activeProductResponse_2 = new ActiveProductResponse();
    activeProductResponse_2.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_2.setProductCode("MTA-22222");
    activeProductResponse_2.setProductSku("TOQ-19301-00002");
    activeProductResponse_2.setProductName("product-name-2");
    activeProductResponse_2.setItemCount(3);

    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<>(Arrays.asList(activeProductResponse_1, activeProductResponse_2), pageable1, 2);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    ProductCollection productCollection_1 = new ProductCollection();
    productCollection_1.setProductCode("MTA-11111");
    productCollection_1.setReviewPending(false);

    ProductCollection productCollection_2 = new ProductCollection();
    productCollection_2.setProductCode("MTA-22222");
    productCollection_2.setReviewPending(false);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111", "MTA-22222")))
        .thenReturn(Arrays.asList(productCollection_1, productCollection_2));

    PageRequest pageRequest_1 = PageRequest.of(0, 5);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00001", "TOQ-19301-00002"));

    ItemSummaryResponse itemSummaryResponse_1 = new ItemSummaryResponse();
    itemSummaryResponse_1.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_1.setItemSku("TOQ-19301-00001-00001");
    itemSummaryResponse_1.setCategoryName("category-name-1");

    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode("category-code-1");
    masterCatalogDTO.setCategory(categoryDTO);
    itemSummaryResponse_1.setMasterCatalog(masterCatalogDTO);

    ItemSummaryResponse itemSummaryResponse_2 = new ItemSummaryResponse();
    itemSummaryResponse_2.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_2.setItemSku("TOQ-19301-00001-00002");
    itemSummaryResponse_2.setCategoryName("category-name-1");

    ItemSummaryResponse itemSummaryResponse_3 = new ItemSummaryResponse();
    itemSummaryResponse_3.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_3.setItemSku("TOQ-19301-00002-00001");
    itemSummaryResponse_3.setCategoryName("category-name-2");

    MasterCatalogDTO masterCatalogDTO_2 = new MasterCatalogDTO();
    CategoryDTO categoryDTO_2 = new CategoryDTO();
    categoryDTO_2.setCategoryCode("category-code-2");
    masterCatalogDTO_2.setCategory(categoryDTO_2);
    itemSummaryResponse_3.setMasterCatalog(masterCatalogDTO_2);

    ItemSummaryResponse itemSummaryResponse_4 = new ItemSummaryResponse();
    itemSummaryResponse_4.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_4.setItemSku("TOQ-19301-00002-00002");
    itemSummaryResponse_4.setCategoryName("category-name-2");

    ItemSummaryResponse itemSummaryResponse_5 = new ItemSummaryResponse();
    itemSummaryResponse_5.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_5.setItemSku("TOQ-19301-00002-00003");
    itemSummaryResponse_5.setCategoryName("category-name-2");

    Pageable pageable2 = PageRequest.of(0, 5);
    Page<ItemSummaryResponse> itemSummaryResponses = new PageImpl<>(Arrays
        .asList(itemSummaryResponse_1, itemSummaryResponse_2, itemSummaryResponse_3, itemSummaryResponse_4,
            itemSummaryResponse_5), pageable2, 5);
    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenReturn(itemSummaryResponses);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setName("category-name-1");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-1")).thenReturn(categoryDetailResponse);

    CategoryDetailResponse categoryDetailResponse_2 = new CategoryDetailResponse();
    categoryDetailResponse_2.setName("category-name-2");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-2")).thenReturn(categoryDetailResponse);

    ProductItemSyncStatus productItemSyncStatus_1 = new ProductItemSyncStatus();
    productItemSyncStatus_1.setGdnItemSku("TOQ-19301-00001-00001");
    productItemSyncStatus_1.setProductSyncStatus(ProductSyncStatus.FAIL);
    ProductItemSyncStatus productItemSyncStatus_2 = new ProductItemSyncStatus();
    productItemSyncStatus_2.setGdnItemSku("TOQ-19301-00001-00002");
    productItemSyncStatus_2.setProductSyncStatus(ProductSyncStatus.SUCCESS);
    Mockito.when(productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"))).thenReturn(Arrays.asList(productItemSyncStatus_1, productItemSyncStatus_2));
    when(applicationProperties.getProductDetailPageUrlPrefix()).thenReturn("http://blibli.com/product-detail");

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(2, summaries.getTotalElements());

    Assertions.assertEquals("TOQ-19301-00001", summaries.getContent().get(0).getProductSku());
    Assertions.assertEquals(1, summaries.getContent().get(0).getTotalItemSkuCount());
    Assertions.assertEquals(1, summaries.getContent().get(0).getItemDetails().size());
    Assertions.assertEquals("TOQ-19301-00001-00001", summaries.getContent().get(0).getItemDetails().get(0).getItemSku());
    Assertions.assertEquals("FAIL", summaries.getContent().get(0).getStatus());
    Assertions.assertEquals(3, summaries.getContent().get(1).getTotalItemSkuCount());
    Assertions.assertEquals(null, summaries.getContent().get(1).getStatus());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false);
    verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
    Mockito.verify(productItemSyncService)
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"));
    verify(applicationProperties, times(5)).getProductDetailPageUrlPrefix();
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-1");
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-2");
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111", "MTA-22222"));
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenItemsFoundInInprogressAndFailedStatus() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(StringUtils.EMPTY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-11111");
    activeProductResponse_1.setProductSku("TOQ-19301-00001");
    activeProductResponse_1.setProductName("product-name-1");
    activeProductResponse_1.setItemCount(2);

    ActiveProductResponse activeProductResponse_2 = new ActiveProductResponse();
    activeProductResponse_2.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_2.setProductCode("MTA-22222");
    activeProductResponse_2.setProductSku("TOQ-19301-00002");
    activeProductResponse_2.setProductName("product-name-2");
    activeProductResponse_2.setItemCount(3);

    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<>(Arrays.asList(activeProductResponse_1, activeProductResponse_2), pageable1, 2);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    ProductCollection productCollection_1 = new ProductCollection();
    productCollection_1.setProductCode("MTA-11111");
    productCollection_1.setReviewPending(false);

    ProductCollection productCollection_2 = new ProductCollection();
    productCollection_2.setProductCode("MTA-22222");
    productCollection_2.setReviewPending(false);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111", "MTA-22222")))
        .thenReturn(Arrays.asList(productCollection_1, productCollection_2));

    PageRequest pageRequest_1 = PageRequest.of(0, 5);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00001", "TOQ-19301-00002"));

    ItemSummaryResponse itemSummaryResponse_1 = new ItemSummaryResponse();
    itemSummaryResponse_1.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_1.setItemSku("TOQ-19301-00001-00001");
    itemSummaryResponse_1.setCategoryName("category-name-1");

    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode("category-code-1");
    masterCatalogDTO.setCategory(categoryDTO);
    itemSummaryResponse_1.setMasterCatalog(masterCatalogDTO);

    ItemSummaryResponse itemSummaryResponse_2 = new ItemSummaryResponse();
    itemSummaryResponse_2.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_2.setItemSku("TOQ-19301-00001-00002");
    itemSummaryResponse_2.setCategoryName("category-name-1");

    ItemSummaryResponse itemSummaryResponse_3 = new ItemSummaryResponse();
    itemSummaryResponse_3.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_3.setItemSku("TOQ-19301-00002-00001");
    itemSummaryResponse_3.setCategoryName("category-name-2");

    MasterCatalogDTO masterCatalogDTO_2 = new MasterCatalogDTO();
    CategoryDTO categoryDTO_2 = new CategoryDTO();
    categoryDTO_2.setCategoryCode("category-code-2");
    masterCatalogDTO_2.setCategory(categoryDTO_2);
    itemSummaryResponse_3.setMasterCatalog(masterCatalogDTO_2);

    ItemSummaryResponse itemSummaryResponse_4 = new ItemSummaryResponse();
    itemSummaryResponse_4.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_4.setItemSku("TOQ-19301-00002-00002");
    itemSummaryResponse_4.setCategoryName("category-name-2");

    ItemSummaryResponse itemSummaryResponse_5 = new ItemSummaryResponse();
    itemSummaryResponse_5.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_5.setItemSku("TOQ-19301-00002-00003");
    itemSummaryResponse_5.setCategoryName("category-name-2");

    Pageable pageable2 = PageRequest.of(0, 5);
    Page<ItemSummaryResponse> itemSummaryResponses = new PageImpl<>(Arrays
        .asList(itemSummaryResponse_1, itemSummaryResponse_2, itemSummaryResponse_3, itemSummaryResponse_4,
            itemSummaryResponse_5), pageable2, 5);
    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenReturn(itemSummaryResponses);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setName("category-name-1");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-1")).thenReturn(categoryDetailResponse);

    CategoryDetailResponse categoryDetailResponse_2 = new CategoryDetailResponse();
    categoryDetailResponse_2.setName("category-name-2");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-2")).thenReturn(categoryDetailResponse);

    ProductItemSyncStatus productItemSyncStatus_1 = new ProductItemSyncStatus();
    productItemSyncStatus_1.setGdnItemSku("TOQ-19301-00001-00001");
    productItemSyncStatus_1.setProductSyncStatus(ProductSyncStatus.FAIL);
    ProductItemSyncStatus productItemSyncStatus_2 = new ProductItemSyncStatus();
    productItemSyncStatus_2.setGdnItemSku("TOQ-19301-00001-00002");
    productItemSyncStatus_2.setProductSyncStatus(ProductSyncStatus.IN_PROGRESS);
    Mockito.when(productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"))).thenReturn(Arrays.asList(productItemSyncStatus_1, productItemSyncStatus_2));
    when(applicationProperties.getProductDetailPageUrlPrefix()).thenReturn("http://blibli.com/product-detail");

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(2, summaries.getTotalElements());

    Assertions.assertEquals("TOQ-19301-00001", summaries.getContent().get(0).getProductSku());
    Assertions.assertEquals(2, summaries.getContent().get(0).getTotalItemSkuCount());
    Assertions.assertEquals("IN_PROGRESS", summaries.getContent().get(0).getStatus());
    Assertions.assertEquals(3, summaries.getContent().get(1).getTotalItemSkuCount());
    Assertions.assertEquals(null, summaries.getContent().get(1).getStatus());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false);
    verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
    Mockito.verify(productItemSyncService)
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"));
    verify(applicationProperties, times(5)).getProductDetailPageUrlPrefix();
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-1");
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-2");
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111", "MTA-22222"));
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenItemsFoundInFailedStatus() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(StringUtils.EMPTY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-11111");
    activeProductResponse_1.setProductSku("TOQ-19301-00001");
    activeProductResponse_1.setProductName("product-name-1");
    activeProductResponse_1.setItemCount(2);

    ActiveProductResponse activeProductResponse_2 = new ActiveProductResponse();
    activeProductResponse_2.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_2.setProductCode("MTA-22222");
    activeProductResponse_2.setProductSku("TOQ-19301-00002");
    activeProductResponse_2.setProductName("product-name-2");
    activeProductResponse_2.setItemCount(3);

    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<>(Arrays.asList(activeProductResponse_1, activeProductResponse_2), pageable1, 2);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    ProductCollection productCollection_1 = new ProductCollection();
    productCollection_1.setProductCode("MTA-11111");
    productCollection_1.setReviewPending(false);

    ProductCollection productCollection_2 = new ProductCollection();
    productCollection_2.setProductCode("MTA-22222");
    productCollection_2.setReviewPending(false);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111", "MTA-22222")))
        .thenReturn(Arrays.asList(productCollection_1, productCollection_2));

    PageRequest pageRequest_1 = PageRequest.of(0, 5);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00001", "TOQ-19301-00002"));

    ItemSummaryResponse itemSummaryResponse_1 = new ItemSummaryResponse();
    itemSummaryResponse_1.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_1.setItemSku("TOQ-19301-00001-00001");
    itemSummaryResponse_1.setCategoryName("category-name-1");

    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode("category-code-1");
    masterCatalogDTO.setCategory(categoryDTO);
    itemSummaryResponse_1.setMasterCatalog(masterCatalogDTO);

    ItemSummaryResponse itemSummaryResponse_2 = new ItemSummaryResponse();
    itemSummaryResponse_2.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_2.setItemSku("TOQ-19301-00001-00002");
    itemSummaryResponse_2.setCategoryName("category-name-1");

    ItemSummaryResponse itemSummaryResponse_3 = new ItemSummaryResponse();
    itemSummaryResponse_3.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_3.setItemSku("TOQ-19301-00002-00001");
    itemSummaryResponse_3.setCategoryName("category-name-2");

    MasterCatalogDTO masterCatalogDTO_2 = new MasterCatalogDTO();
    CategoryDTO categoryDTO_2 = new CategoryDTO();
    categoryDTO_2.setCategoryCode("category-code-2");
    masterCatalogDTO_2.setCategory(categoryDTO_2);
    itemSummaryResponse_3.setMasterCatalog(masterCatalogDTO_2);

    ItemSummaryResponse itemSummaryResponse_4 = new ItemSummaryResponse();
    itemSummaryResponse_4.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_4.setItemSku("TOQ-19301-00002-00002");
    itemSummaryResponse_4.setCategoryName("category-name-2");

    ItemSummaryResponse itemSummaryResponse_5 = new ItemSummaryResponse();
    itemSummaryResponse_5.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_5.setItemSku("TOQ-19301-00002-00003");
    itemSummaryResponse_5.setCategoryName("category-name-2");

    Pageable pageable2 = PageRequest.of(0, 5);
    Page<ItemSummaryResponse> itemSummaryResponses = new PageImpl<>(Arrays
        .asList(itemSummaryResponse_1, itemSummaryResponse_2, itemSummaryResponse_3, itemSummaryResponse_4,
            itemSummaryResponse_5), pageable2, 5);
    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenReturn(itemSummaryResponses);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setName("category-name-1");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-1")).thenReturn(categoryDetailResponse);

    CategoryDetailResponse categoryDetailResponse_2 = new CategoryDetailResponse();
    categoryDetailResponse_2.setName("category-name-2");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-2")).thenReturn(categoryDetailResponse);

    ProductItemSyncStatus productItemSyncStatus = new ProductItemSyncStatus();
    productItemSyncStatus.setGdnItemSku("TOQ-19301-00001-00002");
    productItemSyncStatus.setProductSyncStatus(ProductSyncStatus.FAIL);
    Mockito.when(productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"))).thenReturn(Arrays.asList(productItemSyncStatus));
    when(applicationProperties.getProductDetailPageUrlPrefix()).thenReturn("http://blibli.com/product-detail");

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(2, summaries.getTotalElements());

    Assertions.assertEquals("TOQ-19301-00001", summaries.getContent().get(0).getProductSku());
    Assertions.assertEquals(2, summaries.getContent().get(0).getTotalItemSkuCount());
    Assertions.assertEquals("FAIL", summaries.getContent().get(0).getStatus());
    Assertions.assertEquals(3, summaries.getContent().get(1).getTotalItemSkuCount());
    Assertions.assertEquals(null, summaries.getContent().get(1).getStatus());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false);
    verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
    Mockito.verify(productItemSyncService)
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"));
    verify(applicationProperties, times(5)).getProductDetailPageUrlPrefix();
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-1");
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-2");
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111", "MTA-22222"));
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenFilterApplyOnCategory() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .searchKey("TOQ-19301-00002").categoryCode("category-code-1").build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword("TOQ-19301-00002")
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Arrays.asList("category-code-1"))
        .nameKey(StringUtils.EMPTY).build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-11111");
    activeProductResponse_1.setProductSku("TOQ-19301-00002");
    activeProductResponse_1.setProductName("product-name-2");
    activeProductResponse_1.setItemCount(3);

    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<>(Arrays.asList(activeProductResponse_1), pageable1, 1);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Arrays.asList(CATEGORY_CODE), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    ProductCollection productCollection_1 = new ProductCollection();
    productCollection_1.setProductCode("MTA-11111");
    productCollection_1.setReviewPending(false);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111")))
        .thenReturn(Arrays.asList(productCollection_1));

    PageRequest pageRequest_1 = PageRequest.of(0, 3);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00002"));

    ItemSummaryResponse itemSummaryResponse_1 = new ItemSummaryResponse();
    itemSummaryResponse_1.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_1.setItemSku("TOQ-19301-00002-00001");
    itemSummaryResponse_1.setCategoryName("category-name-1");

    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode("category-code-1");
    masterCatalogDTO.setCategory(categoryDTO);
    itemSummaryResponse_1.setMasterCatalog(masterCatalogDTO);

    ItemSummaryResponse itemSummaryResponse_2 = new ItemSummaryResponse();
    itemSummaryResponse_2.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_2.setItemSku("TOQ-19301-00002-00002");
    itemSummaryResponse_2.setCategoryName("category-name-1");

    ItemSummaryResponse itemSummaryResponse_3 = new ItemSummaryResponse();
    itemSummaryResponse_3.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_3.setItemSku("TOQ-19301-00002-00003");
    itemSummaryResponse_3.setCategoryName("category-name-1");

    Pageable pageable3 = PageRequest.of(0, 3);
    Page<ItemSummaryResponse> itemSummaryResponses_2 =
        new PageImpl<>(Arrays.asList(itemSummaryResponse_1, itemSummaryResponse_2, itemSummaryResponse_3), pageable3,
            3);
    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenReturn(itemSummaryResponses_2);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setName("category-name-1");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-1")).thenReturn(categoryDetailResponse);

    Mockito.when(productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE,
            Arrays.asList("TOQ-19301-00002-00001", "TOQ-19301-00002-00002", "TOQ-19301-00002-00003")))
        .thenReturn(Collections.emptyList());
    when(applicationProperties.getProductDetailPageUrlPrefix()).thenReturn("http://blibli.com/product-detail");

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(1, summaries.getTotalElements());

    Assertions.assertEquals("TOQ-19301-00002", summaries.getContent().get(0).getProductSku());
    Assertions.assertEquals(3, summaries.getContent().get(0).getTotalItemSkuCount());
    Assertions.assertEquals("product-name-2", summaries.getContent().get(0).getProductName());
    Assertions.assertEquals("category-name-1", summaries.getContent().get(0).getCategoryName());
    Assertions.assertEquals(null, summaries.getContent().get(0).getStatus());
    Assertions.assertEquals("TOQ-19301-00002-00001", summaries.getContent().get(0).getItemDetails().get(0).getItemSku());
    Assertions.assertEquals("TOQ-19301-00002-00002", summaries.getContent().get(0).getItemDetails().get(1).getItemSku());
    Assertions.assertEquals("TOQ-19301-00002-00003", summaries.getContent().get(0).getItemDetails().get(2).getItemSku());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Arrays.asList(CATEGORY_CODE), StringUtils.EMPTY, false);
    verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
    Mockito.verify(productItemSyncService)
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE,
            Arrays.asList("TOQ-19301-00002-00001", "TOQ-19301-00002-00002", "TOQ-19301-00002-00003"));
    verify(applicationProperties, times(3)).getProductDetailPageUrlPrefix();
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-1");
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111"));
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenNoItemsFoundInProgressOrFailed() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(StringUtils.EMPTY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-12345");
    activeProductResponse_1.setProductSku("TOQ-19301-00001");
    activeProductResponse_1.setProductName("product-name-1");
    activeProductResponse_1.setItemCount(2);

    ActiveProductResponse activeProductResponse_2 = new ActiveProductResponse();
    activeProductResponse_2.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_2.setProductCode("MTA-67890");
    activeProductResponse_2.setProductSku("TOQ-19301-00002");
    activeProductResponse_2.setProductName("product-name-2");
    activeProductResponse_2.setItemCount(3);

    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<>(Arrays.asList(activeProductResponse_1, activeProductResponse_2), pageable1, 2);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    ProductCollection productCollection_1 = new ProductCollection();
    productCollection_1.setProductCode("MTA-12345");
    productCollection_1.setReviewPending(false);

    ProductCollection productCollection_2 = new ProductCollection();
    productCollection_2.setProductCode("MTA-67890");
    productCollection_2.setReviewPending(false);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-12345", "MTA-67890")))
        .thenReturn(Arrays.asList(productCollection_1, productCollection_2));

    PageRequest pageRequest_1 = PageRequest.of(0, 5);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00001", "TOQ-19301-00002"));

    ItemSummaryResponse itemSummaryResponse_1 = new ItemSummaryResponse();
    itemSummaryResponse_1.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_1.setItemSku("TOQ-19301-00001-00001");
    itemSummaryResponse_1.setCategoryName("category-name-1");

    MasterCatalogDTO masterCatalogDTO_1 = new MasterCatalogDTO();
    CategoryDTO categoryDTO_1 = new CategoryDTO();
    categoryDTO_1.setCategoryCode("category-code-1");
    masterCatalogDTO_1.setCategory(categoryDTO_1);
    itemSummaryResponse_1.setMasterCatalog(masterCatalogDTO_1);

    ItemSummaryResponse itemSummaryResponse_2 = new ItemSummaryResponse();
    itemSummaryResponse_2.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_2.setItemSku("TOQ-19301-00001-00002");
    itemSummaryResponse_2.setCategoryName("category-name-1");

    ItemSummaryResponse itemSummaryResponse_3 = new ItemSummaryResponse();
    itemSummaryResponse_3.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_3.setItemSku("TOQ-19301-00002-00001");
    itemSummaryResponse_3.setCategoryName("category-name-2");

    MasterCatalogDTO masterCatalogDTO_2 = new MasterCatalogDTO();
    CategoryDTO categoryDTO_2 = new CategoryDTO();
    categoryDTO_2.setCategoryCode("category-code-2");
    masterCatalogDTO_2.setCategory(categoryDTO_2);
    itemSummaryResponse_3.setMasterCatalog(masterCatalogDTO_2);

    ItemSummaryResponse itemSummaryResponse_4 = new ItemSummaryResponse();
    itemSummaryResponse_4.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_4.setItemSku("TOQ-19301-00002-00002");
    itemSummaryResponse_4.setCategoryName("category-name-2");

    ItemSummaryResponse itemSummaryResponse_5 = new ItemSummaryResponse();
    itemSummaryResponse_5.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_5.setItemSku("TOQ-19301-00002-00003");
    itemSummaryResponse_5.setCategoryName("category-name-2");

    Pageable pageable2 = PageRequest.of(0, 5);
    Page<ItemSummaryResponse> itemSummaryResponses = new PageImpl<>(Arrays
        .asList(itemSummaryResponse_1, itemSummaryResponse_2, itemSummaryResponse_3, itemSummaryResponse_4,
            itemSummaryResponse_5), pageable2, 5);
    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenReturn(itemSummaryResponses);

    CategoryDetailResponse categoryDetailResponse_1 = new CategoryDetailResponse();
    categoryDetailResponse_1.setName("category-name-1");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-1")).thenReturn(categoryDetailResponse_1);

    CategoryDetailResponse categoryDetailResponse_2 = new CategoryDetailResponse();
    categoryDetailResponse_2.setName("category-name-2");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-2")).thenReturn(categoryDetailResponse_2);

    Mockito.when(productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"))).thenReturn(Collections.emptyList());
    when(applicationProperties.getProductDetailPageUrlPrefix()).thenReturn("http://blibli.com/product-detail");

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(2, summaries.getTotalElements());

    Assertions.assertEquals("TOQ-19301-00001", summaries.getContent().get(0).getProductSku());
    Assertions.assertEquals(2, summaries.getContent().get(0).getTotalItemSkuCount());
    Assertions.assertEquals("product-name-1", summaries.getContent().get(0).getProductName());
    Assertions.assertEquals("category-name-1", summaries.getContent().get(0).getCategoryName());
    Assertions.assertEquals(null, summaries.getContent().get(0).getStatus());
    Assertions.assertEquals("TOQ-19301-00001-00001", summaries.getContent().get(0).getItemDetails().get(0).getItemSku());
    Assertions.assertEquals("TOQ-19301-00001-00002", summaries.getContent().get(0).getItemDetails().get(1).getItemSku());
    Assertions.assertEquals("TOQ-19301-00002", summaries.getContent().get(1).getProductSku());
    Assertions.assertEquals(3, summaries.getContent().get(1).getTotalItemSkuCount());
    Assertions.assertEquals("product-name-2", summaries.getContent().get(1).getProductName());
    Assertions.assertEquals("category-name-2", summaries.getContent().get(1).getCategoryName());
    Assertions.assertEquals(null, summaries.getContent().get(1).getStatus());
    Assertions.assertEquals("TOQ-19301-00002-00001", summaries.getContent().get(1).getItemDetails().get(0).getItemSku());
    Assertions.assertEquals("TOQ-19301-00002-00002", summaries.getContent().get(1).getItemDetails().get(1).getItemSku());
    Assertions.assertEquals("TOQ-19301-00002-00003", summaries.getContent().get(1).getItemDetails().get(2).getItemSku());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false);
    verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
    Mockito.verify(productItemSyncService)
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"));
    verify(applicationProperties, times(5)).getProductDetailPageUrlPrefix();
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-1");
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-2");
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-12345", "MTA-67890"));
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenProductReviewIsPending() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(StringUtils.EMPTY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-12345");
    activeProductResponse_1.setProductSku("TOQ-19301-00001");
    activeProductResponse_1.setProductName("product-name-1");
    activeProductResponse_1.setItemCount(2);

    ActiveProductResponse activeProductResponse_2 = new ActiveProductResponse();
    activeProductResponse_2.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_2.setProductCode("MTA-67890");
    activeProductResponse_2.setProductSku("TOQ-19301-00002");
    activeProductResponse_2.setProductName("product-name-2");
    activeProductResponse_2.setItemCount(3);

    Page<ActiveProductResponse> activeProductResponses =
        new PageImpl<>(Arrays.asList(activeProductResponse_1, activeProductResponse_2), pageable1, 2);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    ProductCollection productCollection_1 = new ProductCollection();
    productCollection_1.setProductCode("MTA-12345");
    productCollection_1.setReviewPending(false);

    ProductCollection productCollection_2 = new ProductCollection();
    productCollection_2.setProductCode("MTA-67890");
    productCollection_2.setReviewPending(true);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-12345", "MTA-67890")))
        .thenReturn(Arrays.asList(productCollection_1, productCollection_2));

    PageRequest pageRequest_1 = PageRequest.of(0, 2);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00001"));

    ItemSummaryResponse itemSummaryResponse_1 = new ItemSummaryResponse();
    itemSummaryResponse_1.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_1.setItemSku("TOQ-19301-00001-00001");
    itemSummaryResponse_1.setCategoryName("category-name-1");

    MasterCatalogDTO masterCatalogDTO_1 = new MasterCatalogDTO();
    CategoryDTO categoryDTO_1 = new CategoryDTO();
    categoryDTO_1.setCategoryCode("category-code-1");
    masterCatalogDTO_1.setCategory(categoryDTO_1);
    itemSummaryResponse_1.setMasterCatalog(masterCatalogDTO_1);

    ItemSummaryResponse itemSummaryResponse_2 = new ItemSummaryResponse();
    itemSummaryResponse_2.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_2.setItemSku("TOQ-19301-00001-00002");
    itemSummaryResponse_2.setCategoryName("category-name-1");

    Pageable pageable2 = PageRequest.of(0, 2);
    Page<ItemSummaryResponse> itemSummaryResponses =
        new PageImpl<>(Arrays.asList(itemSummaryResponse_1, itemSummaryResponse_2), pageable2, 2);
    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenReturn(itemSummaryResponses);

    CategoryDetailResponse categoryDetailResponse_1 = new CategoryDetailResponse();
    categoryDetailResponse_1.setName("category-name-1");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-1")).thenReturn(categoryDetailResponse_1);

    Mockito.when(productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE,
            Arrays.asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002"))).thenReturn(Collections.emptyList());
    when(applicationProperties.getProductDetailPageUrlPrefix()).thenReturn("http://blibli.com/product-detail");

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(1, summaries.getTotalElements());

    Assertions.assertEquals("TOQ-19301-00001", summaries.getContent().get(0).getProductSku());
    Assertions.assertEquals(2, summaries.getContent().get(0).getTotalItemSkuCount());
    Assertions.assertEquals("product-name-1", summaries.getContent().get(0).getProductName());
    Assertions.assertEquals("category-name-1", summaries.getContent().get(0).getCategoryName());
    Assertions.assertEquals(null, summaries.getContent().get(0).getStatus());
    Assertions.assertEquals("TOQ-19301-00001-00001", summaries.getContent().get(0).getItemDetails().get(0).getItemSku());
    Assertions.assertEquals("TOQ-19301-00001-00002", summaries.getContent().get(0).getItemDetails().get(1).getItemSku());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false);
    verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
    Mockito.verify(productItemSyncService)
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE,
            Arrays.asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002"));
    verify(applicationProperties, times(2)).getProductDetailPageUrlPrefix();
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-1");
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-12345", "MTA-67890"));
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenFilterApplyAndProductSkuIsNotValid() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .searchKey(SEARCH_KEY).build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(SEARCH_KEY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    Page<ActiveProductResponse> activeProductResponses = new PageImpl<>(Collections.emptyList(), pageable1, 0);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Collections.emptyList()))
        .thenReturn(Collections.emptyList());

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(0, summaries.getTotalElements());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false);
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Collections.emptyList());
  }

  @Test
  public void productsAvailableToBeCopiedTest_whenProductCodeIsNull() throws Exception {
    Pageable pageable1 = PageRequest.of(0, 10);
    ProductLevel3SummaryFilter filter =
        ProductLevel3SummaryFilter.builder().storeId(STORE_ID).businessPartnerCode(LINKED_PARTNER_CODE)
            .build();

    SummaryFilterRequest summaryFilterRequest = SummaryFilterRequest.builder().businessPartnerCode(LINKED_PARTNER_CODE)
        .suspensionStatus(SuspensionStatus.ACTIVE.toString()).searchKeyword(StringUtils.EMPTY)
        .pickupPointCodes(Collections.emptyList()).categoryCodes(Collections.emptyList()).nameKey(StringUtils.EMPTY)
        .build();

    ActiveProductResponse activeProductResponse_1 = new ActiveProductResponse();
    activeProductResponse_1.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_1.setProductCode("MTA-11111");
    activeProductResponse_1.setProductSku("TOQ-19301-00001");
    activeProductResponse_1.setProductName("product-name-1");
    activeProductResponse_1.setItemCount(2);

    ActiveProductResponse activeProductResponse_2 = new ActiveProductResponse();
    activeProductResponse_2.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_2.setProductCode("MTA-22222");
    activeProductResponse_2.setProductSku("TOQ-19301-00002");
    activeProductResponse_2.setProductName("product-name-2");
    activeProductResponse_2.setItemCount(3);

    ActiveProductResponse activeProductResponse_withNullProductCode = new ActiveProductResponse();
    activeProductResponse_withNullProductCode.setMerchantCode(LINKED_PARTNER_CODE);
    activeProductResponse_withNullProductCode.setProductSku("TOQ-19301-00003");
    activeProductResponse_withNullProductCode.setProductName("product-name-3");
    activeProductResponse_withNullProductCode.setItemCount(3);

    Page<ActiveProductResponse> activeProductResponses = new PageImpl<>(
        Arrays.asList(activeProductResponse_1, activeProductResponse_2, activeProductResponse_withNullProductCode),
        pageable1, 3);

    when(productLevel3Repository
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false))
        .thenReturn(activeProductResponses);

    ProductCollection productCollection_1 = new ProductCollection();
    productCollection_1.setProductCode("MTA-11111");
    productCollection_1.setReviewPending(false);

    ProductCollection productCollection_2 = new ProductCollection();
    productCollection_2.setProductCode("MTA-22222");
    productCollection_2.setReviewPending(false);
    when(productCollectionRepository
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111", "MTA-22222")))
        .thenReturn(Arrays.asList(productCollection_1, productCollection_2));

    PageRequest pageRequest_1 = PageRequest.of(0, 5);
    ItemSummaryRequest itemSummaryRequest_1 = new ItemSummaryRequest();
    itemSummaryRequest_1.setArchived(false);
    itemSummaryRequest_1.setMerchantCode(LINKED_PARTNER_CODE);
    itemSummaryRequest_1.setProductSkus(Arrays.asList("TOQ-19301-00001", "TOQ-19301-00002"));

    ItemSummaryResponse itemSummaryResponse_1 = new ItemSummaryResponse();
    itemSummaryResponse_1.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_1.setItemSku("TOQ-19301-00001-00001");
    itemSummaryResponse_1.setCategoryName("category-name-1");

    MasterCatalogDTO masterCatalogDTO = new MasterCatalogDTO();
    CategoryDTO categoryDTO = new CategoryDTO();
    categoryDTO.setCategoryCode("category-code-1");
    masterCatalogDTO.setCategory(categoryDTO);
    itemSummaryResponse_1.setMasterCatalog(masterCatalogDTO);

    ItemSummaryResponse itemSummaryResponse_2 = new ItemSummaryResponse();
    itemSummaryResponse_2.setProductSku("TOQ-19301-00001");
    itemSummaryResponse_2.setItemSku("TOQ-19301-00001-00002");
    itemSummaryResponse_2.setCategoryName("category-name-1");

    ItemSummaryResponse itemSummaryResponse_3 = new ItemSummaryResponse();
    itemSummaryResponse_3.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_3.setItemSku("TOQ-19301-00002-00001");
    itemSummaryResponse_3.setCategoryName("category-name-2");

    MasterCatalogDTO masterCatalogDTO_2 = new MasterCatalogDTO();
    CategoryDTO categoryDTO_2 = new CategoryDTO();
    categoryDTO_2.setCategoryCode("category-code-2");
    masterCatalogDTO_2.setCategory(categoryDTO_2);
    itemSummaryResponse_3.setMasterCatalog(masterCatalogDTO_2);

    ItemSummaryResponse itemSummaryResponse_4 = new ItemSummaryResponse();
    itemSummaryResponse_4.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_4.setItemSku("TOQ-19301-00002-00002");
    itemSummaryResponse_4.setCategoryName("category-name-2");

    ItemSummaryResponse itemSummaryResponse_5 = new ItemSummaryResponse();
    itemSummaryResponse_5.setProductSku("TOQ-19301-00002");
    itemSummaryResponse_5.setItemSku("TOQ-19301-00002-00003");
    itemSummaryResponse_5.setCategoryName("category-name-2");

    Pageable pageable2 = PageRequest.of(0, 5);
    Page<ItemSummaryResponse> itemSummaryResponses = new PageImpl<>(Arrays
        .asList(itemSummaryResponse_1, itemSummaryResponse_2, itemSummaryResponse_3, itemSummaryResponse_4,
            itemSummaryResponse_5), pageable2, 5);
    when(productLevel3Repository.findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null))
        .thenReturn(itemSummaryResponses);

    CategoryDetailResponse categoryDetailResponse = new CategoryDetailResponse();
    categoryDetailResponse.setName("category-name-1");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-1")).thenReturn(categoryDetailResponse);

    CategoryDetailResponse categoryDetailResponse_2 = new CategoryDetailResponse();
    categoryDetailResponse_2.setName("category-name-2");
    when(productOutbound.getCategoryDetailByCategoryCode("category-code-2")).thenReturn(categoryDetailResponse);

    ProductItemSyncStatus productItemSyncStatus = new ProductItemSyncStatus();
    productItemSyncStatus.setGdnItemSku("TOQ-19301-00001-00001");
    productItemSyncStatus.setProductSyncStatus(ProductSyncStatus.IN_PROGRESS);
    Mockito.when(productItemSyncService
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"))).thenReturn(Arrays.asList(productItemSyncStatus));
    when(applicationProperties.getProductDetailPageUrlPrefix()).thenReturn("http://blibli.com/product-detail");

    Page<AvailableToCopyProductDetailsResponse> summaries =
        this.productLevel3ServiceBean.productsAvailableToBeCopied(filter, BUSINESS_PARTNER_CODE, pageable1);

    Assertions.assertNotNull(summaries);
    Assertions.assertEquals(2, summaries.getTotalElements());

    Assertions.assertEquals("TOQ-19301-00001", summaries.getContent().get(0).getProductSku());
    Assertions.assertEquals(2, summaries.getContent().get(0).getTotalItemSkuCount());
    Assertions.assertEquals("IN_PROGRESS", summaries.getContent().get(0).getStatus());
    Assertions.assertEquals(3, summaries.getContent().get(1).getTotalItemSkuCount());
    Assertions.assertEquals(null, summaries.getContent().get(1).getStatus());
    Assertions.assertEquals("http://blibli.com/product-detail-TOQ.19301.00001.html",
        summaries.getContent().get(0).getItemDetails().get(0).getProductDetailPageLink());
    Assertions.assertEquals("http://blibli.com/product-detail-TOQ.19301.00002.html",
        summaries.getContent().get(1).getItemDetails().get(0).getProductDetailPageLink());

    verify(productLevel3Repository)
        .getAllProducts(summaryFilterRequest, pageable1, Collections.emptyList(), StringUtils.EMPTY, false);
    verify(productLevel3Repository).findSummaryByFilter(itemSummaryRequest_1, pageRequest_1, null);
    Mockito.verify(productItemSyncService)
        .findSyncStatusByItemSkuAndLinkedPartner(STORE_ID, BUSINESS_PARTNER_CODE, LINKED_PARTNER_CODE, Arrays
            .asList("TOQ-19301-00001-00001", "TOQ-19301-00001-00002", "TOQ-19301-00002-00001", "TOQ-19301-00002-00002",
                "TOQ-19301-00002-00003"));
    verify(applicationProperties, times(5)).getProductDetailPageUrlPrefix();
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-1");
    verify(productOutbound).getCategoryDetailByCategoryCode("category-code-2");
    verify(productCollectionRepository)
        .findByBusinessPartnerCodeAndProductCodeIn(LINKED_PARTNER_CODE, Arrays.asList("MTA-11111", "MTA-22222"));
  }

  @Test
  public void takeDownNeedForCorrectionProductTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "schedulesAddEditEnabled", true);
    ProductL3Response productL3Response =  new  ProductL3Response();
    productL3Response.setFreeSample(false);
    productL3Response.setOnline(true);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOnline(true);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 20));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    verify(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true),
          Mockito.anyList(), eq(false), eq(false));
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductForceReviewTrueTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ProductBusinessPartner productBusinessPartner1 = getProductBusinessPartner();
    productBusinessPartner1.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner, productItemBusinessPartner1));
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner1));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(),
            eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(productBusinessPartner1);
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner1, new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    verify(productAppealService).decrementCounterForProductAppeal(eq(Constants.DEFAULT_STORE_ID), any());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductForceReviewTrueItemWholesalePriceMapFalseTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "schedulesAddEditEnabled", true);
    ProductBusinessPartner productBusinessPartner1 = getProductBusinessPartner();
    productBusinessPartner1.setOnline(true);
    productBusinessPartner1.setFreeSample(false);
    productBusinessPartner1.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner, productItemBusinessPartner1));
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner1));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), any()))
        .thenReturn(Arrays.asList(new ProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(productBusinessPartner1);
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    itemSummaryDetailResponse.setWholesalePriceActivated(false);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner1, new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductForceReviewTrueItemWholesalePriceMapTrueTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ProductBusinessPartner productBusinessPartner1 = getProductBusinessPartner();
    productBusinessPartner1.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner, productItemBusinessPartner1));
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner1));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), any()))
        .thenReturn(Arrays.asList(new ProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(productBusinessPartner1);
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    itemSummaryDetailResponse.setWholesalePriceActivated(false);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner1, new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductForceReviewTrueWholesalePriceActivatedNotNullTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ProductBusinessPartner productBusinessPartner1 = getProductBusinessPartner();
    productBusinessPartner1.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner, productItemBusinessPartner1));
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner1));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), any()))
        .thenReturn(Arrays.asList(new ProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(productBusinessPartner1);
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner1, new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());
    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionItemSummaryDetailResponseNullTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ProductBusinessPartner productBusinessPartner1 = getProductBusinessPartner();
    productL3Response.setPreOrderDTO(preOrderDTO);
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner1.setGdnProductItemSku(ITEM_SKU);
    productBusinessPartner1.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner, productItemBusinessPartner1));
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner1));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(productBusinessPartner1);
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner1, new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductProductL3ResponseNullTest() throws Exception {
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(getProductBusinessPartner()));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(null, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
  }

  @Test
  public void takeDownNeedForCorrectionProductProductL3ResponseNullAndItemSummaryResponsesNullTest() throws Exception {
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(getProductBusinessPartner()));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(null, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
  }

  @Test
  public void takeDownNeedForCorrectionProductItemSummaryResponsesNullTest() throws Exception {
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(getProductBusinessPartner()));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
  }


  @Test
  public void takeDownNeedForCorrectionProductWholesaleResponseNullTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "schedulesAddEditEnabled", true);
    ProductL3Response productL3Response =  new  ProductL3Response();
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setOnline(true);
    productBusinessPartner.setFreeSample(true);
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemResponse();
    productAndItemsResponse.getItems().forEach(itemResponse -> itemResponse.setWholesalePriceActivated(null));
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response , REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    verify(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true),
          Mockito.anyList(), eq(false), eq(true));
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());

  }

  @Test
  public void takeDownNeedForCorrectionProductWholesaleResponseFalseTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemResponse();
    productAndItemsResponse.getItems().forEach(itemResponse -> itemResponse.setWholesalePriceActivated(false));
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(getProductBusinessPartner()));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(new  ProductL3Response(), REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    verify(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true),
          Mockito.anyList(), eq(false), eq(false));
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductItemResponseNullTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
   ProductL3Response productL3Response =  new  ProductL3Response();
   ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
   productBusinessPartner.setFreeSample(false);
   productBusinessPartner.setOnline(false);
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemResponse();
    productAndItemsResponse.getItems().get(0).setItemSku(PRODUCT_SKU);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true),
          Mockito.anyList(), eq(false), Mockito.anyBoolean());
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    verify(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true),
          Mockito.anyList(), eq(false), eq(false));
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductPreOrderFalseTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "schedulesAddEditEnabled", true);
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemResponse();
    ProductL3Response productL3Response =  new  ProductL3Response();
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOnline(false);
    productL3Response.setFreeSample(false);
    productL3Response.setOnline(false);
    productAndItemsResponse.getProduct().getPreOrder().setIsPreOrder(false);
    ProductItemWholesalePrice productItemWholesalePrice = getProductItemWholesalePrice();
    productItemWholesalePrice.setItemSku(PRODUCT_SKU);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(productItemWholesalePrice));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true),
          Mockito.anyList(), eq(false), eq(true));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    verify(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true),
          Mockito.anyList(), eq(false), eq(true));
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductPreOrderFalseSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "schedulesAddEditEnabled", false);
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemResponse();
    ProductL3Response productL3Response =  new  ProductL3Response();
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOnline(false);
    productL3Response.setFreeSample(false);
    productL3Response.setOnline(false);
    productAndItemsResponse.getProduct().getPreOrder().setIsPreOrder(false);
    ProductItemWholesalePrice productItemWholesalePrice = getProductItemWholesalePrice();
    productItemWholesalePrice.setItemSku(PRODUCT_SKU);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
      .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
      .thenReturn(new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
      .thenReturn(Arrays.asList(productItemWholesalePrice));
    doNothing().when(xProductOutbound)
      .updateItemViewConfigAndForceReview(eq(true),
        Mockito.anyList(), eq(false), eq(true));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
      .thenReturn(getProductBusinessPartner());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(productL3Response, REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
      .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
      .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
      .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
      .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    verify(xProductOutbound)
      .updateItemViewConfigAndForceReview(eq(true),
        Mockito.anyList(), eq(false), eq(false));
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }
  @Test
  public void takeDownNeedForCorrectionProductPreOrderNullTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "schedulesAddEditEnabled", false);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOnline(false);
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemResponse();
    productAndItemsResponse.getProduct().setPreOrder(null);
    productAndItemsResponse.getProduct().setForceReview(true);
    itemSummaryDetailResponse.setItemSku(ITEM_SKU);
    itemSummaryDetailResponse.setItemCode(ITEM_CODE);
    productAndItemsResponse.getItems().get(0).setItemCode(ITEM_CODE);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());

    Map<String, String> itemCodeSkuMap = new HashMap<>();
    itemCodeSkuMap.put(ITEM_CODE, GDNSKU);
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(new ProductL3Response(), REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse, itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, itemCodeSkuMap, new ProductDetailResponse());

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductPreOrderskipSettingProductItemIdForNullCaseItemIdIsEmptyTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "schedulesAddEditEnabled", false);
    ReflectionTestUtils.setField(productLevel3ServiceBean,"skipSettingProductItemIdForNullCase", true);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOnline(false);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(GDNSKU);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setGdnProductItemSku("itemSku");
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner,productItemBusinessPartner1));
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemResponse();
    productAndItemsResponse.getProduct().setPreOrder(null);
    productAndItemsResponse.getProduct().setForceReview(true);
    itemSummaryDetailResponse.setItemSku(ITEM_SKU);
    itemSummaryDetailResponse.setItemCode(ITEM_CODE);
    productAndItemsResponse.getItems().get(0).setItemCode(ITEM_CODE);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());

    Map<String, String> itemCodeSkuMap = new HashMap<>();
    itemCodeSkuMap.put(ITEM_CODE, GDNSKU);
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(new ProductL3Response(), REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
            .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse, itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, itemCodeSkuMap, new ProductDetailResponse());

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductPreOrderskipSettingProductItemIdForNullCaseItemIdSwitchFalseTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "schedulesAddEditEnabled", false);
    ReflectionTestUtils.setField(productLevel3ServiceBean,"skipSettingProductItemIdForNullCase", false);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOnline(false);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(GDNSKU);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setGdnProductItemSku("itemSku");
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner,productItemBusinessPartner1));
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemResponse();
    productAndItemsResponse.getProduct().setPreOrder(null);
    productAndItemsResponse.getProduct().setForceReview(true);
    itemSummaryDetailResponse.setItemSku(ITEM_SKU);
    itemSummaryDetailResponse.setItemCode(ITEM_CODE);
    productAndItemsResponse.getItems().get(0).setItemCode(ITEM_CODE);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());

    Map<String, String> itemCodeSkuMap = new HashMap<>();
    itemCodeSkuMap.put(ITEM_CODE, GDNSKU);
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(new ProductL3Response(), REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
            .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse, itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(new BasicProductResponse());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, itemCodeSkuMap, new ProductDetailResponse());

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.anyList());
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());
  }

  @Test
  public void takeDownNeedForCorrectionProductBusinessPartnerNullTest() throws Exception {
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(null);

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
  }

  @Test
  public void takeDownNeedForCorrectionProductResponseNullTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemResponse();
    productAndItemsResponse.setProduct(null);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(getProductBusinessPartner()));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, ""));
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(new  ProductL3Response(), REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
  }

  @Test
  public void takeDownNeedForCorrectionItemResponseNullTest() throws Exception {
    ProductAndItemsResponse productAndItemsResponse = getProductAndItemResponse();
    productAndItemsResponse.setItems(null);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(getProductBusinessPartner()));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, productAndItemsResponse, ""));
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(new ProductL3Response(), REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
  }

  @Test
  public void addVatUpdateExternalHistoryTest() throws Exception {
    when(
        productItemBusinessPartnerService.findProductItemByProductItemId(STORE_ID, PRODUCT_ITEM_ID))
        .thenReturn(Arrays.asList(productItemBusinessPartner));
    doNothing().when(updatedProductHistoryService)
        .addAuditLogsForVatUpdate(Arrays.asList(productItemBusinessPartner), ITEM_NAME,
            Boolean.FALSE.toString(), Boolean.TRUE.toString());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(new  ProductL3Response(), REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(new ProductBusinessPartner(), new ArrayList<>()));
    productLevel3ServiceBean.addVatUpdateExternalHistory(vatUpdateHistoryDomainEventModel);

    verify(productItemBusinessPartnerService)
        .findProductItemByProductItemId(STORE_ID, PRODUCT_ITEM_ID);
    verify(updatedProductHistoryService).addAuditLogsForVatUpdate(Arrays.asList(productItemBusinessPartner), ITEM_NAME,
        Boolean.FALSE.toString(), Boolean.TRUE.toString());
  }

  @Test
  public void addVatUpdateExternalHistoryExceptionTest() throws JsonProcessingException {
    when(
        productItemBusinessPartnerService.findProductItemByProductItemId(STORE_ID, PRODUCT_ITEM_ID))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productLevel3ServiceBean.addVatUpdateExternalHistory(vatUpdateHistoryDomainEventModel);
      });
    } finally {
      verify(productItemBusinessPartnerService)
          .findProductItemByProductItemId(STORE_ID, PRODUCT_ITEM_ID);
    }
  }


  private ProductBusinessPartner getProductBusinessPartner() {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setStoreId(Constants.DEFAULT_STORE_ID);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(GDNSKU);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    productBusinessPartner.setAppealedProduct(true);
    return productBusinessPartner;
  }

  private ProductAndItemsResponse getProductAndItemResponse() {
    ProductResponse productResponse = new ProductResponse();
    productResponse.setPreOrder(new PreOrderDTO(true, PRE_ORDER_TYPE, 2, new Date(), false));
    productResponse.setProductType(ProductType.REGULAR);
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setItemSku(GDNSKU);
    itemResponse.setMerchantSku(MERCHANT_SKU);
    itemResponse.setWholesalePriceActivated(true);
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(1000.0);
    priceDTO.setOfferPrice(500.0);
    itemResponse.setPrice(ImmutableSet.of(priceDTO));
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setBuyable(true);
    itemViewConfigDTO.setDiscoverable(true);
    itemResponse.setItemViewConfigs(ImmutableSet.of(itemViewConfigDTO));
    return new ProductAndItemsResponse(productResponse, Arrays.asList(itemResponse));
  }

  private ProductItemWholesalePrice getProductItemWholesalePrice() {
    ProductItemWholesalePrice productItemWholesalePrice = new ProductItemWholesalePrice();
    productItemWholesalePrice.setItemSku(GDNSKU);
    return productItemWholesalePrice;
  }

  @Test
  public void setProductNameInHistoryIfEmpty_NameEmptyTest() throws ApplicationException {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "setProductNameInAudit", true);
    AuditTrailDto auditTrailDto = new AuditTrailDto();
    auditTrailDto.setProductSku(PRODUCT_SKU);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setProductName(PRODUCT_NAME);

    Mockito.when(productBusinessPartnerRepository.findByStoreIdAndGdnProductSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(PRODUCT_SKU))).thenReturn(Arrays.asList(productBusinessPartner));
    productLevel3ServiceBean.setProductNameInHistoryIfEmpty(Arrays.asList(auditTrailDto));
    Mockito.verify(productBusinessPartnerRepository).findByStoreIdAndGdnProductSkuIn(Constants.DEFAULT_STORE_ID,
        Arrays.asList(PRODUCT_SKU));

    Assertions.assertEquals(PRODUCT_NAME, auditTrailDto.getName());
  }

  @Test
  public void setProductNameInHistoryIfEmpty_NameNotEmptyTest() throws ApplicationException {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "setProductNameInAudit", true);
    AuditTrailDto auditTrailDto = new AuditTrailDto();
    auditTrailDto.setProductSku(PRODUCT_SKU);
    auditTrailDto.setName(PRODUCT_NAME);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setProductName(PRODUCT_NAME);

    productLevel3ServiceBean.setProductNameInHistoryIfEmpty(Arrays.asList(auditTrailDto));;

    Assertions.assertEquals(PRODUCT_NAME, auditTrailDto.getName());
  }

  @Test
  public void setProductNameInHistoryIfEmpty_setProductNameInAuditFalseTest() throws ApplicationException {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "setProductNameInAudit", false);
    AuditTrailDto auditTrailDto = new AuditTrailDto();
    auditTrailDto.setProductSku(PRODUCT_SKU);
    auditTrailDto.setGdnSku(GDNSKU);
    GdnRestSingleResponse<SimpleMapStringResponse> response =
        new GdnRestSingleResponse<>(null, null, true, new SimpleMapStringResponse(ImmutableMap.of(GDNSKU, ITEM_NAME)),
            REQUEST_ID);

    Mockito.when(xProductOutbound.getItemNameByItemSkus(Mockito.any(SimpleListStringRequest.class), Mockito.eq(true)))
        .thenReturn(response);
    productLevel3ServiceBean.setProductNameInHistoryIfEmpty(Arrays.asList(auditTrailDto));
    Mockito.verify(xProductOutbound).getItemNameByItemSkus(Mockito.any(SimpleListStringRequest.class), Mockito.eq(true));

    Assertions.assertEquals(ITEM_NAME, auditTrailDto.getName());
  }

  @Test
  public void takeDownNeedForCorrectionProductActiveProductNotActivatedTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(getProductBusinessPartner()));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(getProductBusinessPartner());
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(new  ProductL3Response(), REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
            .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.getBasicProductInfoV2(Mockito.anyString())).thenReturn(null);

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    Mockito.verify(xProductOutbound).getBasicProductInfoV2(Mockito.anyString());

  }

  @Test
  public void takeDownNeedForCorrectionProductActiveProductValidationOffTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "schedulesAddEditEnabled", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", false);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setOnline(false);
    productBusinessPartner.setFreeSample(true);
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductAndItems(true, PRODUCT_SKU, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, getProductAndItemResponse(), ""));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePrice()));
    doNothing().when(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true), Mockito.anyList(), eq(false));
    when(productBusinessPartnerRepository.save(productBusinessPartnerArgumentCaptor.capture()))
        .thenReturn(productBusinessPartner);
    doNothing().when(productItemWholesalePriceService)
        .saveWholesalePrice(productItemWholesalePricesArgumentCaptor.capture());
    Mockito.when(xProductOutbound.getProductDetailsByProductSku(Mockito.any())).thenReturn(new GdnRestSingleResponse(new  ProductL3Response(), REQUEST_ID));
    productSystemParameter.setValue("10");
    Mockito.when(productSystemParameterService
            .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(xProductOutbound.findSummaryDetailsByFilter(Mockito.any(), Mockito.any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 20));
    Mockito.when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
    verify(productItemWholesalePriceService)
        .findByStoreIdAndItemSkus(eq(Constants.DEFAULT_STORE_ID), Mockito.any());
    verify(xProductOutbound)
        .updateItemViewConfigAndForceReview(eq(true),
          Mockito.anyList(), eq(false), eq(true));
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.getValue());
  }

  @Test
  public void takeDownNeedForCorrectionProductNewWholesaleFlowEnabledTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "newWholesaleFlowNrEnabled", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "wholesalePriceFetchBatchSize", WHOLESALE_PRICE_FETCH_BATCH_SIZE);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerWithPickupPoint();
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setOnline(true);
    productL3Response.setForceReview(false);
    
    WholesalePriceSkuResponse wholesalePriceSkuResponse = getWholesalePriceSkuResponse();
    List<ProductItemWholesalePrice> existingWholesalePrices = Arrays.asList(getProductItemWholesalePriceWithPickupPoint());
    
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductDetailsByProductSku(PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(getItemSummaryDetailResponse()), PAGEABLE, 1));
    when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(any(), any(), any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    when(productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(any()))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(existingWholesalePrices);
    when(mapperUtil.mapRequestToString(any())).thenReturn(WHOLESALE_RULES);
    when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(getProductSystemParameter("10"));
    when(xProductOutbound.getBasicProductInfoV2(any())).thenReturn(new BasicProductResponse());
    when(productBusinessPartnerRepository.save(any())).thenReturn(productBusinessPartner);
    doNothing().when(productItemWholesalePriceService).saveWholesalePriceNew(any());
    
    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);
    
    verify(productPricingOutbound).getWholesalePriceByItemSkuAndPickupPointCode(any());
    verify(productItemWholesalePriceService).findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU));
    verify(mapperUtil).mapRequestToString(any());
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.capture());
  }

  @Test
  public void takeDownNeedForCorrectionProductNewWholesaleFlowEmptyResponseTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "newWholesaleFlowNrEnabled", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "wholesalePriceFetchBatchSize", WHOLESALE_PRICE_FETCH_BATCH_SIZE);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerWithPickupPoint();
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setOnline(true);
    productL3Response.setForceReview(false);
    
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductDetailsByProductSku(PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(getItemSummaryDetailResponse()), PAGEABLE, 1));
    when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(any(), any(), any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    when(productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(any()))
        .thenReturn(Collections.emptyList());
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Arrays.asList(getProductItemWholesalePriceWithPickupPoint()));
    when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(getProductSystemParameter("10"));
    when(xProductOutbound.getBasicProductInfoV2(any())).thenReturn(new BasicProductResponse());
    when(productBusinessPartnerRepository.save(any())).thenReturn(productBusinessPartner);
    doNothing().when(productItemWholesalePriceService).saveWholesalePriceNew(any());
    
    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);
    
    verify(productPricingOutbound).getWholesalePriceByItemSkuAndPickupPointCode(any());
    verify(productItemWholesalePriceService).findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU));
  }

  @Test
  public void takeDownNeedForCorrectionProductNewWholesaleFlowNewPriceCreatedTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "newWholesaleFlowNrEnabled", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "wholesalePriceFetchBatchSize", WHOLESALE_PRICE_FETCH_BATCH_SIZE);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerWithPickupPoint();
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setOnline(true);
    productL3Response.setForceReview(false);
    
    WholesalePriceSkuResponse wholesalePriceSkuResponse = getWholesalePriceSkuResponse();
    
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductDetailsByProductSku(PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(getItemSummaryDetailResponse()), PAGEABLE, 1));
    when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(any(), any(), any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    when(productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(any()))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Collections.emptyList());
    when(mapperUtil.mapRequestToString(any())).thenReturn(WHOLESALE_RULES);
    when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(getProductSystemParameter("10"));
    when(xProductOutbound.getBasicProductInfoV2(any())).thenReturn(new BasicProductResponse());
    when(productBusinessPartnerRepository.save(any())).thenReturn(productBusinessPartner);
    doNothing().when(productItemWholesalePriceService).saveWholesalePriceNew(any());
    
    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);
    
    verify(productPricingOutbound).getWholesalePriceByItemSkuAndPickupPointCode(any());
    verify(mapperUtil).mapRequestToString(any());
  }

  @Test
  public void takeDownNeedForCorrectionProductNewWholesaleFlowInactiveSkuTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "newWholesaleFlowNrEnabled", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "wholesalePriceFetchBatchSize", WHOLESALE_PRICE_FETCH_BATCH_SIZE);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerWithPickupPoint();
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setOnline(true);
    productL3Response.setForceReview(false);
    
    WholesalePriceSkuResponse wholesalePriceSkuResponse = getWholesalePriceSkuResponse();
    wholesalePriceSkuResponse.setSkuStatus(SKU_STATUS_INACTIVE);
    
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductDetailsByProductSku(PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(getItemSummaryDetailResponse()), PAGEABLE, 1));
    when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(any(), any(), any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    when(productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(any()))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(Collections.emptyList());
    when(mapperUtil.mapRequestToString(any())).thenReturn(WHOLESALE_RULES);
    when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(getProductSystemParameter("10"));
    when(xProductOutbound.getBasicProductInfoV2(any())).thenReturn(new BasicProductResponse());
    when(productBusinessPartnerRepository.save(any())).thenReturn(productBusinessPartner);
    doNothing().when(productItemWholesalePriceService).saveWholesalePriceNew(any());
    
    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);
    
    verify(productPricingOutbound).getWholesalePriceByItemSkuAndPickupPointCode(any());
    verify(mapperUtil).mapRequestToString(any());
  }

  @Test
  public void takeDownNeedForCorrectionProductNewWholesaleFlowBatchProcessingTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "newWholesaleFlowNrEnabled", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "wholesalePriceFetchBatchSize", 1);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerWithMultipleItems();
    //productBusinessPartner.getProductItemBusinessPartners().get(0).setProductItemId(PRODUCT_CODE);
    productBusinessPartner.getProductItemBusinessPartners().get(1).setProductItemId(PRODUCT_ITEM_ID);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setOnline(true);
    productL3Response.setForceReview(false);
    
    WholesalePriceSkuResponse wholesalePriceSkuResponse1 = getWholesalePriceSkuResponse();
    WholesalePriceSkuResponse wholesalePriceSkuResponse2 = getWholesalePriceSkuResponse();
    wholesalePriceSkuResponse2.setItemSku("TEC-12354-1234456");
    wholesalePriceSkuResponse2.setItemPickupPointId("TEC-12354-1234456-JKT10");
    
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductDetailsByProductSku(PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(getItemSummaryDetailResponse()), PAGEABLE, 1));
    when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(any(), any(), any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    when(productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(any()))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse1))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse2));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(any(), any()))
        .thenReturn(Collections.emptyList());
    when(mapperUtil.mapRequestToString(any())).thenReturn(WHOLESALE_RULES);
    when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(getProductSystemParameter("10"));
    when(xProductOutbound.getBasicProductInfoV2(any())).thenReturn(new BasicProductResponse());
    when(productBusinessPartnerRepository.save(any())).thenReturn(productBusinessPartner);
    doNothing().when(productItemWholesalePriceService).saveWholesalePriceNew(any());

    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setId(PRODUCT_ITEM_ID);
    productItemResponse.setSkuCode(ITEM_CODE);
    productDetailResponse.setProductItemResponses(Collections.singleton(productItemResponse));
    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), productDetailResponse);
    
    verify(productPricingOutbound, times(2)).getWholesalePriceByItemSkuAndPickupPointCode(any());
    verify(mapperUtil, times(2)).mapRequestToString(any());
  }

  @Test
  public void takeDownNeedForCorrectionProductNewWholesaleFlowBatchProcessingTest2() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "newWholesaleFlowNrEnabled", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "wholesalePriceFetchBatchSize", 1);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);

    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerWithMultipleItems();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setGdnProductItemSku(PICKUP_POINT_CODE);
    productBusinessPartner.getProductItemBusinessPartners().get(1).setPickupPointId(PRODUCT_ITEM_ID);
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setOnline(true);
    productL3Response.setForceReview(false);

    WholesalePriceSkuResponse wholesalePriceSkuResponse1 = getWholesalePriceSkuResponse();
    WholesalePriceSkuResponse wholesalePriceSkuResponse2 = getWholesalePriceSkuResponse();
    wholesalePriceSkuResponse2.setItemSku("TEC-12354-1234456");
    wholesalePriceSkuResponse2.setItemPickupPointId("TEC-12354-1234456-JKT10");

    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductDetailsByProductSku(PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(getItemSummaryDetailResponse()), PAGEABLE, 1));
    when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(any(), any(), any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    when(productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(any()))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse1))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse2));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(any(), any()))
        .thenReturn(Collections.emptyList());
    when(mapperUtil.mapRequestToString(any())).thenReturn(WHOLESALE_RULES);
    when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(getProductSystemParameter("10"));
    when(xProductOutbound.getBasicProductInfoV2(any())).thenReturn(new BasicProductResponse());
    when(productBusinessPartnerRepository.save(any())).thenReturn(productBusinessPartner);
    doNothing().when(productItemWholesalePriceService).saveWholesalePriceNew(any());

    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);

    verify(productPricingOutbound, times(2)).getWholesalePriceByItemSkuAndPickupPointCode(any());
    verify(mapperUtil, times(2)).mapRequestToString(any());
  }

  @Test
  public void takeDownNeedForCorrectionProductNewWholesaleFlowMarkForDeletedItemsTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "newWholesaleFlowNrEnabled", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "wholesalePriceFetchBatchSize", WHOLESALE_PRICE_FETCH_BATCH_SIZE);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerWithMarkedForDeleteItems();
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setOnline(true);
    productL3Response.setForceReview(false);
    
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductDetailsByProductSku(PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(getItemSummaryDetailResponse()), PAGEABLE, 1));
    when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(any(), any(), any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(any(), any()))
        .thenReturn(Collections.emptyList());
    when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(getProductSystemParameter("10"));
    when(xProductOutbound.getBasicProductInfoV2(any())).thenReturn(new BasicProductResponse());
    when(productBusinessPartnerRepository.save(any())).thenReturn(productBusinessPartner);
    doNothing().when(productItemWholesalePriceService).saveWholesalePriceNew(any());
    
    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);
    
    verify(productPricingOutbound, never()).getWholesalePriceByItemSkuAndPickupPointCode(any());
  }

  @Test
  public void takeDownNeedForCorrectionProductOldWholesaleFlowTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "newWholesaleFlowNrEnabled", false);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerWithPickupPoint();
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setOnline(true);
    productL3Response.setForceReview(false);
    
    List<ProductItemWholesalePrice> existingWholesalePrices = Arrays.asList(getProductItemWholesalePriceWithPickupPoint());
    ItemSummaryDetailResponse itemSummaryDetailResponse = getItemSummaryDetailResponse();
    itemSummaryDetailResponse.setWholesalePriceActivated(true);
    itemSummaryDetailResponse.setPickupPointCode(PICKUP_POINT_CODE);
    
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductDetailsByProductSku(PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(itemSummaryDetailResponse), PAGEABLE, 1));
    when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(any(), any(), any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(existingWholesalePrices);
    when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(getProductSystemParameter("10"));
    when(xProductOutbound.getBasicProductInfoV2(any())).thenReturn(new BasicProductResponse());
    when(productBusinessPartnerRepository.save(any())).thenReturn(productBusinessPartner);
    doNothing().when(productItemWholesalePriceService).saveWholesalePriceNew(any());
    
    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);
    
    verify(productPricingOutbound, never()).getWholesalePriceByItemSkuAndPickupPointCode(any());
    verify(productItemWholesalePriceService).findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU));
  }

  private ProductBusinessPartner getProductBusinessPartnerWithPickupPoint() {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setStoreId(Constants.DEFAULT_STORE_ID);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setAppealedProduct(true);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(GDNSKU);
    productItemBusinessPartner.setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartner.setMarkForDelete(false);
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    return productBusinessPartner;
  }

  private ProductBusinessPartner getProductBusinessPartnerWithMultipleItems() {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setStoreId(Constants.DEFAULT_STORE_ID);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setAppealedProduct(true);
    
    ProductItemBusinessPartner productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setGdnProductItemSku(GDNSKU);
    productItemBusinessPartner1.setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartner1.setMarkForDelete(false);
    
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku("TEC-12354-1234456");
    productItemBusinessPartner2.setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartner2.setMarkForDelete(false);
    
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner1, productItemBusinessPartner2));
    return productBusinessPartner;
  }

  private ProductBusinessPartner getProductBusinessPartnerWithMarkedForDeleteItems() {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setStoreId(Constants.DEFAULT_STORE_ID);
    productBusinessPartner.setGdnProductSku(PRODUCT_SKU);
    productBusinessPartner.setAppealedProduct(true);
    
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(GDNSKU);
    productItemBusinessPartner.setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartner.setMarkForDelete(true);
    
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));
    return productBusinessPartner;
  }

  private WholesalePriceSkuResponse getWholesalePriceSkuResponse() {
    WholesalePriceSkuResponse response = new WholesalePriceSkuResponse();
    response.setItemSku(GDNSKU);
    response.setPickUpPointCode(PICKUP_POINT_CODE);
    response.setItemPickupPointId(ITEM_PICKUP_POINT_ID);
    response.setSkuStatus(SKU_STATUS_ACTIVE);
    response.setWholesaleRules(new HashMap<>());
    return response;
  }

  private ProductItemWholesalePrice getProductItemWholesalePriceWithPickupPoint() {
    ProductItemWholesalePrice productItemWholesalePrice = new ProductItemWholesalePrice();
    productItemWholesalePrice.setStoreId(Constants.DEFAULT_STORE_ID);
    productItemWholesalePrice.setItemSku(GDNSKU);
    productItemWholesalePrice.setPickupPointCode(PICKUP_POINT_CODE);
    productItemWholesalePrice.setWholesaleRules(WHOLESALE_RULES);
    productItemWholesalePrice.setWholesalePriceActivated(true);
    return productItemWholesalePrice;
  }

  private ItemSummaryDetailResponse getItemSummaryDetailResponse() {
    ItemSummaryDetailResponse response = new ItemSummaryDetailResponse();
    response.setItemSku(GDNSKU);
    response.setPickupPointCode(PICKUP_POINT_CODE);
    response.setWholesalePriceActivated(true);
    response.setMerchantSku(MERCHANT_SKU);
    PriceDTO priceDTO = new PriceDTO();
    priceDTO.setListPrice(1000.0);
    priceDTO.setOfferPrice(500.0);
    response.setPrice(ImmutableSet.of(priceDTO));
    return response;
  }

  private ProductSystemParameter getProductSystemParameter(String value) {
    ProductSystemParameter parameter = new ProductSystemParameter();
    parameter.setValue(value);
    return parameter;
  }

  @Test
  public void takeDownNeedForCorrectionProductNewWholesaleFlowMarkExistingPricesForDeleteTest() throws Exception {
    ReflectionTestUtils.setField(productLevel3ServiceBean, "newWholesaleFlowNrEnabled", true);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "wholesalePriceFetchBatchSize", WHOLESALE_PRICE_FETCH_BATCH_SIZE);
    ReflectionTestUtils.setField(productLevel3ServiceBean, "validateProductActivationDoneOnce", true);
    
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerWithPickupPoint();
    ProductL3Response productL3Response = new ProductL3Response();
    productL3Response.setOnline(true);
    productL3Response.setForceReview(false);
    
    ProductItemWholesalePrice existingWholesalePrice1 = getProductItemWholesalePriceWithPickupPoint();
    existingWholesalePrice1.setItemSku(GDNSKU);
    existingWholesalePrice1.setPickupPointCode(PICKUP_POINT_CODE);
    
    ProductItemWholesalePrice existingWholesalePrice2 = getProductItemWholesalePriceWithPickupPoint();
    existingWholesalePrice2.setItemSku(GDNSKU_2);
    existingWholesalePrice2.setPickupPointCode(PICKUP_POINT_CODE_2);
    
    List<ProductItemWholesalePrice> existingWholesalePrices = Arrays.asList(existingWholesalePrice1, existingWholesalePrice2);
    
    WholesalePriceSkuResponse wholesalePriceSkuResponse = getWholesalePriceSkuResponse();
    wholesalePriceSkuResponse.setItemSku(GDNSKU);
    wholesalePriceSkuResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    wholesalePriceSkuResponse.setItemPickupPointId(GDNSKU + Constants.HYPHEN + PICKUP_POINT_CODE);
    
    when(productBusinessPartnerRepository.findProductBusinessPartnerByProductCode(PRODUCT_CODE))
        .thenReturn(Arrays.asList(productBusinessPartner));
    when(xProductOutbound.getProductDetailsByProductSku(PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(productL3Response, REQUEST_ID));
    when(xProductOutbound.findSummaryDetailsByFilter(any(), any()))
        .thenReturn(new PageImpl<>(Arrays.asList(getItemSummaryDetailResponse()), PAGEABLE, 1));
    when(productBusinessPartnerService.updateProductItemBusinessPartnerStateTakeDownTrue(any(), any(), any()))
        .thenReturn(new ProductBusinessPartnerAndItemViewConfigDto(productBusinessPartner, new ArrayList<>()));
    when(productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(any()))
        .thenReturn(Arrays.asList(wholesalePriceSkuResponse));
    when(productItemWholesalePriceService.findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU)))
        .thenReturn(existingWholesalePrices);
    when(mapperUtil.mapRequestToString(any())).thenReturn(WHOLESALE_RULES);
    when(productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.ITEM_SUMMARY_PAGE_SIZE))
        .thenReturn(getProductSystemParameter("10"));
    when(xProductOutbound.getBasicProductInfoV2(any())).thenReturn(new BasicProductResponse());
    when(productBusinessPartnerRepository.save(any())).thenReturn(productBusinessPartner);
    doNothing().when(productItemWholesalePriceService).saveWholesalePriceNew(any());
    
    productLevel3ServiceBean.takeDownNeedForCorrectionProduct(PRODUCT_CODE, new HashMap<>(), null);
    
    verify(productPricingOutbound).getWholesalePriceByItemSkuAndPickupPointCode(any());
    verify(productItemWholesalePriceService).findByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID, Arrays.asList(GDNSKU));
    verify(mapperUtil).mapRequestToString(any());
    verify(productBusinessPartnerRepository).save(productBusinessPartnerArgumentCaptor.capture());
  }
}
