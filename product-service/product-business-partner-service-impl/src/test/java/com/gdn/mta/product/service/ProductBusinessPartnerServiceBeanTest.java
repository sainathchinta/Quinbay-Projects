package com.gdn.mta.product.service;

import static java.util.function.Predicate.not;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.response.ProductBusinessPartnerAndItemViewConfigDto;
import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.enums.ProductLevel3RetryStatus;
import com.gdn.mta.product.service.config.PreOrderConfig;
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
import org.mockito.MockitoAnnotations;
import org.mockito.verification.VerificationMode;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ProductItemBusinessPartnerLogisticsRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ItemFlagDetails;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemSyncProcess;
import com.gdn.mta.product.entity.ProductItemSyncProcessSummary;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.RejectedSkuProductCollection;
import com.gdn.mta.product.enums.ProductSyncStatus;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerCustomRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductFlow2AuditRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.util.MapperUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.partners.pbp.calendar.CalendarService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.dto.productlevel3.SuspensionItemResponse;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3AggregatorServiceOld;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.partners.pbp.service.sysparam.SystemParameterService;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.entity.Company;
import com.gdn.x.businesspartner.entity.LegalityOfCompany;
import com.gdn.x.businesspartner.entity.Profile;
import com.gdn.x.businesspartner.entity.ResponsiblePerson;
import com.gdn.x.businesspartner.entity.Taxation;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductItem;

@SuppressWarnings("deprecation")
public class ProductBusinessPartnerServiceBeanTest {

  private static final String STORE_ID = "10001";

  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";

  private static final Integer DEFAULT_PAGE = 0;

  private static final Integer DEFAULT_SIZE = 10;

  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);

  private static final VerificationMode NEVER_CALLED = Mockito.times(0);

  private static final VerificationMode CALLED_TWICE = Mockito.times(2);

  private static final String DEFAULT_PRODUCT_BUSINESS_PARTNER_ID = UUID.randomUUID().toString();

  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00001";

  private static final String GDN_PRODUCT_SKU = "dummy-product-sku";
  private static final String SIZE_CHART_CODE = "size-chart-code";

  private static final String GDN_ITEM_SKU = "gdn-item-sku";

  private static final String GDN_ITEM_ID = "gdn-item-id";

  private static final String DEFAULT_PRODUCT_CODE = "test";

  private static final String DEFAULT_ID = "1";

  private static final String DEFAULT_CATEGORY_CODE = "categoryCode";

  private static final String DEFAULT_PICKUP_POINT = "pickupPoint";

  private static final String GDN_SKU = "BP1-00001-00001";

  private static final String ITEM_SKU = "BP1-00001-00001-00001";
  private static final String ITEM_SKU_1 = "BP1-00001-00001-00002";

  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_CODE_1 = "pickupPointCode1";
  private static final String PICKUP_POINT_CODE_2 = "pickupPointCode2";

  private static final String PRODUCT_ID = "productId";

  private static final String PRODUCT_CODE = "productCode";

  private static final String VALUE = "value";

  private static final String ATTRIBUTE_ID = "attributeId";

  private static final String MERCHANT_SKU = "default_merchant_sku";

  private static RejectedSkuProductCollection rejectedSkuProductCollection = new RejectedSkuProductCollection();

  private static List<RejectedSkuProductCollection> rejectedSkuProductCollections =
    new ArrayList<RejectedSkuProductCollection>();

  List<String> productSkus = new ArrayList<>();

  private static final String DEFAULT_PRODUCT_NAME = "PRODUCT-1";
  private static final String IN_PROGRESS_STATE = "IN_PROGRESS";

  private static final String DEFAULT_CATEGORY_NAME = "CATEGORY-1";

  private static final String DEFAULT_BRAND_NAME = "BRAND-1";

  private static final String DEFAULT_REASON = "REASON-1";

  private static final String LINKED_BUSINESS_PARTNER_CODE = "SOA-21412";

  private static final String PROCESS_ID = "process-id";

  private static final String PRODUCT_SUCCESSFULLY_ADDED_AS_FBB = "Semua produk tersalin ke FBB. ";

  private static final String PRODUCT_PARTIALLY_ADDED_AS_FBB = "%s dari %s produk tersalin ke FBB. ";

  private static final String REQUEST_ID = "requestId";

  private static final String USERNAME = "username";

  private static String ORDER_BY = "updatedDate";

  private static String SORT_BY = "desc";

  private static String SEARCH_CRITERIA = "SEARCH_CRITERIA";

  private static final Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);

  private static final String WHOLESALE_RULE_1 = "[{\"quantity\":3,\"wholesaleDiscount\":30.0]";

  private static final String CATEGORY_CODE = "categoryCode";

  private static final String STATE = "STATE";

  private static final double OFFER_PRICE = 10;

  private static final double LIST_PRICE = 100;

  private static final String PRODUCT_NAME = "productName";

  private Page<RejectedSkuProductCollection> page;

  private Page<ProductBusinessPartner> productBusinessPartners;

  private List<ProductAttribute> productAttributeList;

  private ProductAttribute productAttribute;

  private Attribute attribute;

  private ProductAttributeValue productAttributeValue;

  private ProductSystemParameter productSystemParameter;

  private ProductItemBusinessPartner productItemBusinessPartner1;
  private ProductItemBusinessPartner productItemBusinessPartner2;
  private ProductItemBusinessPartner productItemBusinessPartner3;
  private ProductItemBusinessPartner productItemBusinessPartner4;
  private ProductItemBusinessPartner productItemBusinessPartner5;

  private ItemSummaryDetailResponse itemSummaryDetailResponse1;
  private ItemSummaryDetailResponse itemSummaryDetailResponse2;
  private ItemSummaryDetailResponse itemSummaryDetailResponse3;
  private ItemSummaryDetailResponse itemSummaryDetailResponse4;

  private List<ProductItemBusinessPartner> productItemBusinessPartnerList;
  private List<ItemSummaryDetailResponse> itemSummaryDetailResponseList;
  private ItemViewConfigDTO itemViewConfigDTO;
  private PriceDTO priceDTO;
  private ProductLevel3 productLevel3 = new ProductLevel3();
  private ProductL3Response productL3Response = new ProductL3Response();
  private ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
  private ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest =
      new ProductVariantPriceStockAndImagesRequest();
  private ItemPickupPointRequest itemPickupPointRequest1 = new ItemPickupPointRequest();
  private ItemPickupPointRequest itemPickupPointRequest2 = new ItemPickupPointRequest();
  ProductDetailResponse productData = new ProductDetailResponse(new ProductResponse());


  Profile businessPartner = new Profile();

  @Mock
  private PreOrderConfig preOrderConfig;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private ProductBusinessPartnerCustomRepository productBusinessPartnerCustomRepository;

  @Mock
  private ProductWorkflowService productWorkflowService;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private ProductLevel3Service productLevel3Service;

  @Mock
  private ProductFlow2AuditRepository productFlow2AuditRepository;

  @Mock
  private ProductGdnSkuGeneratorService productGdnSkuGeneratorService;

  @Mock
  private ProductNotificationService productNotificationService;

  @Mock
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Mock
  private CalendarService calendarService;

  @Mock
  private ProductStatusPublisherService productStatusPublisherService;

  @Mock
  private ProductItemSyncService syncStatusService;

  @Mock
  private ProductItemSyncProcessService syncProcessService;

  @Mock
  private SystemParameterService systemParameterService;

  @Captor
  private ArgumentCaptor<ProductLevel3SummaryFilter> filterCaptor;

  @Captor
  private ArgumentCaptor<ProductItemSyncProcess> syncProcessCaptor;

  @Captor
  private ArgumentCaptor<List<ProductLevel3Inventory>> argumentCaptorProductLevel3InventoryList;

  @InjectMocks
  private ProductBusinessPartnerServiceBean productBusinessPartnerServiceBean;

  @Mock
  private ProductLevel3AggregatorServiceOld productLevel3DirectAggregatorService;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private MapperUtil mapperUtil;

  @Mock
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Mock
  private ProductLevel3RetryService productLevel3RetryService;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private BundleRecipeService bundleRecipeService;

  @Captor
  private ArgumentCaptor<List<ProductItemWholesalePrice>> listArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductBusinessPartner> productBusinessPartnerArgumentCaptor;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel3InventoryService, this.calendarService);
    Mockito.verifyNoMoreInteractions(this.productStatusPublisherService);
    Mockito.verifyNoMoreInteractions(this.syncStatusService);
    Mockito.verifyNoMoreInteractions(this.syncProcessService, productItemWholesalePriceService, mapperUtil);
    Mockito.verifyNoMoreInteractions(productLevel3RetryService, bundleRecipeService);
  }

  private ProductBusinessPartner getProductBusinessPartner() {
    ProductBusinessPartner productBusinessPartner =
      new ProductBusinessPartner("business-partner-1", "product-1", "BP1-00001-00001",
        new ArrayList<ProductItemBusinessPartner>(),
        new ArrayList<ProductBusinessPartnerAttribute>(), DEFAULT_USERNAME, Calendar
        .getInstance().getTime(), STORE_ID);
    productBusinessPartner.getProductItemBusinessPartners()
      .add(new ProductItemBusinessPartner.Builder().productBusinessPartner(productBusinessPartner)
        .productItemId("product-item-1").productType(1)
        .gdnProductItemSku("BP1-00001-00001-00001").price(10000.0).salePrice(10000.0)
        .saleStartDate(null).saleEndDate(null).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").createdBy(DEFAULT_USERNAME)
        .createdDate(Calendar.getInstance().getTime()).storeId(STORE_ID).build());
    productBusinessPartner.getProductItemBusinessPartners()
      .add(new ProductItemBusinessPartner.Builder().productBusinessPartner(productBusinessPartner)
        .productItemId("product-item-2").productType(1)
        .gdnProductItemSku("BP1-00001-00001-00002").price(10000.0).salePrice(10000.0)
        .saleStartDate(null).saleEndDate(null).stock(10).minimumStock(0)
        .pickupPointId("pickup-point-1").createdBy(DEFAULT_USERNAME)
        .createdDate(Calendar.getInstance().getTime()).storeId(STORE_ID).build());
    ProductBusinessPartnerAttribute productBusinessPartnerAttribute = new ProductBusinessPartnerAttribute();
    productBusinessPartnerAttribute.setAttributeId(ATTRIBUTE_ID);
    productBusinessPartnerAttribute.setValue(VALUE);
    productBusinessPartner.getProductBusinessPartnerAttributes().add(productBusinessPartnerAttribute);
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
      .getProductItemBusinessPartners()) {
      productItemBusinessPartner.setId(UUID.randomUUID().toString());
      productItemBusinessPartner.setUpdatedBy(DEFAULT_USERNAME);
      productItemBusinessPartner.setUpdatedDate(Calendar.getInstance().getTime());
      productItemBusinessPartner.setDisplay(true);
      productItemBusinessPartner.setBuyable(true);
    }
    productBusinessPartner.setId(UUID.randomUUID().toString());
    productBusinessPartner.setUpdatedBy(DEFAULT_USERNAME);
    productBusinessPartner.setUpdatedDate(Calendar.getInstance().getTime());
    return productBusinessPartner;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    productData = new ProductDetailResponse(new ProductResponse());
    productData.setProductCode("MTA-" + UUID.randomUUID().toString());
    productData.setId(UUID.randomUUID().toString());
    productData.setStoreId(STORE_ID);
    productData.setCreatedBy(DEFAULT_USERNAME);
    productData.setCreatedDate(Calendar.getInstance().getTime());
    productData.setUpdatedBy(DEFAULT_USERNAME);
    productData.setUpdatedDate(Calendar.getInstance().getTime());
    productData.setActivated(true);
    productData.setViewable(true);
    productData.setShippingWeight(10D);
    productData.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
    productData.getProductCategoryResponses().add(
      new ProductCategoryResponse(new CategoryResponse(), STORE_ID));
    productData.getProductCategoryResponses().add(
      new ProductCategoryResponse(new CategoryResponse(), STORE_ID));
    productData.getProductCategoryResponses().get(0).setMarkForDelete(true);
    productData.setProductItemResponses(new HashSet<ProductItemResponse>());
    productData.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
    productData.setImages(new ArrayList<Image>());
    ProductItemResponse productItemData = new ProductItemResponse();
    productItemData.setId(UUID.randomUUID().toString());
    productItemData.setImages(new ArrayList<Image>());
    productItemData.getImages().add(new Image());
    productItemData.setSkuCode(productData.getProductCode() + UUID.randomUUID().toString());
    AttributeResponse attributeData = new AttributeResponse();
    attributeData.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    ProductAttributeResponse productAttributeData = new ProductAttributeResponse();
    productAttributeData.setAttribute(attributeData);
    productData.getProductItemResponses().add(productItemData);
    productData.getProductAttributeResponses().add(productAttributeData);
    productData.getImages().add(new Image());
    ProfileResponse businessPartnerData = getProfileResponse();
    ProductBusinessPartner productBusinessPartnerData = getProductBusinessPartner();
    productBusinessPartnerData.getProductItemBusinessPartners().get(0)
      .setProductItemId(productItemData.getId());
    productBusinessPartnerData.getProductItemBusinessPartners().remove(1);
    Page<ProductBusinessPartner> productBusinessPartnerPage =
      new PageImpl<ProductBusinessPartner>(new ArrayList<ProductBusinessPartner>());

    when(
      this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
        Mockito.anyString(), Mockito.anyString(), (Pageable) Mockito.any())).thenReturn(
      productBusinessPartnerPage);
    when(this.productRepository.findDetailById((String) Mockito.any())).thenReturn(productData);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
      .thenReturn(businessPartnerData);
    when(
      this.productBusinessPartnerRepository.saveAndFlush((ProductBusinessPartner) Mockito
        .any())).thenReturn(productBusinessPartnerData);
    when(this.productBusinessPartnerRepository.findById(Mockito.anyString())).thenReturn(
      Optional.of(productBusinessPartnerData));
    Mockito.when(this.productLevel3Service.create(Mockito.anyString(),
        (ProductDetailResponse) Mockito.any(), (ProductBusinessPartner) Mockito.any(),
        eq(false), Mockito.anyList())).thenReturn(true);
    when(
      this.productBusinessPartnerRepository.getMinimumStockByGdnProductItemSku(Mockito.anyString()))
      .thenReturn(1);
    when(this.productBusinessPartnerRepository.findById(Mockito.anyString())).thenReturn(
      Optional.of(productBusinessPartnerData));
    Mockito
      .doNothing()
      .when(this.productBusinessPartnerRepository)
      .updateMinimumStockByGdnProductItemSku(Mockito.anyString(), Mockito.anyInt());
    Mockito.doNothing().when(this.productGdnSkuGeneratorService)
      .generateGdnSkuOnProduct(Mockito.any(), eq(false));

    productAttributeList = new ArrayList<>();
    productAttribute = new ProductAttribute();
    productAttributeValue = new ProductAttributeValue();
    attribute = new Attribute();
    attribute.setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE);
    attribute.setId(ATTRIBUTE_ID);
    productAttribute.setAttribute(attribute);
    productAttributeValue.setDescriptiveAttributeValue(VALUE);
    productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
    productAttribute.setProductAttributeValues(Collections.singletonList(productAttributeValue));
    productAttribute.setAttribute(attribute);
    productAttributeList.add(productAttribute);
    rejectedSkuProductCollection =
      new RejectedSkuProductCollection(DEFAULT_PRODUCT_NAME, DEFAULT_CATEGORY_NAME, DEFAULT_BRAND_NAME, Calendar
        .getInstance().getTime(), DEFAULT_USERNAME, DEFAULT_REASON, Calendar.getInstance()
        .getTime(), DEFAULT_PRODUCT_CODE);
    rejectedSkuProductCollections.add(rejectedSkuProductCollection);
    page =
        new PageImpl<>(rejectedSkuProductCollections);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(rejectedSkuProductCollection, productBusinessPartner);
    productBusinessPartner.setProductId(PRODUCT_ID);
    productBusinessPartners = new PageImpl<>(Arrays.asList(productBusinessPartner));

    productSystemParameter = new ProductSystemParameter();
    productSystemParameter.setValue("1");
    productSkus.add(GDN_ITEM_SKU);
    productSkus.add(GDN_SKU);
    when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.REJECTED_API_SWITCH))
        .thenReturn(new ProductSystemParameter("rejectedApiSwitch", "false", "rejectedApiSwitch", false));

    productItemBusinessPartner1 = new ProductItemBusinessPartner();
    productItemBusinessPartner1.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner1.setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartner1.setDisplay(true);
    productItemBusinessPartner1.setBuyable(false);
    productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner2.setPickupPointId(PICKUP_POINT_CODE_1);
    productItemBusinessPartner3 = new ProductItemBusinessPartner();
    productItemBusinessPartner3.setGdnProductItemSku(ITEM_SKU_1);
    productItemBusinessPartner3.setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartner4 = new ProductItemBusinessPartner();
    productItemBusinessPartner4.setGdnProductItemSku(ITEM_SKU_1);
    productItemBusinessPartner4.setPickupPointId(PICKUP_POINT_CODE_1);
    productItemBusinessPartner5 = new ProductItemBusinessPartner();
    productItemBusinessPartner5.setGdnProductItemSku(ITEM_SKU_1);
    productItemBusinessPartner5.setBuyable(true);

    itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setDiscoverable(false);
    itemViewConfigDTO.setBuyable(true);

    priceDTO = new PriceDTO();
    priceDTO.setListPrice(LIST_PRICE);
    priceDTO.setOfferPrice(OFFER_PRICE);

    itemSummaryDetailResponse1 = new ItemSummaryDetailResponse();
    itemSummaryDetailResponse1.setItemSku(ITEM_SKU);
    itemSummaryDetailResponse1.setPickupPointCode(PICKUP_POINT_CODE);
    itemSummaryDetailResponse1.setProductType(com.gdn.x.product.enums.ProductType.REGULAR);
    itemSummaryDetailResponse1.setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfigDTO)));
    itemSummaryDetailResponse1.setPrice(new HashSet<>(Arrays.asList(priceDTO)));
    itemSummaryDetailResponse2 = new ItemSummaryDetailResponse();
    itemSummaryDetailResponse2.setItemSku(ITEM_SKU);
    itemSummaryDetailResponse2.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemSummaryDetailResponse2.setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfigDTO)));
    itemSummaryDetailResponse2.setProductType(com.gdn.x.product.enums.ProductType.REGULAR);
    itemSummaryDetailResponse2.setPrice(new HashSet<>(Arrays.asList(priceDTO)));
    itemSummaryDetailResponse3 = new ItemSummaryDetailResponse();
    itemSummaryDetailResponse3.setItemSku(ITEM_SKU);
    itemSummaryDetailResponse3.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemSummaryDetailResponse3.setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfigDTO)));
    itemSummaryDetailResponse3.setProductType(com.gdn.x.product.enums.ProductType.REGULAR);
    itemSummaryDetailResponse3.setPrice(new HashSet<>(Arrays.asList(priceDTO)));
    itemSummaryDetailResponse4 = new ItemSummaryDetailResponse();
    itemSummaryDetailResponse4.setItemSku(ITEM_SKU_1);
    itemSummaryDetailResponse4.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemSummaryDetailResponse4.setProductType(com.gdn.x.product.enums.ProductType.REGULAR);
    itemSummaryDetailResponse4.setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfigDTO)));
    itemSummaryDetailResponse4.setPrice(new HashSet<>(Arrays.asList(priceDTO)));

    ProductItemBusinessPartner productItemBusinessPartnerDeleted = new ProductItemBusinessPartner();
    productItemBusinessPartnerDeleted.setMarkForDelete(true);

    productItemBusinessPartnerList = Arrays.asList(productItemBusinessPartner1,
        productItemBusinessPartner2, productItemBusinessPartner3, productItemBusinessPartner4, productItemBusinessPartner5,
        productItemBusinessPartnerDeleted);
    itemSummaryDetailResponseList = Arrays.asList(itemSummaryDetailResponse1,
        itemSummaryDetailResponse2, itemSummaryDetailResponse3, itemSummaryDetailResponse4);
    itemPickupPointRequest1.setItemSku(ITEM_SKU);
    itemPickupPointRequest1.setFbbActive(true);
    itemPickupPointRequest2.setItemSku(ITEM_SKU_1);
    itemPickupPointRequest2.setFbbActive(false);
    productVariantPriceStockAndImagesRequest.setModifiedItemPickupPoints(Arrays.asList(itemPickupPointRequest1,
        itemPickupPointRequest2));
    productBusinessPartner.setId(DEFAULT_ID);
    ProductLevel3Attribute speccialProductLevel3Attribute = new ProductLevel3Attribute();
    speccialProductLevel3Attribute.setId(ATTRIBUTE_ID);
    speccialProductLevel3Attribute.setValues(List.of(VALUE));
    speccialProductLevel3Attribute.setSkuValue(true);
    productLevel3.getAttributes().add(speccialProductLevel3Attribute);
    ProductLevel3Attribute productLevel3Attribute = new ProductLevel3Attribute();
    productLevel3Attribute.setId(ATTRIBUTE_ID);
    productLevel3Attribute.setSkuValue(false);
    productLevel3Attribute.setValues(List.of(VALUE));
    productLevel3.getAttributes().add(productLevel3Attribute);

    ProductLevel3Attribute productLevel3Attribute2 = new ProductLevel3Attribute();
    productLevel3Attribute2.setId(ATTRIBUTE_ID);
    productLevel3Attribute2.setValues(List.of(VALUE));
    productLevel3.getAttributes().add(productLevel3Attribute2);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(false);
  }

  private ProfileResponse getProfileResponse() {
    ProfileResponse businessPartnerData = new ProfileResponse();
    businessPartnerData.setActivated(true);
    businessPartnerData.setMerchantStatus(Constants.ACTIVE);
    businessPartnerData.setCompany(new CompanyDTO());
    return businessPartnerData;
  }

  @Test
  public void testDelete() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.of(productBusinessPartner));
    Mockito.doNothing().when(this.productBusinessPartnerRepository).delete(productBusinessPartner);
    this.productBusinessPartnerServiceBean.delete(productBusinessPartner.getId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
      productBusinessPartner.getId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).delete(
      productBusinessPartner);
  }

  @Test
  public void testDeleteWithDatabaseException() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.of(productBusinessPartner));
    Mockito.doThrow(RuntimeException.class).when(this.productBusinessPartnerRepository)
      .delete(productBusinessPartner);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.delete(productBusinessPartner.getId());
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          productBusinessPartner.getId());
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).delete(
          productBusinessPartner);
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void testDeleteWithInvalidProductBusinessPartnerId() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    Mockito.doNothing().when(this.productBusinessPartnerRepository).delete(productBusinessPartner);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.delete(productBusinessPartner.getId());
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          productBusinessPartner.getId());
        Mockito.verify(this.productBusinessPartnerRepository, NEVER_CALLED).delete(
          productBusinessPartner);
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_NOT_FOUND, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void testFindByActivatedTrue() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setActivated(true);
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<ProductBusinessPartner>();
    productBusinessPartners.add(productBusinessPartner);
    Page<ProductBusinessPartner> page =
      new PageImpl<ProductBusinessPartner>(productBusinessPartners);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    when(
      this.productBusinessPartnerRepository.findByStoreIdAndActivatedTrueAndMarkForDeleteFalse(
        STORE_ID, pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean.findByActivatedTrue(STORE_ID, pageable);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .findByStoreIdAndActivatedTrueAndMarkForDeleteFalse(STORE_ID, pageable);
  }

  @Test
  public void testFindByBusinessPartnerId() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<ProductBusinessPartner>();
    productBusinessPartners.add(productBusinessPartner);
    Page<ProductBusinessPartner> page =
      new PageImpl<ProductBusinessPartner>(productBusinessPartners);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    when(
      this.productBusinessPartnerRepository
        .findByStoreIdAndBusinessPartnerIdAndMarkForDeleteFalseOrderByCreatedDateDesc(
          STORE_ID, "business-partner-1", pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean.findByBusinessPartnerId(STORE_ID,
      "business-partner-1", pageable);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .findByStoreIdAndBusinessPartnerIdAndMarkForDeleteFalseOrderByCreatedDateDesc(
        STORE_ID, "business-partner-1", pageable);
  }

  @Test
  public void findProductStateByStoreIdAndItemSkuTest() throws Exception {
    Mockito.when(productBusinessPartnerRepository.findProductStateByStoreIdAndItemSku(STORE_ID, ITEM_SKU))
        .thenReturn(Constants.ACTIVE);
    productBusinessPartnerServiceBean.findProductStateByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productBusinessPartnerRepository).findProductStateByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findProductStateByStoreIdAndItemSkuStoreIdEmptyTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productBusinessPartnerServiceBean.findProductStateByStoreIdAndItemSku(StringUtils.EMPTY, ITEM_SKU);
    });
  }

  @Test
  public void findProductStateByStoreIdAndItemSkuItemSkuEmptyTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productBusinessPartnerServiceBean.findProductStateByStoreIdAndItemSku(STORE_ID, StringUtils.EMPTY);
    });
  }

  @Test
  public void testValidateProductSku() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    when(productBusinessPartnerRepository.findFirstByGdnProductSku(eq(GDN_PRODUCT_SKU)))
      .thenReturn(productBusinessPartner);

    String result = productBusinessPartnerServiceBean.validateProductSku(GDN_PRODUCT_SKU);
    assertNotNull(result);

    verify(productBusinessPartnerRepository, times(1))
      .findFirstByGdnProductSku(eq(GDN_PRODUCT_SKU));
  }

  @Test
  public void testValidateProductSkuProductBusinessPartnerNull() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    when(productBusinessPartnerRepository.findFirstByGdnProductSku(eq(GDN_PRODUCT_SKU)))
      .thenReturn(null);
    try {
        Assertions.assertThrows(Exception.class, () -> {
        String result = productBusinessPartnerServiceBean.validateProductSku(GDN_PRODUCT_SKU);
      });
    } finally {
      verify(productBusinessPartnerRepository, times(1)).findFirstByGdnProductSku(eq(GDN_PRODUCT_SKU));
    }
  }

  @Test
  public void testFindById() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.of(productBusinessPartner));
    this.productBusinessPartnerServiceBean.findById(productBusinessPartner.getId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
      productBusinessPartner.getId());
  }

  @Test
  public void testFindByPickupPointId() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<ProductBusinessPartner>();
    productBusinessPartners.add(productBusinessPartner);
    Page<ProductBusinessPartner> page =
      new PageImpl<ProductBusinessPartner>(productBusinessPartners);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    when(
      this.productBusinessPartnerRepository.findByStoreIdAndPickupPointIdAndMarkForDeleteFalse(
        STORE_ID, "pickup-point-1", pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean.findByPickupPointId(STORE_ID, "pickup-point-1",
      pageable);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .findByStoreIdAndPickupPointIdAndMarkForDeleteFalse(STORE_ID, "pickup-point-1",
        pageable);
  }

  @Test
  public void testFindByStoreId() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    List<ProductBusinessPartner> productBusinessPartners = new ArrayList<ProductBusinessPartner>();
    productBusinessPartners.add(productBusinessPartner);
    Page<ProductBusinessPartner> page =
      new PageImpl<ProductBusinessPartner>(productBusinessPartners);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    when(
      this.productBusinessPartnerRepository.findByStoreIdAndMarkForDeleteFalse(STORE_ID,
        pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean.findByStoreId(STORE_ID, pageable);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .findByStoreIdAndMarkForDeleteFalse(STORE_ID, pageable);
  }

  @Test
  public void testRetrySave() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "mppForWhEnabled", false);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId("business-partner-1");
    productBusinessPartner.setProductId("BP1-00001-00001");
    productBusinessPartner.setStoreId(STORE_ID);
    ProductBusinessPartner savedProductBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().forEach(productItemBusinessPartner -> productItemBusinessPartner.setFbbActive(true));
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    savedProductBusinessPartner.getProductItemBusinessPartners().forEach(productItemBusinessPartner -> productItemBusinessPartner.setFbbActive(true));
    Catalog catalog =
      new Catalog("Master Katalog", null, CatalogType.MASTER_CATALOG, STORE_ID);
    ProductCategory productCategory =
      new ProductCategory(product, new Category(STORE_ID, "Category 1", 0),
        STORE_ID);
    productCategory.getCategory().setId(UUID.randomUUID().toString());
    productCategory.getCategory().setCatalog(catalog);
    product.getProductCategories().add(productCategory);
    product.setSpecificationDetail("Spesifikasi Detail");
    ProductItem productItem1 = new ProductItem();
    productItem1.setId("product-item-1");
    ProductItem productItem2 = new ProductItem();
    productItem2.setId("product-item-2");
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    product.setId(savedProductBusinessPartner.getProductId());
    product.setActivated(true);
    product.setViewable(true);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(savedProductBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);
    businessPartner.getCompany().setInternationalFlag(true);
    when(this.productBusinessPartnerRepository.findById(Mockito.any())).thenReturn(Optional.of(savedProductBusinessPartner));
    when(this.productRepository.findOne(savedProductBusinessPartner.getProductId()))
      .thenReturn(product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
        savedProductBusinessPartner.getBusinessPartnerId())).thenReturn(businessPartner);
    Mockito.doNothing().when(this.productLevel3Service).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    when(this.productBusinessPartnerRepository.save(savedProductBusinessPartner))
      .thenReturn(null);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    this.productBusinessPartnerServiceBean.retrySave(productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .findById(Mockito.any());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      savedProductBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      savedProductBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productLevel3Service, AT_LEAST_ONE).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productLevel3InventoryService, AT_LEAST_ONE)
      .insertInventory(argumentCaptorProductLevel3InventoryList.capture());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).save(
      savedProductBusinessPartner);
    Assertions.assertEquals(GDN_SKU,
       argumentCaptorProductLevel3InventoryList.getValue().get(0).getProductSku());
    Assertions.assertFalse(argumentCaptorProductLevel3InventoryList.getValue().get(0).isFbbPP());

  }

  @Test
  public void testRetrySaveWithMppForWHEnabled() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "mppForWhEnabled", true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId("business-partner-1");
    productBusinessPartner.setProductId("BP1-00001-00001");
    productBusinessPartner.setStoreId(STORE_ID);
    productBusinessPartner.getProductItemBusinessPartners().forEach(productItemBusinessPartner -> productItemBusinessPartner.setFbbActive(true));
    ProductBusinessPartner savedProductBusinessPartner = getProductBusinessPartner();
    savedProductBusinessPartner.getProductItemBusinessPartners().forEach(productItemBusinessPartner -> productItemBusinessPartner.setFbbActive(true));
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    Catalog catalog =
      new Catalog("Master Katalog", null, CatalogType.MASTER_CATALOG, STORE_ID);
    ProductCategory productCategory =
      new ProductCategory(product, new Category(STORE_ID, "Category 1", 0),
        STORE_ID);
    productCategory.getCategory().setId(UUID.randomUUID().toString());
    productCategory.getCategory().setCatalog(catalog);
    product.getProductCategories().add(productCategory);
    product.setSpecificationDetail("Spesifikasi Detail");
    ProductItem productItem1 = new ProductItem();
    productItem1.setId("product-item-1");
    ProductItem productItem2 = new ProductItem();
    productItem2.setId("product-item-2");
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    product.setId(savedProductBusinessPartner.getProductId());
    product.setActivated(true);
    product.setViewable(true);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(savedProductBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);
    businessPartner.getCompany().setInternationalFlag(true);
    when(this.productBusinessPartnerRepository.findById(Mockito.any())).thenReturn(Optional.of(savedProductBusinessPartner));
    when(this.productRepository.findOne(savedProductBusinessPartner.getProductId()))
      .thenReturn(product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
        savedProductBusinessPartner.getBusinessPartnerId())).thenReturn(businessPartner);
    Mockito.doNothing().when(this.productLevel3Service).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    when(this.productBusinessPartnerRepository.save(savedProductBusinessPartner))
      .thenReturn(null);
    this.productBusinessPartnerServiceBean.retrySave(productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .findById(Mockito.any());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      savedProductBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      savedProductBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productLevel3Service, AT_LEAST_ONE).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productLevel3InventoryService, AT_LEAST_ONE)
      .insertInventory(argumentCaptorProductLevel3InventoryList.capture());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).save(
      savedProductBusinessPartner);
    Assertions.assertEquals(GDN_SKU,
      argumentCaptorProductLevel3InventoryList.getValue().get(0).getProductSku());
    Assertions.assertTrue(argumentCaptorProductLevel3InventoryList.getValue().get(0).isFbbPP());
  }

  @Test
  public void testRetrySaveWithMppForWHEnabledNonFbb() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "mppForWhEnabled", true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId("business-partner-1");
    productBusinessPartner.setProductId("BP1-00001-00001");
    productBusinessPartner.setStoreId(STORE_ID);
    ProductBusinessPartner savedProductBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().forEach(productItemBusinessPartner -> productItemBusinessPartner.setFbbActive(false));
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    Catalog catalog =
      new Catalog("Master Katalog", null, CatalogType.MASTER_CATALOG, STORE_ID);
    ProductCategory productCategory =
      new ProductCategory(product, new Category(STORE_ID, "Category 1", 0),
        STORE_ID);
    productCategory.getCategory().setId(UUID.randomUUID().toString());
    productCategory.getCategory().setCatalog(catalog);
    product.getProductCategories().add(productCategory);
    product.setSpecificationDetail("Spesifikasi Detail");
    ProductItem productItem1 = new ProductItem();
    productItem1.setId("product-item-1");
    ProductItem productItem2 = new ProductItem();
    productItem2.setId("product-item-2");
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    product.setId(savedProductBusinessPartner.getProductId());
    product.setActivated(true);
    product.setViewable(true);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(savedProductBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);
    businessPartner.getCompany().setInternationalFlag(true);
    when(this.productBusinessPartnerRepository.findById(Mockito.any())).thenReturn(Optional.of(savedProductBusinessPartner));
    when(this.productRepository.findOne(savedProductBusinessPartner.getProductId()))
      .thenReturn(product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
        savedProductBusinessPartner.getBusinessPartnerId())).thenReturn(businessPartner);
    Mockito.doNothing().when(this.productLevel3Service).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    when(this.productBusinessPartnerRepository.save(savedProductBusinessPartner))
      .thenReturn(null);
    this.productBusinessPartnerServiceBean.retrySave(productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .findById(Mockito.any());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      savedProductBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      savedProductBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productLevel3Service, AT_LEAST_ONE).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productLevel3InventoryService, AT_LEAST_ONE)
      .insertInventory(argumentCaptorProductLevel3InventoryList.capture());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).save(
      savedProductBusinessPartner);
    Assertions.assertEquals(GDN_SKU,
      argumentCaptorProductLevel3InventoryList.getValue().get(0).getProductSku());
    Assertions.assertFalse(argumentCaptorProductLevel3InventoryList.getValue().get(0).isFbbPP());
  }

  @Test
  public void testRetrySave_ProductItemIdNotMatch() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId("business-partner-1");
    productBusinessPartner.setProductId("BP1-00001-00001");
    productBusinessPartner.setStoreId(STORE_ID);
    ProductBusinessPartner savedProductBusinessPartner = getProductBusinessPartner();

    ProductItem productItem1 = new ProductItem();
    productItem1.setId("not-match");

    Product product =
      new Product.Builder().viewable(true).build();
    product.getProductItems().add(productItem1);
    product.setShippingWeight(12d);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(savedProductBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);

    when(this.productBusinessPartnerRepository.findById(Mockito.any())).thenReturn(Optional.of(savedProductBusinessPartner));
    when(this.productRepository.findOne(savedProductBusinessPartner.getProductId()))
      .thenReturn(product);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
      savedProductBusinessPartner.getBusinessPartnerId())).thenReturn(businessPartner);
    Mockito.doNothing().when(this.productLevel3Service).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.retrySave(productBusinessPartner);
      });
    } catch(Exception e) {
      Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findById(Mockito.any());
      Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
        savedProductBusinessPartner.getProductId());
      Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(
        savedProductBusinessPartner.getBusinessPartnerId());
      throw e;
    }
  }

  @Test
  public void testRetrySave_ProductItemNull() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId("business-partner-1");
    productBusinessPartner.setProductId("BP1-00001-00001");
    productBusinessPartner.setStoreId(STORE_ID);
    ProductBusinessPartner savedProductBusinessPartner = getProductBusinessPartner();

    Product product =
      new Product.Builder().viewable(true).build();
    product.setShippingWeight(11d);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(savedProductBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER);

    when(this.productBusinessPartnerRepository.findById(Mockito.any())).thenReturn(Optional.of(savedProductBusinessPartner));
    when(this.productRepository.findOne(savedProductBusinessPartner.getProductId()))
      .thenReturn(product);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
      savedProductBusinessPartner.getBusinessPartnerId())).thenReturn(businessPartner);
    Mockito.doNothing().when(this.productLevel3Service).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.retrySave(productBusinessPartner);
      });
    } catch(Exception e) {
      Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findById(Mockito.any());
      Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
        savedProductBusinessPartner.getProductId());
      Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
        savedProductBusinessPartner.getBusinessPartnerId());
      Mockito.verify(this.productLevel3Service, AT_LEAST_ONE).create(Mockito.any(),
        (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
      throw e;
    }
  }

  @Test
  public void testRetrySaveWithExistingLevel2InventoryWithSWitchOff() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "mppForWhEnabled", false);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId("business-partner-1");
    productBusinessPartner.setProductId("BP1-00001-00001");
    productBusinessPartner.setStoreId(STORE_ID);
    ProductBusinessPartner savedProductBusinessPartner = getProductBusinessPartner();
    productItemBusinessPartner1.setFbbActive(true);
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    Catalog catalog =
      new Catalog("Master Katalog", null, CatalogType.MASTER_CATALOG, STORE_ID);
    ProductCategory productCategory =
      new ProductCategory(product, new Category(STORE_ID, "Category 1", 0),
        STORE_ID);
    productCategory.getCategory().setId(UUID.randomUUID().toString());
    productCategory.getCategory().setCatalog(catalog);
    product.getProductCategories().add(productCategory);
    product.setSpecificationDetail("Spesifikasi Detail");
    ProductItem productItem1 = new ProductItem();
    productItem1.setId("product-item-1");
    ProductItem productItem2 = new ProductItem();
    productItem2.setId("product-item-2");
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    product.setId(savedProductBusinessPartner.getProductId());
    product.setActivated(true);
    product.setViewable(true);

    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(savedProductBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_COMMISSION);
    when(this.productBusinessPartnerRepository.findById(Mockito.any())).thenReturn(Optional.of(savedProductBusinessPartner));
    when(this.productRepository.findOne(savedProductBusinessPartner.getProductId()))
      .thenReturn(product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
        savedProductBusinessPartner.getBusinessPartnerId())).thenReturn(businessPartner);
    Mockito.doNothing().when(this.productLevel3Service).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    when(this.productBusinessPartnerRepository.save(savedProductBusinessPartner))
      .thenReturn(null);
    this.productBusinessPartnerServiceBean.retrySave(productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .findById(Mockito.any());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      savedProductBusinessPartner.getProductId());
    Mockito.verify(this.productLevel3Service, AT_LEAST_ONE).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productLevel3InventoryService, AT_LEAST_ONE)
      .insertInventory(Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).save(
      savedProductBusinessPartner);
  }

  @Test
  public void testRetrySaveWithInvalidBusinessPartnerActivated() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "mppForWhEnabled", true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId("business-partner-1");
    productBusinessPartner.setProductId("BP1-00001-00001");
    productBusinessPartner.setStoreId(STORE_ID);
    ProductBusinessPartner savedProductBusinessPartner = getProductBusinessPartner();
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    ProductCategory productCategory =
      new ProductCategory(product, new Category(STORE_ID, "Category 1", 0),
        STORE_ID);
    product.getProductCategories().add(productCategory);
    ProductItem productItem1 = new ProductItem();
    productItem1.setId("product-item-1");
    ProductItem productItem2 = new ProductItem();
    productItem2.setId("product-item-2");
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    product.setId(savedProductBusinessPartner.getProductId());
    product.setActivated(true);
    product.setViewable(true);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setActivated(false);
    businessPartner.setMerchantStatus(Constants.NOT_APPLICABLE);
    businessPartner.setId(savedProductBusinessPartner.getBusinessPartnerId());
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_COMMISSION);
    when(this.productBusinessPartnerRepository.findById(Mockito.any())).thenReturn(
      Optional.of(savedProductBusinessPartner));
    when(this.productRepository.findOne(savedProductBusinessPartner.getProductId()))
      .thenReturn(product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
        savedProductBusinessPartner.getBusinessPartnerId())).thenReturn(businessPartner);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productBusinessPartnerServiceBean.retrySave(productBusinessPartner);
        Mockito.verify(this.productLevel3InventoryService).insertInventory(Mockito.any());
      });
    } catch(Exception e) {
      if (e instanceof ApplicationRuntimeException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          Mockito.any());
        Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
          savedProductBusinessPartner.getProductId());
        Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
          savedProductBusinessPartner.getBusinessPartnerId());
        Mockito.verify(this.productLevel3Service, NEVER_CALLED).create(Mockito.anyString(),
          (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository, NEVER_CALLED).save(
          savedProductBusinessPartner);
        ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
        Assertions.assertTrue(applicationException.getErrorMessage().contains(ErrorMessages.INACTIVE_BUSINESS_PARTNER_MSG));
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void testRetrySaveWithInvalidProductViewable() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId("business-partner-1");
    productBusinessPartner.setProductId("BP1-00001-00001");
    productBusinessPartner.setStoreId(STORE_ID);
    ProductBusinessPartner savedProductBusinessPartner = getProductBusinessPartner();
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    ProductCategory productCategory =
      new ProductCategory(product, new Category(STORE_ID, "Category 1", 0),
        STORE_ID);
    product.getProductCategories().add(productCategory);
    ProductItem productItem1 = new ProductItem();
    productItem1.setId("product-item-1");
    ProductItem productItem2 = new ProductItem();
    productItem2.setId("product-item-2");
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    product.setId(savedProductBusinessPartner.getProductId());
    product.setActivated(true);
    when(this.productBusinessPartnerRepository.findById(Mockito.any())).thenReturn(Optional.of(savedProductBusinessPartner));
    when(this.productRepository.findOne(savedProductBusinessPartner.getProductId()))
      .thenReturn(product);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.retrySave(productBusinessPartner);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          Mockito.any());
        Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
          savedProductBusinessPartner.getProductId());
        Mockito.verify(this.productLevel3Service, NEVER_CALLED).create(Mockito.any(),
          (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository, NEVER_CALLED).save(
          savedProductBusinessPartner);
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void testRetrySaveWithNotExistProductBusinessPartner() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId("business-partner-1");
    productBusinessPartner.setProductId("BP1-00001-00001");
    productBusinessPartner.setStoreId(STORE_ID);
    ProductBusinessPartner savedProductBusinessPartner = getProductBusinessPartner();
    when(this.productBusinessPartnerRepository.findById(Mockito.anyString())).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.retrySave(productBusinessPartner);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          Mockito.any());
        Mockito.verify(this.productRepository, NEVER_CALLED).findOne(
          savedProductBusinessPartner.getProductId());
        Mockito.verify(this.productLevel3Service, NEVER_CALLED).create(Mockito.any(),
          (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository, NEVER_CALLED).save(
          savedProductBusinessPartner);
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void testSave() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "mppForWhEnabled", true);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setActivated(true);
    productBusinessPartner.getProductItemBusinessPartners().forEach(productItemBusinessPartner -> productItemBusinessPartner.setFbbActive(true));
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();

    Catalog catalog =
      new Catalog("Master Katalog", null, CatalogType.MASTER_CATALOG, STORE_ID);
    ProductCategory productCategory =
      new ProductCategory(product, new Category(STORE_ID, "Category 1", 0),
        STORE_ID);
    productCategory.getCategory().setId(UUID.randomUUID().toString());
    productCategory.getCategory().setCatalog(catalog);
    product.getProductCategories().add(productCategory);
    product.setSpecificationDetail("Spesifikasi Detail");
    ProductItem productItem1 = new ProductItem();
    productItem1.setId("product-item-1");
    ProductItem productItem2 = new ProductItem();
    productItem2.setId("product-item-2");
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    product.setId(productBusinessPartner.getProductId());
    product.setActivated(true);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.getCompany().setInternationalFlag(true);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_COMMISSION);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    Mockito.doNothing().when(this.productLevel3Service).create(Mockito.anyString(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    this.productBusinessPartnerServiceBean.save(productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
      productBusinessPartner.getId());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      productBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productLevel3Service, AT_LEAST_ONE).create((String) Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productLevel3InventoryService)
      .insertInventory(argumentCaptorProductLevel3InventoryList.capture());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).saveAndFlush(
      productBusinessPartner);
    Assertions.assertEquals(GDN_SKU,
       argumentCaptorProductLevel3InventoryList.getValue().get(0).getProductSku());
  }

  @Test
  public void testSave_1() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setActivated(true);
    productBusinessPartner.setId(null);
    productBusinessPartner.getProductItemBusinessPartners().get(0)
      .setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    Catalog catalog =
      new Catalog("Master Katalog", null, CatalogType.MASTER_CATALOG, STORE_ID);
    ProductCategory productCategory =
      new ProductCategory(product, new Category(STORE_ID, "Category 1", 0),
        STORE_ID);
    productCategory.getCategory().setId(UUID.randomUUID().toString());
    productCategory.getCategory().setCatalog(catalog);
    product.getProductCategories().add(productCategory);
    product.setSpecificationDetail("Spesifikasi Detail");
    ProductItem productItem1 = new ProductItem();
    productItem1.setId("product-item-1");
    ProductItem productItem2 = new ProductItem();
    productItem2.setId("product-item-2");
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    product.setId(productBusinessPartner.getProductId());
    product.setActivated(true);
    businessPartner = new Profile();
    businessPartner.setBusinessPartnerCode("BP1-00001");
    businessPartner.setBusinessPartnerType("1");
    businessPartner.setCompany(new Company());
    businessPartner.setLegalityOfCompany(new LegalityOfCompany());
    businessPartner.setResponsiblePerson(new ResponsiblePerson());
    businessPartner.setPickupPoints(new ArrayList<>());
    businessPartner.setBankAccounts(new ArrayList<>());
    businessPartner.setPaymentAccounts(new ArrayList<>());
    businessPartner.setCategories(new ArrayList<>());
    businessPartner.setTaxation(new Taxation());
    businessPartner.setReasonStatus("reason-status");
    businessPartner.setReactivateCount(0);
    businessPartner.setAcceptedDatePKS(new Date());
    businessPartner.setRelationChanged(Boolean.FALSE);
    businessPartner.setCreatedBy(DEFAULT_USERNAME);
    businessPartner.setCreatedDate(Calendar.getInstance().getTime());
    businessPartner.setStoreId(STORE_ID);
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_COMMISSION);
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    Mockito.doThrow(Exception.class).when(this.productLevel3Service).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    this.productBusinessPartnerServiceBean.save(productBusinessPartner);
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      productBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productLevel3Service, AT_LEAST_ONE).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).saveAndFlush(
      productBusinessPartner);
  }

  @Test
  public void testSave_2() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "mppForWhEnabled", true);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setActivated(true);
    productBusinessPartner.getProductItemBusinessPartners().forEach(productItemBusinessPartner -> productItemBusinessPartner.setFbbActive(false));
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    Catalog catalog =
      new Catalog("Master Katalog", null, CatalogType.MASTER_CATALOG, STORE_ID);
    ProductCategory productCategory =
      new ProductCategory(product, new Category(STORE_ID, "Category 1", 0),
        STORE_ID);
    productCategory.getCategory().setId(UUID.randomUUID().toString());
    productCategory.getCategory().setCatalog(catalog);
    product.getProductCategories().add(productCategory);
    product.setSpecificationDetail("Spesifikasi Detail");
    ProductItem productItem1 = new ProductItem();
    productItem1.setId("product-item-1");
    ProductItem productItem2 = new ProductItem();
    productItem2.setId("product-item-2");
    product.getProductItems().add(productItem1);
    product.getProductItems().add(productItem2);
    product.setId(productBusinessPartner.getProductId());
    product.setActivated(true);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BUSINESS_PARTNER);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_COMMISSION);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    Mockito.doNothing().when(this.productLevel3Service).create(Mockito.anyString(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    this.productBusinessPartnerServiceBean.save(productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
      productBusinessPartner.getId());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      productBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productLevel3Service, AT_LEAST_ONE).create(Mockito.any(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productLevel3InventoryService)
      .insertInventory(Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).saveAndFlush(
      productBusinessPartner);
  }

  @Test
  public void testSaveWithActivatedFalse() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "mppForWhEnabled", false);
    productBusinessPartner.getProductItemBusinessPartners().forEach(productItemBusinessPartner -> productItemBusinessPartner.setFbbActive(true));
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategory> productCategories = getProductCategories();
    product.setProductCategories(productCategories);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    when(this.calendarService
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
        any(Date.class))).thenReturn(new Date());
    this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
      productBusinessPartner.getId());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      productBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).saveAndFlush(
      productBusinessPartner);
    Mockito.verify(this.productGdnSkuGeneratorService).generateGdnSkuOnProduct(
      Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(this.calendarService)
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
        any());
  }

  @Test
  public void testSaveWithActivatedFalseMerchantStatusNotActiveExceptionTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    Product product =
        new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
            .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
            .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
            .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategory> productCategories = getProductCategories();
    product.setProductCategories(productCategories);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    businessPartner.setMerchantStatus(null);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
        .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
        product);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
        .thenReturn(businessPartner);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
      });
    } finally {
      Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(productBusinessPartner.getId());
      Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE)
          .filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
      Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
          productBusinessPartner.getProductId());
    }
  }

  @Test
  public void testSaveWithActivatedFalseMerchantStatusNullExceptionTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    Product product =
        new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
            .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
            .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
            .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategory> productCategories = getProductCategories();
    product.setProductCategories(productCategories);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    businessPartner.setMerchantStatus(null);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
        .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
        product);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
      });
    } finally {
      Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE)
          .filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
      Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
          productBusinessPartner.getProductId());
    }
  }

  @Test
  public void testSaveWithActivatedFalseNonBopisExceptionProduct() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setProductType(ProductType.REGULAR.getProductType());
    Product product =
        new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
            .width(1.0).height(1.0).weight(1.0).shippingWeight(0.0)
            .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
            .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategory> productCategories = getProductCategories();
    product.setProductCategories(productCategories);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
        .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
        product);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
        .thenReturn(businessPartner);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
      });
    } finally {
      Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(productBusinessPartner.getId());
      Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(productBusinessPartner.getProductId());
      Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE)
          .filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    }
  }

  @Test
  public void testSaveWithActivatedBopisProductFalse() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setProductType(ProductType.BOPIS.getProductType());
    Product product =
        new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
            .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
            .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
            .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategory> productCategories = getProductCategories();
    product.setProductCategories(productCategories);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
        .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
        product);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
        .thenReturn(businessPartner);
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
        .thenReturn(productBusinessPartner);
    when(this.calendarService
        .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
            any(Date.class))).thenReturn(new Date());
    this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
        productBusinessPartner.getId());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
        productBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
        productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).saveAndFlush(
        productBusinessPartner);
    Mockito.verify(this.productGdnSkuGeneratorService).generateGdnSkuOnProduct(
        Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(this.calendarService)
        .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
            any());
  }

  @Test
  public void testSaveForInvalidShipping_bopisProductFalse() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setProductType(ProductType.BOPIS.getProductType());
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(null)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategory> productCategories = getProductCategories();
    product.setProductCategories(productCategories);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    when(this.calendarService
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
        any(Date.class))).thenReturn(new Date());
    Assertions.assertThrows(ApplicationException.class, () -> {
      this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
    });
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
      productBusinessPartner.getId());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      productBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      productBusinessPartner.getBusinessPartnerId());
  }

  @Test
  public void testSaveForNegetiveShipping_bopisProductFalse() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setProductType(ProductType.BOPIS.getProductType());
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(-1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategory> productCategories = getProductCategories();
    product.setProductCategories(productCategories);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    when(this.calendarService
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
        any(Date.class))).thenReturn(new Date());
    Assertions.assertThrows(ApplicationException.class, () -> {
      this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
    });
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
      productBusinessPartner.getId());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      productBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      productBusinessPartner.getBusinessPartnerId());
  }


  @Test
  public void testSaveWithActivatedRegularProductTDMerchantFalse() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setProductType(ProductType.REGULAR.getProductType());
    Product product =
        new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
            .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
            .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
            .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategory> productCategories = getProductCategories();
    product.setProductCategories(productCategories);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    businessPartner.getCompany().setMerchantType(Constants.TD_MERCHANT);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
        .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
        product);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
        .thenReturn(businessPartner);
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
        .thenReturn(productBusinessPartner);
    when(this.calendarService
        .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
            any(Date.class))).thenReturn(new Date());
    this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
        productBusinessPartner.getId());
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
        productBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
        productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).saveAndFlush(
        productBusinessPartner);
    Mockito.verify(this.productGdnSkuGeneratorService).generateGdnSkuOnProduct(
        Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(this.calendarService)
        .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
            any());
  }

  private List<ProductCategory> getProductCategories() {
    ProductCategory productCategory = new ProductCategory();
    Category category = new Category();
    category.setId(DEFAULT_ID);
    category.setCategoryCode(DEFAULT_CATEGORY_CODE);
    category.setInternalActivationInterval(24);
    productCategory.setId(DEFAULT_ID);
    productCategory.setCategory(category);
    return Arrays.asList(productCategory);
  }

  private List<ProductCategoryResponse> getProductCategoryResponse() {
    ProductCategoryResponse productCategory = new ProductCategoryResponse();
    CategoryResponse category = new CategoryResponse();
    category.setId(DEFAULT_ID);
    category.setCategoryCode(DEFAULT_CATEGORY_CODE);
    category.setInternalActivationInterval(24);
    productCategory.setId(DEFAULT_ID);
    productCategory.setCategory(category);
    return Arrays.asList(productCategory);
  }

  @Test
  public void testSaveWithActivatedFalse_1() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setId(null);
    productBusinessPartner.getProductItemBusinessPartners().get(0)
      .setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    List<ProductCategory> productCategories = getProductCategories();
    product.setProductCategories(productCategories);
    when(this.calendarService
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
        any(Date.class))).thenReturn(new Date());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      productBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).saveAndFlush(
      productBusinessPartner);
    Mockito.verify(this.productGdnSkuGeneratorService).generateGdnSkuOnProduct(
      Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(this.calendarService)
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
        any());
  }

  @Test
  public void testSaveWithActivatedFalse_withEmptyCategory() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setId(null);
    productBusinessPartner.getProductItemBusinessPartners().get(0)
      .setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
    Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
      productBusinessPartner.getProductId());
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
      productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).saveAndFlush(
      productBusinessPartner);
    Mockito.verify(this.productGdnSkuGeneratorService).generateGdnSkuOnProduct(
      Mockito.any(ProductBusinessPartner.class), eq(false));
  }

  @Test
  public void testSaveWithActivatedFalseAndExistingProductBusinessPartner() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.of(productBusinessPartner));
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          productBusinessPartner.getId());
        Mockito.verify(this.productRepository, NEVER_CALLED).findOne(
          productBusinessPartner.getProductId());
        Mockito.verify(this.productBusinessPartnerRepository, NEVER_CALLED).saveAndFlush(
          productBusinessPartner);
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void testSaveWithActivatedFalseAndInvalidBusinessPartnerActivated() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(false);
    businessPartner.setMerchantStatus(Constants.NOT_APPLICABLE);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationRuntimeException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          productBusinessPartner.getId());
        Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
          productBusinessPartner.getProductId());
        Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
          productBusinessPartner.getBusinessPartnerId());
        Mockito.verify(this.productBusinessPartnerRepository, NEVER_CALLED).saveAndFlush(
          productBusinessPartner);
        ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
        Assertions.assertTrue(applicationException.getErrorMessage().contains(ErrorMessages.INACTIVE_BUSINESS_PARTNER_MSG));
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }



  @Test
  public void testSaveWithExistingProductBusinessPartner() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setActivated(true);
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    product.setActivated(true);
    businessPartner.setCompany(new Company());
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_COMMISSION);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.of(productBusinessPartner));
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    Mockito.doNothing().when(this.productLevel3Service).create(Mockito.anyString(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.save(productBusinessPartner);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          productBusinessPartner.getId());
        Mockito.verify(this.productRepository, NEVER_CALLED).findOne(
          productBusinessPartner.getProductId());
        Mockito.verify(this.productLevel3Service, NEVER_CALLED).create(Mockito.anyString(),
          (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository, NEVER_CALLED).saveAndFlush(
          productBusinessPartner);
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void testSaveWithInvalidBusinessPartnerActivated() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setActivated(true);
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    product.setActivated(true);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.getCompany()
      .setInventoryFulfillment(GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI);
    businessPartner.setActivated(false);
    businessPartner.setMerchantStatus(Constants.NOT_APPLICABLE);
    businessPartner.getCompany().setPurchaseTerm(GdnBaseLookup.PURCHASE_TERM_COMMISSION);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    Mockito.doNothing().when(this.productLevel3Service).create(Mockito.anyString(),
      (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productBusinessPartnerServiceBean.save(productBusinessPartner);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationRuntimeException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          productBusinessPartner.getId());
        Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
          productBusinessPartner.getProductId());
        Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE).filterDetailByBusinessPartnerCode(
          productBusinessPartner.getBusinessPartnerId());
        Mockito.verify(this.productLevel3Service, NEVER_CALLED).create(Mockito.anyString(),
          (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository, NEVER_CALLED).saveAndFlush(
          productBusinessPartner);
        ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
        Assertions.assertTrue(applicationException.getErrorMessage().contains(ErrorMessages.INACTIVE_BUSINESS_PARTNER_MSG));
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void testSaveWithInvalidProductActivated() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setActivated(true);
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.save(productBusinessPartner);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          productBusinessPartner.getId());
        Mockito.verify(this.productRepository, AT_LEAST_ONE).findOne(
          productBusinessPartner.getProductId());
        Mockito.verify(this.productLevel3Service, NEVER_CALLED).create(Mockito.anyString(),
          (Product) Mockito.any(), (ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository, NEVER_CALLED).saveAndFlush(
          productBusinessPartner);
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }



  @Test
  public void testUpdate() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.of(productBusinessPartner));
    when(this.productBusinessPartnerRepository.save(productBusinessPartner)).thenReturn(
      productBusinessPartner);
    this.productBusinessPartnerServiceBean.update(productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
      productBusinessPartner.getId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .save(productBusinessPartner);
  }

  @Test
  public void testUpdateWithInvalidProductBusinessPartnerId() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    when(this.productBusinessPartnerRepository.save(productBusinessPartner)).thenReturn(
      productBusinessPartner);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.update(productBusinessPartner);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE).findById(
          productBusinessPartner.getId());
        Mockito.verify(this.productBusinessPartnerRepository, NEVER_CALLED).save(
          productBusinessPartner);
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_NOT_FOUND, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void findByStoreIdAndProductIdAndMarkForDeleteFalseTest() throws Exception {
    this.productBusinessPartnerServiceBean.findByStoreIdAndProductIdAndMarkForDeleteFalse(
      STORE_ID, UUID.randomUUID().toString());
    Mockito.verify(this.productBusinessPartnerRepository)
      .findByStoreIdAndProductIdAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
        (Pageable) Mockito.any());
  }

  @Test
  public void findByStoreIdAndProductIdTest() throws Exception {
    when(this.productBusinessPartnerRepository.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID)).
        thenReturn(Arrays.asList(getProductBusinessPartner()));
    this.productBusinessPartnerServiceBean.findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(this.productBusinessPartnerRepository).findByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void findByStoreIdAndProductId_emptyStoreIdTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.productBusinessPartnerServiceBean.findByStoreIdAndProductId(StringUtils.EMPTY, PRODUCT_ID);
    });
  }

  @Test
  public void findByStoreIdAndProductId_emptyProductIdTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.productBusinessPartnerServiceBean.findByStoreIdAndProductId(STORE_ID, StringUtils.EMPTY);
    });
  }

  @Test
  public void findInactiveProductBusinessPartnersOfActiveL1Test() throws Exception {
    when(this.productBusinessPartnerRepository.findByStoreIdAndProductIdAndStateAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_ID, IN_PROGRESS_STATE)).thenReturn(Arrays.asList(getProductBusinessPartner()));
    this.productBusinessPartnerServiceBean.findInactiveProductBusinessPartnersOfActiveL1(STORE_ID, PRODUCT_ID);
    Mockito.verify(this.productBusinessPartnerRepository)
        .findByStoreIdAndProductIdAndStateAndMarkForDeleteFalse(STORE_ID, PRODUCT_ID, IN_PROGRESS_STATE);
  }

  @Test
  public void findInactiveProductBusinessPartnersOfActiveL1_emptyStoreIdTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.productBusinessPartnerServiceBean.findInactiveProductBusinessPartnersOfActiveL1(StringUtils.EMPTY,
          PRODUCT_ID);
    });
  }

  @Test
  public void findInactiveProductBusinessPartnersOfActiveL1_emptyProductIdTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.productBusinessPartnerServiceBean.findInactiveProductBusinessPartnersOfActiveL1(STORE_ID, StringUtils.EMPTY);
    });
  }

  @Test
  public void createTest() throws Exception {
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner
      .setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any())).thenReturn(
      businessPartnerData);
    this.productBusinessPartnerServiceBean.create(
      ProductBusinessPartnerServiceBeanTest.STORE_ID, productBusinessPartner, false, new ArrayList());
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.businessPartnerRepository, ProductBusinessPartnerServiceBeanTest.AT_LEAST_ONE)
      .filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).saveAndFlush(
      (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.any());

    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(),
        (ProductDetailResponse) Mockito.any(), (ProductBusinessPartner) Mockito.any(),
        eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save(
      (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productGdnSkuGeneratorService).generateGdnSkuOnProduct(
      Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(productLevel3RetryService)
      .updateCompletedOrOmittedState(STORE_ID, GDN_SKU, ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void createInstoreNewTest() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "instoreNewFlowEnabled", true);
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    productBusinessPartner.setOff2OnChannelActive(false);
    productBusinessPartner.setB2cActivated(false);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(businessPartnerData);
    this.productBusinessPartnerServiceBean.create(ProductBusinessPartnerServiceBeanTest.STORE_ID,
        productBusinessPartner, false, new ArrayList());
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.businessPartnerRepository, ProductBusinessPartnerServiceBeanTest.AT_LEAST_ONE)
        .filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).saveAndFlush((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.any());
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(),
        (ProductBusinessPartner) Mockito.any(), eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productGdnSkuGeneratorService)
        .generateGdnSkuOnProduct(Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(productLevel3RetryService)
        .updateCompletedOrOmittedState(STORE_ID, GDN_SKU, ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void createInstoreNewTestOff2OnActive() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "instoreNewFlowEnabled", true);
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    productBusinessPartner.setOff2OnChannelActive(true);
    productBusinessPartner.setB2cActivated(false);
    productData.setShippingWeight(null);
    when(this.productRepository.findDetailById(Mockito.any())).thenReturn(productData);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(businessPartnerData);
    this.productBusinessPartnerServiceBean.create(ProductBusinessPartnerServiceBeanTest.STORE_ID,
        productBusinessPartner, false, new ArrayList());
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.businessPartnerRepository, ProductBusinessPartnerServiceBeanTest.AT_LEAST_ONE)
        .filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).saveAndFlush((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.any());
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(),
        (ProductBusinessPartner) Mockito.any(), eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productGdnSkuGeneratorService)
        .generateGdnSkuOnProduct(Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(productLevel3RetryService)
        .updateCompletedOrOmittedState(STORE_ID, GDN_SKU, ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void createInstoreNewTestB2cActivated() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "instoreNewFlowEnabled", true);
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    productBusinessPartner.setOff2OnChannelActive(true);
    productBusinessPartner.setB2cActivated(true);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(businessPartnerData);
    this.productBusinessPartnerServiceBean.create(ProductBusinessPartnerServiceBeanTest.STORE_ID,
        productBusinessPartner, false, new ArrayList());
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.businessPartnerRepository, ProductBusinessPartnerServiceBeanTest.AT_LEAST_ONE)
        .filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).saveAndFlush((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.any());
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(),
        (ProductBusinessPartner) Mockito.any(), eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productGdnSkuGeneratorService)
        .generateGdnSkuOnProduct(Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(productLevel3RetryService)
        .updateCompletedOrOmittedState(STORE_ID, GDN_SKU, ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void createInstoreNewTestWithShippingWeight() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "instoreNewFlowEnabled", true);
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    productBusinessPartner.setOff2OnChannelActive(true);
    productBusinessPartner.setB2cActivated(false);
    productData.setShippingWeight(2.03);
    when(this.productRepository.findDetailById(Mockito.any())).thenReturn(productData);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(businessPartnerData);
    this.productBusinessPartnerServiceBean.create(ProductBusinessPartnerServiceBeanTest.STORE_ID,
        productBusinessPartner, false, new ArrayList());
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.businessPartnerRepository, ProductBusinessPartnerServiceBeanTest.AT_LEAST_ONE)
        .filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).saveAndFlush((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.any());
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(),
        (ProductBusinessPartner) Mockito.any(), eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productGdnSkuGeneratorService)
        .generateGdnSkuOnProduct(Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(productLevel3RetryService)
        .updateCompletedOrOmittedState(STORE_ID, GDN_SKU, ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void createInstoreNewTestWithNoShippingWeight() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "instoreNewFlowEnabled", true);
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    productBusinessPartner.setOff2OnChannelActive(true);
    productBusinessPartner.setB2cActivated(false);
    productData.setShippingWeight(0.0);
    when(this.productRepository.findDetailById(Mockito.any())).thenReturn(productData);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(businessPartnerData);
    this.productBusinessPartnerServiceBean.create(ProductBusinessPartnerServiceBeanTest.STORE_ID,
        productBusinessPartner, false, new ArrayList());
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.businessPartnerRepository, ProductBusinessPartnerServiceBeanTest.AT_LEAST_ONE)
        .filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).saveAndFlush((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.any());
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(),
        (ProductBusinessPartner) Mockito.any(), eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productGdnSkuGeneratorService)
        .generateGdnSkuOnProduct(Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(productLevel3RetryService)
        .updateCompletedOrOmittedState(STORE_ID, GDN_SKU, ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void createTestWithWholesalePriceTest() throws Exception {
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(GDN_ITEM_ID);
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setWholesaleDiscount(10);
    productItemWholesalePriceRequest.setQuantity(2);
    ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest = new ProductItemBusinessPartnerRequest();
    productItemBusinessPartnerRequest.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartnerRequest.setProductItemId(GDN_ITEM_ID);
    productItemBusinessPartnerRequest
        .setProductItemWholesalePriceRequests(Arrays.asList(productItemWholesalePriceRequest));
    productItemBusinessPartnerRequest.setProductItemBusinessPartnerLogisticsRequests(
        Arrays.asList(new ProductItemBusinessPartnerLogisticsRequest()));
    Mockito.when(mapperUtil.mapRequestToString(Mockito.any())).thenReturn(WHOLESALE_RULE_1);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(businessPartnerData);
    this.productBusinessPartnerServiceBean
        .create(ProductBusinessPartnerServiceBeanTest.STORE_ID, productBusinessPartner, false,
            Arrays.asList(productItemBusinessPartnerRequest));
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.businessPartnerRepository, ProductBusinessPartnerServiceBeanTest.AT_LEAST_ONE)
        .filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).saveAndFlush( Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.any());
    Mockito.verify(this.productLevel3Service).create(Mockito.any(),
        (ProductDetailResponse) Mockito.any(), Mockito.any(), eq(false),
        Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productGdnSkuGeneratorService)
        .generateGdnSkuOnProduct(Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(mapperUtil).mapRequestToString(Mockito.any());
    Mockito.verify(productItemWholesalePriceService).saveWholesalePrice(listArgumentCaptor.capture());
    Mockito.verify(productLevel3RetryService)
      .updateCompletedOrOmittedState(STORE_ID, GDN_SKU, ProductLevel3RetryStatus.COMPLETED.name());
    Assertions.assertEquals(1, listArgumentCaptor.getValue().size());
  }

  @Test
  public void createTestWithWholesalePriceNotNullWholesalePriceTest() throws Exception {
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    productItemBusinessPartner.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartner.setProductItemId(GDN_ITEM_ID);
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setWholesaleDiscount(10);
    productItemWholesalePriceRequest.setQuantity(2);
    ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest = new ProductItemBusinessPartnerRequest();
    productItemBusinessPartnerRequest.setGdnProductItemSku(ITEM_SKU);
    productItemBusinessPartnerRequest.setProductItemId(GDN_ITEM_ID);
    productItemBusinessPartnerRequest.setWholesalePriceActivated(true);
    productItemBusinessPartnerRequest
        .setProductItemWholesalePriceRequests(Arrays.asList(productItemWholesalePriceRequest));
    Mockito.when(mapperUtil.mapRequestToString(Mockito.any())).thenReturn(WHOLESALE_RULE_1);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any()))
        .thenReturn(businessPartnerData);
    this.productBusinessPartnerServiceBean
        .create(ProductBusinessPartnerServiceBeanTest.STORE_ID, productBusinessPartner, false,
            Arrays.asList(productItemBusinessPartnerRequest));
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.businessPartnerRepository, ProductBusinessPartnerServiceBeanTest.AT_LEAST_ONE)
        .filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).saveAndFlush( Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.any());
    Mockito.verify(this.productLevel3Service).create(Mockito.any(),
        (ProductDetailResponse) Mockito.any(), Mockito.any(), eq(false),
        Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productGdnSkuGeneratorService)
        .generateGdnSkuOnProduct(Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(mapperUtil).mapRequestToString(Mockito.any());
    Mockito.verify(productItemWholesalePriceService).saveWholesalePrice(listArgumentCaptor.capture());
    Mockito.verify(productLevel3RetryService).updateCompletedOrOmittedState(STORE_ID, GDN_SKU,
      ProductLevel3RetryStatus.COMPLETED.name());
    Assertions.assertEquals(1, listArgumentCaptor.getValue().size());
  }

  @Test
  public void createTest_withSkipNotification() throws Exception {
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductItemBusinessPartners(new ArrayList<>());
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_BIG_PRODUCT);
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.any())).thenReturn(
      businessPartnerData);
    this.productBusinessPartnerServiceBean.create(
      ProductBusinessPartnerServiceBeanTest.STORE_ID, productBusinessPartner, true, new ArrayList());
    Mockito.verify(this.productRepository).findDetailById(Mockito.any());
    Mockito.verify(this.businessPartnerRepository, ProductBusinessPartnerServiceBeanTest.AT_LEAST_ONE)
      .filterDetailByBusinessPartnerCode(Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).saveAndFlush(
      (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.any());
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(), (ProductBusinessPartner) Mockito.any(),
        eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save(
      (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productGdnSkuGeneratorService).generateGdnSkuOnProduct(
      Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(productLevel3RetryService)
      .updateCompletedOrOmittedState(STORE_ID, GDN_SKU, ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void createWithExistProductBusinessPartnerIdTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setId(UUID.randomUUID().toString());
    productBusinessPartner
      .setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = getProductItemBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    ProductBusinessPartner productBusinessPartner2 = new ProductBusinessPartner();
    when(
      this.productBusinessPartnerRepository.findById(eq(productBusinessPartner.getId())))
      .thenReturn(Optional.empty());
    when(
      this.productBusinessPartnerRepository.findById(eq(productBusinessPartner2.getId())))
      .thenReturn(Optional.of(productBusinessPartner));
    when(
      this.productBusinessPartnerRepository.saveAndFlush((ProductBusinessPartner) Mockito
        .any())).thenReturn(productBusinessPartner2);
    Mockito.doThrow(Exception.class).when(this.productLevel3Service).create(Mockito.any(),
        (ProductDetailResponse) Mockito.any(), (ProductBusinessPartner) Mockito.any(),
        eq(false), any());
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean
            .create(ProductBusinessPartnerServiceBeanTest.STORE_ID, productBusinessPartner, false, new ArrayList());
      });
    } finally {
        Mockito.verify(this.productRepository).findDetailById(Mockito.any());
        Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository)
          .saveAndFlush((ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository, ProductBusinessPartnerServiceBeanTest.CALLED_TWICE)
          .findById(Mockito.any());
        Mockito.verify(this.productLevel3Service).create(Mockito.any(),
            (ProductDetailResponse) Mockito.any(),
            (ProductBusinessPartner) Mockito.any(), eq(false),
            Mockito.anyList());
        Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productGdnSkuGeneratorService, ProductBusinessPartnerServiceBeanTest.CALLED_TWICE)
          .generateGdnSkuOnProduct(Mockito.any(ProductBusinessPartner.class), eq(false));
    }
  }

  @Test
  public void createWithExistProductBusinessPartnerTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setId(UUID.randomUUID().toString());
    productBusinessPartner
      .setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = getProductItemBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.create(ProductBusinessPartnerServiceBeanTest.STORE_ID,
            productBusinessPartner, false, new ArrayList());

      });
    } catch (Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
        Mockito.verify(this.productRepository, ProductBusinessPartnerServiceBeanTest.NEVER_CALLED)
          .findDetailById(Mockito.anyString());
        Mockito.verify(this.businessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).filterDetailByBusinessPartnerCode(
          Mockito.anyString());
        Mockito.verify(this.productBusinessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).saveAndFlush(
          (ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.anyString());
        Mockito
            .verify(this.productLevel3Service, ProductBusinessPartnerServiceBeanTest.NEVER_CALLED)
            .create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(),
                (ProductBusinessPartner) Mockito.any(), eq(false),
                Mockito.anyList());
        Mockito.verify(this.productBusinessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).save(
          (ProductBusinessPartner) Mockito.any());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void createWithProductViewableIsFalseTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner
      .setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    ProductItemBusinessPartner productItemBusinessPartner = getProductItemBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    ProductDetailResponse productData = new ProductDetailResponse(new ProductResponse());
    when(this.productRepository.findDetailById(Mockito.any()))
      .thenReturn(productData);
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.create(
            ProductBusinessPartnerServiceBeanTest.STORE_ID, productBusinessPartner, false, new ArrayList());
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.INVALID_STATE, applicationException.getErrorCodes());
        Mockito.verify(this.productRepository).findDetailById(Mockito.any());
        Mockito.verify(this.businessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).filterDetailByBusinessPartnerCode(
          Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).saveAndFlush(
          (ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).findById(Mockito.any());
        Mockito
            .verify(this.productLevel3Service, ProductBusinessPartnerServiceBeanTest.NEVER_CALLED)
            .create(Mockito.any(), (ProductDetailResponse) Mockito.any(),
                (ProductBusinessPartner) Mockito.any(), eq(false),
                Mockito.anyList());
        Mockito.verify(this.productBusinessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).save(
          (ProductBusinessPartner) Mockito.any());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }


  @Test
  public void createWithBusinessPartnerActivatedIsFalseTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setId(null);
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.setActivated(false);
    businessPartnerData.setMerchantStatus(Constants.NOT_APPLICABLE);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
      .thenReturn(businessPartnerData);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productBusinessPartnerServiceBean.create(
            ProductBusinessPartnerServiceBeanTest.STORE_ID, productBusinessPartner, false, new ArrayList());
      });
    } catch(Exception e) {
      if (e instanceof ApplicationRuntimeException) {
        ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
        Assertions.assertTrue(applicationException.getErrorMessage().contains(ErrorMessages.INACTIVE_BUSINESS_PARTNER_MSG));
        Mockito.verify(this.productRepository).findDetailById(Mockito.anyString());
        Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(
          Mockito.anyString());
        Mockito.verify(this.productBusinessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).saveAndFlush(
          (ProductBusinessPartner) Mockito.any());
        Mockito.verify(this.productBusinessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).findById(Mockito.anyString());
        Mockito
            .verify(this.productLevel3Service, ProductBusinessPartnerServiceBeanTest.NEVER_CALLED)
            .create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(),
                (ProductBusinessPartner) Mockito.any(), eq(false),
                Mockito.anyList());
        Mockito.verify(this.productBusinessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).save(
          (ProductBusinessPartner) Mockito.any());
        throw e;
      }
    }
  }

  private ProductItemBusinessPartner getProductItemBusinessPartner() {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setProductType(GdnBaseLookup.PRODUCT_TYPE_REGULAR);
    productItemBusinessPartner.setPickupPointId(DEFAULT_PICKUP_POINT);
    return productItemBusinessPartner;
  }

  @Test
  public void retryCreateTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductDetailResponse productData = new ProductDetailResponse(new ProductResponse());
    productData.setActivated(true);
    productData.setViewable(true);
    productData.setShippingWeight(49D);
    Mockito.when(this.productBusinessPartnerRepository.findById(DEFAULT_PRODUCT_BUSINESS_PARTNER_ID))
      .thenReturn(Optional.of(productBusinessPartner));
    Mockito.when(this.productRepository.findDetailById(productBusinessPartner.getProductId())).thenReturn(productData);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(
      businessPartnerData);
    this.productBusinessPartnerServiceBean.retryCreate(
      ProductBusinessPartnerServiceBeanTest.STORE_ID,
      ProductBusinessPartnerServiceBeanTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID, null);
    Mockito.verify(this.productBusinessPartnerRepository,
      ProductBusinessPartnerServiceBeanTest.CALLED_TWICE).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).findDetailById(Mockito.anyString());
    Mockito.verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(productLevel3Service).findDetailByProductSku(GDN_SKU);
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(), (ProductBusinessPartner) Mockito.any(),
        eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(productLevel3RetryService).updateCompletedOrOmittedState(STORE_ID, productBusinessPartner.getGdnProductSku(),
      ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void retryCreateSwitchOn2Test() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "makeInactiveSellerProductOffline", true);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.setMerchantStatus(STATE);
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductDetailResponse productData = new ProductDetailResponse(new ProductResponse());
    productData.setActivated(true);
    productData.setViewable(true);
    productData.setShippingWeight(49D);
    Mockito.when(this.productBusinessPartnerRepository.findById(DEFAULT_PRODUCT_BUSINESS_PARTNER_ID))
        .thenReturn(Optional.of(productBusinessPartner));
    Mockito.when(this.productRepository.findDetailById(productBusinessPartner.getProductId())).thenReturn(productData);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(
        businessPartnerData);
    this.productBusinessPartnerServiceBean.retryCreate(
        ProductBusinessPartnerServiceBeanTest.STORE_ID,
        ProductBusinessPartnerServiceBeanTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID, null);
    Mockito.verify(this.productBusinessPartnerRepository,
        ProductBusinessPartnerServiceBeanTest.CALLED_TWICE).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).findDetailById(Mockito.anyString());
    Mockito.verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(productLevel3Service).findDetailByProductSku(GDN_SKU);
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(), (ProductBusinessPartner) Mockito.any(),
        eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(productLevel3RetryService).updateCompletedOrOmittedState(STORE_ID, productBusinessPartner.getGdnProductSku(),
        ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void retryCreateSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "makeInactiveSellerProductOffline", true);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductDetailResponse productData = new ProductDetailResponse(new ProductResponse());
    productData.setActivated(true);
    productData.setViewable(true);
    productData.setShippingWeight(49D);
    Mockito.when(this.productBusinessPartnerRepository.findById(DEFAULT_PRODUCT_BUSINESS_PARTNER_ID))
        .thenReturn(Optional.of(productBusinessPartner));
    Mockito.when(this.productRepository.findDetailById(productBusinessPartner.getProductId())).thenReturn(productData);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(
        null);
    this.productBusinessPartnerServiceBean.retryCreate(
        ProductBusinessPartnerServiceBeanTest.STORE_ID,
        ProductBusinessPartnerServiceBeanTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID, null);
    Mockito.verify(this.productBusinessPartnerRepository,
        ProductBusinessPartnerServiceBeanTest.CALLED_TWICE).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).findDetailById(Mockito.anyString());
    Mockito.verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(productLevel3Service).findDetailByProductSku(GDN_SKU);
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(), (ProductBusinessPartner) Mockito.any(),
        eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(productLevel3RetryService).updateCompletedOrOmittedState(STORE_ID, productBusinessPartner.getGdnProductSku(),
        ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void retryCreate_alreadyEntityPresentTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductDetailResponse productData = new ProductDetailResponse(new ProductResponse());
    productData.setActivated(true);
    productData.setViewable(true);
    productData.setShippingWeight(49D);
    Mockito.when(this.productRepository.findDetailById(productBusinessPartner.getProductId())).thenReturn(productData);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(
      businessPartnerData);
    this.productBusinessPartnerServiceBean.retryCreate(
      ProductBusinessPartnerServiceBeanTest.STORE_ID,
      ProductBusinessPartnerServiceBeanTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID, productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).findDetailById(Mockito.anyString());
    Mockito.verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(productLevel3Service).findDetailByProductSku(GDN_SKU);
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(), (ProductBusinessPartner) Mockito.any(),
      eq(false), Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(productLevel3RetryService).updateCompletedOrOmittedState(STORE_ID, productBusinessPartner.getGdnProductSku(),
      ProductLevel3RetryStatus.COMPLETED.name());
  }

  @Test
  public void retryCreateTestWithAlreadyInProduct() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    ProfileResponse businessPartnerData = getProfileResponse();
    businessPartnerData.getCompany().setInternationalFlag(true);
    ProductDetailResponse productData = new ProductDetailResponse(new ProductResponse());
    productData.setActivated(true);
    productData.setViewable(true);
    productData.setShippingWeight(49D);
    Mockito.when(this.productBusinessPartnerRepository.findById(DEFAULT_PRODUCT_BUSINESS_PARTNER_ID))
      .thenReturn(Optional.of(productBusinessPartner));
    Mockito.when(this.productRepository.findDetailById(productBusinessPartner.getProductId())).thenReturn(productData);
    Mockito.when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString())).thenReturn(
      businessPartnerData);
    Mockito.when(productLevel3Service.findDetailByProductSku(GDN_SKU)).thenReturn(new ProductAndItemsResponse());
    this.productBusinessPartnerServiceBean.retryCreate(
      ProductBusinessPartnerServiceBeanTest.STORE_ID,
      ProductBusinessPartnerServiceBeanTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID, null);
    Mockito.verify(this.productBusinessPartnerRepository,
      ProductBusinessPartnerServiceBeanTest.CALLED_TWICE).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).findDetailById(Mockito.anyString());
    Mockito.verify(productLevel3Service).findDetailByProductSku(GDN_SKU);
    Mockito.verify(this.businessPartnerRepository)
      .filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productBusinessPartnerRepository).save((ProductBusinessPartner) Mockito.any());
    Mockito.verify(productLevel3RetryService).updateCompletedOrOmittedState(STORE_ID, productBusinessPartner.getGdnProductSku(),
      ProductLevel3RetryStatus.OMITTED.name());
  }

  @Test
  public void retryCreateWithNotExistProductBusinessPartnerTest() throws Exception {
    when(this.productBusinessPartnerRepository.findById(Mockito.anyString())).thenReturn(Optional.empty());
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.retryCreate(
            ProductBusinessPartnerServiceBeanTest.STORE_ID,
            ProductBusinessPartnerServiceBeanTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID, null);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.DATA_ACCESS, applicationException.getErrorCodes());
        Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.anyString());
        Mockito.verify(this.productRepository, ProductBusinessPartnerServiceBeanTest.NEVER_CALLED)
          .findDetailById(Mockito.anyString());
        Mockito.verify(this.businessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).filterDetailByBusinessPartnerCode(
          Mockito.anyString());
        Mockito.verify(this.productLevel3Service, ProductBusinessPartnerServiceBeanTest.NEVER_CALLED)
            .create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(),
                (ProductBusinessPartner) Mockito.any(), eq(false),
                Mockito.anyList());
        Mockito.verify(this.productBusinessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).save(
          (ProductBusinessPartner) Mockito.any());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void retryCreateWithBusinessPartnerActivatedIsFalseTest() throws Exception {
    ProfileResponse businessPartnerData = new ProfileResponse();
    businessPartnerData.setActivated(Boolean.FALSE);
    businessPartnerData.setMerchantStatus(Constants.NOT_APPLICABLE);
    when(
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(Mockito.anyString()))
      .thenReturn(businessPartnerData);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.productBusinessPartnerServiceBean.retryCreate(
            ProductBusinessPartnerServiceBeanTest.STORE_ID,
            ProductBusinessPartnerServiceBeanTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID, null);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationRuntimeException) {
        ApplicationRuntimeException applicationException = (ApplicationRuntimeException) e;
        Assertions.assertTrue(applicationException.getErrorMessage().contains(ErrorMessages.INACTIVE_BUSINESS_PARTNER_MSG));
        Mockito.verify(this.productBusinessPartnerRepository).findById(Mockito.anyString());
        Mockito.verify(this.productRepository).findDetailById(Mockito.anyString());
        Mockito.verify(this.businessPartnerRepository).filterDetailByBusinessPartnerCode(
          Mockito.anyString());
        Mockito
            .verify(this.productLevel3Service, ProductBusinessPartnerServiceBeanTest.NEVER_CALLED)
            .create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(),
                (ProductBusinessPartner) Mockito.any(), eq(false),
                Mockito.anyList());
        Mockito.verify(this.productBusinessPartnerRepository,
          ProductBusinessPartnerServiceBeanTest.NEVER_CALLED).save(
          (ProductBusinessPartner) Mockito.any());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
  }

  @Test
  public void retryCreateWithExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.productLevel3Service).create(Mockito.anyString(),
        (ProductDetailResponse) Mockito.any(), (ProductBusinessPartner) Mockito.any(),
        eq(false), Mockito.anyList());
    try {
      Assertions.assertThrows(ApplicationException.class, () -> {
        this.productBusinessPartnerServiceBean.retryCreate(
            ProductBusinessPartnerServiceBeanTest.STORE_ID,
            ProductBusinessPartnerServiceBeanTest.DEFAULT_PRODUCT_BUSINESS_PARTNER_ID, null);
      });
    } catch(Exception e) {
      if (e instanceof ApplicationException) {
        ApplicationException applicationException = (ApplicationException) e;
        Assertions.assertEquals(ErrorCategory.UNSPECIFIED, applicationException.getErrorCodes());
        throw e;
      } else {
        Assertions.assertFalse(true);
      }
    }
    Mockito.verify(this.productBusinessPartnerRepository,
        ProductBusinessPartnerServiceBeanTest.CALLED_TWICE).findById(Mockito.anyString());
    Mockito.verify(this.productRepository).findDetailById(Mockito.anyString());
    Mockito.verify(this.businessPartnerRepository)
        .filterDetailByBusinessPartnerCode(Mockito.anyString());
    Mockito.verify(this.productLevel3Service).create(Mockito.anyString(), (ProductDetailResponse) Mockito.any(),
        (ProductBusinessPartner) Mockito.any(), eq(false),
        Mockito.anyList());
    Mockito.verify(this.productBusinessPartnerRepository).save(
        (ProductBusinessPartner) Mockito.any());
    Mockito.verify(this.productLevel3RetryService).upsertProductLevel3FailureLog(STORE_ID, GDN_SKU);
  }

  @Test
  public void testFindRejectedProductsByBusinessPartnerId() throws Exception {
    RejectedSkuProductCollection rejectedSkuProductCollection =
      new RejectedSkuProductCollection("PRODUCT-1", "CATEGORY-1", "BRAND-1", Calendar
        .getInstance().getTime(), DEFAULT_USERNAME, "REASON-1", Calendar.getInstance()
        .getTime(), DEFAULT_PRODUCT_CODE);
    List<RejectedSkuProductCollection> rejectedSkuProductCollections =
      new ArrayList<RejectedSkuProductCollection>();
    rejectedSkuProductCollections.add(rejectedSkuProductCollection);
    Page<RejectedSkuProductCollection> page =
      new PageImpl<RejectedSkuProductCollection>(rejectedSkuProductCollections);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    when(
      this.productBusinessPartnerRepository.findRejectedProductsByBusinessPartnerId(
        STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean.findRejectedProductsByBusinessPartnerId(
      STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable, null);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .findRejectedProductsByBusinessPartnerId(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
        pageable);
  }

  @Test
  public void testFindRejectedProductsByBusinessPartnerIdWithTestCriteria() throws Exception {
    RejectedSkuProductCollection rejectedSkuProductCollection =
      new RejectedSkuProductCollection("PRODUCT-1", "CATEGORY-1", "BRAND-1", Calendar.getInstance().getTime(),
        DEFAULT_USERNAME, "REASON-1", Calendar.getInstance().getTime(), DEFAULT_PRODUCT_CODE);
    List<RejectedSkuProductCollection> rejectedSkuProductCollections = new ArrayList<RejectedSkuProductCollection>();
    rejectedSkuProductCollections.add(rejectedSkuProductCollection);
    Page<RejectedSkuProductCollection> page = new PageImpl<RejectedSkuProductCollection>(rejectedSkuProductCollections);
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    when(this.productBusinessPartnerRepository
      .findRejectedProductsByBusinessPartnerIdAndProductName(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
        "SEARCH_CRITERIA", pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean
      .findRejectedProductsByBusinessPartnerId(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
        "SEARCH_CRITERIA");
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .findRejectedProductsByBusinessPartnerIdAndProductName(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
        "SEARCH_CRITERIA", pageable);
  }

  @Test
  public void getSkuValueTrueAttributeList() {
    List<String> attributeIdList = new ArrayList<>();
    String productId = "Product ID";
    this.productBusinessPartnerServiceBean.getSkuValueTrueAttributeList(attributeIdList, productId);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1)).findByProductId(attributeIdList, productId);
  }

  @Test
  public void getProductTypeBasedOnProductId() {
    String productId = "Product ID";
    this.productBusinessPartnerServiceBean.getProductTypeBasedOnProductId(productId);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
      .getProductTypeBasedOnProductId(productId);
  }

  @Test
  public void getMinimumStockByGdnProductItemSkuTest() throws Exception {
    String itemSKU = "Product ID";
    this.productBusinessPartnerServiceBean.getMinimumStockByGdnProductItemSku(itemSKU);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
      .getMinimumStockByGdnProductItemSku(itemSKU);
  }

  @Test
  public void updateMinimumStockByGdnProductItemSkuTest() throws Exception {
    String itemSKU = "Product ID";
    this.productBusinessPartnerServiceBean.updateMinimumStockByGdnProductItemSku(itemSKU, DEFAULT_PAGE);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
      .updateMinimumStockByGdnProductItemSku(itemSKU, DEFAULT_PAGE);
  }

  @Test
  public void updateMinimumStockByGdnProductItemSkuExceptionTest() throws Exception {
    String itemSKU = "Product ID";
    Mockito.doThrow(RuntimeException.class).when(this.productBusinessPartnerRepository)
      .updateMinimumStockByGdnProductItemSku(itemSKU, DEFAULT_PAGE);
    Assertions.assertThrows(ApplicationException.class, () -> {
      this.productBusinessPartnerServiceBean.updateMinimumStockByGdnProductItemSku(itemSKU, DEFAULT_PAGE);
    });
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
      .updateMinimumStockByGdnProductItemSku(itemSKU, DEFAULT_PAGE);
  }

  @Test
  public void testSaveBusinessPartner() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setProductType(0);
    ProductResponse product =
      new ProductDetailResponse.Builder().productCode(UUID.randomUUID().toString())
        .name("Produk 1").length(1.0).width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").build();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse(product);
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategoryResponse> productCategories = getProductCategoryResponse();
    productDetailResponse.setProductCategoryResponses(productCategories);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany().setInternationalFlag(true);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    when(this.productBusinessPartnerRepository.save(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    when(this.calendarService
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE), any(Date.class)))
      .thenReturn(new Date());
    this.productBusinessPartnerServiceBean
      .saveBusinessPartner(productBusinessPartner, productDetailResponse, false);
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE)
      .filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .save(productBusinessPartner);
    Mockito.verify(this.productGdnSkuGeneratorService)
        .generateGdnSkuOnProduct(productBusinessPartnerArgumentCaptor.capture(), eq(false));
    Mockito.verify(this.calendarService)
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE), any());
    Assertions.assertEquals(productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners()
        .get(0).getProductType(), GdnBaseLookup.PRODUCT_TYPE_BOPIS);
    Assertions.assertEquals(productBusinessPartnerArgumentCaptor.getValue().getProductItemBusinessPartners()
        .get(1).getProductType(), GdnBaseLookup.PRODUCT_TYPE_REGULAR);
  }

  @Test
  public void testSaveBusinessWithComapnyNull() throws Exception {

    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    ProductResponse product =
      new ProductDetailResponse.Builder().productCode(UUID.randomUUID().toString())
        .name("Produk 1").length(1.0).width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").build();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse(product);
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategoryResponse> productCategories = getProductCategoryResponse();
    productDetailResponse.setProductCategoryResponses(productCategories);
    ProfileResponse businessPartner = getProfileResponse();
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.getCompany().setMerchantType(Constants.TD_MERCHANT);
    when(this.businessPartnerRepository.filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId()))
      .thenReturn(businessPartner);
    when(this.productBusinessPartnerRepository.save(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    when(this.calendarService
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE), any(Date.class)))
      .thenReturn(new Date());
    this.productBusinessPartnerServiceBean
      .saveBusinessPartner(productBusinessPartner, productDetailResponse, false);
    Mockito.verify(this.businessPartnerRepository, AT_LEAST_ONE)
      .filterDetailByBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
      .save(productBusinessPartner);
    Mockito.verify(this.productGdnSkuGeneratorService)
      .generateGdnSkuOnProduct(Mockito.any(ProductBusinessPartner.class), eq(false));
    Mockito.verify(this.calendarService)
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE), any());

  }

  @Test
  public void testSaveWithActivatedFalseFailed() throws Exception {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    Product product =
      new Product.Builder().productCode(UUID.randomUUID().toString()).name("Produk 1").length(1.0)
        .width(1.0).height(1.0).weight(1.0).shippingWeight(1.0)
        .description("Deskripsi".getBytes()).brand("Brand 1").uom("UOM")
        .uniqueSellingPoint("USP").storeId(STORE_ID).build();
    product.setId(productBusinessPartner.getProductId());
    List<ProductCategory> productCategories = getProductCategories();
    product.setProductCategories(productCategories);
    businessPartner.setId(productBusinessPartner.getBusinessPartnerId());
    businessPartner.setActivated(true);
    businessPartner.setCompany(new Company());
    businessPartner.getCompany().setInternationalFlag(true);
    when(this.productBusinessPartnerRepository.findById(productBusinessPartner.getId()))
      .thenReturn(Optional.empty());
    when(this.productRepository.findOne(productBusinessPartner.getProductId())).thenReturn(
      product);
    when(this.productBusinessPartnerRepository.saveAndFlush(productBusinessPartner))
      .thenReturn(productBusinessPartner);
    when(this.calendarService
      .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE),
        any())).thenReturn(new Date());
    Mockito.doThrow(RuntimeException.class).when(this.productBusinessPartnerRepository)
      .saveAndFlush(productBusinessPartner);
    try {
      this.productBusinessPartnerServiceBean.saveWithActivatedFalse(productBusinessPartner);
    } catch(Exception e) {
      Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findById(productBusinessPartner.getId());
      Mockito.verify(this.productRepository, AT_LEAST_ONE)
        .findOne(productBusinessPartner.getProductId());
      Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .saveAndFlush(productBusinessPartner);
      Mockito.verify(this.productGdnSkuGeneratorService)
        .generateGdnSkuOnProduct(Mockito.any(ProductBusinessPartner.class), eq(false));
      Mockito.verify(this.calendarService)
        .getExpectedActivationDateByCategoryCode(eq(DEFAULT_CATEGORY_CODE), any());
    }
  }

  @Test
  public void markItemsAsUnBuyableAndUnViewableTest() {
    Mockito.when(productBusinessPartnerRepository.markItemsAsUnBuyableAndUnViewable(PRODUCT_ID)).thenReturn(1);
    productBusinessPartnerServiceBean.markItemsAsUnBuyableAndUnViewable(PRODUCT_ID);
    Mockito.verify(productBusinessPartnerRepository).markItemsAsUnBuyableAndUnViewable(PRODUCT_ID);
  }

  @Test
  public void updateSkuValueTrueInProductBusinessPartnerAttributeTest() {
    productBusinessPartnerServiceBean
      .updateSkuValueTrueInProductBusinessPartnerAttribute(productAttributeList, PRODUCT_ID);
    Mockito.verify(productBusinessPartnerRepository)
      .updateSkuTrueAttributeInProductBusinessPartnerAttribute(VALUE, ATTRIBUTE_ID, PRODUCT_ID);
  }

  @Test
  public void updateSkuValueTrueNonSingleType() {
    productAttributeList.get(0).getProductAttributeValues().get(0)
      .setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    productBusinessPartnerServiceBean
      .updateSkuValueTrueInProductBusinessPartnerAttribute(productAttributeList, PRODUCT_ID);
  }

  @Test
  public void markItemsAsDeletedOnProductResubmission() {
    Mockito.doNothing().when(productBusinessPartnerRepository)
      .markItemsAsDeletedOnProductResubmission(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_ID);
    Mockito.doNothing().when(productBusinessPartnerRepository)
      .markProductAsDeletedOnProductResubmission(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_ID);
    Mockito.doNothing().when(productBusinessPartnerRepository)
        .markBusinessPartnerAttributesAsDeletedOnProductResubmission(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_ID);
    productBusinessPartnerServiceBean
      .markItemsAsDeletedOnProductResubmission(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_ID);
    Mockito.verify(this.productBusinessPartnerRepository)
      .markItemsAsDeletedOnProductResubmission(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_ID);
    Mockito.verify(this.productBusinessPartnerRepository)
      .markProductAsDeletedOnProductResubmission(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_ID);
    Mockito.verify(this.productBusinessPartnerRepository)
        .markBusinessPartnerAttributesAsDeletedOnProductResubmission(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_ID);
  }

  @Test
  public void markItemsAsDeletedOnProductResubmissionExceptionTest() {
    Mockito.doThrow(RuntimeException.class).when(productBusinessPartnerRepository)
      .markItemsAsDeletedOnProductResubmission(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_ID);
    productBusinessPartnerServiceBean
      .markItemsAsDeletedOnProductResubmission(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_ID);
    Mockito.verify(this.productBusinessPartnerRepository)
      .markItemsAsDeletedOnProductResubmission(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_ID);
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdAndMerchantSkuTest() throws Exception {
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    Page<RejectedSkuProductCollection> response = new PageImpl<>(rejectedSkuProductCollections);
    Mockito.when(this.productBusinessPartnerRepository
      .findRejectedProductsByBusinessPartnerIdAndMerchantSku(STORE_ID, DEFAULT_PRODUCT_BUSINESS_PARTNER_ID,
        MERCHANT_SKU, pageable)).thenReturn(response);
    Page<RejectedSkuProductCollection> res = productBusinessPartnerServiceBean
      .findRejectedProductsByBusinessPartnerIdAndMerchantSku(STORE_ID, DEFAULT_PRODUCT_BUSINESS_PARTNER_ID,
        pageable, MERCHANT_SKU);
    Mockito.verify(this.productBusinessPartnerRepository)
      .findRejectedProductsByBusinessPartnerIdAndMerchantSku(STORE_ID, DEFAULT_PRODUCT_BUSINESS_PARTNER_ID,
        MERCHANT_SKU, pageable);
    Assertions.assertEquals(DEFAULT_PRODUCT_NAME, res.getContent().get(0).getProductName());
    Assertions.assertEquals(DEFAULT_CATEGORY_NAME, res.getContent().get(0).getCategoryName());
    Assertions.assertEquals(DEFAULT_BRAND_NAME, res.getContent().get(0).getBrand());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, res.getContent().get(0).getProductCode());
  }

  @Test
  public void copyAllProductsTest() throws Exception {
    Pageable pageable = PageRequest.of(0, 20);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(GDN_ITEM_SKU);
    productItemBusinessPartner.setGdnProductItemSku(GDN_PRODUCT_SKU);

    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setProductItemBusinessPartners(Arrays.asList(productItemBusinessPartner));

    Mockito.when(syncStatusService.findItemsAlreadyInSyncProcess(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, LINKED_BUSINESS_PARTNER_CODE))
      .thenReturn(Collections.emptyList());

    Mockito
      .when(syncStatusService
        .copy(eq(STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT),
          eq(false), anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU))), eq(LINKED_BUSINESS_PARTNER_CODE)))
      .thenReturn(1);

    Mockito.when(productLevel3DirectAggregatorService
      .aggregateProductSummaryWithoutInventory(Mockito.any(ProductLevel3SummaryFilter.class), Mockito.eq(pageable)))
      .thenReturn(new PageImpl<>(Arrays.asList(ProductLevel3Summary.builder()
        .itemSku(GDN_ITEM_SKU)
        .productSku(GDN_PRODUCT_SKU)
        .build()
      ), pageable, 1));

    Mockito
      .when(syncStatusService
        .findByItemSkuAndBusinessPartnerCode(STORE_ID, GDN_ITEM_SKU, DEFAULT_BUSINESS_PARTNER_CODE))
      .thenReturn(null);

    productBusinessPartnerServiceBean
      .copyAllProducts(STORE_ID, DEFAULT_USERNAME, LINKED_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
        DEFAULT_PICKUP_POINT);

    Mockito.verify(productLevel3DirectAggregatorService)
      .aggregateProductSummaryWithoutInventory(filterCaptor.capture(), Mockito.eq(pageable));

    Assertions.assertEquals(STORE_ID, filterCaptor.getValue().getStoreId());
    Assertions.assertEquals(false, filterCaptor.getValue().getArchived());
    Assertions.assertEquals(LINKED_BUSINESS_PARTNER_CODE, filterCaptor.getValue().getBusinessPartnerCode());

    Mockito
      .verify(syncStatusService)
      .copy(eq(STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT), eq(false),
        anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU))), eq(LINKED_BUSINESS_PARTNER_CODE));

    Mockito.verify(syncStatusService)
      .findItemsAlreadyInSyncProcess(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, LINKED_BUSINESS_PARTNER_CODE);

    Mockito.verify(syncProcessService).save(syncProcessCaptor.capture());

    assertNotNull(syncProcessCaptor.getValue().getProcessId());
    Assertions.assertFalse(syncProcessCaptor.getValue().isUserNotified());
    Assertions.assertEquals(STORE_ID, syncProcessCaptor.getValue().getStoreId());
  }

  @Test
  public void copyAllProductsTest_whenNoProductsFound() throws Exception {
    Pageable pageable = PageRequest.of(0, 20);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(GDN_ITEM_SKU);

    Mockito
      .when(syncStatusService.findItemsAlreadyInSyncProcess(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, LINKED_BUSINESS_PARTNER_CODE))
      .thenReturn(Arrays.asList("item-sku-one"));

    Mockito
      .when(syncStatusService
        .copy(eq(STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT),
          eq(false), anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU))), eq(LINKED_BUSINESS_PARTNER_CODE)))
      .thenReturn(0);

    Mockito.when(productLevel3DirectAggregatorService
      .aggregateProductSummaryWithoutInventory(Mockito.any(ProductLevel3SummaryFilter.class), Mockito.eq(pageable)))
      .thenReturn(new PageImpl<>(Collections.emptyList(), pageable, 1));

    try {
      productBusinessPartnerServiceBean
        .copyAllProducts(STORE_ID, DEFAULT_USERNAME, LINKED_BUSINESS_PARTNER_CODE, DEFAULT_BUSINESS_PARTNER_CODE,
          DEFAULT_PICKUP_POINT);
    } catch(ApplicationRuntimeException e) {
      Mockito.verify(syncStatusService)
        .findItemsAlreadyInSyncProcess(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, LINKED_BUSINESS_PARTNER_CODE);

      Mockito.verify(productLevel3DirectAggregatorService)
        .aggregateProductSummaryWithoutInventory(filterCaptor.capture(), Mockito.eq(pageable));

      Assertions.assertEquals(Arrays.asList("item-sku-one"), filterCaptor.getValue().getExcludedItemSkus());

      Mockito
        .verify(syncStatusService)
        .copy(eq(STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT), eq(false),
          anyString(), eq(Collections.emptyMap()), eq(LINKED_BUSINESS_PARTNER_CODE));
    }
  }

  @Test
  public void copyTest() {
    Mockito
      .when(syncStatusService
        .copy(eq(STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT),
          eq(false), anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU))), eq(LINKED_BUSINESS_PARTNER_CODE)))
      .thenReturn(0);

    try {
      productBusinessPartnerServiceBean
        .copy(STORE_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT, false, PROCESS_ID,
          Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU)), LINKED_BUSINESS_PARTNER_CODE);
    } catch(ApplicationRuntimeException e){
      Mockito
        .verify(syncStatusService)
        .copy(eq(STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT), eq(false),
          anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU))), eq(LINKED_BUSINESS_PARTNER_CODE));
    }
  }

  @Test
  public void copyTest_withNoItems() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      productBusinessPartnerServiceBean.copy(STORE_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
          DEFAULT_PICKUP_POINT, false, PROCESS_ID, Collections.emptyMap(), LINKED_BUSINESS_PARTNER_CODE);

    });
  }

  @Test
  public void copyTest_withEligibleItemsToCopy() {
    Mockito
      .when(syncStatusService
        .copy(eq(STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT),
          eq(false), anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU))), eq(LINKED_BUSINESS_PARTNER_CODE)))
      .thenReturn(1);

    productBusinessPartnerServiceBean
      .copy(STORE_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_PICKUP_POINT, false, PROCESS_ID,
        Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU)), LINKED_BUSINESS_PARTNER_CODE);

    Mockito
      .verify(syncStatusService)
      .copy(eq(STORE_ID), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE), eq(DEFAULT_PICKUP_POINT), eq(false),
        anyString(), eq(Collections.singletonMap(GDN_PRODUCT_SKU,Arrays.asList(GDN_ITEM_SKU))), eq(LINKED_BUSINESS_PARTNER_CODE));

    Mockito.verify(syncProcessService).save(syncProcessCaptor.capture());

    assertNotNull(syncProcessCaptor.getValue().getProcessId());
    Assertions.assertEquals(STORE_ID, syncProcessCaptor.getValue().getStoreId());
    Assertions.assertFalse(syncProcessCaptor.getValue().isUserNotified());
  }

  @Test
  public void notifyForCopyProcessTest() throws Exception {
    ProductItemSyncProcessSummary syncProcess = ProductItemSyncProcessSummary.builder()
      .businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
      .processId(PROCESS_ID)
      .productSyncStatus(ProductSyncStatus.FAIL)
      .count(1)
      .build();

    ProductItemSyncProcessSummary syncProcess1 = ProductItemSyncProcessSummary.builder()
      .businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
      .processId(PROCESS_ID)
      .productSyncStatus(ProductSyncStatus.SUCCESS)
      .count(1)
      .build();

    Mockito.when(syncProcessService.findAllProcessEligibleForNotification(STORE_ID))
      .thenReturn(Arrays.asList(ProductItemSyncProcess.builder()
        .processId(PROCESS_ID)
        .isUserNotified(false).build()));

    Mockito.when(syncStatusService.productCopyStatusForProcessID(STORE_ID, PROCESS_ID))
      .thenReturn(Arrays.asList(syncProcess, syncProcess1));
    productBusinessPartnerServiceBean.notifyForProductCopyingProcess(STORE_ID);

    Mockito.verify(productNotificationService)
      .sendProductSyncNotification(DEFAULT_BUSINESS_PARTNER_CODE, String.format(PRODUCT_PARTIALLY_ADDED_AS_FBB, 1, 2));
    Mockito.verify(syncProcessService).save(syncProcessCaptor.capture());
    Mockito.verify(syncProcessService).findAllProcessEligibleForNotification(STORE_ID);
    Mockito.verify(syncStatusService).productCopyStatusForProcessID(STORE_ID, PROCESS_ID);

    assertNotNull(syncProcessCaptor.getValue().getProcessId());
    Assertions.assertEquals(PROCESS_ID, syncProcessCaptor.getValue().getProcessId());
    Assertions.assertTrue(syncProcessCaptor.getValue().isUserNotified());
    Assertions.assertTrue(syncProcessCaptor.getValue().isMarkForDelete());
    Mockito.verifyNoMoreInteractions(this.productNotificationService);
  }

  @Test
  public void notifyForCopyProcessTest_whenAllSuccess() throws Exception {
    ProductItemSyncProcessSummary syncProcess = ProductItemSyncProcessSummary.builder()
      .businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
      .processId(PROCESS_ID)
      .productSyncStatus(ProductSyncStatus.SUCCESS)
      .count(1)
      .build();


    Mockito.when(syncProcessService.findAllProcessEligibleForNotification(STORE_ID))
      .thenReturn(Arrays.asList(ProductItemSyncProcess.builder()
        .processId(PROCESS_ID)
        .isUserNotified(false).build()));

    Mockito.when(syncStatusService.productCopyStatusForProcessID(STORE_ID, PROCESS_ID))
      .thenReturn(Arrays.asList(syncProcess, syncProcess));
    productBusinessPartnerServiceBean.notifyForProductCopyingProcess(STORE_ID);

    Mockito.verify(productNotificationService)
      .sendProductSyncNotification(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_SUCCESSFULLY_ADDED_AS_FBB);
    Mockito.verify(syncProcessService).save(syncProcessCaptor.capture());
    Mockito.verify(syncProcessService).findAllProcessEligibleForNotification(STORE_ID);
    Mockito.verify(syncStatusService).productCopyStatusForProcessID(STORE_ID, PROCESS_ID);

    assertNotNull(syncProcessCaptor.getValue().getProcessId());
    Assertions.assertEquals(PROCESS_ID, syncProcessCaptor.getValue().getProcessId());
    Assertions.assertTrue(syncProcessCaptor.getValue().isUserNotified());
    Assertions.assertTrue(syncProcessCaptor.getValue().isMarkForDelete());
    Mockito.verifyNoMoreInteractions(this.productNotificationService);
  }

  @Test
  public void notifyForCopyProcessTest_whenEmptyResponse() throws Exception {

    Mockito.when(syncProcessService.findAllProcessEligibleForNotification(STORE_ID))
      .thenReturn(Arrays.asList(ProductItemSyncProcess.builder()
        .processId(PROCESS_ID)
        .isUserNotified(false).build()));

    Mockito.when(syncStatusService.productCopyStatusForProcessID(STORE_ID, PROCESS_ID))
      .thenReturn(new ArrayList<>());
    productBusinessPartnerServiceBean.notifyForProductCopyingProcess(STORE_ID);

    Mockito.verify(syncProcessService).findAllProcessEligibleForNotification(STORE_ID);
    Mockito.verify(syncStatusService).productCopyStatusForProcessID(STORE_ID, PROCESS_ID);
  }

  @Test
  public void notifyForCopyProcessTest_whenInProgress() throws Exception {
    ProductItemSyncProcessSummary syncProcess = ProductItemSyncProcessSummary.builder()
      .businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
      .processId(PROCESS_ID)
      .productSyncStatus(ProductSyncStatus.IN_PROGRESS)
      .count(1)
      .build();

    ProductItemSyncProcessSummary syncProcess1 = ProductItemSyncProcessSummary.builder()
      .businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
      .processId(PROCESS_ID)
      .productSyncStatus(ProductSyncStatus.SUCCESS)
      .count(1)
      .build();

    Mockito.when(syncProcessService.findAllProcessEligibleForNotification(STORE_ID))
      .thenReturn(Arrays.asList(ProductItemSyncProcess.builder()
        .processId(PROCESS_ID)
        .isUserNotified(false).build()));

    Mockito.when(syncStatusService.productCopyStatusForProcessID(STORE_ID, PROCESS_ID))
      .thenReturn(Arrays.asList(syncProcess, syncProcess1));
    productBusinessPartnerServiceBean.notifyForProductCopyingProcess(STORE_ID);

    Mockito.verify(syncProcessService).findAllProcessEligibleForNotification(STORE_ID);
    Mockito.verify(syncStatusService).productCopyStatusForProcessID(STORE_ID, PROCESS_ID);
  }

  @Test
  public void notifyForCopyProcessExceptionTest() throws Exception {
    ProductItemSyncProcessSummary syncProcess = ProductItemSyncProcessSummary.builder()
      .businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
      .processId(PROCESS_ID)
      .productSyncStatus(ProductSyncStatus.SUCCESS)
      .count(1)
      .build();

    ProductItemSyncProcessSummary syncProcess1 = ProductItemSyncProcessSummary.builder()
      .businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
      .processId(PROCESS_ID)
      .productSyncStatus(ProductSyncStatus.SUCCESS)
      .count(1)
      .build();

    Mockito.when(syncProcessService.findAllProcessEligibleForNotification(STORE_ID))
      .thenReturn(Arrays.asList(ProductItemSyncProcess.builder()
        .processId(PROCESS_ID)
        .isUserNotified(false).build()));

    Mockito.when(syncStatusService.productCopyStatusForProcessID(STORE_ID, PROCESS_ID))
      .thenReturn(Arrays.asList(syncProcess, syncProcess1));

    Mockito.doThrow(Exception.class).when(productNotificationService)
      .sendProductSyncNotification(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_SUCCESSFULLY_ADDED_AS_FBB);

    productBusinessPartnerServiceBean.notifyForProductCopyingProcess(STORE_ID);

    Mockito.verify(productNotificationService)
      .sendProductSyncNotification(DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_SUCCESSFULLY_ADDED_AS_FBB);
    Mockito.verify(syncProcessService).findAllProcessEligibleForNotification(STORE_ID);
    Mockito.verify(syncStatusService).productCopyStatusForProcessID(STORE_ID, PROCESS_ID);
    Mockito.verifyNoMoreInteractions(this.productNotificationService);
  }

  @Test
  public void findActiveProductsByBusinessPartnerIdTest(){
    Pageable pageable = PageRequest.of(0, 20);

    productBusinessPartnerServiceBean
      .findActiveProductsByBusinessPartnerId(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable);

    Mockito
      .verify(productBusinessPartnerRepository)
      .findByStoreIdAndBusinessPartnerIdAndMarkForDeleteTrueOrderByCreatedDateDesc(STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, pageable);
  }

  @Test
  public void findActiveProductsByBusinessPartnerIdTest_withResponseValidation(){
    Pageable pageable = PageRequest.of(0, 20);
    Page<ProductBusinessPartner> mustBeSameObject = new PageImpl<>(Collections.emptyList());

    Mockito
      .when(productBusinessPartnerRepository
      .findByStoreIdAndBusinessPartnerIdAndMarkForDeleteTrueOrderByCreatedDateDesc(STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, pageable)).thenReturn(mustBeSameObject);

    Page<ProductBusinessPartner> response = productBusinessPartnerServiceBean
      .findActiveProductsByBusinessPartnerId(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable);

    Assertions.assertTrue(response == mustBeSameObject);

    Mockito
      .verify(productBusinessPartnerRepository)
      .findByStoreIdAndBusinessPartnerIdAndMarkForDeleteTrueOrderByCreatedDateDesc(STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, pageable);
  }

  @Test
  public void resetProductItemSystemStatusTest() {
    Mockito.when(systemParameterService.getParameter("sync-retry-duration"))
      .thenReturn("48");

    productBusinessPartnerServiceBean.resetProductItemSyncStatus(STORE_ID);

    Mockito.verify(syncStatusService)
      .updateProductItemSyncStatus(STORE_ID, 48L);
  }

  @Test
  public void resetProductItemSystemStatusTest_whenNullSyncRetryDuration() {
    Mockito.when(systemParameterService.getParameter("sync-retry-duration"))
      .thenReturn(null);

    productBusinessPartnerServiceBean.resetProductItemSyncStatus(STORE_ID);

    Mockito.verify(syncStatusService)
      .updateProductItemSyncStatus(STORE_ID, 24L);
  }

  @Test
  public void resetProductItemSystemStatusTest_whenEmptySyncRetryDuration() {
    Mockito.when(systemParameterService.getParameter("sync-retry-duration"))
      .thenReturn(StringUtils.EMPTY);

    productBusinessPartnerServiceBean.resetProductItemSyncStatus(STORE_ID);

    Mockito.verify(syncStatusService)
      .updateProductItemSyncStatus(STORE_ID, 24L);
  }


  @Test
  public void countRejectedProductsByBusinessPartnerIdTest() {
    Mockito.when(productBusinessPartnerRepository
        .countRejectedProductsByBusinessPartnerId(STORE_ID, LINKED_BUSINESS_PARTNER_CODE)).thenReturn(10L);

    productBusinessPartnerServiceBean.countRejectedProductsByBusinessPartnerId(STORE_ID, LINKED_BUSINESS_PARTNER_CODE);

    Mockito.verify(productBusinessPartnerRepository)
        .countRejectedProductsByBusinessPartnerId(STORE_ID, LINKED_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void countRejectedProductsByBusinessPartnerId_NewTest() {
    when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.REJECTED_API_SWITCH))
        .thenReturn(new ProductSystemParameter("rejectedApiSwitch", "true", "rejectedApiSwitch", false));
    Mockito.when(productBusinessPartnerRepository
        .countRejectedProductsByBusinessPartnerIdFromPbp(STORE_ID, LINKED_BUSINESS_PARTNER_CODE)).thenReturn(10L);
    productBusinessPartnerServiceBean.countRejectedProductsByBusinessPartnerId(STORE_ID, LINKED_BUSINESS_PARTNER_CODE);
    Mockito.verify(productBusinessPartnerRepository)
        .countRejectedProductsByBusinessPartnerIdFromPbp(STORE_ID, LINKED_BUSINESS_PARTNER_CODE);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.REJECTED_API_SWITCH);
  }

  @Test
  public void countRejectedProductsByBusinessPartnerIdExceptionTest() {
    try {
      productBusinessPartnerServiceBean
          .countRejectedProductsByBusinessPartnerId(STORE_ID, null);
    } catch (Exception e) {
      Mockito.verify(productBusinessPartnerRepository, times(0))
          .countRejectedProductsByBusinessPartnerId(STORE_ID, LINKED_BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void countSuspendedProductsByBusinessPartnerCodeTest() throws Exception {
    SummaryFilterRequest summaryFilterRequest =
        SummaryFilterRequest.builder().businessPartnerCode(LINKED_BUSINESS_PARTNER_CODE).build();
    Page<SuspensionItemResponse> response =
        new PageImpl<SuspensionItemResponse>(new ArrayList<>(), PageRequest.of(0, 1), 100);
    Mockito.when(productLevel3Service
        .getSuspendedItems(summaryFilterRequest, REQUEST_ID, USERNAME, STORE_ID, PageRequest.of(0, 1)))
        .thenReturn(response);

    long result = productBusinessPartnerServiceBean
        .countSuspendedProductsByBusinessPartnerCode(STORE_ID, REQUEST_ID, USERNAME, LINKED_BUSINESS_PARTNER_CODE);

    Mockito.verify(productLevel3Service).
        getSuspendedItems(summaryFilterRequest, REQUEST_ID, USERNAME, STORE_ID, PageRequest.of(0, 1));
    Assertions.assertEquals(100L, result);
  }

  @Test
  public void countSuspendedProductsByBusinessPartnerCodeExceptionTest() throws Exception {
    try {
      productBusinessPartnerServiceBean
          .countSuspendedProductsByBusinessPartnerCode(STORE_ID, REQUEST_ID, USERNAME, null);
    } catch (Exception e) {
      SummaryFilterRequest summaryFilterRequest =
          SummaryFilterRequest.builder().businessPartnerCode(null).build();
      Mockito.verify(productLevel3Service, times(0)).
          getSuspendedItems(summaryFilterRequest, REQUEST_ID, USERNAME, STORE_ID, PageRequest.of(0, 1));
    }
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdWithOrderByAndSortByTest_New() throws Exception {
    Map<String, String> map = new HashMap<>();
    map.put(PRODUCT_ID, PRODUCT_CODE);
    when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.REJECTED_API_SWITCH))
        .thenReturn(new ProductSystemParameter("rejectedApiSwitch", "true", "rejectedApiSwitch", false));
    when(this.productBusinessPartnerCustomRepository
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductName(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            SEARCH_CRITERIA, pageable, ORDER_BY, SORT_BY)).thenReturn(productBusinessPartners);
    when(productLevel3Service.getProductIdProductCodeMap(STORE_ID, Arrays.asList(PRODUCT_ID))).thenReturn(map);
    when(productWorkflowService.getRejectedNotesByProductIds(Arrays.asList(PRODUCT_ID))).thenReturn(map);
    this.productBusinessPartnerServiceBean
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
            SEARCH_CRITERIA, ORDER_BY, SORT_BY);
    Mockito.verify(this.productBusinessPartnerCustomRepository, AT_LEAST_ONE)
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductName(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            SEARCH_CRITERIA, pageable, ORDER_BY, SORT_BY);
    verify(productLevel3Service).getProductIdProductCodeMap(STORE_ID, Arrays.asList(PRODUCT_ID));
    verify(productWorkflowService).getRejectedNotesByProductIds(Arrays.asList(PRODUCT_ID));
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.REJECTED_API_SWITCH);
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdWithOrderByAndSortByTest_EmptyNew() throws Exception {
    productBusinessPartners = new PageImpl<>(Arrays.asList());
    when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.REJECTED_API_SWITCH))
        .thenReturn(new ProductSystemParameter("rejectedApiSwitch", "true", "rejectedApiSwitch", false));
    when(this.productBusinessPartnerCustomRepository
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductName(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            SEARCH_CRITERIA, pageable, ORDER_BY, SORT_BY)).thenReturn(productBusinessPartners);
    this.productBusinessPartnerServiceBean
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
            SEARCH_CRITERIA, ORDER_BY, SORT_BY);
    Mockito.verify(this.productBusinessPartnerCustomRepository, AT_LEAST_ONE)
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductName(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            SEARCH_CRITERIA, pageable, ORDER_BY, SORT_BY);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.REJECTED_API_SWITCH);
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdWithOrderByAndSortByTest() throws Exception {
    ORDER_BY = "createdDate";
    SORT_BY = "ASC";
    when(this.productBusinessPartnerRepository
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndCreatedDateAsc(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable))
        .thenReturn(page);
    this.productBusinessPartnerServiceBean
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
            null, ORDER_BY, SORT_BY);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndCreatedDateAsc(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable);
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdWithOrderByAndSortByTest8() throws Exception {
    ORDER_BY = "updatedDate";
    SORT_BY = "desc";
    when(this.productBusinessPartnerRepository
        .findRejectedProductsByBusinessPartnerId(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
            null, ORDER_BY, SORT_BY);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findRejectedProductsByBusinessPartnerId(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable);
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdWithOrderByAndSortByTest2() throws Exception {
    ORDER_BY = "createdDate";
    SORT_BY = "Desc";
    when(this.productBusinessPartnerRepository
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndCreatedDateDesc(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable))
        .thenReturn(page);
    this.productBusinessPartnerServiceBean
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
            null, ORDER_BY, SORT_BY);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndCreatedDateDesc(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable);
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdWithOrderByAndSortByTest3() throws Exception {
    ORDER_BY = "updatedDate";
    SORT_BY = "asc";
    when(this.productBusinessPartnerRepository
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndUpdatedDateAsc(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable))
        .thenReturn(page);
    this.productBusinessPartnerServiceBean
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
            null, ORDER_BY, SORT_BY);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndUpdatedDateAsc(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable);
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdWithOrderByAndSortByTest4() throws Exception {
    ORDER_BY = "updatedDate";
    SORT_BY = "desc";
    when(this.productBusinessPartnerRepository
        .findRejectedProductsByBusinessPartnerIdAndProductName(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, SEARCH_CRITERIA,
            pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
            SEARCH_CRITERIA, ORDER_BY, SORT_BY);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findRejectedProductsByBusinessPartnerIdAndProductName(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, SEARCH_CRITERIA,
            pageable);
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdWithOrderByAndSortByTest5() throws Exception {
    ORDER_BY = "createdDate";
    SORT_BY = "desc";
    when(this.productBusinessPartnerRepository
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndCreatedDateDesc(STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, SEARCH_CRITERIA, pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
            SEARCH_CRITERIA, ORDER_BY, SORT_BY);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndCreatedDateDesc(STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, SEARCH_CRITERIA, pageable);
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdWithOrderByAndSortByTest6() throws Exception {
    ORDER_BY = "createdDate";
    SORT_BY = "asc";
    when(this.productBusinessPartnerRepository
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndCreatedDateAsc(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            SEARCH_CRITERIA, pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
            SEARCH_CRITERIA, ORDER_BY, SORT_BY);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndCreatedDateAsc(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            SEARCH_CRITERIA, pageable);
  }

  @Test
  public void findRejectedProductsByBusinessPartnerIdWithOrderByAndSortByTest7() throws Exception {
    ORDER_BY = "updatedDate";
    SORT_BY = "asc";
    when(this.productBusinessPartnerRepository
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndUpdatedDateAsc(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            SEARCH_CRITERIA, pageable)).thenReturn(page);
    this.productBusinessPartnerServiceBean
        .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, pageable,
            SEARCH_CRITERIA, ORDER_BY, SORT_BY);
    Mockito.verify(this.productBusinessPartnerRepository, AT_LEAST_ONE)
        .findRejectedProductsByStoreIdAndBusinessPartnerIdAndProductNameAndUpdatedDateAsc(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            SEARCH_CRITERIA, pageable);
  }

  @Test
  public void isProductMappedToMerchantTest() {
    when(this.productBusinessPartnerRepository.findFirstByStoreIdAndBusinessPartnerId(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(getProductBusinessPartner());
    boolean productMappedToMerchant =
        this.productBusinessPartnerServiceBean.isProductMappedToMerchant(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    verify(this.productBusinessPartnerRepository).findFirstByStoreIdAndBusinessPartnerId(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(productMappedToMerchant);
  }


  @Test
  public void isProductMappedToMerchantNullTest() {
    when(this.productBusinessPartnerRepository.findFirstByStoreIdAndBusinessPartnerId(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    boolean productMappedToMerchant =
        this.productBusinessPartnerServiceBean.isProductMappedToMerchant(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    verify(this.productBusinessPartnerRepository).findFirstByStoreIdAndBusinessPartnerId(STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(productMappedToMerchant);
  }

  @Test
  public void getAllItemSkusViewConfigByProductIdTest() {
    when(productBusinessPartnerRepository.getAllItemSkusViewConfigByProductId(PRODUCT_ID))
        .thenReturn(Collections.singletonList(new ItemFlagDetails(GDN_ITEM_SKU, PICKUP_POINT_CODE, true, true, false, false, GDN_ITEM_ID)));
    List<ItemFlagDetails> response =
        productBusinessPartnerServiceBean.getAllItemSkusViewConfigByProductId(PRODUCT_ID);
    verify(productBusinessPartnerRepository).getAllItemSkusViewConfigByProductId(PRODUCT_ID);
    Assertions.assertEquals(GDN_ITEM_SKU, response.get(0).getItemSku());
    Assertions.assertEquals(GDN_ITEM_ID, response.get(0).getProductItemId());
    Assertions.assertTrue(response.get(0).isBuyable());
    Assertions.assertTrue(response.get(0).isDisplayable());
    Assertions.assertFalse(response.get(0).isCncBuyable());
    Assertions.assertFalse(response.get(0).isCncDisplayable());
  }

  @Test
  public void getMinimumPriceTest() {
    Mockito.when(this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(productSystemParameter);
    Integer response = this.productBusinessPartnerServiceBean.getMinimumPrice(STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID, Constants.MINIMUM_PRICE);
    Assertions.assertEquals(Optional.of(1).get(), response);
  }

  @Test
  public void getMinimumPriceWithNullTest() {
    Mockito.when(this.productSystemParameterService.findByStoreIdAndVariable(STORE_ID, Constants.MINIMUM_PRICE))
        .thenReturn(null);
    Integer response = this.productBusinessPartnerServiceBean.getMinimumPrice(STORE_ID);
    Mockito.verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID, Constants.MINIMUM_PRICE);
    Assertions.assertNull(response);
  }

  @Test
  public void getProductSkusByProductCodeTest() {
    when(this.productBusinessPartnerRepository.getGdnSkuByProductCode(PRODUCT_CODE))
        .thenReturn(productSkus);
    List<String> L3s =
        this.productBusinessPartnerServiceBean.getProductSkusByProductCode(PRODUCT_CODE);
    verify(this.productBusinessPartnerRepository).getGdnSkuByProductCode(PRODUCT_CODE);
    assertNotNull(L3s);
  }

  @Test
  public void updateProductName() {
    doNothing().when(this.productBusinessPartnerRepository)
        .updateProductMasterData(GDN_PRODUCT_SKU, DEFAULT_PRODUCT_NAME, CATEGORY_CODE, DEFAULT_CATEGORY_NAME, StringUtils.EMPTY);
    productBusinessPartnerServiceBean
        .updateProductMasterData(GDN_PRODUCT_SKU, DEFAULT_PRODUCT_NAME, CATEGORY_CODE,
          DEFAULT_CATEGORY_NAME, null,
          USERNAME, false, StringUtils.EMPTY);
    verify(productBusinessPartnerRepository)
        .updateProductMasterData(GDN_PRODUCT_SKU, DEFAULT_PRODUCT_NAME, CATEGORY_CODE, DEFAULT_CATEGORY_NAME, StringUtils.EMPTY);
  }

  @Test
  public void updateProductNameAndSizeChart() {
    doNothing().when(this.productBusinessPartnerRepository)
      .updateProductMasterDataAndSizeChart(GDN_PRODUCT_SKU, DEFAULT_PRODUCT_NAME, CATEGORY_CODE,
        DEFAULT_CATEGORY_NAME, SIZE_CHART_CODE, USERNAME, StringUtils.EMPTY);
    productBusinessPartnerServiceBean
      .updateProductMasterData(GDN_PRODUCT_SKU, DEFAULT_PRODUCT_NAME, CATEGORY_CODE,
        DEFAULT_CATEGORY_NAME, SIZE_CHART_CODE,
        USERNAME, true, StringUtils.EMPTY);
    verify(productBusinessPartnerRepository)
      .updateProductMasterDataAndSizeChart(GDN_PRODUCT_SKU, DEFAULT_PRODUCT_NAME, CATEGORY_CODE,
        DEFAULT_CATEGORY_NAME, SIZE_CHART_CODE, USERNAME, StringUtils.EMPTY);
  }

  @Test
  public void getExpectedActivationDateByCategoryCodeTest() {
    productBusinessPartnerServiceBean.getExpectedActivationDateByCategoryCode(new ProductDetailResponse(), new Date());
  }

  @Test
  public void getExpectedActivationDateByCategoryCodeNotNullTest() {
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    Date date = new Date();
    productDetailResponse.setCategoryCodes(Collections.singletonList(PRODUCT_CODE));
    productBusinessPartnerServiceBean.getExpectedActivationDateByCategoryCode(productDetailResponse, date);
    Mockito.verify(calendarService).getExpectedActivationDateByCategoryCode(PRODUCT_CODE, date);
  }

  @Test
  public void saveProductBusinessPartnerTest() {
    productBusinessPartnerServiceBean.saveProductBusinessPartner(new ProductBusinessPartner());
    Mockito.verify(productBusinessPartnerRepository).save(new ProductBusinessPartner());
  }

  @Test
  public void getProductTypeBasedOnProductCodeOrId() {
    when(xProductOutbound.getProductTypeByProductCode(Mockito.anyString()))
        .thenReturn(new ProductTypeResponse(com.gdn.x.product.enums.ProductType.REGULAR));
    productBusinessPartnerServiceBean.getProductTypeBasedOnProductCodeOrId(PRODUCT_CODE, PRODUCT_ID);
    verify(xProductOutbound).getProductTypeByProductCode(Mockito.anyString());
  }

  @Test
  public void getProductTypeBasedOnProductCodeOrId_Null() {
    when(xProductOutbound.getProductTypeByProductCode(Mockito.anyString())).thenReturn(null);
    when(productBusinessPartnerRepository.getProductTypeBasedOnProductId(Mockito.anyString())).thenReturn(1);
    productBusinessPartnerServiceBean.getProductTypeBasedOnProductCodeOrId(PRODUCT_CODE, PRODUCT_ID);
    verify(xProductOutbound).getProductTypeByProductCode(Mockito.anyString());
    verify(productBusinessPartnerRepository).getProductTypeBasedOnProductId(Mockito.anyString());
  }

  @Test
  public void updateProductBusinessPartnerStateTakenDownTest() {
    when(calendarService.getExpectedActivationDateByCategoryCode(eq(CATEGORY_CODE), any())).thenReturn(new Date());
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerServiceBean.updateProductBusinessPartnerState(
            getProductBusinessPartner(), true, CATEGORY_CODE, null);
    verify(calendarService).getExpectedActivationDateByCategoryCode(eq(CATEGORY_CODE), any());
    Assertions.assertFalse(productBusinessPartner.isMarkForDelete());
    Assertions.assertFalse(productBusinessPartner.isActivated());
    Assertions.assertEquals(ProductLevel3WipSummaryCriteria.IN_PROGRESS.name(), productBusinessPartner.getState());
  }

  @Test
  public void updateProductBusinessPartnerStateTakenDownTRUETest() {
    productL3Response.setOnline(true);
    when(calendarService.getExpectedActivationDateByCategoryCode(eq(CATEGORY_CODE), any())).thenReturn(new Date());
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerServiceBean.updateProductBusinessPartnerState(
            getProductBusinessPartner(), true, CATEGORY_CODE, productL3Response);
    verify(calendarService).getExpectedActivationDateByCategoryCode(eq(CATEGORY_CODE), any());
    Assertions.assertFalse(productBusinessPartner.isMarkForDelete());
    Assertions.assertFalse(productBusinessPartner.isActivated());
    Assertions.assertEquals(ProductLevel3WipSummaryCriteria.IN_PROGRESS.name(), productBusinessPartner.getState());
    Assertions.assertTrue(productBusinessPartner.isOnline());
  }

  @Test
  public void updateProductBusinessPartnerStateActivate() {
    productL3Response.setOnline(true);
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerServiceBean.updateProductBusinessPartnerState(
            getProductBusinessPartner(), false, CATEGORY_CODE, productL3Response);
    Assertions.assertTrue(productBusinessPartner.isMarkForDelete());
    Assertions.assertTrue(productBusinessPartner.isActivated());
    Assertions.assertEquals(ProductLevel1State.ACTIVE, productBusinessPartner.getState());
  }

  @Test
  public void updateProductItemBusinessPartnerStateTakeDownTrueTest() throws Exception {
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productItemBusinessPartnerList.get(0).setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartnerList.get(0).setGdnProductItemSku(ITEM_SKU);
    productLevel3Inventory.setWebItemSku(productItemBusinessPartnerList.get(0).getGdnProductItemSku());
    productLevel3Inventory.setWebPickupPointCode(productItemBusinessPartnerList.get(0).getPickupPointId());
    productLevel3Inventory.setWebAvailable(100);
    productLevel3Inventory.setWebMinAlert(10);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartnerList);
    Mockito.when(
            productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(Mockito.anyList()))
        .thenReturn(Arrays.asList(productLevel3Inventory));
    itemSummaryDetailResponseList.get(0).setItemViewConfigB2b(new HashSet<>(Arrays.asList(new ItemViewConfigDTO())));
    ProductBusinessPartnerAndItemViewConfigDto productBusinessPartnerAndItemViewConfigDto =
        productBusinessPartnerServiceBean.updateProductItemBusinessPartnerStateTakeDownTrue(
            itemSummaryDetailResponseList, itemViewConfigAndItemSkuRequests, productBusinessPartner);
    List<ProductItemBusinessPartner> productItemBusinessPartners =
        productBusinessPartnerAndItemViewConfigDto.getProductBusinessPartner().getProductItemBusinessPartners();
    verify(productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(Mockito.anyList());
    verify(bundleRecipeService, times(4)).convertBundleRecipeToString(any());
    Assertions.assertEquals(5, productBusinessPartnerAndItemViewConfigDto.getItemViewConfigAndItemSkuRequests().size());
    Assertions.assertEquals(7, productItemBusinessPartners.size());
  }

  @Test
  public void updateProductItemBusinessPartnerStateTakeDownTrue_cncForWarehouseFeatureSwitchTest()
      throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "cncForWarehouseFeatureSwitch",
        true);
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productItemBusinessPartnerList.get(0).setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartnerList.get(0).setGdnProductItemSku(ITEM_SKU);
    productLevel3Inventory.setWebItemSku(
        productItemBusinessPartnerList.get(0).getGdnProductItemSku());
    productLevel3Inventory.setWebPickupPointCode(
        productItemBusinessPartnerList.get(0).getPickupPointId());
    productLevel3Inventory.setWebAvailable(100);
    productLevel3Inventory.setWebMinAlert(10);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartnerList);
    Mockito.when(
            productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
                Mockito.anyList()))
        .thenReturn(Arrays.asList(productLevel3Inventory));
    itemSummaryDetailResponseList.get(0)
        .setItemViewConfigB2b(new HashSet<>(Arrays.asList(new ItemViewConfigDTO())));
    ProductBusinessPartnerAndItemViewConfigDto productBusinessPartnerAndItemViewConfigDto =
        productBusinessPartnerServiceBean.updateProductItemBusinessPartnerStateTakeDownTrue(
            itemSummaryDetailResponseList, itemViewConfigAndItemSkuRequests,
            productBusinessPartner);
    List<ProductItemBusinessPartner> productItemBusinessPartners =
        productBusinessPartnerAndItemViewConfigDto.getProductBusinessPartner()
            .getProductItemBusinessPartners();
    verify(
        productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
        Mockito.anyList());
    verify(bundleRecipeService, times(4)).convertBundleRecipeToString(any());
    Assertions.assertEquals(9,
        productBusinessPartnerAndItemViewConfigDto.getItemViewConfigAndItemSkuRequests().size());
    Assertions.assertEquals(7, productItemBusinessPartners.size());
  }

  @Test
  public void updateProductItemBusinessPartnerStateTakeDownTrueOrignalPrice0Test() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "useOriginalPriceEnabled", true);
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productItemBusinessPartnerList.get(0).setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartnerList.get(0).setGdnProductItemSku(ITEM_SKU);
    productLevel3Inventory.setWebItemSku(productItemBusinessPartnerList.get(0).getGdnProductItemSku());
    productLevel3Inventory.setWebPickupPointCode(productItemBusinessPartnerList.get(0).getPickupPointId());
    productLevel3Inventory.setWebAvailable(100);
    productLevel3Inventory.setWebMinAlert(10);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartnerList);
    Mockito.when(
            productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(Mockito.anyList()))
        .thenReturn(Arrays.asList(productLevel3Inventory));
    itemSummaryDetailResponseList.get(0).setItemViewConfigB2b(new HashSet<>(Arrays.asList(new ItemViewConfigDTO())));
    itemSummaryDetailResponseList.get(0).getPrice().iterator().next().setOfferPrice(10);
    ProductBusinessPartnerAndItemViewConfigDto productBusinessPartnerAndItemViewConfigDto =
        productBusinessPartnerServiceBean.updateProductItemBusinessPartnerStateTakeDownTrue(
            itemSummaryDetailResponseList, itemViewConfigAndItemSkuRequests, productBusinessPartner);
    List<ProductItemBusinessPartner> productItemBusinessPartners =
        productBusinessPartnerAndItemViewConfigDto.getProductBusinessPartner().getProductItemBusinessPartners();
    verify(productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(Mockito.anyList());
    verify(bundleRecipeService, times(4)).convertBundleRecipeToString(any());
    Assertions.assertEquals(5, productBusinessPartnerAndItemViewConfigDto.getItemViewConfigAndItemSkuRequests().size());
    Assertions.assertEquals(7, productItemBusinessPartners.size());
  }

  @Test
  public void updateProductItemBusinessPartnerStateTakeDownTrueOrignalPriceTest() throws Exception {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "useOriginalPriceEnabled", true);
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productItemBusinessPartnerList.get(0).setPickupPointId(PICKUP_POINT_CODE);
    productItemBusinessPartnerList.get(0).setGdnProductItemSku(ITEM_SKU);
    productLevel3Inventory.setWebItemSku(productItemBusinessPartnerList.get(0).getGdnProductItemSku());
    productLevel3Inventory.setWebPickupPointCode(productItemBusinessPartnerList.get(0).getPickupPointId());
    productLevel3Inventory.setWebAvailable(100);
    productLevel3Inventory.setWebMinAlert(10);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartnerList);
    Mockito.when(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(Mockito.anyList()))
            .thenReturn(Arrays.asList(productLevel3Inventory));
    itemSummaryDetailResponseList.get(0).setItemViewConfigB2b(new HashSet<>(Arrays.asList(new ItemViewConfigDTO())));
    itemSummaryDetailResponseList.get(0).setOriginalPrice(100);
    ProductBusinessPartnerAndItemViewConfigDto productBusinessPartnerAndItemViewConfigDto =
        productBusinessPartnerServiceBean.updateProductItemBusinessPartnerStateTakeDownTrue(
            itemSummaryDetailResponseList, itemViewConfigAndItemSkuRequests, productBusinessPartner);
    List<ProductItemBusinessPartner> productItemBusinessPartners =
        productBusinessPartnerAndItemViewConfigDto.getProductBusinessPartner().getProductItemBusinessPartners();
    verify(productLevel3InventoryService).findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(Mockito.anyList());
    verify(bundleRecipeService, times(4)).convertBundleRecipeToString(any());
    Assertions.assertEquals(5, productBusinessPartnerAndItemViewConfigDto.getItemViewConfigAndItemSkuRequests().size());
    Assertions.assertEquals(7, productItemBusinessPartners.size());
  }

  @Test
  public void updateProductItemBusinessPartnerStateTakeDownFalseTest() {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartnerList);
    productBusinessPartnerServiceBean.updateProductItemBusinessPartnerStateTakeDownFalse(
         itemViewConfigAndItemSkuRequests, productBusinessPartner, true);
    Assertions.assertEquals(productItemBusinessPartnerList.size(), itemViewConfigAndItemSkuRequests.size());
    Assertions.assertFalse(itemViewConfigAndItemSkuRequests.get(0).isBuyable());
    Assertions.assertTrue(itemViewConfigAndItemSkuRequests.get(0).isDiscoverable());
  }

  @Test
  public void updateProductItemBusinessPartnerStateTakeDownFalse_cncOnTest() {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "cncForWarehouseFeatureSwitch", true);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setCncDiscoverable(true);
    productBusinessPartner.getProductItemBusinessPartners().get(0).setCncBuyable(true);
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartnerList);
    productBusinessPartnerServiceBean.updateProductItemBusinessPartnerStateTakeDownFalse(
        itemViewConfigAndItemSkuRequests, productBusinessPartner, true);
    Assertions.assertFalse(itemViewConfigAndItemSkuRequests.get(0).isBuyable());
    Assertions.assertTrue(itemViewConfigAndItemSkuRequests.get(0).isDiscoverable());
    Assertions.assertFalse(itemViewConfigAndItemSkuRequests.get(1).isDiscoverable());
    Assertions.assertFalse(itemViewConfigAndItemSkuRequests.get(1).isBuyable());
  }

  @Test
  public void updateProductItemBusinessPartnerStateTakeDownTrue_cncOnTest() {
    ReflectionTestUtils.setField(productBusinessPartnerServiceBean, "cncForWarehouseFeatureSwitch", true);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.getProductItemBusinessPartners().get(0).setCncDiscoverable(true);
    productBusinessPartner.getProductItemBusinessPartners().get(0).setCncBuyable(true);
    productBusinessPartner.getProductItemBusinessPartners().get(0).setMarkForDelete(true);
    productBusinessPartner.getProductItemBusinessPartners().removeIf(not(ProductItemBusinessPartner::isMarkForDelete));
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    productBusinessPartnerServiceBean.updateProductItemBusinessPartnerStateTakeDownFalse(
        itemViewConfigAndItemSkuRequests, productBusinessPartner, true);
    Assertions.assertFalse(itemViewConfigAndItemSkuRequests.get(0).isBuyable());
    Assertions.assertFalse(itemViewConfigAndItemSkuRequests.get(0).isDiscoverable());
    Assertions.assertFalse(itemViewConfigAndItemSkuRequests.get(1).isDiscoverable());
    Assertions.assertFalse(itemViewConfigAndItemSkuRequests.get(1).isBuyable());
  }

  @Test
  public void updateProductItemBusinessPartnerStateTakeDownFalseBusinessPartnerActiveTest() {
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    List<ItemViewConfigAndItemSkuRequest> itemViewConfigAndItemSkuRequests = new ArrayList<>();
    productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartnerList);
    productBusinessPartnerServiceBean
        .updateProductItemBusinessPartnerStateTakeDownFalse(itemViewConfigAndItemSkuRequests, productBusinessPartner,
            false);
    Assertions.assertFalse(itemViewConfigAndItemSkuRequests.get(0).isBuyable());
    Assertions.assertFalse(itemViewConfigAndItemSkuRequests.get(0).isDiscoverable());
  }

  @Test
  public void findByStoreIdAndBusinessPartnerIdAndStateAndMarkForDeleteFalseTest() {
    productBusinessPartnerServiceBean.findByStoreIdAndBusinessPartnerIdAndStateAndMarkForDeleteFalse(
      STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.singletonList(STATE));
    Mockito.verify(this.productBusinessPartnerRepository)
      .findByStoreIdAndAndBusinessPartnerIdAndStateInAndMarkForDeleteFalseOrderByUpdatedDate(STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE, Collections.singletonList(STATE));
  }

  @Test
  public void updateNeedRevisionL3DetailsTest() {
    productLevel3.setFreeSample(false);
    productLevel3.setOff2OnChannelActive(false);
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setSizeChartCode(SIZE_CHART_CODE);
    productLevel3.setSizeChartChanged(false);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOff2OnChannelActive(false);
    productBusinessPartner.setProductName(PRODUCT_NAME);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU)).thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
  }

  @Test
  public void updateNeedRevisionL3DetailsWithSizeChartChangeTest() {
    productLevel3.setFreeSample(false);
    productLevel3.setOff2OnChannelActive(false);
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setSizeChartCode(SIZE_CHART_CODE);
    productLevel3.setSizeChartChanged(true);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOff2OnChannelActive(false);
    productBusinessPartner.setProductName(PRODUCT_NAME);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU)).thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
  }

  @Test
  public void updateNeedRevisionL3Details_Brand_Category_Updated() {
    productLevel3.setFreeSample(false);
    productLevel3.setOff2OnChannelActive(false);
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setSizeChartCode(SIZE_CHART_CODE);
    productLevel3.setCategoryUpdated(true);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOff2OnChannelActive(false);
    productBusinessPartner.setProductName(PRODUCT_NAME);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU)).thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
  }


  @Test
  public void updateNeedRevisionL3DetailsProductNameUpdateTest() {
    productLevel3.setFreeSample(false);
    productLevel3.setOff2OnChannelActive(false);
    productLevel3.setProductName(PRODUCT_NAME);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOff2OnChannelActive(false);
    productBusinessPartner.setProductName("");
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU)).thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerRepository).save(any(ProductBusinessPartner.class));
  }

  @Test
  public void updateNeedRevisionL3DetailsFreeSampleUpdateTest() {
    productLevel3.setFreeSample(false);
    productLevel3.setOff2OnChannelActive(false);
    productLevel3.setProductName(PRODUCT_NAME);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(true);
    productBusinessPartner.setOff2OnChannelActive(false);
    productBusinessPartner.setProductName(PRODUCT_NAME);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU)).thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerRepository).save(any(ProductBusinessPartner.class));
  }

  @Test
  public void updateNeedRevisionL3DetailsOff2OnChannelActiveUpdateTest() {
    productLevel3.setFreeSample(false);
    productLevel3.setOff2OnChannelActive(false);
    productLevel3.setProductName(PRODUCT_NAME);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOff2OnChannelActive(true);
    productBusinessPartner.setProductName(PRODUCT_NAME);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU)).thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerRepository).save(any(ProductBusinessPartner.class));
  }

  @Test
  public void updateNeedRevisionL3DetailsDiffTest() {
    productLevel3.setFreeSample(false);
    productLevel3.setOff2OnChannelActive(false);
    productLevel3.setProductName(PRODUCT_NAME);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(true);
    productBusinessPartner.setOff2OnChannelActive(true);
    productBusinessPartner.setProductName("");
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU)).thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
  }

  @Test
  public void updateNeedRevisionL3DetailsProductNameSameTest() {
    productLevel3.setFreeSample(false);
    productLevel3.setOff2OnChannelActive(false);
    productLevel3.setProductName(PRODUCT_NAME);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(true);
    productBusinessPartner.setOff2OnChannelActive(true);
    productBusinessPartner.setProductName(PRODUCT_NAME);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU)).thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerRepository).save(any(ProductBusinessPartner.class));
  }

  @Test
  public void updateNeedRevisionL3DetailsFreeSampleSameTest() {
    productLevel3.setFreeSample(false);
    productLevel3.setOff2OnChannelActive(false);
    productLevel3.setProductName(PRODUCT_NAME);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(false);
    productBusinessPartner.setOff2OnChannelActive(true);
    productBusinessPartner.setProductName("");
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU)).thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerRepository).save(any(ProductBusinessPartner.class));
  }

  @Test
  public void updateNeedRevisionL3DetailsSpecialProductAttributeValueChangeTest() {
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.getAttributes().stream().filter(ProductLevel3Attribute::getSkuValue)
        .findFirst().orElse(new ProductLevel3Attribute()).setValues(List.of(PRODUCT_NAME));
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setProductName(PRODUCT_NAME);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerRepository).save(any(ProductBusinessPartner.class));
  }

  @Test
  public void updateNeedRevisionL3DetailsSpecialProductAttributeDeletedTest() {
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setAttributes(productLevel3.getAttributes().stream().filter(
        productLevel3Attribute -> Objects.nonNull(productLevel3Attribute.getSkuValue()) && Boolean.FALSE.equals(
            productLevel3Attribute.getSkuValue())).collect(Collectors.toList()));
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setProductName(PRODUCT_NAME);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerRepository).save(any(ProductBusinessPartner.class));
    Assertions.assertTrue(productBusinessPartner.getProductBusinessPartnerAttributes().stream().findFirst()
        .orElse(new ProductBusinessPartnerAttribute()).isMarkForDelete());
  }

  @Test
  public void updateNeedRevisionL3DetailsNoSpecialAttributeUpdateTest() {
    productLevel3.setProductName(PRODUCT_NAME);
    productLevel3.setAttributes(new ArrayList<>());
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setProductName(PRODUCT_NAME);
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
  }

  @Test
  public void updateNeedRevisionL3DetailsNewSpecialProductAttributeAddedTest() {
    productLevel3.setProductName(PRODUCT_NAME);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setProductName(PRODUCT_NAME);
    productBusinessPartner.setProductBusinessPartnerAttributes(new ArrayList<>());
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU))
        .thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerRepository).save(any(ProductBusinessPartner.class));
    Assertions.assertEquals(1, productBusinessPartner.getProductBusinessPartnerAttributes().size());
  }

  @Test
  public void updateNeedRevisionL3DetailsOff2OnChannelActiveSameTest() {
    productLevel3.setFreeSample(false);
    productLevel3.setOff2OnChannelActive(false);
    productLevel3.setProductName(PRODUCT_NAME);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartner();
    productBusinessPartner.setFreeSample(true);
    productBusinessPartner.setOff2OnChannelActive(false);
    productBusinessPartner.setProductName("");
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(GDN_PRODUCT_SKU)).thenReturn(productBusinessPartner);
    productBusinessPartnerServiceBean.updateNeedRevisionL3Details(GDN_PRODUCT_SKU, productLevel3);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(GDN_PRODUCT_SKU);
    Mockito.verify(productBusinessPartnerRepository).save(any(ProductBusinessPartner.class));
  }

  @Test
  public void generateNewProductItemBusinessPartnerDataNoAddedVariantsTest() {
    List<ProductItemBusinessPartner> productItemBusinessPartners =
        productBusinessPartnerServiceBean.generateNewProductItemBusinessPartnerData(new ArrayList<>(), null);
    Assertions.assertEquals(productItemBusinessPartners, new ArrayList<>());
  }

  @Test
  public void generateNewProductItemBusinessPartnerDataWithTest() {
    Mockito.when(productBusinessPartnerRepository.findFirstByGdnProductSku(any()))
        .thenReturn(productBusinessPartner);
    List<ProductItemBusinessPartner> productItemBusinessPartners =
        productBusinessPartnerServiceBean.generateNewProductItemBusinessPartnerData(Arrays.asList(
            productVariantPriceStockAndImagesRequest), null);
    Assertions.assertEquals(productItemBusinessPartners.size(), 2);
    Mockito.verify(productBusinessPartnerRepository).findFirstByGdnProductSku(any());
  }

  public void findFirstByStoreIdAndProductIdTest() {
    Mockito.when(productBusinessPartnerRepository.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID))
        .thenReturn(new ProductBusinessPartner());
    productBusinessPartnerServiceBean.findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(productBusinessPartnerRepository).findFirstByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }


  @Test
  public void deleteProductBusinessPartnerByStoreIdAndProductIdTest() {
    productBusinessPartnerServiceBean.deleteProductBusinessPartnerByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    Mockito.verify(productBusinessPartnerRepository).deleteByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }

  @Test
  public void updateSizeChartDetails(){
    productBusinessPartnerServiceBean.updateSizeChartDetailsAndBrandDetails(PRODUCT_CODE, SIZE_CHART_CODE,
      USERNAME, true, false, StringUtils.EMPTY);
    Mockito.verify(productBusinessPartnerRepository).updateSizeChartDetails(PRODUCT_CODE,
      SIZE_CHART_CODE, USERNAME);
  }

  @Test
  public void updateSizeChartDetailsWithNoChangeInSizeChartCode(){
    productBusinessPartnerServiceBean.updateSizeChartDetailsAndBrandDetails(PRODUCT_CODE, SIZE_CHART_CODE,
      USERNAME, false, false, StringUtils.EMPTY);
    Mockito.verifyNoInteractions(productBusinessPartnerRepository);
  }

  @Test
  void testUpdateSizeChartAndBrand_WhenBothChanged() {
    String productSku = "SKU123";
    String sizeChartCode = "SIZE001";
    String updatedBy = "tester";
    String brand = "Nike";
    productBusinessPartnerServiceBean.updateSizeChartDetailsAndBrandDetails(
        productSku, sizeChartCode, updatedBy, true, true, brand);
    verify(productBusinessPartnerRepository, times(1))
        .updateSizeChartDetailsAndBrand(productSku, sizeChartCode, brand, updatedBy);
  }


  @Test
  void testUpdateBrand_WhenOnlyBrandUpdated() {
    String productSku = "SKU456";
    String sizeChartCode = "SIZE002";
    String updatedBy = "tester";
    String brand = "Adidas";
    productBusinessPartnerServiceBean.updateSizeChartDetailsAndBrandDetails(
        productSku, sizeChartCode, updatedBy, false, true, brand);
    verify(productBusinessPartnerRepository, times(1))
        .updateBrand(productSku, brand, updatedBy);
  }

  @Test
  public void findByProductCodeTest() {
    productBusinessPartnerServiceBean.findByProductCode(PRODUCT_CODE);
    Mockito.verify(productBusinessPartnerRepository).findProductBusinessPartnerByProductCode(PRODUCT_CODE);
  }

  @Test
  public void updateBrandTest() {
    ProductBusinessPartner productBusinessPartner1 = new ProductBusinessPartner();
    productBusinessPartner1.setBrand(DEFAULT_BRAND_NAME+"2");
    productBusinessPartnerServiceBean.updateBrand(List.of(productBusinessPartner1), DEFAULT_BRAND_NAME);
    productBusinessPartner1.setBrand(DEFAULT_BRAND_NAME);
    verify(productBusinessPartnerRepository).save(productBusinessPartner1);
  }

  @Test
  void testUpdateProductMasterDataWithSizeChartChange() {
    String productSku = "TEST-SKU-001";
    String productName = "Test Product";
    String categoryCode = "CAT001";
    String categoryName = "Test Category";
    String sizeChartCode = "SC001";
    String updatedBy = "testUser";
    boolean sizeChartChanged = true;
    String brandName = "Test Brand";
    productBusinessPartnerServiceBean.updateProductMasterData(productSku, productName, categoryCode, categoryName, sizeChartCode, updatedBy, sizeChartChanged, brandName);
    verify(productBusinessPartnerRepository, times(1)).updateProductMasterDataAndSizeChart(productSku, productName, categoryCode, categoryName, sizeChartCode, updatedBy, brandName);
  }

  @Test
  void testUpdateProductMasterDataWithoutSizeChartChange() {
    String productSku = "TEST-SKU-001";
    String productName = "Test Product";
    String categoryCode = "CAT001";
    String categoryName = "Test Category";
    String sizeChartCode = "SC001";
    String updatedBy = "testUser";
    boolean sizeChartChanged = false;
    String brandName = "Test Brand";
    productBusinessPartnerServiceBean.updateProductMasterData(productSku, productName, categoryCode, categoryName, sizeChartCode, updatedBy, sizeChartChanged, brandName);
    verify(productBusinessPartnerRepository, times(1)).updateProductMasterData(productSku, productName, categoryCode, categoryName, brandName);
  }

}
