package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.rest.web.model.dto.PickupPointDetailsDTO;
import com.gdn.x.product.rest.web.model.request.EanUpcPickupPointCodeRequest;
import com.gdn.x.product.rest.web.model.response.EanUpcPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.SwitchContext;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.ItemPickupPointRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemPickupPointMigrationEvent;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.ItemMigrationStatus;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.exception.ApiErrorCodes;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemPickupPointMigration;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.AddDeleteVariantRequestVo;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import com.gdn.x.product.model.vo.BundleRecipeRequest;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.model.vo.ItemCategoryVO;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.outbound.impl.XbpOutboundImpl;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.AutoCreatePickupPointRequest;
import com.gdn.x.product.rest.web.model.request.AutoCreatePickupPointRequestList;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointL5Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointMigrationService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPickupPointSummaryService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.DataSourcePrimaryEnabledProperties;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import jakarta.validation.ConstraintViolationException;

public class ItemPickupPointWrapperServiceImplTest {

  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String ITEM_SKU = "itemSku";
  private static final String ITEM_CODE = "itemCode";
  private static final String ITEM_SKU_2 = "itemSku2";
  private static final String STATUS = "status";
  private static final String PICKUP_POINT_CODE = "ppCode";
  private static final String PICKUP_POINT_CODE_3 = "ppCode3";
  private static final String DEFAULT_PICKUP_POINT_CODE = "defaultPPCode";
  private static final String PICKUP_POINT_CODE_2 = "ppCode2";
  private static final String OFFLINE_ITEM_ID = ITEM_SKU + Constants.COLON + PICKUP_POINT_CODE;
  private static final int MAX_COUNT = 2;
  private static final int PAGE_SIZE = 1;
  private static final Double OFFER_PRICE = 10.0;
  private static final Double LIST_PRICE = 10.0;
  private static final String EAN_UPC = "ean/Upc";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String MERCHANT_CODE_2 = "merchantCode2";
  private static final String MERCHANT_SKU = "merchantSKU";
  private static final String PRODUCT_SKU = "productSku";
  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String ATTRIBUTE_NAME = "attributeName";
  private static final String ATTRIBUTE_CODE = "attributeCode";
  private static final String ATTRIBUTE_CODE_2 = "attributeCode2";
  private static final String ATTRIBUTE_CODE_3 = "attributeCode3";
  private static final String ATTRIBUTE_VALUE = "attributeValue";
  private static final String SETTLEMENT_TYPE = "settlementType";
  private static final String GENERATED_ITEM_NAME = "generatedItemName";

  private SystemParameter maxItemsSystemParameter = new SystemParameter();
  private SystemParameter pageSizeSystemParameter = new SystemParameter();
  private SystemParameter payloadSizeSystemParameter = new SystemParameter();
  private SystemParameter eventPublishWaitTimeSystemParameter = new SystemParameter();
  private SystemParameter pageFetchSystemParameter = new SystemParameter();
  private ItemPickupPointMigration itemPickupPointMigration = new ItemPickupPointMigration();
  private ItemPickupPointMigrationEvent itemPickupPointMigrationEvent =
      new ItemPickupPointMigrationEvent();
  private Item item = new Item();
  private ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
  private ItemPickupPoint deliveryItemPickupPoint = new ItemPickupPoint();
  private ItemChange itemChange = new ItemChange();
  private OfflineItemChange offlineItemChange = new OfflineItemChange();
  private SystemParameter itemChangeSwitch = new SystemParameter();
  private SystemParameter offlineItemChangeSwitch = new SystemParameter();
  private List<DiscountPrice> listOfDiscountPrices = new ArrayList<>();
  private List<ItemPickupPointRequest> itemPickupPointRequests;
  private Map<String, Item> itemMap;
  private Map<String, ItemPickupPoint> itemPickupPointMap;

  private List<ItemPickupPointRequest> itemPickupPointRequestList;
  private Product product;
  private com.gdn.x.product.model.vo.ItemCatalogVO itemCatalogVO;
  private ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest;
  private Map<String, ItemPickupPoint> itemSkuToItemPickupPointMap;
  private Map<Integer, List<String>> deleteStatusItemSkuMap;
  private AutoCreatePickupPointRequestList autoCreatePickupPointRequestList;
  private EditProductDetailDTO editProductDetailDTO;
  private List<PickupPointDetailsDTO> pickupPointDetailsDTOS;
  private EanUpcPickupPointCodeRequest eanUpcPickupPointCodeRequest;

  @Mock
  private ItemPickupPointRepository itemPickupPointRepository;

  @Mock
  private ItemPickupPointMigrationService itemPickupPointMigrationService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private ItemService itemService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ProductService productService;
  @Mock
  private ItemHelperService itemHelperService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private CatalogService catalogService;

  @Mock
  private MasterDataConstructorService masterDataConstructorService;

  @Mock
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ProductCacheableService productCacheableService;

  @Mock
  private ItemPickupPointSummaryService itemPickupPointSummaryService;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @InjectMocks
  private ItemPickupPointWrapperServiceImpl itemPickupPointWrapperService;

  @Mock
  private ItemPickupPointSummaryServiceImpl itemPickupPointSummaryServiceImpl;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private ItemPriceService itemPriceService;

  @Captor
  private ArgumentCaptor<ItemPickupPoint> itemPickupPointArgumentCaptor;

  @Captor
  private ArgumentCaptor<Item> itemArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<Item>> itemListArgumentCaptor;

  @Mock
  private ApplicationContext applicationContext;

  @Captor
  private ArgumentCaptor<List<ItemPickupPointMigration>> itemPickupPointMigrationListArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ItemPickupPoint>> itemPickupPointListArgumentCaptor;

  @Mock
  private XbpOutboundImpl xbpOutbound;

  @Mock
  private DataSourcePrimaryEnabledProperties dataSourcePrimaryEnabledProperties;

  @Mock
  private DataSourceWrapperServiceImpl dataSourceWrapperService;

  @Mock
  SwitchContext switchContext;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    ReflectionTestUtils.setField(itemPickupPointWrapperService, "maxItemsForTransaction", 3);

    itemPickupPointMigration.setItemSku(ITEM_SKU);
    maxItemsSystemParameter.setValue(String.valueOf(MAX_COUNT));
    pageSizeSystemParameter.setValue(String.valueOf(PAGE_SIZE));
    payloadSizeSystemParameter.setValue(String.valueOf(PAGE_SIZE));
    eventPublishWaitTimeSystemParameter.setValue(String.valueOf(MAX_COUNT));
    pageFetchSystemParameter.setValue(String.valueOf(MAX_COUNT));
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MIGRATION_MAX_ITEMS_TO_PUBLISH)).thenReturn(maxItemsSystemParameter);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MIGRATION_PAGE_SIZE_FOR_ITEMS)).thenReturn(pageSizeSystemParameter);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MIGRATION_EVENT_PAYLOAD_SIZE)).thenReturn(payloadSizeSystemParameter);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MIGRATION_EVENT_PUBLISH_WAIT_TIME)).thenReturn(eventPublishWaitTimeSystemParameter);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MIGRATION_PAGE_FETCH_WAIT_TIME)).thenReturn(pageFetchSystemParameter);
    Mockito.when(this.itemPickupPointMigrationService.findByStatusAndLimit(STATUS, PAGE_SIZE))
        .thenReturn(Collections.singletonList(itemPickupPointMigration));
    itemPickupPointMigrationEvent.setItemSkuList(Collections.singletonList(ITEM_SKU));
    item.setItemSku(ITEM_SKU);
    item.setStoreId(STORE_ID);
    item.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(this.itemService.getItemsByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID,
        Collections.singleton(ITEM_SKU))).thenReturn(Collections.singletonList(item));
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPrice(ImmutableSet.of(new Price()));
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
    itemViewConfigSet.add(itemViewConfig1);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    item.setProductSku(PRODUCT_SKU);
    deliveryItemPickupPoint.setItemSku(ITEM_SKU);
    deliveryItemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_2);
    deliveryItemPickupPoint.setDelivery(true);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setBuyable(true);
    deliveryItemPickupPoint.setItemViewConfig(Collections.singleton(itemViewConfig));
    itemChange.setItemSku(ITEM_SKU);
    itemChange.setPickupPointCode(PICKUP_POINT_CODE);
    itemChange.setStoreId(STORE_ID);
    offlineItemChange.setItemSku(ITEM_SKU);
    offlineItemChange.setPickupPointCode(PICKUP_POINT_CODE);
    offlineItemChange.setStoreId(STORE_ID);
    offlineItemChange.setOfferPrice(OFFER_PRICE);
    offlineItemChange.setListPrice(LIST_PRICE);
    itemChangeSwitch.setValue(String.valueOf(Boolean.TRUE));
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ITEM_CHANGE_SWITCH)).thenReturn(itemChangeSwitch);
    offlineItemChangeSwitch.setValue(String.valueOf(Boolean.TRUE));
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.OFFLINE_ITEM_CHANGE_SWITCH)).thenReturn(offlineItemChangeSwitch);
    Mockito.when(itemPriceService.validMerchantPromoDiscountPrice(Mockito.any(DiscountPrice.class))).thenReturn(true);

    ItemPickupPointRequest itemPickupPointRequest1 = new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    ItemPickupPointRequest itemPickupPointRequest2 = new ItemPickupPointRequest(ITEM_SKU_2, PICKUP_POINT_CODE_2);
    itemPickupPointRequests = Arrays.asList(itemPickupPointRequest1, itemPickupPointRequest2);

    item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setCategoryCode(CATEGORY_CODE_2);
    item.setDangerousLevel(0);
    item.setProductSku(PRODUCT_SKU);
    item.setGeneratedItemName(GENERATED_ITEM_NAME);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(new ArrayList<>());
    item.setMasterDataItem(masterDataItem);

    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeName(ATTRIBUTE_NAME);
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    item.setDefiningAttributes(Arrays.asList(productAttributeDetail));

    product = new Product();
    product.setProductSku(PRODUCT_SKU);
    product.setProductType(ProductType.REGULAR);
    product.setSynchronized(true);

    ItemCategoryVO itemCategoryVO1 = new ItemCategoryVO();
    itemCategoryVO1.setProductCategoryCode(CATEGORY_CODE_1);
    itemCategoryVO1.setDocumentType("document");
    ItemCategoryVO itemCategoryVO2 = new ItemCategoryVO();
    itemCategoryVO2.setProductCategoryCode(CATEGORY_CODE_2);
    itemCategoryVO2.setDocumentType("document");
    itemCatalogVO = new com.gdn.x.product.model.vo.ItemCatalogVO();
    itemCatalogVO.setItemCategories(Arrays.asList(itemCategoryVO1, itemCategoryVO2));

    PreOrder preOrder = new PreOrder();
    preOrder.setPreOrderType(Constants.DATE);
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderDate(DateUtils.addDays(new Date(), -2));
    product.setPreOrder(preOrder);

    ProductSpecialAttribute productSpecialAttribute1 = new ProductSpecialAttribute();
    productSpecialAttribute1.setAttributeCode(ATTRIBUTE_CODE);
    ProductSpecialAttribute productSpecialAttribute2 = new ProductSpecialAttribute();
    productSpecialAttribute2.setAttributeCode(ATTRIBUTE_CODE_2);
    productSpecialAttribute2.setAttributeName(Constants.GUARANTEE_ATTRIBUTE_NAME.get(0));
    productSpecialAttribute2.setAttributeValue(ATTRIBUTE_VALUE);
    ProductSpecialAttribute productSpecialAttribute3 = new ProductSpecialAttribute();
    productSpecialAttribute3.setAttributeCode(ATTRIBUTE_CODE_3);
    productSpecialAttribute3.setAttributeName(Constants.GUARANTEE_DURATION_ATTRIBUTE_NAME.get(0));
    productSpecialAttribute3.setAttributeValue(ATTRIBUTE_VALUE);
    product.setProductSpecialAttributes(
        Arrays.asList(null, productSpecialAttribute1, productSpecialAttribute2, productSpecialAttribute3));

    SalesCatalog salesCatalog = new SalesCatalog();
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_1);
    salesCatalog.setListOfCategories(Arrays.asList(category));

    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCategory(category);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setMasterCatalog(masterCatalog);
    product.setMasterCatalog(masterCatalog);
    product.setMasterDataProduct(masterDataProduct);
    product.setSalesCatalogs(Arrays.asList(salesCatalog));
    itemPickupPointViewConfigBaseRequest = new ItemPickupPointViewConfigBaseRequest(false, false, false);

    editProductDetailDTO = new EditProductDetailDTO();
    itemMap = new HashMap<>();
    itemPickupPointMap = new HashMap<>();
    itemMap.put(ITEM_SKU, new Item());
    itemPickupPointMap.put(OFFLINE_ITEM_ID, new ItemPickupPoint());
    editProductDetailDTO.setProduct(new Product());
    editProductDetailDTO.setAllItemMap(itemMap);
    editProductDetailDTO.setAllItemPickupPointMap(itemPickupPointMap);

    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);

    itemSkuToItemPickupPointMap = new HashMap<>();
    deleteStatusItemSkuMap = new HashMap<>();
    AutoCreatePickupPointRequest autoCreatePickupPointRequest = new AutoCreatePickupPointRequest();
    autoCreatePickupPointRequest.setWebProductSku(PRODUCT_SKU);
    autoCreatePickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    autoCreatePickupPointRequest.setMerchantCode(MERCHANT_CODE);
    autoCreatePickupPointRequest.setWebItemSku(ITEM_SKU);
    autoCreatePickupPointRequestList = new AutoCreatePickupPointRequestList();
    autoCreatePickupPointRequestList.getAutoCreatePickupPointRequests().add(autoCreatePickupPointRequest);

    pickupPointDetailsDTOS = new ArrayList<>();
    pickupPointDetailsDTOS.add(new PickupPointDetailsDTO(MERCHANT_CODE, PICKUP_POINT_CODE));
    pickupPointDetailsDTOS.add(new PickupPointDetailsDTO(MERCHANT_CODE, PICKUP_POINT_CODE_2));
    pickupPointDetailsDTOS.add(new PickupPointDetailsDTO(MERCHANT_CODE_2, PICKUP_POINT_CODE_3));

    eanUpcPickupPointCodeRequest = new EanUpcPickupPointCodeRequest();
    eanUpcPickupPointCodeRequest.setEanUpcCode(EAN_UPC);
    eanUpcPickupPointCodeRequest.setPickupPointDetails(pickupPointDetailsDTOS);
    switchContext.setUsePcbMasterData(true);
    Mockito.when(dataSourcePrimaryEnabledProperties.isUpdateItemPickupPointReadFromPrimaryEnabled()).thenReturn(false);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.itemPickupPointRepository);
    Mockito.verifyNoMoreInteractions(this.itemPickupPointMigrationService);
    Mockito.verifyNoMoreInteractions(this.systemParameterService);
    Mockito.verifyNoMoreInteractions(this.itemService);
    Mockito.verifyNoMoreInteractions(this.itemPickupPointService);
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.catalogService);
    Mockito.verifyNoMoreInteractions(this.productHelperService);
    Mockito.verifyNoMoreInteractions(this.businessPartnerPickupPointService);
    Mockito.verifyNoMoreInteractions(this.itemPickupPointSummaryService);
    Mockito.verifyNoMoreInteractions(this.kafkaProducer);
    Mockito.verifyNoMoreInteractions(this.saveOperationService);
    Mockito.verifyNoMoreInteractions(this.objectConverterService);
    Mockito.verifyNoMoreInteractions(this.productCacheableService);
    Mockito.verifyNoMoreInteractions(this.itemPickupPointSummaryServiceImpl);
    Mockito.verifyNoMoreInteractions(this.xbpOutbound);
    Mockito.verifyNoMoreInteractions(this.inventoryOutbound);
    Mockito.verifyNoMoreInteractions(this.dataSourcePrimaryEnabledProperties);
    Mockito.verifyNoMoreInteractions(this.dataSourceWrapperService, this.itemPriceService);
  }

  @Test
  public void migrateItemPickupPointCollectionTest(){
    itemPickupPointWrapperService.migrateItemPickupPointCollection(STORE_ID, null, STATUS);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_MAX_ITEMS_TO_PUBLISH);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_PAGE_SIZE_FOR_ITEMS);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_EVENT_PAYLOAD_SIZE);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MIGRATION_EVENT_PUBLISH_WAIT_TIME);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_PAGE_FETCH_WAIT_TIME);
    Mockito.verify(this.itemPickupPointMigrationService, times(2)).findByStatusAndLimit(STATUS,
        PAGE_SIZE);
    Mockito.verify(this.saveAndPublishService, times(2))
        .publishItemsForMigration(Collections.singletonList(ITEM_SKU));
    Mockito.verify(this.itemPickupPointMigrationService, times(2))
        .saveCollection(itemPickupPointMigrationListArgumentCaptor.capture());
    Assertions.assertEquals(ItemMigrationStatus.PUBLISHED.name(),
        itemPickupPointMigrationListArgumentCaptor.getValue().get(0).getStatus());
  }

  @Test
  public void migrateItemPickupPointCollection_withItemSkuTest() {
    Mockito.when(this.itemPickupPointMigrationService.findByItemSku(ITEM_SKU))
        .thenReturn(itemPickupPointMigration);
    itemPickupPointWrapperService.migrateItemPickupPointCollection(STORE_ID, ITEM_SKU, STATUS);
    Mockito.verify(this.itemPickupPointMigrationService).findByItemSku(ITEM_SKU);
    Mockito.verify(this.saveAndPublishService)
        .publishItemsForMigration(Collections.singletonList(ITEM_SKU));
    Mockito.verify(this.itemPickupPointMigrationService)
        .saveCollection(itemPickupPointMigrationListArgumentCaptor.capture());
    Assertions.assertEquals(ItemMigrationStatus.PUBLISHED.name(),
        itemPickupPointMigrationListArgumentCaptor.getValue().get(0).getStatus());
  }

  @Test
  public void migrateItemPickupPointCollection_withItemSku_nullMigrationTest() {
    Mockito.when(this.itemPickupPointMigrationService.findByItemSku(ITEM_SKU))
        .thenReturn(null);
    itemPickupPointWrapperService.migrateItemPickupPointCollection(STORE_ID, ITEM_SKU, STATUS);
    Mockito.verify(this.itemPickupPointMigrationService).findByItemSku(ITEM_SKU);
    Mockito.verify(this.saveAndPublishService)
        .publishItemsForMigration(Collections.singletonList(ITEM_SKU));
    Mockito.verify(this.itemPickupPointMigrationService)
        .saveCollection(itemPickupPointMigrationListArgumentCaptor.capture());
    Assertions.assertEquals(ItemMigrationStatus.PUBLISHED.name(),
        itemPickupPointMigrationListArgumentCaptor.getValue().get(0).getStatus());
  }

  @Test
  public void migrateItemPickupPointCollection_disableApiTest() {
    maxItemsSystemParameter.setValue(String.valueOf(0));
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MIGRATION_MAX_ITEMS_TO_PUBLISH)).thenReturn(maxItemsSystemParameter);
    itemPickupPointWrapperService.migrateItemPickupPointCollection(STORE_ID, null, STATUS);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_MAX_ITEMS_TO_PUBLISH);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_PAGE_SIZE_FOR_ITEMS);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_EVENT_PAYLOAD_SIZE);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MIGRATION_EVENT_PUBLISH_WAIT_TIME);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_PAGE_FETCH_WAIT_TIME);
  }

  @Test
  public void migrateItemPickupPointCollection_emptyResultTest() {
    Mockito.when(this.itemPickupPointMigrationService.findByStatusAndLimit(STATUS, PAGE_SIZE))
        .thenReturn(Collections.emptyList());
    itemPickupPointWrapperService.migrateItemPickupPointCollection(STORE_ID, null, STATUS);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_MAX_ITEMS_TO_PUBLISH);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_PAGE_SIZE_FOR_ITEMS);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_EVENT_PAYLOAD_SIZE);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MIGRATION_EVENT_PUBLISH_WAIT_TIME);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_PAGE_FETCH_WAIT_TIME);
    Mockito.verify(this.itemPickupPointMigrationService).findByStatusAndLimit(STATUS, PAGE_SIZE);
  }

  @Test
  public void migrateItemPickupPointCollection_exceptionTest() {
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MIGRATION_MAX_ITEMS_TO_PUBLISH)).thenThrow(RuntimeException.class);
    itemPickupPointWrapperService.migrateItemPickupPointCollection(STORE_ID, null, STATUS);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MIGRATION_MAX_ITEMS_TO_PUBLISH);
  }

  @Test
  public void processItemMigrationEventTest() {
    this.itemPickupPointWrapperService.processItemMigrationEvent(itemPickupPointMigrationEvent);
    Mockito.verify(this.itemPickupPointMigrationService).updateStatusByItemSku(
        itemPickupPointMigrationEvent.getItemSkuList(), ItemMigrationStatus.IN_PROGRESS.name());
    Mockito.verify(this.itemService).getItemsByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID,
        Collections.singleton(ITEM_SKU));
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPoint(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(this.itemPickupPointMigrationService)
        .updateStatusByItemSku(Collections.singletonList(ITEM_SKU),
            ItemMigrationStatus.COMPLETED.name());
  }

  @Test
  public void processItemMigrationEvent_constraintViolationExceptionTest() {
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(this.itemPickupPointService
        .saveItemPickupPoint(any(ItemPickupPoint.class))).thenThrow(
        ConstraintViolationException.class).thenReturn(itemPickupPoint);
    this.itemPickupPointWrapperService.processItemMigrationEvent(itemPickupPointMigrationEvent);
    Mockito.verify(this.itemService).getItemsByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID,
        Collections.singleton(ITEM_SKU));
    Mockito.verify(this.itemPickupPointService, times(2))
        .saveItemPickupPoint(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(this.itemPickupPointMigrationService)
        .updateStatusByItemSku(Collections.singletonList(ITEM_SKU),
            ItemMigrationStatus.COMPLETED_WITH_CONFLICT.name());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointMigrationService).updateStatusByItemSku(
        itemPickupPointMigrationEvent.getItemSkuList(), ItemMigrationStatus.IN_PROGRESS.name());
  }

  @Test
  public void processItemMigrationEvent_constraintViolationAndExceptionTest() {
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
        ErrorCategory.UNSPECIFIED.getMessage()));
    Mockito.when(this.itemPickupPointService
        .saveItemPickupPoint(any(ItemPickupPoint.class))).thenThrow(
        ConstraintViolationException.class);
    this.itemPickupPointWrapperService.processItemMigrationEvent(itemPickupPointMigrationEvent);
    Mockito.verify(this.itemService).getItemsByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID,
        Collections.singleton(ITEM_SKU));
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPoint(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(this.itemPickupPointMigrationService).updateFailedStatusByItemSku(
        Collections.singletonMap(ITEM_SKU,
            ErrorCategory.UNSPECIFIED.getMessage() + ErrorCategory.UNSPECIFIED.getMessage()));
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointMigrationService).updateStatusByItemSku(
        itemPickupPointMigrationEvent.getItemSkuList(), ItemMigrationStatus.IN_PROGRESS.name());
  }

  @Test
  public void processItemMigrationEvent_exceptionTest() {
    Mockito.when(
            this.itemPickupPointService.saveItemPickupPoint(any(ItemPickupPoint.class)))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED,
            ErrorCategory.UNSPECIFIED.getMessage()));
    this.itemPickupPointWrapperService.processItemMigrationEvent(itemPickupPointMigrationEvent);
    Mockito.verify(this.itemService).getItemsByStoreIdAndItemSkus(Constants.DEFAULT_STORE_ID,
        Collections.singleton(ITEM_SKU));
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPoint(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(this.itemPickupPointMigrationService).updateFailedStatusByItemSku(
        Collections.singletonMap(ITEM_SKU,
            ErrorCategory.UNSPECIFIED.getMessage() + ErrorCategory.UNSPECIFIED.getMessage()));
    Mockito.verify(this.itemPickupPointMigrationService).updateStatusByItemSku(
        itemPickupPointMigrationEvent.getItemSkuList(), ItemMigrationStatus.IN_PROGRESS.name());
  }

  @Test
  public void updateItemPickupPointOnItemChangeTest() throws Exception {
    Mockito.when(this.itemPickupPointMigrationService.findByItemSku(itemChange.getItemSku()))
        .thenReturn(itemPickupPointMigration);
    Mockito.when(
            this.itemPickupPointService.findByItemSkuAndDelivery(
                itemChange.getStoreId(), ITEM_SKU))
        .thenReturn(deliveryItemPickupPoint);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    this.itemPickupPointWrapperService.updateItemPickupPointOnItemChange(itemChange);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(itemChange.getStoreId(), ITEM_SKU);
    Mockito.verify(this.itemPickupPointMigrationService).findByItemSku(itemChange.getItemSku());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPointCollection(itemPickupPointListArgumentCaptor.capture());
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ITEM_CHANGE_SWITCH);
    Assertions.assertNotNull(itemPickupPointListArgumentCaptor.getValue());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        itemPickupPointListArgumentCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertTrue(itemPickupPointListArgumentCaptor.getValue().get(0).isDelivery());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertTrue(itemPickupPointListArgumentCaptor.getValue().get(1).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(1).isDelivery());
  }

  @Test
  public void updateItemPickupPointOnItemChange_deliveryAndCncTrueTest() throws Exception {
    deliveryItemPickupPoint.setCncActive(true);
    Mockito.when(this.itemPickupPointMigrationService.findByItemSku(itemChange.getItemSku()))
        .thenReturn(itemPickupPointMigration);
    Mockito.when(
            this.itemPickupPointService.findByItemSkuAndDelivery(
                itemChange.getStoreId(), ITEM_SKU))
        .thenReturn(deliveryItemPickupPoint);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    this.itemPickupPointWrapperService.updateItemPickupPointOnItemChange(itemChange);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(itemChange.getStoreId(), ITEM_SKU);
    Mockito.verify(this.itemPickupPointMigrationService).findByItemSku(itemChange.getItemSku());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPointCollection(itemPickupPointListArgumentCaptor.capture());
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ITEM_CHANGE_SWITCH);
    Assertions.assertNotNull(itemPickupPointListArgumentCaptor.getValue());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        itemPickupPointListArgumentCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertTrue(itemPickupPointListArgumentCaptor.getValue().get(0).isDelivery());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(1).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(1).isDelivery());
  }

  @Test
  public void updateItemPickupPointOnItemChange_switchOffTest() throws Exception {
    itemChangeSwitch.setValue(String.valueOf(Boolean.FALSE));
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ITEM_CHANGE_SWITCH)).thenReturn(itemChangeSwitch);
    this.itemPickupPointWrapperService.updateItemPickupPointOnItemChange(itemChange);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ITEM_CHANGE_SWITCH);
  }

  @Test
  public void updateItemPickupPointOnItemChange_deliveryAndCncTrue_samePickupPointCodeTest() throws Exception {
    deliveryItemPickupPoint.setCncActive(true);
    deliveryItemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(this.itemPickupPointMigrationService.findByItemSku(itemChange.getItemSku()))
        .thenReturn(itemPickupPointMigration);
    Mockito.when(
            this.itemPickupPointService.findByItemSkuAndDelivery(
                itemChange.getStoreId(), ITEM_SKU))
        .thenReturn(deliveryItemPickupPoint);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    this.itemPickupPointWrapperService.updateItemPickupPointOnItemChange(itemChange);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(itemChange.getStoreId(), ITEM_SKU);
    Mockito.verify(this.itemPickupPointMigrationService).findByItemSku(itemChange.getItemSku());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ITEM_CHANGE_SWITCH);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPointCollection(itemPickupPointListArgumentCaptor.capture());
    Assertions.assertNotNull(itemPickupPointListArgumentCaptor.getValue());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        itemPickupPointListArgumentCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertTrue(itemPickupPointListArgumentCaptor.getValue().get(0).isDelivery());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(1).isMarkForDelete());
    Assertions.assertTrue(itemPickupPointListArgumentCaptor.getValue().get(1).isDelivery());
  }

  @Test
  public void updateItemPickupPointOnItemChange_nullExistingL5Test() throws Exception {
    Mockito.when(this.itemPickupPointMigrationService.findByItemSku(itemChange.getItemSku()))
        .thenReturn(itemPickupPointMigration);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(null);
    Mockito.when(
            this.itemPickupPointService.findByItemSkuAndDelivery(
                itemChange.getStoreId(), ITEM_SKU))
        .thenReturn(deliveryItemPickupPoint);
    this.itemPickupPointWrapperService.updateItemPickupPointOnItemChange(itemChange);
    Mockito.verify(this.itemPickupPointMigrationService).findByItemSku(itemChange.getItemSku());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndDelivery(
        itemChange.getStoreId(), itemChange.getItemSku());
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ITEM_CHANGE_SWITCH);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPointCollection(itemPickupPointListArgumentCaptor.capture());
    Assertions.assertNotNull(itemPickupPointListArgumentCaptor.getValue().get(0));
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListArgumentCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertTrue(itemPickupPointListArgumentCaptor.getValue().get(0).isDelivery());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        itemPickupPointListArgumentCaptor.getValue().get(0).getPickupPointCode());
  }

  @Test
  public void updateItemPickupPointOnItemChange_nullExistingL5_cncActiveTest() throws Exception {
    deliveryItemPickupPoint.setCncActive(true);
    Mockito.when(this.itemPickupPointMigrationService.findByItemSku(itemChange.getItemSku()))
        .thenReturn(itemPickupPointMigration);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(null);
    Mockito.when(
            this.itemPickupPointService.findByItemSkuAndDelivery(
                itemChange.getStoreId(), ITEM_SKU))
        .thenReturn(deliveryItemPickupPoint);
    this.itemPickupPointWrapperService.updateItemPickupPointOnItemChange(itemChange);
    Mockito.verify(this.itemPickupPointMigrationService).findByItemSku(itemChange.getItemSku());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndDelivery(
        itemChange.getStoreId(), itemChange.getItemSku());
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPointCollection(itemPickupPointListArgumentCaptor.capture());
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ITEM_CHANGE_SWITCH);
    Assertions.assertNotNull(itemPickupPointListArgumentCaptor.getValue().get(0));
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListArgumentCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertTrue(itemPickupPointListArgumentCaptor.getValue().get(0).isDelivery());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(0).isMarkForDelete());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        itemPickupPointListArgumentCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListArgumentCaptor.getValue().get(1).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE_2,
        itemPickupPointListArgumentCaptor.getValue().get(1).getPickupPointCode());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(1).isDelivery());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(1).isMarkForDelete());
  }

  @Test
  public void updateItemPickupPointOnItemChange_nullExistingL5_emptyPickupPointTest() throws Exception {
    itemChange.setPickupPointCode(StringUtils.EMPTY);
    Mockito.when(this.itemPickupPointMigrationService.findByItemSku(itemChange.getItemSku()))
        .thenReturn(itemPickupPointMigration);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        StringUtils.EMPTY)).thenReturn(null);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndDelivery(
        itemChange.getStoreId(), itemChange.getItemSku())).thenReturn(deliveryItemPickupPoint);
    this.itemPickupPointWrapperService.updateItemPickupPointOnItemChange(itemChange);
    Mockito.verify(this.itemPickupPointMigrationService).findByItemSku(itemChange.getItemSku());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        StringUtils.EMPTY);
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndDelivery(
        itemChange.getStoreId(), itemChange.getItemSku());
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPointCollection(itemPickupPointListArgumentCaptor.capture());
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ITEM_CHANGE_SWITCH);
    Assertions.assertNotNull(itemPickupPointListArgumentCaptor.getValue().get(0));
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(StringUtils.EMPTY,
        itemPickupPointListArgumentCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertTrue(itemPickupPointListArgumentCaptor.getValue().get(0).isDelivery());
    Assertions.assertFalse(itemPickupPointListArgumentCaptor.getValue().get(0).isMarkForDelete());
  }

  @Test
  public void updateItemPickupPointOnItemChange_pendingMigrationL5Test() throws Exception {
    itemPickupPointMigration.setStatus(ItemMigrationStatus.PENDING.name());
    Mockito.when(this.itemPickupPointMigrationService.findByItemSku(itemChange.getItemSku()))
        .thenReturn(itemPickupPointMigration);
    this.itemPickupPointWrapperService.updateItemPickupPointOnItemChange(itemChange);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ITEM_CHANGE_SWITCH);
    Mockito.verify(this.itemPickupPointMigrationService).findByItemSku(itemChange.getItemSku());
  }

  @Test
  public void updateItemPickupPointOnItemChange_nullMigrationL5Test() throws Exception {
    Mockito.when(this.itemPickupPointMigrationService.findByItemSku(itemChange.getItemSku()))
        .thenReturn(null);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(
            this.itemPickupPointService.findByItemSkuAndDelivery(
                itemChange.getStoreId(), ITEM_SKU))
        .thenReturn(deliveryItemPickupPoint);
    this.itemPickupPointWrapperService.updateItemPickupPointOnItemChange(itemChange);
    Mockito.verify(this.itemPickupPointMigrationService).findByItemSku(itemChange.getItemSku());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ITEM_CHANGE_SWITCH);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(itemChange.getStoreId(), ITEM_SKU);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPointCollection(itemPickupPointListArgumentCaptor.capture());
    Assertions.assertNotNull(itemPickupPointListArgumentCaptor.getValue());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListArgumentCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        itemPickupPointListArgumentCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertTrue(itemPickupPointListArgumentCaptor.getValue().get(0).isDelivery());
  }

  @Test
  public void updateItemPickupPointOnOfflineItemChangeTest() {
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    this.itemPickupPointWrapperService.updateItemPickupPointOnOfflineItemChange(offlineItemChange);
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.OFFLINE_ITEM_CHANGE_SWITCH);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPoint(itemPickupPointArgumentCaptor.capture());
    Assertions.assertEquals(
        itemPickupPointArgumentCaptor.getValue().getPrice().stream().findFirst().get().getOfferPrice(), OFFER_PRICE, 0);
    Assertions.assertEquals(
        itemPickupPointArgumentCaptor.getValue().getPrice().stream().findFirst().get().getListPrice(), LIST_PRICE, 0);
    Assertions.assertEquals(ITEM_SKU, itemPickupPointArgumentCaptor.getValue().getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE,
        itemPickupPointArgumentCaptor.getValue().getPickupPointCode());
  }

  @Test
  public void updateItemPickupPointOnOfflineItemChange_mfdTrueTest() {
    offlineItemChange.setMarkForDelete(true);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    this.itemPickupPointWrapperService.updateItemPickupPointOnOfflineItemChange(offlineItemChange);
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPoint(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.OFFLINE_ITEM_CHANGE_SWITCH);
    Assertions.assertTrue(itemPickupPointArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
  public void updateItemPickupPointOnOfflineItemChange_mfdTrueDeliveryTest() {
    itemPickupPoint.setDelivery(true);
    offlineItemChange.setMarkForDelete(true);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    this.itemPickupPointWrapperService.updateItemPickupPointOnOfflineItemChange(offlineItemChange);
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPoint(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.OFFLINE_ITEM_CHANGE_SWITCH);
    Assertions.assertFalse(itemPickupPointArgumentCaptor.getValue().isMarkForDelete());
    Assertions.assertFalse(itemPickupPointArgumentCaptor.getValue().isCncActive());
  }

  @Test
  public void updateItemPickupPointOnOfflineItemChange_nullExistingL5Test() {
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(null);
    this.itemPickupPointWrapperService.updateItemPickupPointOnOfflineItemChange(offlineItemChange);
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPoint(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.OFFLINE_ITEM_CHANGE_SWITCH);
    Assertions.assertTrue(itemPickupPointArgumentCaptor.getValue().isCncActive());
    Assertions.assertFalse(itemPickupPointArgumentCaptor.getValue().isDelivery());
    Assertions.assertTrue(
        CollectionUtils.isEmpty(itemPickupPointArgumentCaptor.getValue().getActivePromoBundlings()));
  }

  @Test
  public void updateItemPickupPointOnOfflineItemChange_nullExistingL5mfdTrueTest() {
    offlineItemChange.setMarkForDelete(true);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(null);
    this.itemPickupPointWrapperService.updateItemPickupPointOnOfflineItemChange(offlineItemChange);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.OFFLINE_ITEM_CHANGE_SWITCH);
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
  }

  @Test
  public void updateItemPickupPointOnOfflineItemChange_switchOffTest() {
    offlineItemChangeSwitch.setValue(String.valueOf(Boolean.FALSE));
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.OFFLINE_ITEM_CHANGE_SWITCH)).thenReturn(offlineItemChangeSwitch);
    this.itemPickupPointWrapperService.updateItemPickupPointOnOfflineItemChange(offlineItemChange);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.OFFLINE_ITEM_CHANGE_SWITCH);
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeTest() {
    Mockito.when(this.itemPickupPointService.findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID,
        MERCHANT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    this.itemPickupPointWrapperService.updateMarkForDeleteByMerchantCode(STORE_ID, MERCHANT_CODE);
    Mockito.verify(this.itemPickupPointService).findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID,
        MERCHANT_CODE);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPointCollection(Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void updateMarkForDeleteByMerchantCode_cncActiveTest() {
    itemPickupPoint.setCncActive(true);
    Mockito.when(this.itemPickupPointService.findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID,
        MERCHANT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    this.itemPickupPointWrapperService.updateMarkForDeleteByMerchantCode(STORE_ID, MERCHANT_CODE);
    Mockito.verify(this.itemPickupPointService).findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID,
        MERCHANT_CODE);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPointCollection(Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void updateMarkForDeleteByMerchantCode_cncActive_deliveryTest() {
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setDelivery(true);
    Mockito.when(this.itemPickupPointService.findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID,
        MERCHANT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    this.itemPickupPointWrapperService.updateMarkForDeleteByMerchantCode(STORE_ID, MERCHANT_CODE);
    Mockito.verify(this.itemPickupPointService).findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID,
        MERCHANT_CODE);
    Mockito.verify(this.itemPickupPointService)
        .saveItemPickupPointCollection(Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void findProductForTransactionByItemSkusAndPickupPointCodeTest() throws Exception {
    product.setSynchronized(true);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2, Arrays.asList(itemCatalogVO)));
    when(
        productHelperService.getCurrentBuyableStatusForItem(Mockito.anySet(), Mockito.anyString(),
            Mockito.anyBoolean())).thenReturn(true);
    when(productHelperService.getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(true);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID),
        any(Product.class), anyList())).thenReturn(new ProductAndItemsVO());
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(productHelperService.getSettlementType(any(Product.class), any(Item.class))).thenReturn(SETTLEMENT_TYPE);

    itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, itemPickupPointRequests);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(catalogService).getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID), anyList());
    verify(productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID),
        any(Product.class), anyList());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
    verify(productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void findProductForTransactionByItemSkusAndPickupPointCodeOverrideArchiveFlagTest() throws Exception {
    switchContext.setUsePcbMasterData(true);
    ReflectionTestUtils.setField(this.itemPickupPointWrapperService, "overrideArchiveFlagFromL3", true);
    product.setSynchronized(true);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCT_SKU))).thenReturn(Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2, Arrays.asList(itemCatalogVO)));
    when(
        productHelperService.getCurrentBuyableStatusForItem(Mockito.anySet(), Mockito.anyString(),
            Mockito.anyBoolean())).thenReturn(true);
    when(productHelperService.getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(true);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID),
        any(Product.class), anyList())).thenReturn(new ProductAndItemsVO());
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(productHelperService.getSettlementType(any(Product.class), any(Item.class))).thenReturn(
        SETTLEMENT_TYPE);
    itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME,
        itemPickupPointRequests);
    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(catalogService).getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList());
    verify(productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(eq(STORE_ID), eq(USERNAME),
        eq(REQUEST_ID), any(Product.class), anyList());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
    verify(productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void findProductForTransactionByItemSkusAndPickupPointCodeOverrideArchiveFlag2Test() throws Exception {
    ReflectionTestUtils.setField(this.itemPickupPointWrapperService, "overrideArchiveFlagFromL3", true);
    product.setSynchronized(true);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    product.setProductSku(ITEM_SKU);
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCT_SKU))).thenReturn(Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2, Arrays.asList(itemCatalogVO)));
    when(
        productHelperService.getCurrentBuyableStatusForItem(Mockito.anySet(), Mockito.anyString(),
            Mockito.anyBoolean())).thenReturn(true);
    when(productHelperService.getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(true);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID),
        any(Product.class), anyList())).thenReturn(new ProductAndItemsVO());
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(productHelperService.getSettlementType(any(Product.class), any(Item.class))).thenReturn(
        SETTLEMENT_TYPE);
    itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME,
        itemPickupPointRequests);
    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(catalogService).getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList());
    verify(productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(eq(STORE_ID), eq(USERNAME),
        eq(REQUEST_ID), any(Product.class), any());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
    verify(productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void findProductForTransactionByItemSkusAndPickupPointCodeWithMasterDataTest() throws Exception {
    product.setCategoryCode(CATEGORY_CODE_2);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2, Arrays.asList(itemCatalogVO)));
    when(
        productHelperService.getCurrentBuyableStatusForItem(Mockito.anySet(), Mockito.anyString(),
            Mockito.anyBoolean())).thenReturn(true);
    when(productHelperService.getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(productHelperService.getSettlementType(any(Product.class), any(Item.class))).thenReturn(SETTLEMENT_TYPE);

    itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, itemPickupPointRequests);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(catalogService).getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID), anyList());
    verify(productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
    verify(productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void findProductForTransactionByItemSkusAndPickupPointCodeWithIgnoreSyncFlagTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "ignoreSyncFlagWhileSettingCategoryCode", true);
    product.setCategoryCode(CATEGORY_CODE_2);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2, Arrays.asList(itemCatalogVO)));
    when(
        productHelperService.getCurrentBuyableStatusForItem(Mockito.anySet(), Mockito.anyString(),
            Mockito.anyBoolean())).thenReturn(true);
    when(productHelperService.getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(productHelperService.getSettlementType(any(Product.class), any(Item.class))).thenReturn(SETTLEMENT_TYPE);

    itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, itemPickupPointRequests);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(catalogService).getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID), anyList());
    verify(productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
    verify(productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void findProductForTransactionByItemSkusAndPickupPointCodeNoCategoryTest() throws Exception {
    item.setItemSku(ITEM_SKU_2);
    product.setMasterCatalog(null);
    product.setMasterDataProduct(null);
    product.setItemCatalogs(Arrays.asList(itemCatalogVO));

    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(productHelperService.getSettlementType(any(Product.class), any(Item.class))).thenReturn(SETTLEMENT_TYPE);

    itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, itemPickupPointRequests);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
    verify(productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void findProductForTransactionByItemSkusAndPickupPointCodeUnSyncProductTest() throws Exception {
    item.setItemSku(ITEM_SKU_2);
    product.setMasterCatalog(null);
    product.setMasterDataProduct(null);
    product.setItemCatalogs(Arrays.asList(itemCatalogVO));
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());
    when(productHelperService.getSettlementType(any(Product.class), any(Item.class))).thenReturn(SETTLEMENT_TYPE);


    itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, itemPickupPointRequests);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
    verify(productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void findItemSummaryByItemSkusAndPickupPointCodeTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 0);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    ReflectionTestUtils.setField(itemPickupPointWrapperService,"listOfClientsToExcludeItemCatalogs",Constants.DEFAULT_CHANNEL);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    itemPickupPoint1.setPrice(Collections.singleton(new Price()));
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.B2B).build());
    itemViewConfigs.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.DEFAULT_CHANNEL).build());
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPoint2.setItemSku(ITEM_SKU);
    itemPickupPoint2.setProductSku(PRODUCT_SKU);
    product.setCurationStatus(CurationStatus.APPROVED);
    itemPickupPoint2.setItemViewConfig(Collections.singleton(
        ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.DEFAULT_CHANNEL).build()));
    Price price = new Price();
    DiscountPrice merchantPromoDiscountPrice = new DiscountPrice();
    merchantPromoDiscountPrice.setStartDateTime(new Date());
    Date date = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date futureDate = calendar.getTime();
    merchantPromoDiscountPrice.setEndDateTime(futureDate);
    price.setMerchantPromoDiscountPrice(merchantPromoDiscountPrice);
    itemPickupPoint2.setPrice(Collections.singleton(price));
    itemPickupPoint1.setPrice(Collections.singleton(price));
    List<ItemPickupPointRequest> pickupPointRequests =
        Arrays.asList(ItemPickupPointRequest.builder().pickupPointCode(PICKUP_POINT_CODE_2).itemSku(ITEM_SKU).build(),
            ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build(),
            ItemPickupPointRequest.builder().itemSku(itemPickupPoint.getItemSku())
                .pickupPointCode(itemPickupPoint.getPickupPointCode()).build());
    Mockito.when(productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean())).thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, pickupPointRequests, false)).thenReturn(
        Arrays.asList(itemPickupPoint1, itemPickupPoint2, itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU_2, PICKUP_POINT_CODE_2)).thenReturn(
        null);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    Mockito.when(itemPriceService.validMerchantPromoDiscountPrice(Mockito.any(DiscountPrice.class))).thenReturn(true)
        .thenReturn(false);
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2, Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(discountPrice);
    itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME,
        false, pickupPointRequests, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, false);
    verify(itemHelperService, times(2)).processDiscountPricesByPriority(listOfDiscountPrices);
    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, pickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(productHelperService, times(3)).getCurrentBuyableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService, times(3)).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(catalogService).getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID), anyList());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
    verify(productHelperService, times(3)).getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean());
    verify(productHelperService, times(3)).getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean());
    verify(itemPriceService, times(2)).validMerchantPromoDiscountPrice(Mockito.any(DiscountPrice.class));
  }

  @Test
  public void fetchItemSummaryByItemSkusAndPickupPointCode() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
      "itemSummaryListResponseCacheClients", List.of(Constants.DEFAULT_CLIENT_ID));
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
      "itemSummaryListRequestSize", 20);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 0);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    ReflectionTestUtils.setField(itemPickupPointWrapperService,"listOfClientsToExcludeItemCatalogs",Constants.DEFAULT_CHANNEL);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    itemPickupPoint1.setPrice(Collections.singleton(new Price()));
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.B2B).build());
    itemViewConfigs.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.DEFAULT_CHANNEL).build());
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPoint2.setItemSku(ITEM_SKU);
    itemPickupPoint2.setProductSku(PRODUCT_SKU);
    product.setCurationStatus(CurationStatus.APPROVED);
    itemPickupPoint2.setItemViewConfig(Collections.singleton(
      ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.DEFAULT_CHANNEL).build()));
    Price price = new Price();
    DiscountPrice merchantPromoDiscountPrice = new DiscountPrice();
    merchantPromoDiscountPrice.setStartDateTime(new Date());
    Date date = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date futureDate = calendar.getTime();
    merchantPromoDiscountPrice.setEndDateTime(futureDate);
    price.setMerchantPromoDiscountPrice(merchantPromoDiscountPrice);
    itemPickupPoint2.setPrice(Collections.singleton(price));
    itemPickupPoint1.setPrice(Collections.singleton(price));
    List<ItemPickupPointRequest> pickupPointRequests =
      Arrays.asList(ItemPickupPointRequest.builder().pickupPointCode(PICKUP_POINT_CODE_2).itemSku(ITEM_SKU).build(),
        ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build(),
        ItemPickupPointRequest.builder().itemSku(itemPickupPoint.getItemSku())
          .pickupPointCode(itemPickupPoint.getPickupPointCode()).build());
    Mockito.when(productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean())).thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, pickupPointRequests, false)).thenReturn(
      Arrays.asList(itemPickupPoint1, itemPickupPoint2, itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU_2, PICKUP_POINT_CODE_2)).thenReturn(
      null);
    Mockito.when(applicationContext.getBean(ItemPickupPointWrapperServiceImpl.class))
      .thenReturn(itemPickupPointWrapperService);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
      Arrays.asList(product));
    Mockito.when(itemPriceService.validMerchantPromoDiscountPrice(Mockito.any(DiscountPrice.class))).thenReturn(true)
      .thenReturn(false);
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
      anyList())).thenReturn(
      ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2, Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(discountPrice);
    itemPickupPointWrapperService.fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID,
      USERNAME,
      false, pickupPointRequests, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, true);
    verify(itemHelperService, times(2)).processDiscountPricesByPriority(listOfDiscountPrices);
    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, pickupPointRequests,
      false);
    Mockito.verify(applicationContext).getBean(ItemPickupPointWrapperServiceImpl.class);
    verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(productHelperService, times(3)).getCurrentBuyableStatusForItem(Mockito.anySet(),
      Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService, times(3)).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      Mockito.anyString(), Mockito.anyBoolean());
    verify(catalogService).getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID), anyList());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
      anyList());
    verify(productHelperService, times(3)).getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean());
    verify(productHelperService, times(3)).getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean());
    verify(itemPriceService, times(2)).validMerchantPromoDiscountPrice(Mockito.any(DiscountPrice.class));
  }

  @Test
  public void fetchItemSummaryByItemSkusAndPickupPointCodeSwitchOff() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
      "itemSummaryListResponseCacheClients", List.of(Constants.DEFAULT_CLIENT_ID));
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "itemSummaryListRequestSize", 1);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 0);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
      "listOfClientsToExcludeItemCatalogs", Constants.DEFAULT_CHANNEL);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    itemPickupPoint1.setPrice(Collections.singleton(new Price()));
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(
      ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.B2B).build());
    itemViewConfigs.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true)
      .channel(Constants.DEFAULT_CHANNEL).build());
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPoint2.setItemSku(ITEM_SKU);
    itemPickupPoint2.setProductSku(PRODUCT_SKU);
    product.setCurationStatus(CurationStatus.APPROVED);
    itemPickupPoint2.setItemViewConfig(Collections.singleton(
      ItemViewConfig.builder().isBuyable(true).isDiscoverable(true)
        .channel(Constants.DEFAULT_CHANNEL).build()));
    Price price = new Price();
    DiscountPrice merchantPromoDiscountPrice = new DiscountPrice();
    merchantPromoDiscountPrice.setStartDateTime(new Date());
    Date date = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date futureDate = calendar.getTime();
    merchantPromoDiscountPrice.setEndDateTime(futureDate);
    price.setMerchantPromoDiscountPrice(merchantPromoDiscountPrice);
    itemPickupPoint2.setPrice(Collections.singleton(price));
    itemPickupPoint1.setPrice(Collections.singleton(price));
    List<ItemPickupPointRequest> pickupPointRequests = Arrays.asList(
      ItemPickupPointRequest.builder().pickupPointCode(PICKUP_POINT_CODE_2).itemSku(ITEM_SKU)
        .build(),
      ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build(),
      ItemPickupPointRequest.builder().itemSku(itemPickupPoint.getItemSku())
        .pickupPointCode(itemPickupPoint.getPickupPointCode()).build());
    Mockito.when(productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
      .thenReturn(true);
    Mockito.when(
        productHelperService.getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean()))
      .thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
      pickupPointRequests, false)).thenReturn(
      Arrays.asList(itemPickupPoint1, itemPickupPoint2, itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU_2,
      PICKUP_POINT_CODE_2)).thenReturn(null);
    Mockito.when(applicationContext.getBean(ItemPickupPointWrapperServiceImpl.class))
      .thenReturn(itemPickupPointWrapperService);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
      Arrays.asList(item));
    when(
      productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
      Arrays.asList(product));
    Mockito.when(itemPriceService.validMerchantPromoDiscountPrice(Mockito.any(DiscountPrice.class)))
      .thenReturn(true).thenReturn(false);
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
      anyList())).thenReturn(
      ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2,
        Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(
      discountPrice);
    itemPickupPointWrapperService.fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID,
      USERNAME, false, pickupPointRequests, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, false);
    verify(itemHelperService, times(2)).processDiscountPricesByPriority(listOfDiscountPrices);
    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
      pickupPointRequests, false);
    verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(productHelperService, times(3)).getCurrentBuyableStatusForItem(Mockito.anySet(),
      Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService, times(3)).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
      Mockito.anyString(), Mockito.anyBoolean());
    verify(catalogService).getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
      anyList());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(
      eq(STORE_ID), anyList());
    verify(productHelperService, times(3)).getOriginalBuyableStatusForItem(any(),
      Mockito.anyBoolean());
    verify(productHelperService, times(3)).getOriginalDiscoverableStatusForItem(any(),
      Mockito.anyBoolean());
    verify(itemPriceService, times(2)).validMerchantPromoDiscountPrice(
      Mockito.any(DiscountPrice.class));
  }

  @Test
  public void findItemSummaryByItemSkusAndPickupPointCodeExcludeItemCatalogTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 0);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    ReflectionTestUtils.setField(itemPickupPointWrapperService,"listOfClientsToExcludeItemCatalogs",Constants.DEFAULT_CLIENT_ID);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    itemPickupPoint1.setPrice(Collections.singleton(new Price()));
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.B2B).build());
    itemViewConfigs.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.DEFAULT_CHANNEL).build());
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPoint2.setItemSku(ITEM_SKU);
    itemPickupPoint2.setProductSku(PRODUCT_SKU);
    product.setCurationStatus(CurationStatus.APPROVED);
    itemPickupPoint2.setItemViewConfig(Collections.singleton(
        ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.DEFAULT_CHANNEL).build()));
    itemPickupPoint2.setPrice(Collections.singleton(new Price()));
    List<ItemPickupPointRequest> pickupPointRequests =
        Arrays.asList(ItemPickupPointRequest.builder().pickupPointCode(PICKUP_POINT_CODE_2).itemSku(ITEM_SKU).build(),
            ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build(),
            ItemPickupPointRequest.builder().itemSku(itemPickupPoint.getItemSku())
                .pickupPointCode(itemPickupPoint.getPickupPointCode()).build());
    Mockito.when(productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean())).thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, pickupPointRequests, false)).thenReturn(
        Arrays.asList(itemPickupPoint1, itemPickupPoint2, itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU_2, PICKUP_POINT_CODE_2)).thenReturn(
        null);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2, Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(discountPrice);
    itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, false,
        pickupPointRequests, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, false);
    verify(itemHelperService, times(3)).processDiscountPricesByPriority(listOfDiscountPrices);
    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, pickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(productHelperService, times(3)).getCurrentBuyableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService, times(3)).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
    verify(productHelperService, times(3)).getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean());
    verify(productHelperService, times(3)).getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean());
  }


  @Test
  public void findItemSummaryByItemSkusAndPickupPointCodeTestForException() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 0);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "mainImageFromMainImageUrlForUnsync", false);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    List<Item> itemList = new ArrayList<>();
    item.setPermanentDelete(true);
    itemList.add(item);
    Mockito.when(productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
        .thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean()))
        .thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU_2, PICKUP_POINT_CODE_2)).thenReturn(
        null);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(eq(STORE_ID), any())).thenReturn(itemList);
    when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2, Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(discountPrice);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME,
          false, itemPickupPointRequests, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, false));
    } finally {
      verify(itemHelperService).processDiscountPricesByPriority(listOfDiscountPrices);
      verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
          itemPickupPointRequests, false);
      verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    }
  }

  @Test
  public void fetchItemSummaryByItemSkusAndPickupPointCodeTestForException() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 0);
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
      "mainImageFromMainImageUrlForUnsync", false);
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
      "itemSummaryListResponseCacheClients", List.of(Constants.CREATED_DATE));
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    List<Item> itemList = new ArrayList<>();
    item.setPermanentDelete(true);
    itemList.add(item);
    Mockito.when(productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
      .thenReturn(true);
    Mockito.when(
        productHelperService.getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean()))
      .thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
      itemPickupPointRequests, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU_2,
      PICKUP_POINT_CODE_2)).thenReturn(null);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
      Arrays.asList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(eq(STORE_ID), any()))
      .thenReturn(itemList);
    when(
      productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
      Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
      anyList())).thenReturn(
      ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2,
        Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(
      discountPrice);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class,
        () -> itemPickupPointWrapperService.fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID,
          REQUEST_ID, USERNAME, false, itemPickupPointRequests, null, Constants.DEFAULT_CLIENT_ID,
          Constants.ALL, false));
    } finally {
      verify(itemHelperService).processDiscountPricesByPriority(listOfDiscountPrices);
      verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false);
      verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    }
  }

  @Test
  public void fetchItemSummaryByItemSkusAndPickupPointCodeTestCached() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 0);
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
      "mainImageFromMainImageUrlForUnsync", false);
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
      "itemSummaryListResponseCacheClients", List.of(Constants.DEFAULT_CLIENT_ID));
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
      "itemSummaryListRequestSize", 20);

    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    List<Item> itemList = new ArrayList<>();
    item.setPermanentDelete(true);
    itemList.add(item);
    Mockito.when(productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
      .thenReturn(true);
    Mockito.when(
        productHelperService.getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean()))
      .thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
      itemPickupPointRequests, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU_2,
      PICKUP_POINT_CODE_2)).thenReturn(null);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
      Arrays.asList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(eq(STORE_ID), any()))
      .thenReturn(itemList);
    when(
      productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
      Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
      anyList())).thenReturn(
      ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2,
        Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(
      discountPrice);
    Mockito.when(applicationContext.getBean(ItemPickupPointWrapperServiceImpl.class))
      .thenReturn(itemPickupPointWrapperService);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class,
        () -> itemPickupPointWrapperService.fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID,
          REQUEST_ID, USERNAME, false, itemPickupPointRequests, null, Constants.DEFAULT_CLIENT_ID,
          Constants.ALL, false));
    } finally {
      verify(itemHelperService).processDiscountPricesByPriority(listOfDiscountPrices);
      Mockito.verify(applicationContext).getBean(ItemPickupPointWrapperServiceImpl.class);
      verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false);
      verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    }
  }

  @Test
  public void findItemSummaryByDuplicateItemSkusAndPickupPointCodeTest()
      throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 2);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    List<Item> itemList = new ArrayList<>();
    item.setPermanentDelete(true);
    itemList.add(item);
    Mockito.when(
            productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
        .thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(),
        Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    ItemPickupPointRequest itemPickupPointRequest1 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    itemPickupPointRequestList =
        Arrays.asList(itemPickupPointRequest1, itemPickupPointRequest1);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(eq(STORE_ID), any()))
        .thenReturn(itemList);
    when(
        productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2,
            Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(
        discountPrice);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID,
        REQUEST_ID, USERNAME, false, itemPickupPointRequestList, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL,
          false));
    } finally {
      verify(itemHelperService).processDiscountPricesByPriority(listOfDiscountPrices);
      verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
          itemPickupPointRequestList, false);
      verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    }
  }

  @Test
  public void findItemSummaryBySameItemSkusAndDifferentPickupPointCodeTest()
      throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 2);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    List<Item> itemList = new ArrayList<>();
    item.setPermanentDelete(true);
    itemList.add(item);
    Mockito.when(
            productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
        .thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(),
        Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    ItemPickupPointRequest itemPickupPointRequest1 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    ItemPickupPointRequest itemPickupPointRequest2 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE_2);
    itemPickupPointRequestList =
        Arrays.asList(itemPickupPointRequest1, itemPickupPointRequest2);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(eq(STORE_ID), any()))
        .thenReturn(itemList);
    when(
        productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2,
            Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(
        discountPrice);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID,
        REQUEST_ID, USERNAME, false, itemPickupPointRequestList, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL,
          false));
    } finally {
      verify(itemHelperService).processDiscountPricesByPriority(listOfDiscountPrices);
      verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
          itemPickupPointRequestList, false);
      verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    }
  }

  @Test
  public void findItemSummaryBySameItemSkusAndDifferentPickupPointCodeWithSingleRequest()
      throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 2);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    List<Item> itemList = new ArrayList<>();
    item.setPermanentDelete(true);
    itemList.add(item);
    Mockito.when(
            productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
        .thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(),
        Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    ItemPickupPointRequest itemPickupPointRequest1 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    ItemPickupPointRequest itemPickupPointRequest2 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE_2);
    itemPickupPointRequestList =
        Arrays.asList(itemPickupPointRequest1);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(eq(STORE_ID), any()))
        .thenReturn(itemList);
    when(
        productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2,
            Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(
        discountPrice);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, false,
          itemPickupPointRequestList, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, false));
    } finally {
      verify(itemHelperService).processDiscountPricesByPriority(listOfDiscountPrices);
      verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID,
          ITEM_SKU, PICKUP_POINT_CODE);
      verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    }
  }

  @Test()
  public void findItemSummaryBySameItemSkusAndDifferentPickupPointCodeWithSingleRequestForDeletedItem()
      throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 2);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    List<Item> itemList = new ArrayList<>();
    item.setPermanentDelete(true);
    itemList.add(item);
    itemPickupPoint.setMarkForDelete(true);
    Mockito.when(
            productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
        .thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(),
        Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    ItemPickupPointRequest itemPickupPointRequest1 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    ItemPickupPointRequest itemPickupPointRequest2 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE_2);
    itemPickupPointRequestList =
        Arrays.asList(itemPickupPointRequest1);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(eq(STORE_ID), any()))
        .thenReturn(itemList);
    when(
        productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2,
            Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(
        discountPrice);
    itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, false,
        itemPickupPointRequestList, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, false);
      verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE);
  }

  @Test()
  public void findItemSummaryBySameItemSkusAndDifferentPickupPointCodeWithSingleRequestForDeletedItemNullTest()
      throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 2);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    List<Item> itemList = new ArrayList<>();
    item.setPermanentDelete(true);
    itemList.add(item);
    itemPickupPoint.setMarkForDelete(true);
    Mockito.when(
            productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
        .thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(),
        Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    ItemPickupPointRequest itemPickupPointRequest1 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    ItemPickupPointRequest itemPickupPointRequest2 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE_2);
    itemPickupPointRequestList =
        Arrays.asList(itemPickupPointRequest1);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(null);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(eq(STORE_ID), any()))
        .thenReturn(itemList);
    when(
        productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2,
            Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(
        discountPrice);
    List<ItemSummaryListResponse> itemSummaryListResponseList =  itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID,
        REQUEST_ID, USERNAME, false, itemPickupPointRequestList, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL,
        false);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE);
    Assertions.assertEquals(Collections.EMPTY_LIST,itemSummaryListResponseList);
  }


  @Test
  public void findItemSummaryByDifferentItemSkusAndSamePickupPointCodeTest()
      throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 2);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    List<Item> itemList = new ArrayList<>();
    item.setPermanentDelete(true);
    itemList.add(item);
    Mockito.when(
            productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
        .thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(),
        Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    ItemPickupPointRequest itemPickupPointRequest1 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    ItemPickupPointRequest itemPickupPointRequest2 =
        new ItemPickupPointRequest(ITEM_SKU_2, PICKUP_POINT_CODE);
    itemPickupPointRequestList =
        Arrays.asList(itemPickupPointRequest1, itemPickupPointRequest2);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(eq(STORE_ID), any()))
        .thenReturn(itemList);
    when(
        productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2,
            Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(
        discountPrice);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, false,
          itemPickupPointRequestList, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, false));
    } finally {
      verify(itemHelperService).processDiscountPricesByPriority(listOfDiscountPrices);
      verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
          itemPickupPointRequestList, false);
      verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    }
  }

  @Test
  public void findItemSummaryBySingleItemSkusAndPickupPointCodeTest()
      throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 1);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    List<Item> itemList = new ArrayList<>();
    item.setPermanentDelete(true);
    itemList.add(item);
    Mockito.when(
            productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean()))
        .thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(),
        Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    ItemPickupPointRequest itemPickupPointRequest1 =
        new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    itemPickupPointRequestList =
        Arrays.asList(itemPickupPointRequest1);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequestList, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(eq(STORE_ID), any()))
        .thenReturn(itemList);
    when(
        productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2,
            Arrays.asList(itemCatalogVO)));
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(
        discountPrice);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, false,
          itemPickupPointRequestList, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, false));
    } finally {
      verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
          itemPickupPointRequestList, false);
      verify(itemHelperService).processDiscountPricesByPriority(listOfDiscountPrices);
      verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    }
  }

  @Test
  public void findItemSummaryWhenPickupPointListIsEmptyTest()
      throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    itemPickupPointRequestList = new ArrayList<>();
    try {
      itemPickupPointWrapperService.findItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, false,
          itemPickupPointRequestList, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, false);
    } finally {
      verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
          itemPickupPointRequestList, false);
    }
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusTest() throws Exception {
    this.itemPickupPointWrapperService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID,
        PRODUCT_SKU, itemPickupPointViewConfigBaseRequest);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
            itemPickupPointViewConfigBaseRequest);
  }


  @Test
  public void updateItemPickupPointViewConfigWithProductStatusItemPickupPointViewConfigBaseRequestNullTest()
      throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointWrapperService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID,
        PRODUCT_SKU, null));
  }

  @Test
  public void updateItemPickupPointTest() throws Exception {
    itemPickupPointWrapperService.updateItemPickupPoint(null, new ItemPickupPointUpdateRequestVo(),
        editProductDetailDTO);
    Mockito.verify(itemPickupPointSummaryService)
        .updateItemPickupPoint(eq(null), any(), anyMap(), eq(false), eq(false));
    Mockito.verify(dataSourcePrimaryEnabledProperties).isUpdateItemPickupPointReadFromPrimaryEnabled();
  }

  @Test
  public void updateItemPickupPointAddDeleteVariantTest() throws Exception {
    Set<BundleRecipe> bundleRecipeSet = new HashSet<>();
    BundleRecipe bundleRecipe = new BundleRecipe();
    bundleRecipe.setItemSku(ITEM_SKU_2);
    bundleRecipeSet.add(bundleRecipe);
    item.setBundleRecipe(bundleRecipeSet);
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo = new ItemPickupPointUpdateRequestVo();
    itemPickupPointUpdateRequestVo.setProductSku(PRODUCT_SKU);
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addVariantRequestVo.setItemSku(ITEM_SKU_2);
    addDeleteVariantRequestVo.setAddVariantsList(Collections.singletonList(addVariantRequestVo));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    BundleRecipeRequest bundleRecipeRequest = new BundleRecipeRequest();
    bundleRecipeRequest.setItemSku(ITEM_SKU);
    Set<BundleRecipeVo> bundleRecipeVoSet = new HashSet<>();
    BundleRecipeVo bundleRecipeVo = new BundleRecipeVo();
    bundleRecipeVo.setItemSku(ITEM_SKU);
    bundleRecipeVoSet.add(bundleRecipeVo);
    bundleRecipeRequest.setBundleRecipe(bundleRecipeVoSet);
    itemPickupPointUpdateRequestVo.setBundleRecipeRequests(Collections.singleton(bundleRecipeRequest));
    product.setStoreId(STORE_ID);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID);
    Mockito.when(
            dataSourceWrapperService.getProductByStoreIdAndProductSku(any(), any(), Mockito.anyBoolean()))
        .thenReturn(product);
    Mockito.when(
            itemPickupPointSummaryServiceImpl.getItem(eq(STORE_ID), eq(ITEM_SKU), any(), eq(false)))
        .thenReturn(item);
    Mockito.when(
            productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(itemHelperService.convertToItem(any(), any(), any())).thenReturn(item);
    Mockito.when(saveOperationService.saveNewlyAddedItems(any()))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, product.getProductSku()))
        .thenReturn(Collections.singletonList(item));
    itemPickupPointWrapperService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        editProductDetailDTO);
    Mockito.verify(itemPickupPointSummaryService)
        .updateItemPickupPoint(any(), any(), any(), eq(true), eq(false));
    Mockito.verify(dataSourceWrapperService)
        .getProductByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
    Mockito.verify(itemHelperService).convertToItem(any(), any(), any());
    Mockito.verify(saveOperationService,Mockito.times(2)).saveNewlyAddedItems(any());
    Mockito.verify(itemPickupPointSummaryServiceImpl)
        .getItem(eq(STORE_ID), eq(ITEM_SKU), any(), eq(false));
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(anyList());
    Mockito.verify(dataSourceWrapperService).findItemByStoreIdAndItemSkus(any(), any(), Mockito.anyBoolean());
    Mockito.verify(dataSourcePrimaryEnabledProperties, times(3)).isUpdateItemPickupPointReadFromPrimaryEnabled();
    Mockito.verify(itemService).updateRecipeForSharedProducts(any(), any());
  }

  @Test
  public void updateItemPickupPointAddDeleteVariantEmptyBundleRecipeTest() throws Exception {
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo = new ItemPickupPointUpdateRequestVo();
    itemPickupPointUpdateRequestVo.setProductSku(PRODUCT_SKU);
    item.setItemCode(ITEM_CODE);
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addVariantRequestVo.setItemSku(ITEM_SKU_2);
    addDeleteVariantRequestVo.setAddVariantsList(Collections.singletonList(addVariantRequestVo));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    BundleRecipeRequest bundleRecipeRequest = new BundleRecipeRequest();
    bundleRecipeRequest.setItemSku(ITEM_SKU);
    Set<BundleRecipeVo> bundleRecipeVoSet = new HashSet<>();
    BundleRecipeVo bundleRecipeVo = new BundleRecipeVo();
    bundleRecipeVo.setItemSku(ITEM_SKU);
    bundleRecipeVoSet.add(bundleRecipeVo);
    bundleRecipeRequest.setBundleRecipe(bundleRecipeVoSet);
    itemPickupPointUpdateRequestVo.setBundleRecipeRequests(Collections.singleton(bundleRecipeRequest));
    product.setStoreId(STORE_ID);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID);
    Mockito.when(
            dataSourceWrapperService.getProductByStoreIdAndProductSku(any(), any(), Mockito.anyBoolean()))
        .thenReturn(product);
    Mockito.when(
            itemPickupPointSummaryServiceImpl.getItem(eq(STORE_ID), eq(ITEM_SKU), any(), eq(false)))
        .thenReturn(item);
    Mockito.when(
            productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(itemHelperService.convertToItem(any(), any(), any())).thenReturn(item);
    Mockito.when(saveOperationService.saveNewlyAddedItems(any()))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, product.getProductSku()))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(dataSourceWrapperService.findItemByStoreIdAndItemSkus(any(), any(), Mockito.anyBoolean()))
        .thenReturn(Collections.singletonList(item));
    itemPickupPointWrapperService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        editProductDetailDTO);
    Mockito.verify(itemPickupPointSummaryService)
        .updateItemPickupPoint(any(), any(), any(), eq(true), eq(false));
    Mockito.verify(dataSourceWrapperService)
        .getProductByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
    Mockito.verify(itemHelperService).convertToItem(any(), any(), any());
    Mockito.verify(saveOperationService,Mockito.times(2)).saveNewlyAddedItems(any());
    Mockito.verify(itemPickupPointSummaryServiceImpl)
        .getItem(eq(STORE_ID), eq(ITEM_SKU), any(), eq(false));
    Mockito.verify(itemPickupPointSummaryService).updateExternalHistoryInPBP(anyList());
    Mockito.verify(dataSourceWrapperService).findItemByStoreIdAndItemSkus(any(), any(), Mockito.anyBoolean());
    Mockito.verify(dataSourcePrimaryEnabledProperties, times(3)).isUpdateItemPickupPointReadFromPrimaryEnabled();
    Mockito.verify(itemService).updateRecipeForSharedProducts(any(), any());
  }

  @Test
  public void updateItemPickupPointItemIsNullTest() throws Exception {
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo = new ItemPickupPointUpdateRequestVo();
    itemPickupPointUpdateRequestVo.setProductSku(PRODUCT_SKU);
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addVariantRequestVo.setItemSku(ITEM_SKU_2);
    addDeleteVariantRequestVo.setAddVariantsList(Collections.singletonList(addVariantRequestVo));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    BundleRecipeRequest bundleRecipeRequest = new BundleRecipeRequest();
    bundleRecipeRequest.setItemSku(ITEM_SKU);
    itemPickupPointUpdateRequestVo.setBundleRecipeRequests(Collections.singleton(bundleRecipeRequest));
    product.setStoreId(STORE_ID);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID);
    Mockito.when(
            dataSourceWrapperService.getProductByStoreIdAndProductSku(any(), any(), Mockito.anyBoolean()))
        .thenReturn(product);
    Mockito.when(
            itemPickupPointSummaryServiceImpl.getItem(eq(STORE_ID), eq(ITEM_SKU), any(), eq(false)))
        .thenReturn(null);
    Mockito.when(
            productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(itemHelperService.convertToItem(any(), any(), any())).thenReturn(item);
    Mockito.when(saveOperationService.saveNewlyAddedItems(any()))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, product.getProductSku()))
        .thenReturn(Collections.singletonList(item));
    itemPickupPointWrapperService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        editProductDetailDTO);
    Mockito.verify(itemPickupPointSummaryService)
        .updateItemPickupPoint(any(), any(), any(), eq(true), eq(false));
    Mockito.verify(dataSourceWrapperService)
        .getProductByStoreIdAndProductSku(Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
    Mockito.verify(itemHelperService).convertToItem(any(), any(), any());
    Mockito.verify(saveOperationService).saveNewlyAddedItems(any());
    Mockito.verify(itemPickupPointSummaryServiceImpl)
        .getItem(eq(STORE_ID), eq(ITEM_SKU), any(), eq(false));
    Mockito.verify(dataSourcePrimaryEnabledProperties, times(3)).isUpdateItemPickupPointReadFromPrimaryEnabled();
  }

  @Test
  public void updateItemPickupPointAddDeleteVariantTest2() throws Exception {
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo = new ItemPickupPointUpdateRequestVo();
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    addDeleteVariantRequestVo.setDeleteVariantsList(Collections.singletonList(ITEM_SKU));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID);
    Mockito.when(
            productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(itemHelperService.convertToItem(any(), any(), any())).thenReturn(item);
    Mockito.when(
        dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalse(Mockito.anyString(),
            anyList(), Mockito.anyBoolean())).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(saveOperationService.saveNewlyAddedItems(any()))
        .thenReturn(Collections.singletonList(item));
    itemPickupPointWrapperService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        editProductDetailDTO);
    Mockito.verify(itemPickupPointSummaryService)
        .updateItemPickupPoint(any(), any(), any(), eq(false), eq(false));
    Mockito.verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalse(Mockito.anyString(), anyList(),
            Mockito.anyBoolean());
    Mockito.verify(dataSourcePrimaryEnabledProperties, times(2)).isUpdateItemPickupPointReadFromPrimaryEnabled();
  }


  @Test
  public void updateItemPickupPointAddDeleteVariantCombinedRequestProductNullTest() throws Exception {
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo = new ItemPickupPointUpdateRequestVo();
    itemPickupPointUpdateRequestVo.setProductSku(PRODUCT_SKU);
    BundleRecipeRequest bundleRecipeRequest = new BundleRecipeRequest();
    bundleRecipeRequest.setItemSku(ITEM_SKU);
    itemPickupPointUpdateRequestVo.setBundleRecipeRequests(Collections.singleton(bundleRecipeRequest));
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    addDeleteVariantRequestVo.setDeleteVariantsList(Collections.singletonList(ITEM_SKU));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID);
    BundleRecipeRequest bundleRecipeRequest1 = new BundleRecipeRequest();
    bundleRecipeRequest1.setItemSku(ITEM_SKU);
    BundleRecipeVo bundleRecipeVo = new BundleRecipeVo();
    bundleRecipeVo.setItemSku(ITEM_SKU);
    bundleRecipeRequest1.setBundleRecipe(Collections.singleton(bundleRecipeVo));
    itemPickupPointUpdateRequestVo.setBundleRecipeRequests(Collections.singleton(bundleRecipeRequest1));
    product.setStoreId(STORE_ID);
    editProductDetailDTO.setProduct(product);
    BundleRecipe bundleRecipe = new BundleRecipe();
    bundleRecipe.setItemSku(ITEM_SKU);
    Set<BundleRecipe> bundleRecipeSet = new HashSet<>();
    bundleRecipeSet.add(bundleRecipe);
    item.setBundleRecipe(bundleRecipeSet);
    Mockito.when(
            dataSourceWrapperService.getProductByStoreIdAndProductSku(any(), any(), Mockito.anyBoolean()))
        .thenReturn(null);
    item.setItemSku(ITEM_SKU_2);
    Mockito.when(itemPickupPointSummaryServiceImpl.getItem(eq(STORE_ID), eq(ITEM_SKU), any(),
        eq(true))).thenReturn(item);
    Mockito.when(itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, product.getProductSku()))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(
            productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(itemHelperService.convertToItem(any(), any(), any())).thenReturn(item);
    Mockito.when(saveOperationService.saveNewlyAddedItems(any())).thenReturn(Collections.singletonList(item));
    itemPickupPointWrapperService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        editProductDetailDTO);
    Mockito.verify(itemPickupPointSummaryService)
        .updateItemPickupPoint(any(), any(), any(), eq(true), eq(false));
    Mockito.verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalse(any(), any(),
            Mockito.anyBoolean());
    Mockito.verify(dataSourceWrapperService)
        .getProductByStoreIdAndProductSku(any(), any(), Mockito.anyBoolean());
    Mockito.verify(dataSourcePrimaryEnabledProperties, times(3)).isUpdateItemPickupPointReadFromPrimaryEnabled();
  }


  @Test
  public void findProductForTransactionByItemSkusAndPickupPointCodeItemMapEmptyTest() throws Exception {
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(new ArrayList<>());
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());

    itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, itemPickupPointRequests);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
  }

  @Test
  public void findProductForTransactionByItemSkusAndPickupPointCodeProductMapEmptyTest() throws Exception {
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(new ArrayList<>());
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList())).thenReturn(new ArrayList<>());

    itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, itemPickupPointRequests);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        anyList());
  }

  @Test
  public void findProductForTransactionByItemSkusAndPickupPointCodeEmptyItemPickupPointTest() throws Exception {
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false)).thenReturn(new ArrayList<>());

    itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, itemPickupPointRequests);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        false);
  }

  @Test
  public void testFindPriceDetailsByItemSkuAndPickupPointCode() {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "itemPickupPointFetchSize", 10);
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(eq(STORE_ID), eq(ITEM_SKU),
        eq(PICKUP_POINT_CODE))).thenReturn(itemPickupPoint);
    this.itemPickupPointWrapperService.findPriceDetailsByItemSkuAndPickupPointCode(STORE_ID,
        Collections.singletonList(
            ItemPickupPointRequest.builder().pickupPointCode(PICKUP_POINT_CODE).itemSku(ITEM_SKU)
                .build()));
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
  }

  @Test
  public void testFindPriceDetailsByItemSkuAndPickupPointCodeWithInavlidFetchSize()
      throws InvocationTargetException, IllegalAccessException {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "itemPickupPointFetchSize", 1);
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(eq(STORE_ID), eq(ITEM_SKU),
        eq(PICKUP_POINT_CODE))).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    BeanUtils.copyProperties(itemPickupPoint,itemPickupPoint2);
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPoint2.setItemSku(ITEM_SKU_2);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointWrapperService.findPriceDetailsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests));
  }

  @Test
  public void testFindPriceDetailsByItemSkuAndPickupPointCode2() {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "itemPickupPointFetchSize", 10);
    this.itemPickupPoint.setMarkForDelete(true);
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(eq(STORE_ID), eq(ITEM_SKU),
        eq(PICKUP_POINT_CODE))).thenReturn(itemPickupPoint);
    List<ItemPickupPointPriceResponse> details =
        this.itemPickupPointWrapperService.findPriceDetailsByItemSkuAndPickupPointCode(STORE_ID,
            Collections.singletonList(
                ItemPickupPointRequest.builder().pickupPointCode(PICKUP_POINT_CODE).itemSku(ITEM_SKU)
                    .build()));
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Assertions.assertTrue(CollectionUtils.isEmpty(details));
  }

  @Test
  public void testFindPriceDetailsByItemSkuAndPickupPointCode3() {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "itemPickupPointFetchSize", 10);
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(eq(STORE_ID), eq(ITEM_SKU),
        eq(PICKUP_POINT_CODE))).thenReturn(null);
    List<ItemPickupPointPriceResponse> details =
        this.itemPickupPointWrapperService.findPriceDetailsByItemSkuAndPickupPointCode(STORE_ID,
            Collections.singletonList(
                ItemPickupPointRequest.builder().pickupPointCode(PICKUP_POINT_CODE).itemSku(ITEM_SKU)
                    .build()));
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE);
    Assertions.assertTrue(CollectionUtils.isEmpty(details));
  }

  @Test
  public void deleteItemPickupPointsByPickupPointCodeTest() throws Exception {
    itemPickupPoint.setFbbActivated(true);
    DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setBusinessPartnerCode(MERCHANT_CODE);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setProductSku(PRODUCT_SKU);
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(deleteItemPickupPointRequest.getBusinessPartnerCode(), itemPickupPoint.getItemSku(),
            Constants.PICKUP_POINT_DELETED, deleteItemPickupPointRequest.getPickupPointCode(), StringUtils.EMPTY, null,
            product.getProductSku(), item.getGeneratedItemName(), Constants.HYPHEN, false);
    AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
    auditTrailListResponse.setAuditTrailResponseList(Collections.singletonList(auditTrailDto));
    auditTrailListResponse.setAccessChannel(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setChangedBy(Constants.DEFAULT_USERNAME);
    auditTrailListResponse.setClientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setRequestId(Constants.DEFAULT_REQUEST_ID);
    auditTrailListResponse.setUpdateDirectly(true);
    Mockito.when(productService.getProductDeletedOrUndeleted(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(
        itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(2l);
    Mockito.when(itemService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(item);
    List<DeleteItemPickupPointResponse> responses =
        itemPickupPointWrapperService.deleteItemPickupPointsByPickupPointCode(STORE_ID, deleteItemPickupPointRequest);
    Mockito.verify(productService).getProductDeletedOrUndeleted(STORE_ID, deleteItemPickupPointRequest.getProductSku());
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointsByProductSkuAndPPcode(STORE_ID, deleteItemPickupPointRequest.getProductSku(),
            deleteItemPickupPointRequest.getPickupPointCode());
    Mockito.verify(itemPickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemPickupPoint.getItemSku());
    Mockito.verify(objectConverterService).convertToItemPickupPointChangeEventModel(any(),Mockito.anyBoolean());
    Mockito.verify(itemPickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(any(), eq(Collections.EMPTY_MAP));
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, itemPickupPoint.getItemSku());
    Mockito.verify(itemPickupPointService)
        .updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, item,
            Collections.singletonList(itemPickupPoint), true);
    verify(kafkaProducer).send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY,
        auditTrailListResponse);
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT));
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any());
    Assertions.assertEquals(new ArrayList<>(), responses);
  }

  @Test
  public void deleteItemPickupPointsByPickupPointCodeFailedToDeleteTest() throws Exception {
    DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setBusinessPartnerCode(MERCHANT_CODE);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setDefaultPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setProductSku(PRODUCT_SKU);
    item.setCncActivated(true);
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(deleteItemPickupPointRequest.getBusinessPartnerCode(), itemPickupPoint.getItemSku(),
            Constants.PICKUP_POINT_DELETED, deleteItemPickupPointRequest.getPickupPointCode(), StringUtils.EMPTY, null,
            product.getProductSku(), item.getGeneratedItemName(), Constants.HYPHEN, false);
    Mockito.when(productService.getProductDeletedOrUndeleted(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(
        itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(1l);
    Mockito.when(itemService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString())).thenReturn(item);
    Map<String, ItemPickupPoint> offlineIdAndItemPickupPointMap = new HashMap<>();
    offlineIdAndItemPickupPointMap.put(OFFLINE_ITEM_ID, itemPickupPoint);
    Mockito.when(
        itemService.updatePickupPointInItemPickupPoints(deleteItemPickupPointRequest.getDefaultPickupPointCode(),
            itemPickupPoint, item, new HashMap<>(), false, null, false)).thenReturn(offlineIdAndItemPickupPointMap);
    Mockito.when(
            itemPickupPointService.saveItemPickupPoint(Arrays.asList(offlineIdAndItemPickupPointMap.get(OFFLINE_ITEM_ID))))
        .thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(
        itemPickupPointSummaryService.setCncActiveAtProductLevel(eq(PRODUCT_SKU), Mockito.anySet(), eq(product),
          eq(false))).thenReturn(true);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any()))
        .thenReturn(new ProductAndItemEventModel());

    List<DeleteItemPickupPointResponse> responses =
        itemPickupPointWrapperService.deleteItemPickupPointsByPickupPointCode(STORE_ID, deleteItemPickupPointRequest);

    Mockito.verify(kafkaProducer).send(Mockito.any(), Mockito.any());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any());
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT));
    Mockito.verify(itemService).saveItems(anyList());
    Mockito.verify(itemPickupPointSummaryService)
        .setCncActiveAtProductLevel(eq(PRODUCT_SKU), Mockito.anySet(), eq(product), eq(false));
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.any(), any(), Mockito.any());
    Mockito.verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(anyList(), anyList(),
      eq(Collections.EMPTY_MAP));
    Mockito.verify(itemPickupPointService)
        .saveItemPickupPoint(Arrays.asList(offlineIdAndItemPickupPointMap.get(OFFLINE_ITEM_ID)));
    Mockito.verify(itemService)
        .updatePickupPointInItemPickupPoints(deleteItemPickupPointRequest.getDefaultPickupPointCode(), itemPickupPoint,
            item, new HashMap<>(), false, null, false);
    Mockito.verify(productService)
        .getProductDeletedOrUndeleted(STORE_ID, deleteItemPickupPointRequest.getProductSku());
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointsByProductSkuAndPPcode(STORE_ID, deleteItemPickupPointRequest.getProductSku(),
            deleteItemPickupPointRequest.getPickupPointCode());
    Mockito.verify(itemPickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemPickupPoint.getItemSku());
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, itemPickupPoint.getItemSku());
  }

  @Test
  public void deleteItemPickupPointsByPickupPointCodeFailedToDeleteWithOldItemPickupPointTest() throws Exception {
    DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setBusinessPartnerCode(MERCHANT_CODE);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setDefaultPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setProductSku(PRODUCT_SKU);
    item.setCncActivated(true);
    itemPickupPoint.setFbbActivated(true);
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(deleteItemPickupPointRequest.getBusinessPartnerCode(), itemPickupPoint.getItemSku(),
            Constants.PICKUP_POINT_DELETED, deleteItemPickupPointRequest.getPickupPointCode(), StringUtils.EMPTY, null,
            product.getProductSku(), item.getGeneratedItemName(), Constants.HYPHEN, false);
    Mockito.when(productService.getProductDeletedOrUndeleted(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(
        itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(1l);
    Mockito.when(itemService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString())).thenReturn(item);
    Map<String, ItemPickupPoint> offlineIdAndItemPickupPointMap = new HashMap<>();
    offlineIdAndItemPickupPointMap.put(OFFLINE_ITEM_ID, itemPickupPoint);
    Mockito.when(
        itemService.updatePickupPointInItemPickupPoints(deleteItemPickupPointRequest.getDefaultPickupPointCode(),
            itemPickupPoint, item, new HashMap<>(), false, null, false)).thenReturn(offlineIdAndItemPickupPointMap);
    Mockito.when(
            itemPickupPointService.saveItemPickupPoint(Arrays.asList(offlineIdAndItemPickupPointMap.get(OFFLINE_ITEM_ID))))
        .thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(
        itemPickupPointSummaryService.setCncActiveAtProductLevel(eq(PRODUCT_SKU), Mockito.anySet(), eq(product),
          eq(false))).thenReturn(true);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any()))
        .thenReturn(new ProductAndItemEventModel());

    List<DeleteItemPickupPointResponse> responses =
        itemPickupPointWrapperService.deleteItemPickupPointsByPickupPointCode(STORE_ID, deleteItemPickupPointRequest);

    Mockito.verify(kafkaProducer).send(Mockito.any(), Mockito.any());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any());
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT));
    Mockito.verify(itemService).saveItems(anyList());
    Mockito.verify(itemPickupPointSummaryService)
        .setCncActiveAtProductLevel(eq(PRODUCT_SKU), Mockito.anySet(), eq(product), eq(false));
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.any(), any(), Mockito.any());
    Mockito.verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(anyList(), anyList(),
      eq(Collections.EMPTY_MAP));
    Mockito.verify(itemPickupPointService)
        .saveItemPickupPoint(Arrays.asList(offlineIdAndItemPickupPointMap.get(OFFLINE_ITEM_ID)));
    Mockito.verify(itemService)
        .updatePickupPointInItemPickupPoints(deleteItemPickupPointRequest.getDefaultPickupPointCode(), itemPickupPoint,
            item, new HashMap<>(), false, null, false);
    Mockito.verify(productService)
        .getProductDeletedOrUndeleted(STORE_ID, deleteItemPickupPointRequest.getProductSku());
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointsByProductSkuAndPPcode(STORE_ID, deleteItemPickupPointRequest.getProductSku(),
            deleteItemPickupPointRequest.getPickupPointCode());
    Mockito.verify(itemPickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemPickupPoint.getItemSku());
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, itemPickupPoint.getItemSku());
  }

  @Test
  public void deleteItemPickupPointsByPickupPointCodeFailedToDeleteCncFalseTest() throws Exception {
    DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setBusinessPartnerCode(MERCHANT_CODE);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setDefaultPickupPointCode(DEFAULT_PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setProductSku(PRODUCT_SKU);
    deleteItemPickupPointRequest.setDefaultPickupPointFbbActive(true);
    item.setCncActivated(false);
    itemPickupPoint.setFbbActivated(true);
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(deleteItemPickupPointRequest.getBusinessPartnerCode(), itemPickupPoint.getItemSku(),
            Constants.PICKUP_POINT_DELETED, deleteItemPickupPointRequest.getPickupPointCode(), StringUtils.EMPTY, null,
            product.getProductSku(), item.getGeneratedItemName(), Constants.HYPHEN, false);
    product.setFbbActivated(true);
    Mockito.when(productService.getProductDeletedOrUndeleted(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(itemPickupPointService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(STORE_ID,
        product.getProductSku(), true)).thenReturn(1L);
    Mockito.when(
        itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(1l);
    Mockito.when(itemService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString())).thenReturn(item);
    Map<String, ItemPickupPoint> offlineIdAndItemPickupPointMap = new HashMap<>();
    offlineIdAndItemPickupPointMap.put(OFFLINE_ITEM_ID, itemPickupPoint);
    Mockito.when(
        itemService.updatePickupPointInItemPickupPoints(deleteItemPickupPointRequest.getDefaultPickupPointCode(),
            itemPickupPoint, item, new HashMap<>(), false, null, false)).thenReturn(offlineIdAndItemPickupPointMap);
    Mockito.when(
            itemPickupPointService.saveItemPickupPoint(Arrays.asList(offlineIdAndItemPickupPointMap.get(OFFLINE_ITEM_ID))))
        .thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(
        itemPickupPointSummaryService.setCncActiveAtProductLevel(eq(PRODUCT_SKU), Mockito.anySet(), eq(product),
          eq(false))).thenReturn(true);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any()))
        .thenReturn(new ProductAndItemEventModel());

    List<DeleteItemPickupPointResponse> responses =
        itemPickupPointWrapperService.deleteItemPickupPointsByPickupPointCode(STORE_ID, deleteItemPickupPointRequest);

    Mockito.verify(itemPickupPointService).findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(STORE_ID,
        product.getProductSku(), true);
    Mockito.verify(kafkaProducer).send(Mockito.any(), Mockito.any());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any());
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT));
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointData(Mockito.any(), any(), Mockito.any());
    Mockito.verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(anyList(), anyList(),
      eq(Collections.EMPTY_MAP));
    Mockito.verify(itemPickupPointService)
        .saveItemPickupPoint(Arrays.asList(offlineIdAndItemPickupPointMap.get(OFFLINE_ITEM_ID)));
    Mockito.verify(itemService)
        .updatePickupPointInItemPickupPoints(deleteItemPickupPointRequest.getDefaultPickupPointCode(), itemPickupPoint,
            item, new HashMap<>(), false, null, false);
    Mockito.verify(productService)
        .getProductDeletedOrUndeleted(STORE_ID, deleteItemPickupPointRequest.getProductSku());
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointsByProductSkuAndPPcode(STORE_ID, deleteItemPickupPointRequest.getProductSku(),
            deleteItemPickupPointRequest.getPickupPointCode());
    Mockito.verify(itemPickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemPickupPoint.getItemSku());
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, itemPickupPoint.getItemSku());
  }

  @Test
  public void deleteItemPickupPointsByPickupPointCodeFailedToDeleteDefaultPpCodeEmptyTest() throws Exception {
    DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setBusinessPartnerCode(MERCHANT_CODE);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setDefaultPickupPointCode(StringUtils.EMPTY);
    deleteItemPickupPointRequest.setProductSku(PRODUCT_SKU);
    item.setCncActivated(true);
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(deleteItemPickupPointRequest.getBusinessPartnerCode(), itemPickupPoint.getItemSku(),
            Constants.PICKUP_POINT_DELETED, deleteItemPickupPointRequest.getPickupPointCode(), StringUtils.EMPTY, null,
            product.getProductSku(), item.getGeneratedItemName(), Constants.HYPHEN, false);
    Mockito.when(productService.getProductDeletedOrUndeleted(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(
        itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(1l);
    Mockito.when(itemService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString())).thenReturn(item);
    Map<String, ItemPickupPoint> offlineIdAndItemPickupPointMap = new HashMap<>();
    offlineIdAndItemPickupPointMap.put(OFFLINE_ITEM_ID, itemPickupPoint);
    Mockito.when(
        itemPickupPointSummaryService.setCncActiveAtProductLevel(eq(PRODUCT_SKU), Mockito.anySet(), eq(product),
          eq(false))).thenReturn(true);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any()))
        .thenReturn(new ProductAndItemEventModel());

    List<DeleteItemPickupPointResponse> responses =
        itemPickupPointWrapperService.deleteItemPickupPointsByPickupPointCode(STORE_ID, deleteItemPickupPointRequest);

    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any());
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT));
    Mockito.verify(itemService).saveItems(anyList());
    Mockito.verify(itemPickupPointSummaryService)
        .setCncActiveAtProductLevel(eq(PRODUCT_SKU), Mockito.anySet(), eq(product), eq(false));
    Mockito.verify(productService)
        .getProductDeletedOrUndeleted(STORE_ID, deleteItemPickupPointRequest.getProductSku());
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointsByProductSkuAndPPcode(STORE_ID, deleteItemPickupPointRequest.getProductSku(),
            deleteItemPickupPointRequest.getPickupPointCode());
    Mockito.verify(itemPickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemPickupPoint.getItemSku());
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, itemPickupPoint.getItemSku());
  }

  @Test
  public void deleteItemPickupPointsByPickupPointCodeFailedToDeleteNoPpDeleteTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "deleteItemPickupPointFromInventory", true);
    DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setBusinessPartnerCode(MERCHANT_CODE);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setDefaultPickupPointCode(StringUtils.EMPTY);
    deleteItemPickupPointRequest.setProductSku(PRODUCT_SKU);
    item.setCncActivated(true);
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(deleteItemPickupPointRequest.getBusinessPartnerCode(), itemPickupPoint.getItemSku(),
            Constants.PICKUP_POINT_DELETED, deleteItemPickupPointRequest.getPickupPointCode(), StringUtils.EMPTY, null,
            product.getProductSku(), item.getGeneratedItemName(), Constants.HYPHEN, false);
    Mockito.when(productService.getProductDeletedOrUndeleted(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(product);
    Mockito.when(
        itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(2l);
    Mockito.when(itemService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString())).thenReturn(item);
    Map<String, ItemPickupPoint> offlineIdAndItemPickupPointMap = new HashMap<>();
    offlineIdAndItemPickupPointMap.put(OFFLINE_ITEM_ID, itemPickupPoint);
    Mockito.when(
        itemPickupPointSummaryService.setCncActiveAtProductLevel(eq(PRODUCT_SKU), Mockito.anySet(), eq(product),
          eq(false))).thenReturn(true);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any()))
        .thenReturn(new ProductAndItemEventModel());

    List<DeleteItemPickupPointResponse> responses =
        itemPickupPointWrapperService.deleteItemPickupPointsByPickupPointCode(STORE_ID, deleteItemPickupPointRequest);

    Mockito.verify(kafkaProducer).send(Mockito.any(), Mockito.any());
    Mockito.verify(itemPickupPointService).updateItemViewConfigByItemSku(
        anyList(), eq(STORE_ID), eq(Constants.DEFAULT_USERNAME), eq(true), eq(item), anyList(), eq(true));
    Mockito.verify(objectConverterService).convertToItemPickupPointChangeEventModel(any(),Mockito.anyBoolean());
    Mockito.verify(itemPickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(any(), eq(Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any());
    Mockito.verify(productService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT));
    Mockito.verify(productService)
        .getProductDeletedOrUndeleted(STORE_ID, deleteItemPickupPointRequest.getProductSku());
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointsByProductSkuAndPPcode(STORE_ID, deleteItemPickupPointRequest.getProductSku(),
            deleteItemPickupPointRequest.getPickupPointCode());
    Mockito.verify(itemPickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemPickupPoint.getItemSku());
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, itemPickupPoint.getItemSku());
    Mockito.verify(inventoryOutbound).deleteByItemSkuAndPickupPointCode(Mockito.any(), Mockito.any(),
        any());
  }

  @Test
  @Disabled("Disabled due to the issue jenkins ci issue")
  public void deleteItemPickupPointsByPickupPointCodeFailedToDeleteNoPpDeleteExceptionTest() throws Exception {
    MDC.put("username","username");
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, "request-id");
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "deleteItemPickupPointFromInventory", true);
    DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setBusinessPartnerCode(MERCHANT_CODE);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setDefaultPickupPointCode(StringUtils.EMPTY);
    deleteItemPickupPointRequest.setProductSku(PRODUCT_SKU);
    ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setProduct(product);
    WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO webItemSkuAndPickupPointCodeDTO =
        new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO();
    webItemSkuAndPickupPointCodeDTO.setWebItemSku(ITEM_SKU);
    webItemSkuAndPickupPointCodeDTO.setPickupPointCode(PICKUP_POINT_CODE);
    AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
    auditTrailListResponse.setAccessChannel(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setChangedBy(Constants.DEFAULT_USERNAME);
    auditTrailListResponse.setClientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setRequestId(Constants.DEFAULT_REQUEST_ID);
    auditTrailListResponse.setUpdateDirectly(true);
    item.setCncActivated(true);
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(deleteItemPickupPointRequest.getBusinessPartnerCode(), itemPickupPoint.getItemSku(),
            Constants.PICKUP_POINT_DELETED, deleteItemPickupPointRequest.getPickupPointCode(), StringUtils.EMPTY, null,
            product.getProductSku(), item.getGeneratedItemName(), Constants.HYPHEN, false);
    auditTrailListResponse.setAuditTrailResponseList(Collections.singletonList(auditTrailDto));
    Mockito.when(productService.getProductDeletedOrUndeleted(STORE_ID,
        deleteItemPickupPointRequest.getProductSku())).thenReturn(product);
    Mockito.when(itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(STORE_ID,
            deleteItemPickupPointRequest.getProductSku(), deleteItemPickupPointRequest.getPickupPointCode()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        itemPickupPoint.getItemSku())).thenReturn(2l);
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, itemPickupPoint.getItemSku())).thenReturn(item);
    Map<String, ItemPickupPoint> offlineIdAndItemPickupPointMap = new HashMap<>();
    offlineIdAndItemPickupPointMap.put(OFFLINE_ITEM_ID, itemPickupPoint);
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(productAndItemsVO))
        .thenReturn(new ProductAndItemEventModel());
    Mockito.when(inventoryOutbound.deleteByItemSkuAndPickupPointCode(null, USERNAME,
        Collections.singletonList(webItemSkuAndPickupPointCodeDTO))).thenThrow(ApplicationRuntimeException.class);

    try {
      List<DeleteItemPickupPointResponse> responses =
          itemPickupPointWrapperService.deleteItemPickupPointsByPickupPointCode(STORE_ID, deleteItemPickupPointRequest);
    } finally {
      Mockito.verify(kafkaProducer).send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
      Mockito.verify(itemPickupPointService)
          .updateItemViewConfigByItemSku(anyList(), eq(STORE_ID), eq(Constants.DEFAULT_USERNAME), eq(true), eq(item),
              eq(Collections.singletonList(itemPickupPoint)), eq(true));
      Mockito.verify(objectConverterService).convertToItemPickupPointChangeEventModel(itemPickupPoint, false);
      Mockito.verify(itemPickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(null, Collections.EMPTY_MAP);
      Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(Collections.singletonList(productAndItemEventModel));
      Mockito.verify(objectConverterService).convertToProductAndItemEventModel(productAndItemsVO);
      Mockito.verify(productService).saveProductWithoutUpdatingSolr(product, new ArrayList<>(), Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT);
      Mockito.verify(productService)
          .getProductDeletedOrUndeleted(STORE_ID, deleteItemPickupPointRequest.getProductSku());
      Mockito.verify(itemPickupPointService)
          .findItemPickupPointsByProductSkuAndPPcode(STORE_ID, deleteItemPickupPointRequest.getProductSku(),
              deleteItemPickupPointRequest.getPickupPointCode());
      Mockito.verify(itemPickupPointService)
          .findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemPickupPoint.getItemSku());
      Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, itemPickupPoint.getItemSku());
      Mockito.verify(inventoryOutbound)
          .deleteByItemSkuAndPickupPointCode("request-id", USERNAME, Collections.singletonList(webItemSkuAndPickupPointCodeDTO));
    }
  }

  @Test
  public void testSaveProduct_cncChangedAtL3Level() {
    Product product = new Product();
    boolean cncChangedAtL3Level = true;
    boolean fbbChangedAtL3Level = false;
    boolean pickupPointDeleteSuccess = false;
    boolean pickupPointUpdated = false;
    itemPickupPointWrapperService.saveProduct(product, cncChangedAtL3Level, fbbChangedAtL3Level,
        pickupPointDeleteSuccess, pickupPointUpdated);
    Mockito.verify(productService, Mockito.times(1)).saveProductWithoutUpdatingSolr(product, new ArrayList<>(),
        Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT);
  }

  @Test
  public void testSaveProduct_fbbChangedAtL3Level() {
    Product product = new Product();
    boolean cncChangedAtL3Level = false;
    boolean fbbChangedAtL3Level = true;
    boolean pickupPointDeleteSuccess = false;
    boolean pickupPointUpdated = false;
    itemPickupPointWrapperService.saveProduct(product, cncChangedAtL3Level, fbbChangedAtL3Level,
        pickupPointDeleteSuccess, pickupPointUpdated);
    Mockito.verify(productService, Mockito.times(1)).saveProductWithoutUpdatingSolr(product, new ArrayList<>(),
        Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT);
  }

  @Test
  public void testSaveProduct_pickupPointDeleteSuccess() {
    Product product = new Product();
    boolean cncChangedAtL3Level = false;
    boolean fbbChangedAtL3Level = false;
    boolean pickupPointDeleteSuccess = true;
    boolean pickupPointUpdated = false;
    itemPickupPointWrapperService.saveProduct(product, cncChangedAtL3Level, fbbChangedAtL3Level,
        pickupPointDeleteSuccess, pickupPointUpdated);
    Mockito.verify(productService, Mockito.times(1)).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE),
        Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT);
  }

  @Test
  public void testSaveProduct_pickupPointUpdated() {
    Product product = new Product();
    boolean cncChangedAtL3Level = false;
    boolean fbbChangedAtL3Level = false;
    boolean pickupPointDeleteSuccess = false;
    boolean pickupPointUpdated = true;
    itemPickupPointWrapperService.saveProduct(product, cncChangedAtL3Level, fbbChangedAtL3Level,
        pickupPointDeleteSuccess, pickupPointUpdated);
    Mockito.verify(productService, Mockito.times(1))
        .saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE),
            Constants.PRODUCT_PUBLISH_SOURCE_DELETE_PICKUP_POINT);
  }

  @Test
  public void testSaveProduct_noFlagsSet() {
    Product product = new Product();
    boolean cncChangedAtL3Level = false;
    boolean fbbChangedAtL3Level = false;
    boolean pickupPointDeleteSuccess = false;
    boolean pickupPointUpdated = false;
    itemPickupPointWrapperService.saveProduct(product, cncChangedAtL3Level, fbbChangedAtL3Level,
        pickupPointDeleteSuccess, pickupPointUpdated);
    Mockito.verify(productService, Mockito.never()).saveProductWithoutUpdatingSolr(product, new ArrayList<>(),
        StringUtils.EMPTY);
  }

  @Test
  public void testIsFbbChangedAtL3Level_NoFbbActivatedItems_ReturnsFalse() {
    List<String> itemSkusToBeDeleted = new ArrayList<>();
    deleteStatusItemSkuMap.put(0, itemSkusToBeDeleted);
    product.setFbbActivated(true);
    boolean result = itemPickupPointWrapperService.isFbbChangedAtL3Level(product, false);
    Assertions.assertFalse(result);
  }

  @Test
  public void testIsFbbChangedAtL3Level_FbbActivatedFalse() {
    Map<String, ItemPickupPoint> itemSkuToItemPickupPointMap = new HashMap<>();
    itemPickupPoint.setFbbActivated(true);
    itemSkuToItemPickupPointMap.put(ITEM_SKU, itemPickupPoint);

    Map<Integer, List<String>> deleteStatusItemSkuMap = new HashMap<>();
    List<String> itemSkusToBeDeleted = new ArrayList<>();
    itemSkusToBeDeleted.add(ITEM_SKU);
    deleteStatusItemSkuMap.put(0, itemSkusToBeDeleted);

    product.setFbbActivated(true);
    boolean isFbbChanged =
        itemPickupPointWrapperService.isFbbChangedAtL3Level(product, true);

    Assertions.assertTrue(isFbbChanged);
    Assertions.assertFalse(product.isFbbActivated());
  }

  @Test
  public void testIsFbbChangedAtL3Level_FbbNotActivated() {
    Map<String, ItemPickupPoint> itemSkuToItemPickupPointMap = new HashMap<>();
    itemPickupPoint.setFbbActivated(true);
    itemSkuToItemPickupPointMap.put(ITEM_SKU, itemPickupPoint);

    Map<Integer, List<String>> deleteStatusItemSkuMap = new HashMap<>();
    List<String> itemSkusToBeDeleted = new ArrayList<>();
    itemSkusToBeDeleted.add(ITEM_SKU);
    deleteStatusItemSkuMap.put(0, itemSkusToBeDeleted);

    Mockito.when(itemPickupPointService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(STORE_ID,
        product.getProductSku(), true)).thenReturn(2L);

    boolean isFbbChanged =
        itemPickupPointWrapperService.isFbbChangedAtL3Level(product, true);

    Assertions.assertFalse(isFbbChanged);
    Assertions.assertFalse(product.isFbbActivated());
  }

  @Test
  public void createFbbPickupPointAlreadyExistsTest() {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = getCreateFbbPickupPointRequest();
    Mockito.when(
            itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, createFbbPickupPointRequest.getItemSku()))
        .thenReturn(item);
    CreateFbbPickupPointResponse createFbbPickupPointResponse = new CreateFbbPickupPointResponse();
    createFbbPickupPointResponse.setReason(ErrorMessages.FBB_PICKUP_POINT_ALREADY_EXISTS);
    Mockito.when(
            itemPickupPointService.createFbbPickupPoint(Mockito.anyString(), any(), any(), any()))
        .thenReturn(createFbbPickupPointResponse);
    createFbbPickupPointResponse =
        itemPickupPointWrapperService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest);
    Mockito.verify(itemService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, createFbbPickupPointRequest.getItemSku());
    Mockito.verify(itemPickupPointService)
        .createFbbPickupPoint(Mockito.anyString(), any(), any(), any());
    Assertions.assertEquals(ErrorMessages.FBB_PICKUP_POINT_ALREADY_EXISTS, createFbbPickupPointResponse.getReason());
  }

  @Test
  public void createFbbPickupPointInternalServerErrorTest() {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = getCreateFbbPickupPointRequest();
    Mockito.when(
            itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, createFbbPickupPointRequest.getItemSku()))
        .thenReturn(item);
    Mockito.when(
            itemPickupPointService.createFbbPickupPoint(Mockito.anyString(), any(), any(), any()))
        .thenThrow(RuntimeException.class);
    CreateFbbPickupPointResponse createFbbPickupPointResponse =
        itemPickupPointWrapperService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest);
    Mockito.verify(itemService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, createFbbPickupPointRequest.getItemSku());
    Mockito.verify(itemPickupPointService)
        .createFbbPickupPoint(Mockito.anyString(), any(), any(), any());
    Assertions.assertEquals(ProductErrorCodesEnum.INTERNAL_SERVER.getMessage(), createFbbPickupPointResponse.getReason());
  }

  @Test
  public void createFbbPickupPointTest() {
    item.setMerchantSku(MERCHANT_SKU);
    CreateFbbPickupPointRequest createFbbPickupPointRequest = getCreateFbbPickupPointRequest();
    Mockito.when(
            itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, createFbbPickupPointRequest.getItemSku()))
        .thenReturn(item);
    Mockito.when(
            itemPickupPointService.createFbbPickupPoint(Mockito.anyString(), any(), any(), any()))
        .thenReturn(new CreateFbbPickupPointResponse());
    product.setFbbActivated(false);
    product.setPickupPointCodes(new HashSet<>());
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, item.getProductSku()))
        .thenReturn(product);
    CreateFbbPickupPointResponse createFbbPickupPointResponse =
        itemPickupPointWrapperService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest);
    Mockito.verify(itemService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, createFbbPickupPointRequest.getItemSku());
    Mockito.verify(itemPickupPointService)
        .createFbbPickupPoint(Mockito.anyString(), any(), any(), any());
    Mockito.verify(productService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, item.getProductSku());
    Mockito.verify(saveOperationService).saveProduct(any());
    Assertions.assertEquals(null, createFbbPickupPointResponse.getReason());
  }

  @Test
  public void createFbbPickupPointNoProductUpdateTest() {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = getCreateFbbPickupPointRequest();
    Mockito.when(
            itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, createFbbPickupPointRequest.getItemSku()))
        .thenReturn(item);
    Mockito.when(
            itemPickupPointService.createFbbPickupPoint(Mockito.anyString(), any(), any(), any()))
        .thenReturn(new CreateFbbPickupPointResponse());
    product.setFbbActivated(true);
    product.setPickupPointCodes(Collections.singleton(createFbbPickupPointRequest.getPickupPointCode()));
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, item.getProductSku()))
        .thenReturn(product);
    CreateFbbPickupPointResponse createFbbPickupPointResponse =
        itemPickupPointWrapperService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest);
    Mockito.verify(itemService)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, createFbbPickupPointRequest.getItemSku());
    Mockito.verify(itemPickupPointService)
        .createFbbPickupPoint(Mockito.anyString(), any(), any(), any());
    Mockito.verify(productService)
        .findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, item.getProductSku());
    Assertions.assertEquals(null, createFbbPickupPointResponse.getReason());
  }

  @Test
  public void findProductL5DetailByItemSkusAndPickupPointCodeTest() throws Exception {
    product.setSynchronized(true);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID,
        ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCT_SKU))).thenReturn(Arrays.asList(product));
    when(productHelperService.getCurrentBuyableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(true);
    when(productHelperService.getCurrentDiscoverableStatusForItem(
        Mockito.anySet(), Mockito.anyString(),
        Mockito.anyBoolean())).thenReturn(true);

    itemPickupPointWrapperService.findProductL5DetailByItemSkusAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false, null);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID,
        ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCT_SKU));
    verify(productHelperService).getCurrentBuyableStatusForItem(
        Mockito.anySet(), Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService).getCurrentDiscoverableStatusForItem(
        Mockito.anySet(), Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void findProductL5DetailByItemSkusAndPickupPointCodeInAllProductsTest() throws Exception {
    product.setSynchronized(true);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        true)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Collections.singletonList(item));
    when(productService.findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU))).thenReturn(
        Collections.singletonList(product));
    when(
        productHelperService.getCurrentBuyableStatusForItem(Mockito.anySet(), Mockito.anyString(),
            Mockito.anyBoolean())).thenReturn(true);
    when(productHelperService.getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(true);

    itemPickupPointWrapperService.findProductL5DetailByItemSkusAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        true, null);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, itemPickupPointRequests,
        true);
    verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU));
    verify(productHelperService).getCurrentBuyableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(productHelperService).getCurrentDiscoverableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean());
  }

  @Test
  public void findProductL5DetailByItemSkusAndPickupPointCodeTestEmptyItemPickupPointTest()
      throws Exception {
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false)).thenReturn(new ArrayList<>());
    itemPickupPointWrapperService.findProductL5DetailByItemSkusAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false, null);
    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false);

  }

  @Test
  public void findProductL5DetailByItemSkusAndPickupPointCodeItemMapEmptyTest() throws Exception {
    product.setSynchronized(true);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID,
        ImmutableSet.of(ITEM_SKU))).thenReturn(new ArrayList<>());
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCT_SKU))).thenReturn(Arrays.asList(product));

    itemPickupPointWrapperService.findProductL5DetailByItemSkusAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false, null);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID,
        ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCT_SKU));
  }

  @Test
  public void findProductL5DetailByItemSkusAndPickupPointCodeProductMapEmptyTest() throws Exception {
    product.setSynchronized(true);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID,
        ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCT_SKU))).thenReturn(new ArrayList<>());
    when(productHelperService.getCurrentBuyableStatusForItem(Mockito.anySet(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(true);
    when(productHelperService.getCurrentDiscoverableStatusForItem(
        Mockito.anySet(), Mockito.anyString(),
        Mockito.anyBoolean())).thenReturn(true);

    itemPickupPointWrapperService.findProductL5DetailByItemSkusAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false, null);

    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        itemPickupPointRequests, false);
    verify(itemService).getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID,
        ImmutableSet.of(ITEM_SKU));
    verify(productService).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCT_SKU));
  }

  private static CreateFbbPickupPointRequest getCreateFbbPickupPointRequest() {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = new CreateFbbPickupPointRequest();
    createFbbPickupPointRequest.setItemSku(ITEM_SKU);
    createFbbPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    createFbbPickupPointRequest.setBusinessPartnerCode(MERCHANT_CODE);
    return createFbbPickupPointRequest;
  }

  @Test
  public void autoCreatePickupPointTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    Mockito.when(xbpOutbound.getBusinessPartnerDetails(any(), any(), any(), any())).thenReturn(new ProfileResponse());
    Mockito.when(itemPickupPointService.findByStoreIdAndPickupPointCode(any(), any())).thenReturn(Collections.singletonList(itemPickupPoint));
    AutoCreatePickupPointListResponse autoCreatePickupPointListResponse =
        itemPickupPointWrapperService.autoCreatePickupPoint(autoCreatePickupPointRequestList, mandatoryRequestParam);
    Mockito.verify(itemPickupPointService)
        .autoCreateL5(eq(autoCreatePickupPointRequestList.getAutoCreatePickupPointRequests().get(0)),
            eq(mandatoryRequestParam), eq(item), any(), eq(product.getProductType()));
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(saveOperationService).saveProduct(product);
    Assertions.assertEquals(1, autoCreatePickupPointListResponse.getData().size());
    Assertions.assertTrue(autoCreatePickupPointListResponse.getData().get(0).isSuccess());
  }

  @Test
  public void autoCreatePickupPointProductIsFbbActiveTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID);
    product.setFbbActivated(true);
    product.getPickupPointCodes().add(PICKUP_POINT_CODE);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    Mockito.when(xbpOutbound.getBusinessPartnerDetails(any(), any(), any(), any())).thenReturn(new ProfileResponse());
    Mockito.when(itemPickupPointService.findByStoreIdAndPickupPointCode(any(), any())).thenReturn(Collections.singletonList(itemPickupPoint));
    AutoCreatePickupPointListResponse autoCreatePickupPointListResponse =
        itemPickupPointWrapperService.autoCreatePickupPoint(autoCreatePickupPointRequestList, mandatoryRequestParam);
    Mockito.verify(itemPickupPointService)
        .autoCreateL5(eq(autoCreatePickupPointRequestList.getAutoCreatePickupPointRequests().get(0)),
            eq(mandatoryRequestParam), eq(item), any(), eq(product.getProductType()));
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Assertions.assertEquals(1, autoCreatePickupPointListResponse.getData().size());
    Assertions.assertTrue(autoCreatePickupPointListResponse.getData().get(0).isSuccess());
  }

  @Test
  public void autoCreatePickupPointExceptionProductSuspendedTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID);
    product.setSuspended(true);
    Mockito.when(productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU))
        .thenReturn(product);
    Mockito.when(xbpOutbound.getBusinessPartnerDetails(any(), any(), any(), any())).thenReturn(new ProfileResponse());
    Mockito.when(itemPickupPointService.findByStoreIdAndPickupPointCode(any(), any())).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(item);
    AutoCreatePickupPointListResponse autoCreatePickupPointListResponse =
        itemPickupPointWrapperService.autoCreatePickupPoint(autoCreatePickupPointRequestList, mandatoryRequestParam);
    Mockito.verify(productService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Assertions.assertEquals(1, autoCreatePickupPointListResponse.getData().size());
    Assertions.assertFalse(autoCreatePickupPointListResponse.getData().get(0).isSuccess());
    Assertions.assertEquals(ApiErrorCodes.PRODUCT_SUSPENDED.getErrorMessage() + product.getProductSku(),
        autoCreatePickupPointListResponse.getData().get(0).getErrorMessage());
  }

  @Test
  public void autoCreatePickupPointExceptionProductSkuNullTest() throws Exception {
    autoCreatePickupPointRequestList.getAutoCreatePickupPointRequests().get(0).setWebProductSku(null);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID);
    AutoCreatePickupPointListResponse autoCreatePickupPointListResponse =
        itemPickupPointWrapperService.autoCreatePickupPoint(autoCreatePickupPointRequestList, mandatoryRequestParam);
    Assertions.assertEquals(1, autoCreatePickupPointListResponse.getData().size());
    Assertions.assertFalse(autoCreatePickupPointListResponse.getData().get(0).isSuccess());
    Assertions.assertEquals(ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK,
        autoCreatePickupPointListResponse.getData().get(0).getErrorMessage());
  }

  @Test
  public void findFbbTrueOnlinePickupPointsAndItemSkusInTest() {
    itemPickupPointWrapperService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, new ArrayList<>());
    Mockito.verify(itemPickupPointService).findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, new ArrayList<>());
  }

  @Test
  public void findItemPickupPointsByItemSkusNullTest() {
    itemPickupPointWrapperService.findItemPickupPointsByItemSkus(STORE_ID,
        new SimpleListStringRequest(Collections.singletonList(ITEM_SKU)), 0, 50, null);
    Mockito.verify(itemPickupPointService).findItemPickupPointByProductSkusOrItemSkusForFbb(STORE_ID, new ArrayList<>(),
        Collections.singletonList(ITEM_SKU), false, 0, 50);
  }

  @Test
  public void findItemPickupPointsByItemSkusEmptyTest() {
    Mockito.when(itemPickupPointService.findItemPickupPointByProductSkusOrItemSkus(STORE_ID, new ArrayList<>(),
        Collections.singletonList(ITEM_SKU), 0, 50)).thenReturn(new PageImpl<>(new ArrayList<>()));
    itemPickupPointWrapperService.findItemPickupPointsByItemSkus(STORE_ID,
        new SimpleListStringRequest(Collections.singletonList(ITEM_SKU)), 0, 50, null);
    Mockito.verify(itemPickupPointService).findItemPickupPointByProductSkusOrItemSkusForFbb(STORE_ID, new ArrayList<>(),
        Collections.singletonList(ITEM_SKU), false, 0, 50);
  }

  @Test
  public void findItemPickupPointsByItemSkusTest() {
    Mockito.when(itemPickupPointService.findItemPickupPointByProductSkusOrItemSkusForFbb(STORE_ID, new ArrayList<>(),
            Collections.singletonList(ITEM_SKU), false, 0, 50))
        .thenReturn(new PageImpl<>(Collections.singletonList(itemPickupPoint)));
    Page<ItemPickupPointL5Response> itemPickupPointsByItemSkus =
        itemPickupPointWrapperService.findItemPickupPointsByItemSkus(STORE_ID,
            new SimpleListStringRequest(Collections.singletonList(ITEM_SKU)), 0, 50, null);
    Mockito.verify(itemPickupPointService).findItemPickupPointByProductSkusOrItemSkusForFbb(STORE_ID, new ArrayList<>(),
        Collections.singletonList(ITEM_SKU), false, 0, 50);
    Mockito.verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        Collections.singletonList(itemPickupPoint.getPickupPointCode()));
    Assertions.assertEquals(itemPickupPoint.getPickupPointCode(),
        itemPickupPointsByItemSkus.getContent().get(0).getPickUpPointCode());
    Assertions.assertEquals(itemPickupPoint.getItemSku(), itemPickupPointsByItemSkus.getContent().get(0).getItemSku());
  }

  @Test
  public void fetchBasicDetailsByItemSkuAndPickupPointCodeListTest() {
    List<ItemPickupPointRequest> itemPickupPointRequestList = new ArrayList<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequestList.add(itemPickupPointRequest);
    when(itemPickupPointService.fetchBasicDetailsByItemSkuAndPickupPointCodeList(
        Constants.DEFAULT_STORE_ID, itemPickupPointRequestList)).thenReturn(
        Collections.singletonList(itemPickupPoint));
    itemPickupPointWrapperService.fetchBasicDetailsByItemSkuAndPickupPointCodeList(
        Constants.DEFAULT_STORE_ID, itemPickupPointRequestList);
    Mockito.verify(itemPickupPointService)
        .fetchBasicDetailsByItemSkuAndPickupPointCodeList(Constants.DEFAULT_STORE_ID,
            itemPickupPointRequestList);
  }

  @Test
  public void fetchItemDetailsByEanUpcCodeTest() {
    Mockito.when(
        itemService.fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(
            any(), any(), anySet())).thenReturn(Collections.singletonList(item));
    Mockito.when(
            itemPickupPointService.findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(
                any(), any(), anyBoolean(), eq(Constants.CNC), anyInt()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(any(), any())).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT, "100",
            SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT));
    List<EanUpcPickupPointCodeResponse> responses =
        itemPickupPointWrapperService.fetchItemDetailsByEanUpcCode(STORE_ID,
            eanUpcPickupPointCodeRequest, Constants.CNC);
    Assertions.assertEquals(responses.size(), 1);
    Mockito.verify(itemService)
        .fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(any(),
            any(), anySet());
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(any(), any(),
            anyBoolean(), eq(Constants.CNC), anyInt());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(any(), any());
  }

  @Test
  public void fetchItemDetailsByEanUpcCodeBlankRequestTest() {
    eanUpcPickupPointCodeRequest.getPickupPointDetails().add(new PickupPointDetailsDTO("", ""));
    Mockito.when(
        itemService.fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(
            any(), any(), anySet())).thenReturn(Collections.singletonList(item));
    Mockito.when(
            itemPickupPointService.findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(
                any(), any(), anyBoolean(), eq(Constants.CNC), anyInt()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(any(), any())).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT, "100",
            SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT));
    List<EanUpcPickupPointCodeResponse> responses =
        itemPickupPointWrapperService.fetchItemDetailsByEanUpcCode(STORE_ID,
            eanUpcPickupPointCodeRequest, Constants.CNC);
    Assertions.assertEquals(responses.size(), 1);
    Mockito.verify(itemService)
        .fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(any(),
            any(), anySet());
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(any(), any(),
            anyBoolean(), eq(Constants.CNC), anyInt());
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(any(), any());
  }

  @Test
  public void fetchItemDetailsByEanUpcCodeBlankRequestDefaultChannelTest() {
    item.setMerchantCode(MERCHANT_CODE);
    eanUpcPickupPointCodeRequest.setPickupPointDetails(
        Collections.singletonList(new PickupPointDetailsDTO(MERCHANT_CODE, PICKUP_POINT_CODE)));
    Mockito.when(
        itemService.fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(STORE_ID,
            EAN_UPC, Collections.singleton(MERCHANT_CODE))).thenReturn(Collections.singletonList(item));
    Mockito.when(
        itemPickupPointService.findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(STORE_ID,
            Collections.singleton(item.getItemSku() + Constants.HYPHEN + PICKUP_POINT_CODE), true, Constants.DEFAULT,
            100)).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT, "100",
            SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT));
    List<EanUpcPickupPointCodeResponse> responses =
        itemPickupPointWrapperService.fetchItemDetailsByEanUpcCode(STORE_ID, eanUpcPickupPointCodeRequest,
            Constants.DEFAULT);
    Mockito.verify(itemService)
        .fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(STORE_ID, EAN_UPC,
            Collections.singleton(MERCHANT_CODE));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(STORE_ID,
            Collections.singleton(item.getItemSku() + Constants.HYPHEN + PICKUP_POINT_CODE), true, Constants.DEFAULT,
            100);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT);
    Assertions.assertEquals(responses.size(), 1);
  }

  @Test
  public void fetchItemDetailsByEanUpcCodeBlankRequestNoChannelTest() {
    item.setMerchantCode(MERCHANT_CODE);
    eanUpcPickupPointCodeRequest.setPickupPointDetails(
        Collections.singletonList(new PickupPointDetailsDTO(MERCHANT_CODE, PICKUP_POINT_CODE)));
    Mockito.when(
        itemService.fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(STORE_ID,
            EAN_UPC, Collections.singleton(MERCHANT_CODE))).thenReturn(Collections.singletonList(item));
    Mockito.when(
        itemPickupPointService.findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(STORE_ID,
            Collections.singleton(item.getItemSku() + Constants.HYPHEN + PICKUP_POINT_CODE), true, StringUtils.EMPTY,
            100)).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT, "100",
            SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT));
    List<EanUpcPickupPointCodeResponse> responses =
        itemPickupPointWrapperService.fetchItemDetailsByEanUpcCode(STORE_ID, eanUpcPickupPointCodeRequest,
            StringUtils.EMPTY);
    Mockito.verify(itemService)
        .fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(STORE_ID, EAN_UPC,
            Collections.singleton(MERCHANT_CODE));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(STORE_ID,
            Collections.singleton(item.getItemSku() + Constants.HYPHEN + PICKUP_POINT_CODE), true, StringUtils.EMPTY,
            100);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT);
    Assertions.assertEquals(responses.size(), 1);
  }

  @Test
  public void fetchItemDetailsByEanUpcCodeBlankRequestAllChannelTest() {
    item.setMerchantCode(MERCHANT_CODE);
    eanUpcPickupPointCodeRequest.setPickupPointDetails(
        Collections.singletonList(new PickupPointDetailsDTO(MERCHANT_CODE, PICKUP_POINT_CODE)));
    Mockito.when(
        itemService.fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(STORE_ID,
            EAN_UPC, Collections.singleton(MERCHANT_CODE))).thenReturn(Collections.singletonList(item));
    Mockito.when(
        itemPickupPointService.findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(STORE_ID,
            Collections.singleton(item.getItemSku() + Constants.HYPHEN + PICKUP_POINT_CODE), true, StringUtils.EMPTY,
            100)).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT, "100",
            SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT));
    List<EanUpcPickupPointCodeResponse> responses =
        itemPickupPointWrapperService.fetchItemDetailsByEanUpcCode(STORE_ID, eanUpcPickupPointCodeRequest,
            Constants.ALL);
    Mockito.verify(itemService)
        .fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(STORE_ID, EAN_UPC,
            Collections.singleton(MERCHANT_CODE));
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(STORE_ID,
            Collections.singleton(item.getItemSku() + Constants.HYPHEN + PICKUP_POINT_CODE), true, StringUtils.EMPTY,
            100);
    Mockito.verify(systemParameterService)
        .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterNames.EAN_UPC_FETCH_MAX_LIMIT);
    Assertions.assertEquals(responses.size(), 1);
  }

  @Test
  public void getCncAtL5ByProductSkuTest() {
    Mockito.when(itemPickupPointService.findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(
            STORE_ID, PRODUCT_SKU, true))
        .thenReturn(itemPickupPoint);
    itemPickupPointWrapperService.getCncAtL5ByProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService).findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, true);
  }

  @Test
  public void getCncAtL5ByProductSkuTestWithEmptyProductSku() {
    Mockito.when(itemPickupPointService.findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(
            STORE_ID, "", true))
        .thenReturn(itemPickupPoint);
    itemPickupPointWrapperService.getCncAtL5ByProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService).findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, true);
  }

  @Test
  public void fetchItemSummaryByItemSkusAndPickupPointCodeEmpty() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
        "itemSummaryListResponseCacheClients", List.of(Constants.DEFAULT_CLIENT_ID));
    ReflectionTestUtils.setField(itemPickupPointWrapperService,
        "itemSummaryListRequestSize", 20);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "configuredMaxCacheFetchSize", 0);
    ReflectionTestUtils.setField(itemPickupPointWrapperService, "usePcbMasterData", false);
    ReflectionTestUtils.setField(itemPickupPointWrapperService,"listOfClientsToExcludeItemCatalogs",Constants.DEFAULT_CHANNEL);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setProductSku(PRODUCT_SKU);
    itemPickupPoint1.setPrice(Collections.singleton(new Price()));
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.B2B).build());
    itemViewConfigs.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.DEFAULT_CHANNEL).build());
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPoint2.setItemSku(ITEM_SKU);
    itemPickupPoint2.setProductSku(PRODUCT_SKU);
    product.setCurationStatus(CurationStatus.APPROVED);
    itemPickupPoint2.setItemViewConfig(Collections.singleton(
        ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.DEFAULT_CHANNEL).build()));
    Price price = new Price();
    DiscountPrice merchantPromoDiscountPrice = new DiscountPrice();
    merchantPromoDiscountPrice.setStartDateTime(new Date());
    Date date = new Date();
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(date);
    calendar.add(Calendar.DAY_OF_MONTH, 10);
    Date futureDate = calendar.getTime();
    merchantPromoDiscountPrice.setEndDateTime(futureDate);
    price.setMerchantPromoDiscountPrice(merchantPromoDiscountPrice);
    itemPickupPoint2.setPrice(Collections.singleton(price));
    itemPickupPoint1.setPrice(Collections.singleton(price));
    List<ItemPickupPointRequest> pickupPointRequests =
        Arrays.asList(ItemPickupPointRequest.builder().pickupPointCode(PICKUP_POINT_CODE_2).itemSku(ITEM_SKU).build(),
            ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build(),
            ItemPickupPointRequest.builder().itemSku(itemPickupPoint.getItemSku())
                .pickupPointCode(itemPickupPoint.getPickupPointCode()).build());
    Mockito.when(productHelperService.getOriginalBuyableStatusForItem(any(), Mockito.anyBoolean())).thenReturn(true);
    Mockito.when(productHelperService.getOriginalDiscoverableStatusForItem(any(), Mockito.anyBoolean())).thenReturn(true);
    DiscountPrice discountPrice = new DiscountPrice();
    itemPickupPoint1.setDistribution(true);
    itemPickupPoint2.setDistribution(true);
    itemPickupPoint.setDistribution(true);
    when(itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, pickupPointRequests, false)).thenReturn(
        Arrays.asList(itemPickupPoint1, itemPickupPoint2, itemPickupPoint));
    when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU_2, PICKUP_POINT_CODE_2)).thenReturn(
        null);
    Mockito.when(applicationContext.getBean(ItemPickupPointWrapperServiceImpl.class))
        .thenReturn(itemPickupPointWrapperService);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductSkuIn(STORE_ID, Arrays.asList(PRODUCT_SKU))).thenReturn(
        Arrays.asList(product));
    Mockito.when(itemPriceService.validMerchantPromoDiscountPrice(Mockito.any(DiscountPrice.class))).thenReturn(true)
        .thenReturn(false);
    when(catalogService.getCategoryCodeToItemCatalogsMap(eq(USERNAME), eq(REQUEST_ID),
        anyList())).thenReturn(
        ImmutableMap.of(CATEGORY_CODE_1, Arrays.asList(itemCatalogVO), CATEGORY_CODE_2, Arrays.asList(itemCatalogVO)));
    when(itemHelperService.processDiscountPricesByPriority(listOfDiscountPrices)).thenReturn(discountPrice);
    List<ItemSummaryListResponse> itemSummaryListResponses =
        itemPickupPointWrapperService.fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME,
            false, pickupPointRequests, null, Constants.DEFAULT_CLIENT_ID, Constants.ALL, true);
    Assertions.assertTrue(itemSummaryListResponses.isEmpty());
    verify(itemPickupPointService).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, pickupPointRequests,
        false);
  }
}
