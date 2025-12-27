package com.gdn.x.product.service.impl;

import static com.gdn.common.helper.GdnCollectionHelper.toSet;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.CncActivatedPickupPointResponse;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.ItemPickupPointRepository;
import com.gdn.x.product.dao.api.OfflineItemRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.DeleteOfflineItemVO;
import com.gdn.x.product.model.vo.ItemPickupPointPriceVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.UpsertOfflineItemPriceResponseVO;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PickupPointService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductL3SolrReindexStatusService;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ReindexService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.google.common.collect.ImmutableSet;

public class OfflineItemServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String OFFLINE_ITEM_ID = "offline-item-id";
  private static final String OFFLINE_ITEM_ID_2 = "offline-item-id-2";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String MERCHANT_CODE_2 = "merchant-code-2";
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final String ITEM_CODE = "item-code";
  private static final String ITEM_SKU = "item-sku";
  private static final List<String> ITEM_SKUS = Arrays.asList(ITEM_SKU);
  private static final Set<String> ITEM_SKUS_SET = new HashSet<>(ITEM_SKUS);
  private static final String EXTERNAL_PICKUP_POINT = "external-pickup-point";
  private static final String PICKUP_POINT = "pickup-point";
  private static final String PICKUP_POINT_2 = "pickup-point-2";
  private static final Double LIST_PRICE = 15000.0;
  private static final Double OFFER_PRICE = 10000.0;
  private static final Double LIST_PRICE_2 = 25000.0;
  private static final Double OFFER_PRICE_2 = 20000.0;
  private static final String USERNAME = "username";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String BLANK = "";

  private static final String PRODUCT_SKU_1 = "product-sku-1";
  private static final String PRODUCT_SKU_2 = "product-sku-2";
  private static final double LIST_PRICE_1500 = 1500.0;
  private static final double OFFER_PRICE_1000 = 1000.0;
  private static final double LIST_PRICE_2000 = 2000.0;
  private static final double OFFER_PRICE_1500 = 1500.0;
  private static final double LIST_PRICE_25000 = 25000.0;
  private static final double OFFER_PRICE_20000 = 20000.0;
  private static final double LIST_PRICE_26500 = 26500.0;
  private static final double OFFER_PRICE_21500 = 21500.0;
  private static final String ITEM_SKU1 = "item-sku1";
  private static final String ITEM_SKU2 = "item-sku2";
  private static final String ITEM_SKU3 = "item-sku3";
  private static final String ITEM_SKU4 = "item-sku4";
  private static final String PRODUCT_OR_ITEM_NOT_FOUND = "Product or Item not Found";
  private static final String PRISTINE_ID = "pristine-id";
  private static final boolean IS_PV_ENABLED = true;
  private static final boolean IN_ALL_PRODUCT = false;
  private static final Double MINIMUM_PRICE = 1000d;
  private static final Integer LIMIT_OFFLINE_ITEMS = 10;
  private static final String DEFAULT_CHANNEL = "default channel";
  private static final Pageable pageable = PageRequest.of(0, 10);
  private Map<String, Set<String>> productSkuAndPickupPointCodeMap = new HashMap<>();

  @InjectMocks
  private OfflineItemServiceImpl offlineItemServiceImpl;

  @Mock
  private OfflineItemRepository offlineItemRepository;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ItemService itemService;

  @Mock
  private GdnMapper gdnMapper;

  @Mock
  private ProductService productService;

  @Mock
  private SkuValidatorImpl skuValidator;

  @Mock
  private ProductCacheableService productCacheableService;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private MasterDataConstructorService masterDataConstructorService;

  @Mock
  private ProductSearchHelperService productSearchHelperService;

  @Mock
  private CacheEvictItemService cacheEvictItemService;

  @Mock
  private ProductSearchService productSearchService;

  @Mock
  private SaveOperationService saveOperationService;

  @Captor
  private ArgumentCaptor<Pageable> pageableCaptor;

  @Captor
  private ArgumentCaptor<List<OfflineItem>> offlineItemsCaptor;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ItemPickupPointRepository itemPickupPointRepository;

  @Mock
  private ChannelService channelService;

  @Mock
  private ReindexService reindexService;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Mock
  private ProductL3SolrReindexStatusService productL3SolrReindexStatusService;

  @Mock
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Mock
  private KafkaPublisher kafkaProducer;

  private OfflineItem offlineItem;
  private OfflineItem offlineItem2;
  private OfflineItem offlineItemForUpsert;
  private OfflineItem offlineItemForUpsert2;
  private OfflineItem existedOfflineItem;
  private OfflineItem offlineItemAnotherPickupPoint;
  private List<OfflineItem> offlineItemList;
  private List<OfflineItem> offlineItemListForUpsert;
  private MandatoryRequestParam mandatoryRequestParam;
  private List<String> pickuPointCodes;
  private SystemParameter minimumPriceSystemParameter;
  private SystemParameter enableDeferredSolrReindexSystemParameter;
  private SystemParameter disableDeferredSolrReindexSystemParameter;
  private SystemParameter enableOfflineDeferredSolrReindexSystemParameter;
  private SystemParameter disableOfflineDeferredSolrReindexSystemParameter;
  private SystemParameter limitOfflineItemsSystemParameter;
  private CncActivatedPickupPointResponse cncActivatedPickupPointResponse;
  private GdnRestSingleResponse<CncActivatedPickupPointResponse> restSingleResponse;
  private Item item;
  private Item item2;
  private List<String> productSkus;
  private Product product1;
  private Product product2;
  private List<DeleteOfflineItemVO> deleteOfflineItemVOs;
  private List<Item> itemList;
  private List<PickupPoint> pickupPoints;
  private PickupPoint pickupPoint;
  private PickupPoint pickupPoint2;
  private ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
  private Price price = new Price();

  private UpsertOfflineItemRequest upsertOfflineItemRequest;
  private UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest;
  private BusinessPartnerPickupPoint businessPartnerPickupPoint;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    ReflectionTestUtils.setField(offlineItemServiceImpl, "isProductVisibilityEnabled", IS_PV_ENABLED);

    this.offlineItem = new OfflineItem();
    this.offlineItem.setPickupPointCode(OfflineItemServiceImplTest.PICKUP_POINT);
    this.offlineItem.setListPrice(OfflineItemServiceImplTest.LIST_PRICE);
    this.offlineItem.setOfferPrice(OfflineItemServiceImplTest.OFFER_PRICE);
    this.offlineItem.setExternalPickupPointCode(OfflineItemServiceImplTest.EXTERNAL_PICKUP_POINT);
    this.offlineItem.setMerchantSku(OfflineItemServiceImplTest.MERCHANT_SKU);
    this.offlineItem.setMerchantCode(OfflineItemServiceImplTest.MERCHANT_CODE);
    this.offlineItem.setStoreId(OfflineItemServiceImplTest.STORE_ID);
    this.offlineItem.setItemSku(OfflineItemServiceImplTest.ITEM_SKU);
    this.offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);

    this.offlineItem2 = new OfflineItem();
    this.offlineItem2.setPickupPointCode(OfflineItemServiceImplTest.PICKUP_POINT_2);
    this.offlineItem2.setListPrice(OfflineItemServiceImplTest.LIST_PRICE_2);
    this.offlineItem2.setOfferPrice(OfflineItemServiceImplTest.OFFER_PRICE_2);
    this.offlineItem2.setExternalPickupPointCode(OfflineItemServiceImplTest.EXTERNAL_PICKUP_POINT);
    this.offlineItem2.setMerchantSku(OfflineItemServiceImplTest.MERCHANT_SKU);
    this.offlineItem2.setMerchantCode(OfflineItemServiceImplTest.MERCHANT_CODE);
    this.offlineItem2.setStoreId(OfflineItemServiceImplTest.STORE_ID);
    this.offlineItem2.setItemSku(OfflineItemServiceImplTest.ITEM_SKU);
    this.offlineItem2.setOfflineItemId(OFFLINE_ITEM_ID_2);

    this.existedOfflineItem = new OfflineItem();
    this.existedOfflineItem.setItemSku(ITEM_SKU);

    this.offlineItemList = new ArrayList<>();
    this.offlineItemList.add(this.offlineItem);

    this.offlineItemForUpsert = new OfflineItem();
    this.offlineItemForUpsert.setPickupPointCode(OfflineItemServiceImplTest.PICKUP_POINT);
    this.offlineItemForUpsert.setListPrice(OfflineItemServiceImplTest.LIST_PRICE);
    this.offlineItemForUpsert.setOfferPrice(OfflineItemServiceImplTest.OFFER_PRICE);
    this.offlineItemForUpsert.setMerchantCode(OfflineItemServiceImplTest.MERCHANT_CODE);
    this.offlineItemForUpsert.setStoreId(OfflineItemServiceImplTest.STORE_ID);
    this.offlineItemForUpsert.setItemSku(OfflineItemServiceImplTest.ITEM_SKU);
    this.offlineItemForUpsert.setOfflineItemId(OFFLINE_ITEM_ID);

    this.offlineItemForUpsert2 = new OfflineItem();
    this.offlineItemForUpsert2.setPickupPointCode(OfflineItemServiceImplTest.PICKUP_POINT);
    this.offlineItemForUpsert2.setListPrice(OfflineItemServiceImplTest.LIST_PRICE);
    this.offlineItemForUpsert2.setOfferPrice(OfflineItemServiceImplTest.OFFER_PRICE_2);
    this.offlineItemForUpsert2.setMerchantCode(OfflineItemServiceImplTest.MERCHANT_CODE);
    this.offlineItemForUpsert2.setStoreId(OfflineItemServiceImplTest.STORE_ID);
    this.offlineItemForUpsert2.setItemSku(OfflineItemServiceImplTest.ITEM_SKU);
    this.offlineItemForUpsert2.setOfflineItemId(OFFLINE_ITEM_ID_2);

    this.offlineItemListForUpsert = new ArrayList<>();
    this.offlineItemListForUpsert.add(this.offlineItemForUpsert);

    this.mandatoryRequestParam = MandatoryRequestParam.generateMandatoryRequestParam(OfflineItemServiceImplTest.STORE_ID,
        OfflineItemServiceImplTest.CHANNEL_ID, OfflineItemServiceImplTest.CLIENT_ID, OfflineItemServiceImplTest.REQUEST_ID,
        OfflineItemServiceImplTest.USERNAME, OfflineItemServiceImplTest.BLANK);

    this.pickuPointCodes = new ArrayList<>();
    this.pickuPointCodes.add(PICKUP_POINT);

    this.cncActivatedPickupPointResponse = new CncActivatedPickupPointResponse();
    this.cncActivatedPickupPointResponse.setCncActivatedPickupPointCodes(pickuPointCodes);
    this.restSingleResponse = new GdnRestSingleResponse<>();
    this.restSingleResponse.setValue(this.cncActivatedPickupPointResponse);

    this.item = new Item();
    this.item.setMerchantCode(MERCHANT_CODE);
    this.item.setItemSku(ITEM_SKU);

    this.item2 = new Item();
    this.item2.setMerchantCode(MERCHANT_CODE_2);
    this.item2.setItemSku(ITEM_SKU2);

    this.productSkus = new ArrayList<>();
    productSkus.add(PRODUCT_SKU_1);
    productSkus.add(PRODUCT_SKU_2);

    product1 = new Product();
    product1.setProductSku(PRODUCT_SKU_1);

    product2 = new Product();
    product2.setProductSku(PRODUCT_SKU_2);

    this.offlineItemAnotherPickupPoint = new OfflineItem();
    this.offlineItemAnotherPickupPoint.setPickupPointCode(PICKUP_POINT_2);

    deleteOfflineItemVOs = Arrays.asList(DeleteOfflineItemVO.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT).build(),
        DeleteOfflineItemVO.builder().itemSku(ITEM_SKU2).pickupPointCode(PICKUP_POINT).build(),
        DeleteOfflineItemVO.builder().itemSku(ITEM_SKU3).pickupPointCode(PICKUP_POINT_2).build(),
        DeleteOfflineItemVO.builder().itemSku(ITEM_SKU4).pickupPointCode(PICKUP_POINT_2).build());

    minimumPriceSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.MINIMUM_PRICE, MINIMUM_PRICE + "", "");

    enableDeferredSolrReindexSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX, "true", "");
    disableDeferredSolrReindexSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX, "false", "");
    enableOfflineDeferredSolrReindexSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX, "true", "");
    disableOfflineDeferredSolrReindexSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX, "false", "");
    limitOfflineItemsSystemParameter =
        new SystemParameter(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS, String.valueOf(LIMIT_OFFLINE_ITEMS), "");

    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(disableDeferredSolrReindexSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX)).thenReturn(
        disableOfflineDeferredSolrReindexSystemParameter);
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(DEFAULT_CHANNEL);

    this.pickupPoints = new ArrayList<>();
    this.pickupPoint = new PickupPoint();
    this.pickupPoint.setPickupPointCode(PICKUP_POINT);
    this.pickupPoint.setCncActivated(true);
    this.pickupPoints.add(pickupPoint);

    price.setOfferPrice(OFFER_PRICE_1000);
    price.setListPrice(LIST_PRICE_1500);
    this.itemPickupPoint.setItemSku(ITEM_SKU1);

    upsertOfflineItemRequest = new UpsertOfflineItemRequest();
    updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();

    Price price = new Price();
    price.setListPrice(1.0);
    price.setOfferPrice(2.0);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>(Collections.singletonList(itemViewConfig));
    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.getPrice().add(price);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    Set<Item> items = new HashSet<>();
    items.add(item);
    Mockito.when(saveOperationService
        .updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyMap(), Mockito.anyMap(), Mockito.eq(items), Mockito.anyList(),
            Mockito.anyMap(), Mockito.anyList(), Mockito.anyList())).thenReturn(new ArrayList<>());
    Mockito.when(
        itemPickupPointService.updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(itemService.findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet()))
        .thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());

    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCncActivated(true);
    businessPartnerPickupPoint.setCode(PICKUP_POINT);
    Mockito.when(
            this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(Arrays.asList(businessPartnerPickupPoint));

    UpsertOfflineItemPriceResponseVO upsertOfflineItemPriceResponseVO = new UpsertOfflineItemPriceResponseVO();
    BeanUtils.copyProperties(upsertOfflineItemRequest, upsertOfflineItemPriceResponseVO);
    upsertOfflineItemPriceResponseVO.setItemSku(ITEM_SKU);
    upsertOfflineItemPriceResponseVO.setPickupPointCode(PICKUP_POINT);
    upsertOfflineItemPriceResponseVO.setOfflineItemId(OFFLINE_ITEM_ID);
    upsertOfflineItemPriceResponseVO.setOfferPrice(1000d);
    upsertOfflineItemPriceResponseVO.setListPrice(1000d);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(), Mockito.eq(UpsertOfflineItemPriceResponseVO.class)))
        .thenReturn(upsertOfflineItemPriceResponseVO);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(), Mockito.any()))
        .thenReturn(new ItemPickupPointDataChangeEventModel());
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.offlineItemRepository);
    verifyNoMoreInteractions(this.productAndItemSolrIndexerService);
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.masterDataConstructorService);
    verifyNoMoreInteractions(this.productSearchHelperService);
    verifyNoMoreInteractions(this.cacheEvictItemService);
    verifyNoMoreInteractions(this.pickupPointService);
    verifyNoMoreInteractions(this.reindexService);
    verifyNoMoreInteractions(this.inventoryOutbound);
    verifyNoMoreInteractions(this.productL3SolrReindexStatusService);
  }

  @Test
  public void upsertOfflineItemTest_multiple_itemPickupPoint_mppTrue() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    UpsertOfflineItemRequest upsertOfflineItemRequest2 = new UpsertOfflineItemRequest();
    BeanUtils.copyProperties(offlineItem2, upsertOfflineItemRequest2);
    restSingleResponse.getValue().setCncActivatedPickupPointCodes(new ArrayList<>());

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(), Mockito.any()))
        .thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Map<String, Product> productSkuAndProductMap = new HashMap<>();
    productSkuAndProductMap.put(PRODUCT_SKU_1, product1);
    Mockito.when(productCacheableService.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.any())).thenReturn(productSkuAndProductMap);
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_2);
    businessPartnerPickupPoint.setActivated(false);
    Mockito.when(
        businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString())).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequest.setCncActive(false);
    upsertOfflineItemRequest.setBuyable(true);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest2.setCncActive(false);
    upsertOfflineItemRequest2.setDiscoverable(true);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest2);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequests);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService, times(2)).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    assertEquals(responses.get(0).isFbbActivated(), itemPickupPoint.isFbbActivated());
  }

  @Test
  public void upsertOfflineItemTest_multiple_itemPickupPoint_mppTrue1() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    UpsertOfflineItemRequest upsertOfflineItemRequest2 = new UpsertOfflineItemRequest();
    BeanUtils.copyProperties(offlineItem2, upsertOfflineItemRequest2);
    restSingleResponse.getValue().setCncActivatedPickupPointCodes(new ArrayList<>());

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(), Mockito.any()))
        .thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Map<String, Product> productSkuAndProductMap = new HashMap<>();
    productSkuAndProductMap.put(PRODUCT_SKU_1, product1);
    Mockito.when(productCacheableService.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.any())).thenReturn(productSkuAndProductMap);
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_2);
    businessPartnerPickupPoint.setActivated(false);
    Mockito.when(
        businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString())).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequest.setCncActive(false);
    upsertOfflineItemRequest.setBuyable(true);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest2.setCncActive(false);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest2);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequests);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService, times(2)).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTest_multiple_itemPickupPoint_mppTrue_isPriceChanged() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    UpsertOfflineItemRequest upsertOfflineItemRequest2 = new UpsertOfflineItemRequest();
    BeanUtils.copyProperties(offlineItem2, upsertOfflineItemRequest2);
    restSingleResponse.getValue().setCncActivatedPickupPointCodes(new ArrayList<>());
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(), Mockito.any()))
        .thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Map<String, Product> productSkuAndProductMap = new HashMap<>();
    productSkuAndProductMap.put(PRODUCT_SKU_1, product1);
    Mockito.when(productCacheableService.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.any())).thenReturn(productSkuAndProductMap);
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_2);
    businessPartnerPickupPoint.setActivated(false);
    Mockito.when(
        businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString())).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    itemPickupPoint.getPrice().iterator().next().setListPrice(LIST_PRICE_2);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(OFFER_PRICE_2);
    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequest.setCncActive(false);
    upsertOfflineItemRequest.setBuyable(true);
    upsertOfflineItemRequest.setOfferPrice(itemPickupPoint.getPrice().iterator().next().getOfferPrice());
    upsertOfflineItemRequest.setListPrice(itemPickupPoint.getPrice().iterator().next().getListPrice());
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest2.setCncActive(false);
    upsertOfflineItemRequest2.setDiscoverable(true);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest2);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequests);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService, times(2)).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTest_multiple_itemPickupPoint_buyableFlag_update_mppTrue() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    UpsertOfflineItemRequest upsertOfflineItemRequest2 = new UpsertOfflineItemRequest();
    BeanUtils.copyProperties(offlineItem2, upsertOfflineItemRequest2);
    restSingleResponse.getValue().setCncActivatedPickupPointCodes(new ArrayList<>());

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Map<String, Product> productSkuAndProductMap = new HashMap<>();
    productSkuAndProductMap.put(PRODUCT_SKU_1, product1);
    Mockito.when(productCacheableService.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.any())).thenReturn(productSkuAndProductMap);
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_2);
    businessPartnerPickupPoint.setActivated(false);
    Mockito.when(
        businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString())).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequest.setCncActive(false);
    upsertOfflineItemRequest.setDiscoverable(false);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest2.setCncActive(false);
    upsertOfflineItemRequest2.setDiscoverable(false);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest2);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequests);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService, times(2)).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTest_multiple_itemPickupPoint_discoverable_update_mppTrue() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    UpsertOfflineItemRequest upsertOfflineItemRequest2 = new UpsertOfflineItemRequest();
    BeanUtils.copyProperties(offlineItem2, upsertOfflineItemRequest2);
    restSingleResponse.getValue()
        .setCncActivatedPickupPointCodes(new ArrayList<>());

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Map<String, Product> productSkuAndProductMap = new HashMap<>();
    productSkuAndProductMap.put(PRODUCT_SKU_1, product1);
    Mockito.when(productCacheableService.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.any())).thenReturn(productSkuAndProductMap);
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_2);
    businessPartnerPickupPoint.setActivated(false);
    BusinessPartnerPickupPoint businessPartnerPickupPoint2 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint2.setCode(PICKUP_POINT);
    businessPartnerPickupPoint2.setActivated(false);
    Mockito.when(
            businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(Arrays.asList(businessPartnerPickupPoint, businessPartnerPickupPoint2));

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequest.setCncActive(false);
    upsertOfflineItemRequest.setBuyable(true);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest2.setCncActive(false);
    upsertOfflineItemRequest2.setBuyable(true);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest2);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequests);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService, times(2)).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(itemService, times(2)).validatePriceForDeliveryTrueItemPickupPoint(any(), any(), any());
  }

  @Test
  public void upsertOfflineItemTest_multiple_itemPickupPoint_mppTrueInvalidCncFalsePickupPoint() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    UpsertOfflineItemRequest upsertOfflineItemRequest2 = new UpsertOfflineItemRequest();
    BeanUtils.copyProperties(offlineItem2, upsertOfflineItemRequest2);
    restSingleResponse.getValue().setCncActivatedPickupPointCodes(new ArrayList<>());

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    List<BusinessPartnerPickupPoint> businessPartnerPickupPointList = new ArrayList<>();
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(StringUtils.SPACE);
    businessPartnerPickupPoint.setActivated(false);
    businessPartnerPickupPointList.add(businessPartnerPickupPoint);
    BusinessPartnerPickupPoint businessPartnerPickupPoint1 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint1.setCode(StringUtils.SPACE);
    businessPartnerPickupPoint1.setActivated(false);
    businessPartnerPickupPointList.add(businessPartnerPickupPoint1);
    Mockito.when(
        businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString())).thenReturn(businessPartnerPickupPointList);

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequest.setCncActive(false);
    upsertOfflineItemRequest.setBuyable(true);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest2.setCncActive(false);
    upsertOfflineItemRequest2.setDiscoverable(true);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest2);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequests);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService, times(2)).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTest_multiple_itemPickupPointContainsDuplicateValues_mppTrue() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    restSingleResponse.getValue().setCncActivatedPickupPointCodes(new ArrayList<>());
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(
        this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString())).thenReturn(new ArrayList<>());
    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);
    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequests);
    verify(this.systemParameterService, times(1)).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService, times(1)).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTest_itempickupPoint_is_NewData() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    expectedResult.setCncActive(true);
    expectedResult.setBuyable(false);
    expectedResult.setDiscoverable(false);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setNewData(true);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointIsCNC() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(1000.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(1000.0);
    itemPickupPoint.setCncActive(true);

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    itemPickupPoint.setMarkForDelete(true);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointIsDeliveryTruePriceChangeCncUpdateTest() throws Exception {
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(1500.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(1500.0);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setDelivery(true);

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(
        this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest.setCncActive(false);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        new UpdateOfflineItemPriceRequest(upsertOfflineItemRequest.getItemSku(),
            upsertOfflineItemRequest.getListPrice(), upsertOfflineItemRequest.getOfferPrice()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
      any(AuditTrailListResponse.class));
    verify(itemService).findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(Mockito.anyString(),
        Mockito.isNull(), Mockito.anyBoolean());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointIsDeliveryTrueOfferPriceChangeCncUpdateTest() throws Exception {
    UpsertOfflineItemRequest upsertOfflineItemRequest1 = new UpsertOfflineItemRequest();
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest1.setCncActive(true);
    BeanUtils.copyProperties(offlineItem2, upsertOfflineItemRequest1);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setCncActive(true);
    upsertOfflineItemRequest.setListPrice(1500.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(1500.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(1500.0);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setDelivery(true);

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(
            this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    BusinessPartnerPickupPoint businessPartnerPickupPoint1 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint1.setCncActivated(true);
    businessPartnerPickupPoint1.setCode(PICKUP_POINT_2);
    Mockito.when(
            this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(Arrays.asList(businessPartnerPickupPoint, businessPartnerPickupPoint1));
    restSingleResponse.getValue().setCncActivatedPickupPointCodes(Arrays.asList(PICKUP_POINT, PICKUP_POINT_2));

    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT)).thenReturn(itemPickupPoint);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT_2)).thenReturn(null);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequest.setCncActive(false);
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest1);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService, Mockito.times(2)).findByItemSkuAndPickupPointCode(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
      any(AuditTrailListResponse.class));
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointIsDeliveryTrueOfferPriceFalseChangeCncUpdate1Test() throws Exception {
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(1500.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(1500.0);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setDelivery(true);

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(
            this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(
            itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(STORE_ID, ITEM_SKU, true))
        .thenReturn(10L);
    Mockito.when(
        itemService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(STORE_ID, item.getProductSku(),
            true)).thenReturn(10L);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest.setCncActive(false);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        new UpdateOfflineItemPriceRequest(upsertOfflineItemRequest.getItemSku(),
            upsertOfflineItemRequest.getListPrice(), upsertOfflineItemRequest.getOfferPrice()));
    verify(kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
      any(AuditTrailListResponse.class));
  }

  @Test
  public void upsertOfflineItemTestItempickupPointIsDeliveryTrueOfferPriceFalseChangeCncUpdateTest() throws Exception {
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(1500.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(1500.0);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setDelivery(true);

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(
            this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest.setCncActive(false);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        new UpdateOfflineItemPriceRequest(upsertOfflineItemRequest.getItemSku(),
            upsertOfflineItemRequest.getListPrice(), upsertOfflineItemRequest.getOfferPrice()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
      any(AuditTrailListResponse.class));
    verify(itemService).findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(Mockito.anyString(),
        Mockito.isNull(), Mockito.anyBoolean());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointIsDeliveryTrueOfferPriceFalseChangeCncUpdate2Test() throws Exception {
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(1500.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(1500.0);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setDelivery(true);

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(
            this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    Mockito.when(
        itemService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(STORE_ID, item.getProductSku(),
            true)).thenReturn(10L);
    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest.setCncActive(false);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        new UpdateOfflineItemPriceRequest(upsertOfflineItemRequest.getItemSku(),
            upsertOfflineItemRequest.getListPrice(), upsertOfflineItemRequest.getOfferPrice()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
      any(AuditTrailListResponse.class));
    verify(itemService).findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(Mockito.anyString(),
        Mockito.isNull(), Mockito.anyBoolean());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointIsDeliveryTruePriceChangeTest() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(1500.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(1500.0);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setDelivery(true);

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(
            this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        new UpdateOfflineItemPriceRequest(upsertOfflineItemRequest.getItemSku(),
            upsertOfflineItemRequest.getListPrice(), upsertOfflineItemRequest.getOfferPrice()));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY),
      any(AuditTrailListResponse.class));  }

  @Test
  public void upsertOfflineItemTestItempickupPointNewDataTest() throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(
            this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(null);

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointNewDataTestCnc1pNoCncDiscoverablechange() throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    itemPickupPoint.getAllItemViewConfigs().add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(false).channel(Constants.CNC).build());
    Price price = new Price();
    price.setOfferPrice(OFFER_PRICE);
    price.setListPrice(LIST_PRICE);
    itemPickupPoint.setPrice(Set.of(price));
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    UpsertOfflineItemRequest upsertOfflineItemRequest2 = new UpsertOfflineItemRequest();
    BeanUtils.copyProperties(offlineItem2, upsertOfflineItemRequest2);
    restSingleResponse.getValue()
        .setCncActivatedPickupPointCodes(new ArrayList<>());

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Map<String, Product> productSkuAndProductMap = new HashMap<>();
    productSkuAndProductMap.put(PRODUCT_SKU_1, product1);
    Mockito.when(productCacheableService.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.any())).thenReturn(productSkuAndProductMap);
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_2);
    businessPartnerPickupPoint.setActivated(false);
    BusinessPartnerPickupPoint businessPartnerPickupPoint2 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint2.setCode(PICKUP_POINT);
    businessPartnerPickupPoint2.setActivated(false);
    Mockito.when(
            businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(Arrays.asList(businessPartnerPickupPoint, businessPartnerPickupPoint2));

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequest.setCncActive(false);
    upsertOfflineItemRequest.setBuyable(true);
    upsertOfflineItemRequest.setListPrice(LIST_PRICE);
    upsertOfflineItemRequest.setOfferPrice(OFFER_PRICE);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest2.setCncActive(false);
    upsertOfflineItemRequest2.setBuyable(true);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest2);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequests);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService, times(2)).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointNewDataTestCnc1pCncDiscoverablechange() throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    itemPickupPoint.getAllItemViewConfigs().add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.CNC).build());
    Price price = new Price();
    price.setOfferPrice(OFFER_PRICE);
    price.setListPrice(LIST_PRICE);
    itemPickupPoint.setPrice(Set.of(price));
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    UpsertOfflineItemRequest upsertOfflineItemRequest2 = new UpsertOfflineItemRequest();
    BeanUtils.copyProperties(offlineItem2, upsertOfflineItemRequest2);
    restSingleResponse.getValue()
        .setCncActivatedPickupPointCodes(new ArrayList<>());

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Map<String, Product> productSkuAndProductMap = new HashMap<>();
    productSkuAndProductMap.put(PRODUCT_SKU_1, product1);
    Mockito.when(productCacheableService.findProductsByStoreIdAndProductSkusAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.any())).thenReturn(productSkuAndProductMap);
    BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_2);
    businessPartnerPickupPoint.setActivated(false);
    BusinessPartnerPickupPoint businessPartnerPickupPoint2 = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint2.setCode(PICKUP_POINT);
    businessPartnerPickupPoint2.setActivated(false);
    Mockito.when(
            businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString()))
        .thenReturn(Arrays.asList(businessPartnerPickupPoint, businessPartnerPickupPoint2));

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequests = new ArrayList<>();
    upsertOfflineItemRequest.setCncActive(false);
    upsertOfflineItemRequest.setBuyable(true);
    upsertOfflineItemRequest.setListPrice(LIST_PRICE);
    upsertOfflineItemRequest.setOfferPrice(OFFER_PRICE);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest);
    upsertOfflineItemRequest2.setCncActive(false);
    upsertOfflineItemRequest2.setBuyable(true);
    upsertOfflineItemRequests.add(upsertOfflineItemRequest2);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequests);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService, times(2)).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointIsDeliveryTruePriceChangeExceptionTest() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(1500.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(1500.0);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setDelivery(true);

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(false);
    Mockito.when(
            this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        new UpdateOfflineItemPriceRequest(upsertOfflineItemRequest.getItemSku(),
            upsertOfflineItemRequest.getListPrice(), upsertOfflineItemRequest.getOfferPrice()));
  }

  @Test
  public void upsertOfflineItemTestItempickupPointIsCNCDelivery() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(1000.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(1000.0);
    itemPickupPoint.setCncActive(true);

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(true);
    itemPickupPoint.setDelivery(true);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTest_pickupPoint_not_cnc() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(1000d);
    expectedResult.setOfferPrice(1000d);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.FALSE);
    expectedResult.setCncActive(true);
    expectedResult.setBuyable(false);
    expectedResult.setDiscoverable(false);
    expectedResult.setErrorMessage(ErrorMessages.PICKUP_POINT_NOT_ELIGIBLE_FOR_CNC);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    restSingleResponse.getValue().setCncActivatedPickupPointCodes(new ArrayList<>());

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    businessPartnerPickupPoint.setCode(PICKUP_POINT);
    businessPartnerPickupPoint.setCncActivated(false);
    Mockito.when(
        this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString())).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    assertEquals(expectedResults, responses);
  }

  @Test
  @Disabled
  public void upsertOfflineItemTest_InvalidpickupPoint() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(1000.0);
    expectedResult.setOfferPrice(1000.0);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.FALSE);
    expectedResult.setCncActive(false);
    expectedResult.setBuyable(false);
    expectedResult.setDiscoverable(false);
    expectedResult.setErrorMessage("Can not process invalid input data :failed to update offline item price");
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    restSingleResponse.getValue().setCncActivatedPickupPointCodes(new ArrayList<>());

    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    businessPartnerPickupPoint.setCode(PICKUP_POINT);
    businessPartnerPickupPoint.setCncActivated(false);
    Mockito.when(
        this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByBusinessPartnerCode(Mockito.anyString())).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequest.setCncActive(false);
    upsertOfflineItemRequest.setPickupPointCode(PICKUP_POINT_2);
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(),
        Mockito.any());
  }

  @Test
  public void upsertOfflineItemTest_exception_nullItem() throws Exception {
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(null);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
        upsertOfflineItemRequestList);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
  }

  @Test
  public void upsertOfflineItemTest_exception_itemIsAlreadyArchived() throws Exception {
    this.item.setArchived(true);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
        upsertOfflineItemRequestList);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
  }


  @Test
  public void updateMarkForDeleteTrueByMerchantCodeAndPickupPointCode() throws Exception {
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndPickupPointCode(STORE_ID, MERCHANT_CODE, PICKUP_POINT)).thenReturn(this.offlineItemList);
    offlineItemServiceImpl.updateMarkForDeleteByMerchantCodeAndPickupPointCode(STORE_ID, offlineItemList, Boolean.TRUE,
        MERCHANT_CODE, PICKUP_POINT);
    this.offlineItem2.setMarkForDelete(true);
    offlineItemList.add(this.offlineItem2);
    verify(offlineItemRepository).saveAll(anyList());
    verify(this.saveAndPublishService).publishListOfOfflineItems(anyList(), eq(null));
  }

  @Test
  public void updateMarkForDeleteTrueByMerchantCodeAndPickupPointCode_ThrowExceptionTest() throws Exception {
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndPickupPointCode(STORE_ID, MERCHANT_CODE, PICKUP_POINT)).thenReturn(this.offlineItemList);
    doThrow(new ApplicationRuntimeException()).when(saveAndPublishService)
        .publishListOfOfflineItems(this.offlineItemList, null);
    offlineItemServiceImpl.updateMarkForDeleteByMerchantCodeAndPickupPointCode(STORE_ID, offlineItemList, Boolean.TRUE,
        MERCHANT_CODE, PICKUP_POINT);
    verify(offlineItemRepository).saveAll(this.offlineItemList);
    verify(this.offlineItemRepository).saveAll(this.offlineItemList);
    verify(this.saveAndPublishService).publishListOfOfflineItems(this.offlineItemList, null);
  }

  @Test
  public void updateMarkForDeleteTrueByMerchantCodeAndPickupPointCode_storeIdNull() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateMarkForDeleteByMerchantCodeAndPickupPointCode(null, offlineItemList, Boolean.FALSE,
        MERCHANT_CODE, PICKUP_POINT));
  }

  @Test
  public void updateMarkForDeleteTrueByMerchantCodeAndPickupPointCode_merchantCodeNull() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateMarkForDeleteByMerchantCodeAndPickupPointCode(STORE_ID, offlineItemList, Boolean.FALSE,
        null, PICKUP_POINT));
  }

  @Test
  public void updateMarkForDeleteTrueByMerchantCodeAndPickupPointCode_pickupPointCodeNull() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateMarkForDeleteByMerchantCodeAndPickupPointCode(STORE_ID, offlineItemList, Boolean.FALSE,
        MERCHANT_CODE, null));
  }


  @Test
  public void updateMarkForDeleteByMerchantCodeAndPickupPointCode_ItemPickupPointTest_CncActivatedTrue_DeliveryTrueTest()
      throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.TRUE);
    itemPickupPoint.setDelivery(Boolean.TRUE);
    itemPickupPoint.setCncActive(Boolean.TRUE);
    itemPickupPointList.add(itemPickupPoint);
    offlineItemServiceImpl.updateItemPickupPointForCNCDeactivation(STORE_ID,
        itemPickupPointList, MERCHANT_CODE, PICKUP_POINT);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(this.saveAndPublishService).publishListOfItemsPickupPoints(anyList(), eq(null));
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndPickupPointCode_ItemPickupPointTest_CncActivatedTrue_DeliveryTrue_mppTrueTest()
      throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.TRUE);
    itemPickupPoint.setDelivery(Boolean.TRUE);
    itemPickupPoint.setCncActive(Boolean.TRUE);
    itemPickupPointList.add(itemPickupPoint);
    offlineItemServiceImpl.updateItemPickupPointForCNCDeactivation(STORE_ID,
        itemPickupPointList, MERCHANT_CODE, PICKUP_POINT);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(this.saveAndPublishService).publishListOfItemsPickupPoints(anyList(), eq(null));
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndPickupPointCode_ItemPickupPointTest_CncActivatedTrue_DeliveryTrue_cnc1PTrueTest()
      throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "cncForWarehouseFeatureSwitch",
        Boolean.valueOf("true"));
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.TRUE);
    itemPickupPoint.setDelivery(Boolean.TRUE);
    itemPickupPoint.setCncActive(Boolean.TRUE);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setMerchantCode(MERCHANT_CODE_2);

    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel("CNC");
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel("B2B");
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    Set<ItemViewConfig> configSet = new HashSet<>();
    configSet.add(itemViewConfig);
    configSet.add(itemViewConfig1);
    itemPickupPoint.setItemViewConfig(configSet);

    itemPickupPointList.add(itemPickupPoint);
    itemPickupPointList.add(itemPickupPoint1);
    when(channelService.getCncChannel()).thenReturn("CNC");
    offlineItemServiceImpl.updateItemPickupPointForCNCDeactivation(STORE_ID, itemPickupPointList,
        MERCHANT_CODE, PICKUP_POINT);
    verify(channelService).getCncChannel();
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(this.saveAndPublishService).publishListOfItemsPickupPoints(anyList(), eq(null));
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndPickupPointCode_ItemPickupPointTest_ThrowsException()
      throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.TRUE);
    itemPickupPoint.setDelivery(Boolean.FALSE);
    itemPickupPoint.setCncActive(Boolean.TRUE);
    itemPickupPointList.add(itemPickupPoint);
    doThrow(new ApplicationRuntimeException()).when(saveAndPublishService)
        .publishListOfItemsPickupPoints(anyList(), eq(null));
    offlineItemServiceImpl.updateItemPickupPointForCNCDeactivation(STORE_ID,
        itemPickupPointList, MERCHANT_CODE, PICKUP_POINT);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(this.saveAndPublishService).publishListOfItemsPickupPoints(anyList(), eq(null));
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndPickupPointCode_ItemPickupPoint_storeIdNull() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateItemPickupPointForCNCDeactivation(null, itemPickupPointList, MERCHANT_CODE, PICKUP_POINT));
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndPickupPointCode_ItemPickupPoint_merchantCodeNull() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateItemPickupPointForCNCDeactivation(STORE_ID,
        itemPickupPointList, null, PICKUP_POINT));
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndPickupPointCode_ItemPickupPoint_pickupPointCodeNull() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateItemPickupPointForCNCDeactivation(STORE_ID,
        itemPickupPointList, MERCHANT_CODE, null));
  }

  @Test
  public void updateItemPickupPointForCNCDeactivation_ItemPickupPointEmpty() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    offlineItemServiceImpl.updateItemPickupPointForCNCDeactivation(STORE_ID, itemPickupPointList,
        MERCHANT_CODE, PICKUP_POINT);
    Assertions.assertEquals(0, itemPickupPointList.size());
  }

  @Test
  public void updateItemPickupPointForDeliveryDeactivation_ItemPickupPointEmpty() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    offlineItemServiceImpl.updateItemPickupPointForDeliveryDeactivation(STORE_ID,
        itemPickupPointList, MERCHANT_CODE, PICKUP_POINT);
    Assertions.assertEquals(0, itemPickupPointList.size());
  }

  @Test
  public void updateItemPickupPointForDeliveryDeactivation_ItemPickupPointTest_ThrowsException()
      throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.TRUE);
    itemPickupPoint.setDelivery(Boolean.FALSE);
    itemPickupPoint.setCncActive(Boolean.TRUE);
    itemPickupPointList.add(itemPickupPoint);
    doThrow(new ApplicationRuntimeException()).when(saveAndPublishService)
        .publishListOfItemsPickupPoints(anyList(), eq(null));
    offlineItemServiceImpl.updateItemPickupPointForDeliveryDeactivation(STORE_ID,
        itemPickupPointList, MERCHANT_CODE, PICKUP_POINT);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(this.saveAndPublishService).publishListOfItemsPickupPoints(anyList(), eq(null));
  }

  @Test
  public void updateItemPickupPointForDeliveryDeactivation_ItemPickupPointTest_CncActivatedTrue_DeliveryTrue_cnc1PTrueTest()
      throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "cncForWarehouseFeatureSwitch",
        Boolean.valueOf("true"));
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.TRUE);
    itemPickupPoint.setDelivery(Boolean.TRUE);
    itemPickupPoint.setCncActive(Boolean.TRUE);

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setMerchantCode(MERCHANT_CODE_2);

    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(DEFAULT_CHANNEL);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel("B2B");
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    Set<ItemViewConfig> configSet = new HashSet<>();
    configSet.add(itemViewConfig);
    configSet.add(itemViewConfig1);
    itemPickupPoint.setItemViewConfig(configSet);

    itemPickupPointList.add(itemPickupPoint);
    itemPickupPointList.add(itemPickupPoint1);
    offlineItemServiceImpl.updateItemPickupPointForDeliveryDeactivation(STORE_ID,
        itemPickupPointList, MERCHANT_CODE, PICKUP_POINT);
    verify(channelService).getDefaultChannel();
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(this.saveAndPublishService).publishListOfItemsPickupPoints(anyList(), eq(null));
  }

  @Test
  public void updateMarkForDeleteByItemSkuAndPickupPointCode() throws Exception {
    this.item = new Item();
    this.item.setArchived(false);
    this.item.setItemSku(ITEM_SKU);
    this.itemList = new ArrayList<>();
    this.itemList.add(item);

    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndPickupPointCode(STORE_ID, MERCHANT_CODE, PICKUP_POINT)).thenReturn(this.offlineItemList);
    Set<String> itemSkus = offlineItemList.stream().map(OfflineItem::getItemSku).collect(Collectors.toSet());

    when(itemService.findByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(this.itemList);
    offlineItemServiceImpl.updateMarkForDeleteByItemSkuAndPickupPointCode(STORE_ID, offlineItemList, Boolean.TRUE,
        MERCHANT_CODE, PICKUP_POINT);

    verify(offlineItemRepository).saveAll(this.offlineItemList);
    verify(itemService).findByStoreIdAndItemSkus(STORE_ID, itemSkus);
    verify(this.saveAndPublishService).publishListOfOfflineItems(this.offlineItemList, null);
  }

  @Test
  public void updateMarkForDeleteByItemSkuAndPickupPointCode_multipleOfflineItems() throws Exception {
    this.offlineItem2.setItemSku(ITEM_SKU2);
    this.offlineItemList.add(this.offlineItem2);

    this.itemList = new ArrayList<>();
    this.itemList.add(item);
    this.itemList.add(item2);

    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndPickupPointCode(STORE_ID, MERCHANT_CODE, PICKUP_POINT)).thenReturn(this.offlineItemList);
    Set<String> itemSkus = offlineItemList.stream().map(OfflineItem::getItemSku).collect(Collectors.toSet());

    when(itemService.findByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(this.itemList);
    offlineItemServiceImpl.updateMarkForDeleteByItemSkuAndPickupPointCode(STORE_ID, offlineItemList, Boolean.TRUE,
        MERCHANT_CODE, PICKUP_POINT);

    verify(itemService).findByStoreIdAndItemSkus(STORE_ID, itemSkus);
    verify(this.offlineItemRepository).saveAll(this.offlineItemList);
    verify(this.saveAndPublishService).publishListOfOfflineItems(this.offlineItemList, null);
  }

  @Test
  public void updateMarkForDeleteByItemSkuAndPickupPointCode_setArchivedTrue() throws Exception {
    this.item = new Item();
    this.item.setArchived(true);
    this.item.setItemSku(ITEM_SKU);
    this.itemList = new ArrayList<>();
    this.itemList.add(item);

    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndPickupPointCode(STORE_ID, MERCHANT_CODE, PICKUP_POINT)).thenReturn(this.offlineItemList);
    Set<String> itemSkus = offlineItemList.stream().map(OfflineItem::getItemSku).collect(Collectors.toSet());

    when(itemService.findByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(this.itemList);
    offlineItemServiceImpl.updateMarkForDeleteByItemSkuAndPickupPointCode(STORE_ID, offlineItemList, Boolean.TRUE,
        MERCHANT_CODE, PICKUP_POINT);

    verify(itemService).findByStoreIdAndItemSkus(STORE_ID, itemSkus);
    verify(this.offlineItemRepository).saveAll(Collections.emptyList());
    verify(this.saveAndPublishService).publishListOfOfflineItems(Collections.emptyList(), null);
  }

  @Test
  public void updateMarkForDeleteByItemSkuAndPickupPointCode_twoPickupPoints() throws Exception {
    this.item = new Item();
    this.item.setArchived(true);
    this.item.setItemSku(ITEM_SKU);
    this.itemList = new ArrayList<>();
    this.itemList.add(item);

    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndPickupPointCode(STORE_ID, MERCHANT_CODE, PICKUP_POINT)).thenReturn(this.offlineItemList);
    Set<String> itemSkus = offlineItemList.stream().map(OfflineItem::getItemSku).collect(Collectors.toSet());

    when(itemService.findByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(this.itemList);
    offlineItemServiceImpl.updateMarkForDeleteByItemSkuAndPickupPointCode(STORE_ID, offlineItemList, Boolean.TRUE,
        MERCHANT_CODE, PICKUP_POINT);

    verify(itemService).findByStoreIdAndItemSkus(STORE_ID, itemSkus);
    verify(this.offlineItemRepository).saveAll(Collections.emptyList());
    verify(this.saveAndPublishService).publishListOfOfflineItems(Collections.emptyList(), null);
  }

  @Test
  public void updateMarkForDeleteByItemSkuAndPickupPointCode_ThrowExceptionTest() throws Exception {
    this.item = new Item();
    this.item.setArchived(false);
    this.item.setItemSku(ITEM_SKU);
    this.itemList = new ArrayList<>();
    this.itemList.add(item);
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndPickupPointCode(STORE_ID, MERCHANT_CODE, PICKUP_POINT)).thenReturn(this.offlineItemList);
    Set<String> itemSkus = offlineItemList.stream().map(OfflineItem::getItemSku).collect(Collectors.toSet());

    when(itemService.findByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(this.itemList);
    doThrow(new ApplicationRuntimeException()).when(this.saveAndPublishService)
        .publishListOfOfflineItems(this.offlineItemList, null);

    offlineItemServiceImpl.updateMarkForDeleteByItemSkuAndPickupPointCode(STORE_ID, offlineItemList, Boolean.TRUE,
        MERCHANT_CODE, PICKUP_POINT);
    verify(offlineItemRepository).saveAll(this.offlineItemList);
    verify(itemService).findByStoreIdAndItemSkus(STORE_ID, itemSkus);
  }

  @Test
  public void updateMarkForDeleteByItemSkuAndPickupPointCode_storeIdNull() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateMarkForDeleteByItemSkuAndPickupPointCode(null, offlineItemList, Boolean.FALSE,
        MERCHANT_CODE, PICKUP_POINT));
  }

  @Test
  public void updateMarkForDeleteByItemSkuAndPickupPointCode_merchantCodeNull() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateMarkForDeleteByItemSkuAndPickupPointCode(STORE_ID, offlineItemList, Boolean.FALSE, null,
        PICKUP_POINT));
  }

  @Test
  public void updateMarkForDeleteByItemSkuAndPickupPointCode_pickupPointCodeNull() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateMarkForDeleteByItemSkuAndPickupPointCode(STORE_ID, offlineItemList, Boolean.FALSE,
        MERCHANT_CODE, null));
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndItemSkuTest_mfdFalse() throws Exception {
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU)).thenReturn(
        this.offlineItemList);

    when(pickupPointService.findPickupPointListByPickupPointCodeInAndCncActivatedFalse(STORE_ID,
        Arrays.asList(PICKUP_POINT))).thenReturn(new ArrayList<>());
    offlineItemServiceImpl.updateMarkForDeleteByMerchantCodeAndItemSku(STORE_ID, Boolean.FALSE, item);
    verify(pickupPointService).findPickupPointListByPickupPointCodeInAndCncActivatedFalse(STORE_ID, Arrays.asList(PICKUP_POINT));
    verify(offlineItemRepository).saveAll(this.offlineItemList);
    verify(this.saveAndPublishService).publishListOfOfflineItems(this.offlineItemList, null);
    verify(offlineItemRepository).findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU);
    verify(productAndItemSolrIndexerService).applyItem(item);
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndItemSkuTest_mfdFalse_exceptionTest() throws Exception {
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU)).thenReturn(
        this.offlineItemList);

    when(pickupPointService.findPickupPointListByPickupPointCodeInAndCncActivatedFalse(STORE_ID,
        Arrays.asList(PICKUP_POINT))).thenReturn(new ArrayList<>());
    Mockito.doThrow(Exception.class).when(productAndItemSolrIndexerService).applyItem(this.item);
    offlineItemServiceImpl.updateMarkForDeleteByMerchantCodeAndItemSku(STORE_ID, Boolean.FALSE, item);
    verify(pickupPointService).findPickupPointListByPickupPointCodeInAndCncActivatedFalse(STORE_ID, Arrays.asList(PICKUP_POINT));
    verify(offlineItemRepository).saveAll(this.offlineItemList);
    verify(this.saveAndPublishService).publishListOfOfflineItems(this.offlineItemList, null);
    verify(offlineItemRepository).findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU);
    verify(productAndItemSolrIndexerService).applyItem(item);
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndItemSkuTest_mfdFalse_cncActivatedFalse() throws Exception {
    pickupPoint2 = new PickupPoint();
    pickupPoint2.setCncActivated(false);
    pickupPoint2.setPickupPointCode(PICKUP_POINT_2);
    pickupPoints = new ArrayList<>();
    pickupPoints.add(pickupPoint2);

    this.offlineItemList.add(this.offlineItem2);

    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU)).thenReturn(
        this.offlineItemList);

    when(pickupPointService.findPickupPointListByPickupPointCodeInAndCncActivatedFalse(STORE_ID,
        Arrays.asList(PICKUP_POINT, PICKUP_POINT_2))).thenReturn(this.pickupPoints);
    offlineItemServiceImpl.updateMarkForDeleteByMerchantCodeAndItemSku(STORE_ID, Boolean.FALSE, item);
    verify(pickupPointService).findPickupPointListByPickupPointCodeInAndCncActivatedFalse(STORE_ID, Arrays.asList(PICKUP_POINT, PICKUP_POINT_2));
    verify(offlineItemRepository).saveAll(this.offlineItemList);
    verify(this.saveAndPublishService).publishListOfOfflineItems(this.offlineItemList, null);
    verify(offlineItemRepository).findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU);
    verify(productAndItemSolrIndexerService).applyItem(item);
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndItemSkuTest_mfdTrue() throws Exception {
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU)).thenReturn(
        this.offlineItemList);
    offlineItemServiceImpl.updateMarkForDeleteByMerchantCodeAndItemSku(STORE_ID, Boolean.TRUE, item);
    verify(offlineItemRepository).saveAll(this.offlineItemList);
    verify(offlineItemRepository).findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU);
    verify(this.saveAndPublishService).publishListOfOfflineItems(this.offlineItemList, null);
  }

  @Test
  public void updateMarkForDeleteByMerchantCodeAndItemSkuTest_exception() throws Exception {
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU)).thenReturn(
        this.offlineItemList);
    when(pickupPointService.findPickupPointListByPickupPointCodeInAndCncActivatedFalse(STORE_ID,
        Arrays.asList(PICKUP_POINT))).thenReturn(new ArrayList<>());
    doThrow(new RuntimeException()).when(this.saveAndPublishService)
        .publishListOfOfflineItems(this.offlineItemList, null);

    offlineItemServiceImpl.updateMarkForDeleteByMerchantCodeAndItemSku(STORE_ID, Boolean.FALSE, item);

    verify(pickupPointService).findPickupPointListByPickupPointCodeInAndCncActivatedFalse(STORE_ID, Arrays.asList(PICKUP_POINT));
    verify(offlineItemRepository).saveAll(offlineItemList);
    verify(offlineItemRepository).findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU);
    verify(this.saveAndPublishService).publishListOfOfflineItems(this.offlineItemList, null);
  }

  @Test
  public void getOfflineItemByItemSkuAndPickupPointCode() throws Exception {
    this.offlineItemServiceImpl.findByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(OfflineItemServiceImplTest.STORE_ID, OfflineItemServiceImplTest.ITEM_SKU,
        OfflineItemServiceImplTest.PICKUP_POINT);
    verify(this.offlineItemRepository).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        OfflineItemServiceImplTest.STORE_ID, OfflineItemServiceImplTest.ITEM_SKU,
        OfflineItemServiceImplTest.PICKUP_POINT);
  }

  @Test
  public void findByMerchantCodeAndItemSku_blankStoreId() throws Exception {
    try {
      offlineItemServiceImpl.findByMerchantCodeAndItemSku(null, MERCHANT_CODE, ITEM_SKU);
      assert false;
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() + OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByMerchantCodeAndItemSku_blankMerchantCode() throws Exception {
    try {
      offlineItemServiceImpl.findByMerchantCodeAndItemSku(STORE_ID, null, ITEM_SKU);
      assert false;
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() + OfflineItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByMerchantCodeAndItemSku_blankItemSku() throws Exception {
    try {
      offlineItemServiceImpl.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, null);
      assert false;
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() + OfflineItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByMerchantCodeAndItemSku_success() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();

    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU)).thenReturn(
        offlineItems);

    List<OfflineItem> result = offlineItemServiceImpl.findByMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU);

    verify(offlineItemRepository).findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE, ITEM_SKU);
    assertEquals(offlineItems, result);
  }

  @Test
  public void deleteOfflineItemByMerchantCodeExceptionTest() {
    when(this.itemPickupPointService.findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(STORE_ID, MERCHANT_CODE,
        Boolean.TRUE, Boolean.FALSE)).thenThrow(NullPointerException.class);
    offlineItemServiceImpl.deleteOfflineItemByMerchantCode(STORE_ID, MERCHANT_CODE, USERNAME);
    verify(this.itemPickupPointService).findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(STORE_ID,
        MERCHANT_CODE, Boolean.TRUE, Boolean.FALSE);
  }

  @Test
  public void deleteOfflineItemByMerchantCodeMultiPickupPointEnabledTrue() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setCncActive(Boolean.TRUE);
    itemPickupPoint.setMarkForDelete(Boolean.FALSE);
    itemPickupPoint.setDelivery(Boolean.FALSE);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU_1);
    itemPickupPointList.add(itemPickupPoint);
    when(this.itemPickupPointService.findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(STORE_ID, MERCHANT_CODE,
        Boolean.TRUE, Boolean.FALSE)).thenReturn(itemPickupPointList);
    offlineItemServiceImpl.deleteOfflineItemByMerchantCode(STORE_ID, MERCHANT_CODE, USERNAME);
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(ITEM_SKU);
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU_1);
    verify(this.itemService).updateCncActivatedByItemSkusAndPublish(STORE_ID, itemSkuSet, Boolean.FALSE, USERNAME);
    verify(this.itemPickupPointService).findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(STORE_ID,
        MERCHANT_CODE, Boolean.TRUE, Boolean.FALSE);
    verify(this.productService).updateCncActivatedByProductSkuAndSolrL3(STORE_ID, productSkuSet, Boolean.FALSE,
        USERNAME);
    verify(this.itemPickupPointService).saveItemPickupPoint(Mockito.anyList());
  }

  @Test
  public void deleteOfflineItemByMerchantCode_cncForWarehouseTrue() throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "cncForWarehouseFeatureSwitch",
        Boolean.valueOf("true"));
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setCncActive(Boolean.TRUE);
    itemPickupPoint.setMarkForDelete(Boolean.FALSE);
    itemPickupPoint.setDelivery(Boolean.FALSE);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU_1);

    ItemViewConfig itemViewConfig= new ItemViewConfig();
    itemViewConfig.setChannel("CNC");
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setBuyable(false);
    itemPickupPoint.setItemViewConfig(Collections.singleton(itemViewConfig));
    itemPickupPointList.add(itemPickupPoint);
    when(channelService.getCncChannel()).thenReturn("CNC");
    when(this.itemPickupPointService.findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDelete(
        STORE_ID, MERCHANT_CODE, Boolean.TRUE, Boolean.FALSE, "CNC")).thenReturn(
        itemPickupPointList);
    offlineItemServiceImpl.deleteOfflineItemByMerchantCode(STORE_ID, MERCHANT_CODE, USERNAME);
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(ITEM_SKU);
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU_1);
    verify(this.itemService).updateCncActivatedByItemSkusAndPublish(STORE_ID, itemSkuSet,
        Boolean.FALSE, USERNAME);
    verify(
        this.itemPickupPointService).findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDelete(
        STORE_ID, MERCHANT_CODE, Boolean.TRUE, Boolean.FALSE, "CNC");
    verify(this.productService).updateCncActivatedByProductSkuAndSolrL3(STORE_ID, productSkuSet,
        Boolean.FALSE, USERNAME);
    verify(this.itemPickupPointService).saveItemPickupPoint(Mockito.anyList());
  }

  @Test
  public void deleteOfflineItemByMerchantCode_nullStoreId_throwException() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.deleteOfflineItemByMerchantCode(null, MERCHANT_CODE, USERNAME));
  }

  @Test
  public void deleteOfflineItemByMerchantCode_nullMerchantCode_throwException() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.deleteOfflineItemByMerchantCode(STORE_ID, null, USERNAME));
  }

  @Test
  public void findAndRemoveByPickupPointCodeTest_success() throws Exception {
    Mockito.when(offlineItemRepository.findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT))
        .thenReturn(this.offlineItemList);
    offlineItemServiceImpl.deleteByPickupPointCode(STORE_ID, PICKUP_POINT);

    verify(offlineItemRepository).deleteByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT);
    verify(offlineItemRepository).findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT);
    verify(saveAndPublishService).publishListOfOfflineItems(this.offlineItemList, null);
  }

  @Test
  public void findAndRemoveByPickupPointCodeTest_exception() throws Exception {
    Mockito.when(offlineItemRepository.findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT))
        .thenReturn(this.offlineItemList);
    Mockito.doThrow(ApplicationRuntimeException.class).when(saveAndPublishService).
        publishListOfOfflineItems(this.offlineItemList, null);
    offlineItemServiceImpl.deleteByPickupPointCode(STORE_ID, PICKUP_POINT);

    verify(offlineItemRepository).deleteByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT);
    verify(offlineItemRepository).findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT);
    verify(saveAndPublishService).publishListOfOfflineItems(this.offlineItemList, null);
  }

  @Test
  public void findAndRemoveByPickupPointCodeTest_blankStoreId() throws Exception {
    try {
      offlineItemServiceImpl.deleteByPickupPointCode(null, PICKUP_POINT);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findAndRemoveByPickupPointCodeTest_blankPickupPointCode() throws Exception {
    try {
      offlineItemServiceImpl.deleteByPickupPointCode(STORE_ID, null);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void deleteByPickupPointCode_ItemPickupPoint_successTest() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.FALSE);
    itemPickupPoint.setDelivery(Boolean.FALSE);
    itemPickupPoint.setCncActive(Boolean.TRUE);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPointList.add(itemPickupPoint);

    Mockito.when(itemPickupPointService
        .findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT, true, false))
        .thenReturn(itemPickupPointList);
    offlineItemServiceImpl.deleteByPickupPointCode_ItemPickupPoint(STORE_ID, PICKUP_POINT);

    verify(inventoryOutbound).deleteByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList());
    verify(itemPickupPointService).saveItemPickupPoint(itemPickupPointList);
    verify(itemPickupPointService)
        .findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT, true, false);
    verify(saveAndPublishService).publishListOfItemsPickupPoints(Mockito.anyList(), eq(null));
  }

  @Test
  public void deleteByPickupPointCode_ItemPickupPointDeliveryTrue_successTest() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.FALSE);
    itemPickupPoint.setDelivery(Boolean.TRUE);
    itemPickupPoint.setCncActive(Boolean.TRUE);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPointList.add(itemPickupPoint);

    Mockito.when(itemPickupPointService
        .findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT, true, false))
        .thenReturn(itemPickupPointList);
    offlineItemServiceImpl.deleteByPickupPointCode_ItemPickupPoint(STORE_ID, PICKUP_POINT);
    verify(itemPickupPointService).saveItemPickupPoint(itemPickupPointList);
    verify(itemPickupPointService)
        .findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT, true, false);
    verify(saveAndPublishService).publishListOfItemsPickupPoints(Mockito.anyList(), eq(null));
  }

  @Test
  public void deleteByPickupPointCode_ItemPickupPoint_exceptionTest() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.FALSE);
    itemPickupPoint.setDelivery(Boolean.FALSE);
    itemPickupPoint.setCncActive(Boolean.TRUE);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPointList.add(itemPickupPoint);

    Mockito.when(itemPickupPointService.findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT, true, false))
        .thenReturn(itemPickupPointList);
    doThrow(ApplicationRuntimeException.class).when(saveAndPublishService).
        publishListOfItemsPickupPoints(Mockito.anyList(), eq(null));
    offlineItemServiceImpl.deleteByPickupPointCode_ItemPickupPoint(STORE_ID, PICKUP_POINT);
    verify(inventoryOutbound).deleteByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList());
    verify(itemPickupPointService).saveItemPickupPoint(itemPickupPointList);
    verify(itemPickupPointService)
        .findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT, true, false);
    verify(saveAndPublishService).publishListOfItemsPickupPoints(Mockito.anyList(), eq(null));
  }

  @Test
  public void deleteByPickupPointCode_ItemPickupPoint_blankStoreIdTest() throws Exception {
    try {
      offlineItemServiceImpl.deleteByPickupPointCode_ItemPickupPoint(null, PICKUP_POINT);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void deleteByPickupPointCode_ItemPickupPoint_blankPickupPointCodeTest() throws Exception {
    try {
      offlineItemServiceImpl.deleteByPickupPointCode_ItemPickupPoint(STORE_ID, null);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void validateAndUpdateProductCncActivatedFalseEmptyTest() {
    offlineItemServiceImpl.validateAndUpdateProductCncActivatedFalse(STORE_ID, new ArrayList<>(), USERNAME, USERNAME);
  }

  @Test
  public void validateAndUpdateProductCncActivatedFalseCncItemPickupPointNullTest() {
    offlineItemServiceImpl
        .validateAndUpdateProductCncActivatedFalse(STORE_ID, Collections.singletonList(itemPickupPoint), USERNAME,
            USERNAME);
    Mockito.verify(itemPickupPointService)
        .findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID, itemPickupPoint.getProductSku(), true, ImmutableSet.of(PICKUP_POINT));
    Mockito.verify(productService)
        .updateCncActivatedByProductSkuAndSolrL3(STORE_ID, Collections.singleton(itemPickupPoint.getProductSku()),
            false, USERNAME);
  }

  @Test
  public void validateAndUpdateProductCncActivatedFalseCncItemPickupPointNull_cncForWarehouseTrueTest() {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "cncForWarehouseFeatureSwitch",
        Boolean.valueOf("true"));
    offlineItemServiceImpl.validateAndUpdateProductCncActivatedFalse(STORE_ID,
        Collections.singletonList(itemPickupPoint), USERNAME, USERNAME);
    Mockito.verify(itemPickupPointService)
        .findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID,
            itemPickupPoint.getProductSku(), ImmutableSet.of(PICKUP_POINT), true);
    Mockito.verify(productService).updateCncActivatedByProductSkuAndSolrL3(STORE_ID,
        Collections.singleton(itemPickupPoint.getProductSku()), false, USERNAME);
  }

  @Test
  public void validateAndUpdateProductCncActivatedFalseCncItemPickupPointNonNullTest() {
    Mockito.when(itemPickupPointService
        .findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID, itemPickupPoint.getProductSku(), true, ImmutableSet.of(PICKUP_POINT)))
        .thenReturn(itemPickupPoint);
    offlineItemServiceImpl
        .validateAndUpdateProductCncActivatedFalse(STORE_ID, Collections.singletonList(itemPickupPoint), USERNAME,
            USERNAME);
    Mockito.verify(itemPickupPointService)
        .findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID, itemPickupPoint.getProductSku(), true, ImmutableSet.of(PICKUP_POINT));
  }

  @Test
  public void validateAndUpdateCncActivatedFalseTest_mppTruesuccess() throws Exception {
    Mockito.when(itemPickupPointService.findOneByItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true))
        .thenReturn(null);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    Mockito.when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, Boolean.FALSE, USERNAME))
        .thenReturn(item);
    offlineItemServiceImpl.validateAndUpdateProductItemCncActivatedFalse(STORE_ID, offlineItemList, USERNAME,
        Constants.DEFAULT_USERNAME);
    verify(systemParameterService, times(0)).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    verify(itemPickupPointService).findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true,
        ImmutableSet.of(PICKUP_POINT));
    verify(itemService).updateCncActivated(STORE_ID, ITEM_SKU, Boolean.FALSE, USERNAME);
    verify(itemService , times(0)).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(productAndItemSolrIndexerService, times(0)).applyItem(item);
    verify(this.reindexService, times(0)).reindexPendingL3SolrAndDatabase(
        Collections.singletonList(item.getProductSku()), new HashMap<>());
  }

  @Test
  public void validateAndUpdateCncActivatedFalseTest_cncWarehouseTrueSuccess() throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "cncForWarehouseFeatureSwitch",
        Boolean.valueOf("true"));
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.TRUE);
    itemPickupPoint.setDelivery(Boolean.TRUE);
    itemPickupPoint.setCncActive(Boolean.TRUE);

    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel("CNC");
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel("B2B");
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    Set<ItemViewConfig> configSet = new HashSet<>();
    configSet.add(itemViewConfig);
    configSet.add(itemViewConfig1);
    itemPickupPoint.setItemViewConfig(configSet);

    Mockito.when(
        itemPickupPointService.findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
            STORE_ID, ITEM_SKU, ImmutableSet.of(PICKUP_POINT), true)).thenReturn(itemPickupPoint);
    Mockito.when(
            itemService.getItemsByStoreIdAndItemSkus(STORE_ID,
                new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    Mockito.when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, Boolean.FALSE, USERNAME))
        .thenReturn(item);
    offlineItemServiceImpl.validateAndUpdateProductItemCncActivatedFalse(STORE_ID, offlineItemList,
        USERNAME, Constants.DEFAULT_USERNAME);
    verify(systemParameterService, times(0)).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    verify(
        itemPickupPointService).findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU, ImmutableSet.of(PICKUP_POINT), true);
    verify(itemService, times(0)).getItemsByStoreIdAndItemSkus(STORE_ID,
        new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(productAndItemSolrIndexerService, times(0)).applyItem(item);
    verify(this.reindexService, times(0)).reindexPendingL3SolrAndDatabase(
        Collections.singletonList(item.getProductSku()), new HashMap<>());
  }

  @Test
  public void validateAndUpdateCncActivatedFalseTest_offlineItemListEmpty() throws Exception {
    offlineItemServiceImpl.validateAndUpdateProductItemCncActivatedFalse(STORE_ID, new ArrayList<>(), USERNAME,
        Constants.DEFAULT_USERNAME);
  }

  @Test
  public void validateAndUpdateCncActivatedFalseTest_blankStoreId() throws Exception {
    try {
      offlineItemServiceImpl.validateAndUpdateProductItemCncActivatedFalse(null, offlineItemList, USERNAME,
          Constants.DEFAULT_USERNAME);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void validateAndUpdateCncActivatedFalseTest_blankPickupPoint() throws Exception {
    try {
      offlineItemServiceImpl.validateAndUpdateProductItemCncActivatedFalse(STORE_ID, null, USERNAME,
          Constants.DEFAULT_USERNAME);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void validateAndUpdateCncActivatedTrueTest_success() throws Exception {
    Mockito.when(offlineItemRepository.findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT))
        .thenReturn(offlineItemList);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, true, USERNAME)).thenReturn(item);
    Mockito.doNothing().when(productAndItemSolrIndexerService).applyItem(item);
    offlineItemServiceImpl.updateProductItemCncActivatedTrue(STORE_ID, PICKUP_POINT, USERNAME, offlineItemList);
    verify(itemService).updateCncActivated(STORE_ID, ITEM_SKU, Boolean.TRUE, USERNAME);
    verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(this.reindexService).reindexPendingL3SolrAndDatabase(
      Collections.singletonList(item.getProductSku()), new HashMap<>());
  }

  @Test
  public void validateAndUpdateCncActivatedTrueTest_exception() throws Exception {
    Mockito.when(offlineItemRepository.findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT))
        .thenReturn(offlineItemList);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, true, USERNAME)).thenReturn(item);
    Mockito.doThrow(Exception.class).when(productAndItemSolrIndexerService).applyItem(item);
    offlineItemServiceImpl.updateProductItemCncActivatedTrue(STORE_ID, PICKUP_POINT, USERNAME, offlineItemList);
    verify(itemService).updateCncActivated(STORE_ID, ITEM_SKU, Boolean.TRUE, USERNAME);
    verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
  }

  @Test
  public void validateAndUpdateCncActivatedTrueTestWithDeferred() throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "excludedUserNames", "ALL");
    Mockito.when(offlineItemRepository.findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT))
        .thenReturn(offlineItemList);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableOfflineDeferredSolrReindexSystemParameter);
    when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, true, USERNAME)).thenReturn(item);
    Mockito.doThrow(Exception.class).when(productAndItemSolrIndexerService).applyItem(item);
    offlineItemServiceImpl.updateProductItemCncActivatedTrue(STORE_ID, PICKUP_POINT, USERNAME, offlineItemList);
    verify(itemService).updateCncActivated(STORE_ID, ITEM_SKU, Boolean.TRUE, USERNAME);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    verify(this.saveOperationService).deferredReindexItems(Mockito.anyString(), Mockito.anyList(),
        Mockito.anyBoolean(), Mockito.eq(ReindexType.OFFLINE_ITEM_REINDEX),
      eq(true));
    verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
  }

  @Test
  public void validateAndUpdateCncTrueTestWithDeferred() throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "excludedUserNames", "ALL");
    Mockito.when(offlineItemRepository.findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT))
        .thenReturn(offlineItemList);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableOfflineDeferredSolrReindexSystemParameter);
    when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, true, USERNAME)).thenReturn(item);
    Mockito.doThrow(Exception.class).when(productAndItemSolrIndexerService).applyItem(item);
    offlineItemServiceImpl.updateProductItemCncActivatedTrue(STORE_ID, PICKUP_POINT, USERNAME, offlineItemList);
    verify(itemService).updateCncActivated(STORE_ID, ITEM_SKU, Boolean.TRUE, USERNAME);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    verify(this.saveOperationService).deferredReindexItems(Mockito.anyString(), Mockito.anyList(),
        Mockito.anyBoolean(), Mockito.eq(ReindexType.OFFLINE_ITEM_REINDEX),
      eq(true));
    verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
  }

  @Test
  public void validateAndUpdateCncTrueTestWithDeferredNone() throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "excludedUserNames", "NONE");
    Mockito.when(offlineItemRepository.findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT))
        .thenReturn(offlineItemList);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableOfflineDeferredSolrReindexSystemParameter);
    when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, true, USERNAME)).thenReturn(item);
    Mockito.doThrow(Exception.class).when(productAndItemSolrIndexerService).applyItem(item);
    offlineItemServiceImpl.updateProductItemCncActivatedTrue(STORE_ID, PICKUP_POINT, USERNAME, offlineItemList);
    verify(itemService).updateCncActivated(STORE_ID, ITEM_SKU, Boolean.TRUE, USERNAME);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(itemService).getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)));
  }

  @Test
  public void validateAndUpdateCncActivatedTrueTest_blankStoreId() throws Exception {
    try {
      offlineItemServiceImpl.updateProductItemCncActivatedTrue(null, PICKUP_POINT, USERNAME, offlineItemList);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void validateAndUpdateCncActivatedTrueTest_blankPickupPoint() throws Exception {
    try {
      offlineItemServiceImpl.updateProductItemCncActivatedTrue(STORE_ID, null, USERNAME, offlineItemList);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByMerchantCodeAndMerchantSkuTest_success() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();

    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndMerchantSku(STORE_ID, MERCHANT_CODE, MERCHANT_SKU))
        .thenReturn(offlineItems);

    List<OfflineItem> result = offlineItemServiceImpl.findByMerchantCodeAndMerchantSku(STORE_ID, MERCHANT_CODE, MERCHANT_SKU);

    verify(offlineItemRepository).findByStoreIdAndMerchantCodeAndMerchantSku(STORE_ID, MERCHANT_CODE, MERCHANT_SKU);
    assertEquals(offlineItems, result);
  }

  @Test
  public void findByMerchantCodeAndMerchantSkuTest_blankStoreId() throws Exception {
    try {
      offlineItemServiceImpl.findByMerchantCodeAndMerchantSku(null, MERCHANT_CODE, MERCHANT_SKU);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByMerchantCodeAndMerchantSkuTest_blankMerchantCode() throws Exception {
    try {
      offlineItemServiceImpl.findByMerchantCodeAndMerchantSku(STORE_ID, null, MERCHANT_SKU);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByMerchantCodeAndMerchantSkuTest_blankMerchantSku() throws Exception {
    try {
      offlineItemServiceImpl.findByMerchantCodeAndMerchantSku(STORE_ID, MERCHANT_CODE, null);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.MERCHANT_SKU_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByItemSkuTest_success() throws Exception {
    offlineItemServiceImpl.findByItemSku(STORE_ID, ITEM_SKU);
    verify(offlineItemRepository).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findByItemSkuTest_blankStoreId() throws Exception {
    try {
      offlineItemServiceImpl.findByItemSku(null, ITEM_SKU);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByItemSkuTest_blankItemSku() throws Exception {
    try {
      offlineItemServiceImpl.findByItemSku(STORE_ID, null);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findOfflineItemsByProductSkus_twoProductSku_successForHappyFlow() throws Exception {

    List<Item> items1 = new ArrayList<>();
    items1.add(createItem(PRODUCT_SKU_1, ITEM_SKU1));
    items1.add(createItem(PRODUCT_SKU_1, ITEM_SKU2));

    List<Item> items2 = new ArrayList<>();
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU3));
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU4));

    List<ItemPickupPoint> offlineItems1a = new ArrayList<>();
    offlineItems1a.add(createItemPickupPoint(LIST_PRICE_1500, OFFER_PRICE_1000, ITEM_SKU1));
    offlineItems1a.add(createItemPickupPoint(LIST_PRICE_2000, OFFER_PRICE_1500, ITEM_SKU1));
    List<ItemPickupPoint> offlineItems1b = new ArrayList<>();
    offlineItems1b.add(createItemPickupPoint(LIST_PRICE_2000, OFFER_PRICE_1500, ITEM_SKU2));
    List<ItemPickupPoint> offlineItems2a = new ArrayList<>();
    offlineItems2a.add(createItemPickupPoint(LIST_PRICE_25000, OFFER_PRICE_20000, ITEM_SKU3));
    List<ItemPickupPoint> offlineItems2b = new ArrayList<>();
    offlineItems2b.add(createItemPickupPoint(LIST_PRICE_26500, OFFER_PRICE_21500, ITEM_SKU4));

    List<ItemPickupPoint> allOfflineItemsOfProduct1 = new ArrayList<>();
    allOfflineItemsOfProduct1.addAll(offlineItems1a);
    allOfflineItemsOfProduct1.addAll(offlineItems1b);

    List<ItemPickupPoint> allOfflineItemsOfProduct2 = new ArrayList<>();
    allOfflineItemsOfProduct2.addAll(offlineItems2a);
    allOfflineItemsOfProduct2.addAll(offlineItems2b);

    ProductAndItemsVO productAndItemsVo1 = new ProductAndItemsVO(product1, items1);
    ProductAndItemsVO productAndItemsVo2 = new ProductAndItemsVO(product2, items2);

    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU1, ITEM_SKU2), true)).thenReturn(allOfflineItemsOfProduct1);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU_1)).thenReturn(product1);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true)).thenReturn(items1);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product1, items1, IS_PV_ENABLED, IN_ALL_PRODUCT)).thenReturn(productAndItemsVo1);

    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true)).thenReturn(allOfflineItemsOfProduct2);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU_2)).thenReturn(product2);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true)).thenReturn(items2);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT)).thenReturn(productAndItemsVo2);

    List<ProductAndItemsVO> responses = offlineItemServiceImpl.findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus);

    assertEquals(responses.size(), 2);

    assertEquals(responses.get(0).getProduct().getProductSku(), PRODUCT_SKU_1);
    assertEquals(responses.get(0).getItems().size(), 2);

    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getListPrice(), LIST_PRICE_1500, 0);
    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_1000, 0);
    assertEquals(responses.get(0).getItems().get(0).getItemSku(), ITEM_SKU1);
    assertTrue(responses.get(0).getItems().get(0).getItemViewConfigs().iterator().next().isBuyable());

    assertEquals(responses.get(0).getItems().get(1).getPrice().iterator().next().getListPrice(), LIST_PRICE_2000, 0);
    assertEquals(responses.get(0).getItems().get(1).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_1500, 0);
    assertEquals(responses.get(0).getItems().get(1).getItemSku(), ITEM_SKU2);
    assertTrue(responses.get(0).getItems().get(1).getItemViewConfigs().iterator().next().isBuyable());

    assertEquals(responses.get(1).getProduct().getProductSku(), PRODUCT_SKU_2);
    assertEquals(responses.get(1).getItems().size(), 2);

    assertEquals(responses.get(1).getItems().get(0).getPrice().iterator().next().getListPrice(), LIST_PRICE_25000, 0);
    assertEquals(responses.get(1).getItems().get(0).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_20000, 0);
    assertEquals(responses.get(1).getItems().get(0).getItemSku(), ITEM_SKU3);
    assertTrue(responses.get(1).getItems().get(0).getItemViewConfigs().iterator().next().isBuyable());

    assertEquals(responses.get(1).getItems().get(1).getPrice().iterator().next().getListPrice(), LIST_PRICE_26500, 0);
    assertEquals(responses.get(1).getItems().get(1).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_21500, 0);
    assertEquals(responses.get(1).getItems().get(1).getItemSku(), ITEM_SKU4);
    assertTrue(responses.get(1).getItems().get(1).getItemViewConfigs().iterator().next().isBuyable());

    verify(
      itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(
      STORE_ID, toSet(ITEM_SKU1, ITEM_SKU2), true);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_1);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true);
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product1, items1, IS_PV_ENABLED, IN_ALL_PRODUCT);
    verify(this.productSearchHelperService).setItemCatalogs(STORE_ID, USERNAME, REQUEST_ID, true,
        Collections.singletonList(productAndItemsVo1), new HashMap<>());

    verify(
      itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(
      STORE_ID, toSet(ITEM_SKU3, ITEM_SKU4), true);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_2);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true);
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT);
    verify(this.productSearchHelperService).setItemCatalogs(STORE_ID, USERNAME, REQUEST_ID, true,
        Collections.singletonList(productAndItemsVo2), new HashMap<>());
  }

  @Test
  public void findOfflineItemsByProductSkus_twoPoductSku_successAndProductNotFoundForFirstProduct() throws Exception {

    List<Item> items2 = new ArrayList<>();
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU3));
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU4));
    List<ItemPickupPoint> offlineItems2a = new ArrayList<>();
    offlineItems2a.add(createItemPickupPoint(LIST_PRICE_25000, OFFER_PRICE_20000, ITEM_SKU3));
    List<ItemPickupPoint> offlineItems2b = new ArrayList<>();
    offlineItems2b.add(createItemPickupPoint(LIST_PRICE_26500, OFFER_PRICE_21500, ITEM_SKU4));

    List<ItemPickupPoint> allOfflineItemsOfProduct2 = new ArrayList<>();
    allOfflineItemsOfProduct2.addAll(offlineItems2a);
    allOfflineItemsOfProduct2.addAll(offlineItems2b);

    ProductAndItemsVO productAndItemsVo2 = new ProductAndItemsVO(product2, items2);

    when(productService.getProduct(STORE_ID, PRODUCT_SKU_1)).thenReturn(null);

    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true)).thenReturn(allOfflineItemsOfProduct2);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU_2)).thenReturn(product2);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true)).thenReturn(items2);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT)).thenReturn(productAndItemsVo2);

    List<ProductAndItemsVO> responses = offlineItemServiceImpl.findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus);

    assertEquals(responses.size(), 1);

    assertEquals(responses.get(0).getProduct().getProductSku(), PRODUCT_SKU_2);
    assertEquals(responses.get(0).getItems().size(), 2);

    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getListPrice(), LIST_PRICE_25000, 0);
    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_20000, 0);
    assertEquals(responses.get(0).getItems().get(0).getItemSku(), ITEM_SKU3);
    assertTrue(responses.get(0).getItems().get(0).getItemViewConfigs().iterator().next().isBuyable());

    assertEquals(responses.get(0).getItems().get(1).getPrice().iterator().next().getListPrice(), LIST_PRICE_26500, 0);
    assertEquals(responses.get(0).getItems().get(1).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_21500, 0);
    assertEquals(responses.get(0).getItems().get(1).getItemSku(), ITEM_SKU4);
    assertTrue(responses.get(0).getItems().get(1).getItemViewConfigs().iterator().next().isBuyable());

    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_1);

    verify(itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_2);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true);
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT);
    verify(this.productSearchHelperService).setItemCatalogs(STORE_ID, USERNAME, REQUEST_ID, true,
        Collections.singletonList(productAndItemsVo2), new HashMap<>());
  }

  @Test
  public void findOfflineItemsByProductSkus_twoPoductSku_successAndItemNullForFirstProduct() throws Exception {

    List<Item> items2 = new ArrayList<>();
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU3));
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU4));
    items2.get(1).setPrice(Collections.singleton(price));

    List<ItemPickupPoint> offlineItems2a = new ArrayList<>();
    offlineItems2a.add(createItemPickupPoint(LIST_PRICE_25000, OFFER_PRICE_20000, ITEM_SKU3));
    List<ItemPickupPoint> offlineItems2b = new ArrayList<>();
    offlineItems2b.add(createItemPickupPoint(LIST_PRICE_26500, OFFER_PRICE_21500, ITEM_SKU4));

    List<ItemPickupPoint> allOfflineItemsOfProduct2 = new ArrayList<>();
    allOfflineItemsOfProduct2.addAll(offlineItems2a);
    allOfflineItemsOfProduct2.addAll(offlineItems2b);

    ProductAndItemsVO productAndItemsVo2 = new ProductAndItemsVO(product2, items2);

    when(productService.getProduct(STORE_ID, PRODUCT_SKU_1)).thenReturn(product1);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true)).thenReturn(null);

    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true)).thenReturn(allOfflineItemsOfProduct2);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU_2)).thenReturn(product2);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true)).thenReturn(items2);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT)).thenReturn(productAndItemsVo2);

    List<ProductAndItemsVO> responses = offlineItemServiceImpl.findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus);

    assertEquals(responses.size(), 1);

    assertEquals(responses.get(0).getProduct().getProductSku(), PRODUCT_SKU_2);
    assertEquals(responses.get(0).getItems().size(), 2);

    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getListPrice(),
        LIST_PRICE_25000, 0);
    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getOfferPrice(),
        OFFER_PRICE_20000, 0);
    assertEquals(responses.get(0).getItems().get(0).getItemSku(), ITEM_SKU3);
    assertTrue(responses.get(0).getItems().get(0).getItemViewConfigs().iterator().next().isBuyable());

    assertEquals(responses.get(0).getItems().get(1).getPrice().iterator().next().getListPrice(),
        LIST_PRICE_26500, 0);
    assertEquals(responses.get(0).getItems().get(1).getPrice().iterator().next().getOfferPrice(),
        OFFER_PRICE_21500, 0);
    assertEquals(responses.get(0).getItems().get(1).getItemSku(), ITEM_SKU4);
    assertTrue(responses.get(0).getItems().get(1).getItemViewConfigs().iterator().next().isBuyable());

    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_1);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true);

    verify(itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_2);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true);
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT);
    verify(this.productSearchHelperService).setItemCatalogs(STORE_ID, USERNAME, REQUEST_ID, true,
        Collections.singletonList(productAndItemsVo2), new HashMap<>());
  }

  @Test
  public void findOfflineItemsByProductSkus_twoPoductSku_successAndItemNotFoundForFirstProduct() throws Exception {

    List<Item> items2 = new ArrayList<>();
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU3));
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU4));

    List<ItemPickupPoint> offlineItems2a = new ArrayList<>();
    offlineItems2a.add(createItemPickupPoint(LIST_PRICE_25000, OFFER_PRICE_20000, ITEM_SKU3));
    List<ItemPickupPoint> offlineItems2b = new ArrayList<>();
    offlineItems2b.add(createItemPickupPoint(LIST_PRICE_26500, OFFER_PRICE_21500, ITEM_SKU4));

    List<ItemPickupPoint> allOfflineItemsOfProduct2 = new ArrayList<>();
    allOfflineItemsOfProduct2.addAll(offlineItems2a);
    allOfflineItemsOfProduct2.addAll(offlineItems2b);

    ProductAndItemsVO productAndItemsVo2 = new ProductAndItemsVO(product2, items2);

    when(productService.getProduct(STORE_ID, PRODUCT_SKU_1)).thenReturn(product1);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true)).thenReturn(new ArrayList<>());

    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true)).thenReturn(allOfflineItemsOfProduct2);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU_2)).thenReturn(product2);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true)).thenReturn(items2);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT)).thenReturn(productAndItemsVo2);

    List<ProductAndItemsVO> responses = offlineItemServiceImpl.findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus);

    assertEquals(responses.size(), 1);

    assertEquals(responses.get(0).getProduct().getProductSku(), PRODUCT_SKU_2);
    assertEquals(responses.get(0).getItems().size(), 2);

    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getListPrice(), LIST_PRICE_25000, 0);
    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_20000, 0);
    assertEquals(responses.get(0).getItems().get(0).getItemSku(), ITEM_SKU3);
    assertTrue(responses.get(0).getItems().get(0).getItemViewConfigs().iterator().next().isBuyable());

    assertEquals(responses.get(0).getItems().get(1).getPrice().iterator().next().getListPrice(), LIST_PRICE_26500, 0);
    assertEquals(responses.get(0).getItems().get(1).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_21500, 0);
    assertEquals(responses.get(0).getItems().get(1).getItemSku(), ITEM_SKU4);
    assertTrue(responses.get(0).getItems().get(1).getItemViewConfigs().iterator().next().isBuyable());

    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_1);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true);

    verify(itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_2);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true);
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT);
    verify(this.productSearchHelperService).setItemCatalogs(STORE_ID, USERNAME, REQUEST_ID, true,
        Collections.singletonList(productAndItemsVo2), new HashMap<>());
  }

  @Test
  public void findOfflineItemsByProductSkus_twoPoductSku_successAndOfflineItemNullFoundForFirstProduct() throws Exception {

    List<Item> items1 = new ArrayList<>();
    items1.add(createItem(PRODUCT_SKU_1, ITEM_SKU1));
    items1.add(createItem(PRODUCT_SKU_1, ITEM_SKU2));

    List<Item> items2 = new ArrayList<>();
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU3));
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU4));

    List<ItemPickupPoint> offlineItems1a = new ArrayList<>();
    offlineItems1a.add(createItemPickupPoint(LIST_PRICE_1500, OFFER_PRICE_1000, ITEM_SKU1));
    offlineItems1a.add(createItemPickupPoint(LIST_PRICE_2000, OFFER_PRICE_1500, ITEM_SKU1));
    List<ItemPickupPoint> offlineItems1b = new ArrayList<>();
    offlineItems1b.add(createItemPickupPoint(LIST_PRICE_2000, OFFER_PRICE_1500, ITEM_SKU2));
    List<ItemPickupPoint> offlineItems2a = new ArrayList<>();
    offlineItems2a.add(createItemPickupPoint(LIST_PRICE_25000, OFFER_PRICE_20000, ITEM_SKU3));
    List<ItemPickupPoint> offlineItems2b = new ArrayList<>();
    offlineItems2b.add(createItemPickupPoint(LIST_PRICE_26500, OFFER_PRICE_21500, ITEM_SKU4));

    List<ItemPickupPoint> allOfflineItemsOfProduct1 = new ArrayList<>();
    allOfflineItemsOfProduct1.addAll(offlineItems1b);

    List<ItemPickupPoint> allOfflineItemsOfProduct2 = new ArrayList<>();
    allOfflineItemsOfProduct2.addAll(offlineItems2a);
    allOfflineItemsOfProduct2.addAll(offlineItems2b);

    ProductAndItemsVO productAndItemsVo1 = new ProductAndItemsVO(product1, items1);
    ProductAndItemsVO productAndItemsVo2 = new ProductAndItemsVO(product2, items2);

    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU1, ITEM_SKU2), true)).thenReturn(allOfflineItemsOfProduct1);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU_1)).thenReturn(product1);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true)).thenReturn(items1);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product1, items1, IS_PV_ENABLED, IN_ALL_PRODUCT)).thenReturn(productAndItemsVo1);

    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true)).thenReturn(allOfflineItemsOfProduct2);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU_2)).thenReturn(product2);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true)).thenReturn(items2);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT)).thenReturn(productAndItemsVo2);

    List<ProductAndItemsVO> responses = offlineItemServiceImpl.findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus);

    assertEquals(responses.size(), 2);

    assertEquals(responses.get(0).getProduct().getProductSku(), PRODUCT_SKU_1);
    assertEquals(responses.get(0).getItems().size(), 1);

    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getListPrice(), LIST_PRICE_2000, 0);
    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_1500, 0);
    assertEquals(responses.get(0).getItems().get(0).getItemSku(), ITEM_SKU2);
    assertTrue(responses.get(0).getItems().get(0).getItemViewConfigs().iterator().next().isBuyable());

    assertEquals(responses.get(1).getProduct().getProductSku(), PRODUCT_SKU_2);
    assertEquals(responses.get(1).getItems().size(), 2);

    assertEquals(responses.get(1).getItems().get(0).getPrice().iterator().next().getListPrice(), LIST_PRICE_25000, 0);
    assertEquals(responses.get(1).getItems().get(0).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_20000, 0);
    assertEquals(responses.get(1).getItems().get(0).getItemSku(), ITEM_SKU3);
    assertTrue(responses.get(1).getItems().get(0).getItemViewConfigs().iterator().next().isBuyable());

    assertEquals(responses.get(1).getItems().get(1).getPrice().iterator().next().getListPrice(), LIST_PRICE_26500, 0);
    assertEquals(responses.get(1).getItems().get(1).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_21500, 0);
    assertEquals(responses.get(1).getItems().get(1).getItemSku(), ITEM_SKU4);
    assertTrue(responses.get(1).getItems().get(1).getItemViewConfigs().iterator().next().isBuyable());

    verify(itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU1, ITEM_SKU2), true);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_1);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true);
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product1, items1, IS_PV_ENABLED, IN_ALL_PRODUCT);
    verify(this.productSearchHelperService).setItemCatalogs(STORE_ID, USERNAME, REQUEST_ID, true,
        Collections.singletonList(productAndItemsVo1), new HashMap<>());

    verify(itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_2);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true);
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT);
    verify(this.productSearchHelperService).setItemCatalogs(STORE_ID, USERNAME, REQUEST_ID, true,
        Collections.singletonList(productAndItemsVo2), new HashMap<>());
  }

  @Test
  public void findOfflineItemsByProductSkus_twoPoductSku_successAndOfflineItemNotFoundFoundForFirstProduct() throws Exception {

    List<Item> items1 = new ArrayList<>();
    items1.add(createItem(PRODUCT_SKU_1, ITEM_SKU1));
    items1.add(createItem(PRODUCT_SKU_1, ITEM_SKU2));

    List<Item> items2 = new ArrayList<>();
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU3));
    items2.add(createItem(PRODUCT_SKU_2, ITEM_SKU4));

    List<ItemPickupPoint> offlineItems1a = new ArrayList<>();
    offlineItems1a.add(createItemPickupPoint(LIST_PRICE_1500, OFFER_PRICE_1000, ITEM_SKU1));
    offlineItems1a.add(createItemPickupPoint(LIST_PRICE_2000, OFFER_PRICE_1500, ITEM_SKU1));
    List<ItemPickupPoint> offlineItems1b = new ArrayList<>();
    offlineItems1b.add(createItemPickupPoint(LIST_PRICE_2000, OFFER_PRICE_1500, ITEM_SKU2));
    List<ItemPickupPoint> offlineItems2a = new ArrayList<>();
    offlineItems2a.add(createItemPickupPoint(LIST_PRICE_25000, OFFER_PRICE_20000, ITEM_SKU3));
    List<ItemPickupPoint> offlineItems2b = new ArrayList<>();
    offlineItems2b.add(createItemPickupPoint(LIST_PRICE_26500, OFFER_PRICE_21500, ITEM_SKU4));

    ProductAndItemsVO productAndItemsVo1 = new ProductAndItemsVO(product1, items1);
    ProductAndItemsVO productAndItemsVo2 = new ProductAndItemsVO(product2, items2);

    List<ItemPickupPoint> allOfflineItemsOfProductSku1 = new ArrayList<>();
    allOfflineItemsOfProductSku1.addAll(offlineItems1b);

    List<ItemPickupPoint> allOfflineItemsOfProductSku2 = new ArrayList<>();
    allOfflineItemsOfProductSku2.addAll(offlineItems2a);
    allOfflineItemsOfProductSku2.addAll(offlineItems2b);

    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU1, ITEM_SKU2), true)).thenReturn(allOfflineItemsOfProductSku1);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU_1)).thenReturn(product1);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true)).thenReturn(items1);
    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
      Collections.singleton(ITEM_SKU1), true)).thenReturn(new ArrayList<>());
    when(masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product1, items1, IS_PV_ENABLED, IN_ALL_PRODUCT)).thenReturn(productAndItemsVo1);

    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true)).thenReturn(allOfflineItemsOfProductSku2);
    when(productService.getProduct(STORE_ID, PRODUCT_SKU_2)).thenReturn(product2);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true)).thenReturn(items2);
    when(masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT)).thenReturn(productAndItemsVo2);

    List<ProductAndItemsVO> responses = offlineItemServiceImpl.findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus);

    assertEquals(responses.size(), 2);

    assertEquals(responses.get(0).getProduct().getProductSku(), PRODUCT_SKU_1);
    assertEquals(responses.get(0).getItems().size(), 1);

    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getListPrice(), LIST_PRICE_2000, 0);
    assertEquals(responses.get(0).getItems().get(0).getPrice().iterator().next().getOfferPrice(), OFFER_PRICE_1500, 0);
    assertEquals(responses.get(0).getItems().get(0).getItemSku(), ITEM_SKU2);
    assertTrue(responses.get(0).getItems().get(0).getItemViewConfigs().iterator().next().isBuyable());

    assertEquals(responses.get(1).getProduct().getProductSku(), PRODUCT_SKU_2);
    assertEquals(responses.get(1).getItems().size(), 2);

    assertEquals(responses.get(1).getItems().get(0).getPrice().iterator().next().getListPrice(),
        LIST_PRICE_25000, 0);
    assertEquals(responses.get(1).getItems().get(0).getPrice().iterator().next().getOfferPrice(),
        OFFER_PRICE_20000, 0);
    assertEquals(responses.get(1).getItems().get(0).getItemSku(), ITEM_SKU3);
    assertTrue(responses.get(1).getItems().get(0).getItemViewConfigs().iterator().next().isBuyable());

    assertEquals(responses.get(1).getItems().get(1).getPrice().iterator().next().getListPrice(),
        LIST_PRICE_26500, 0);
    assertEquals(responses.get(1).getItems().get(1).getPrice().iterator().next().getOfferPrice(),
        OFFER_PRICE_21500, 0);
    assertEquals(responses.get(1).getItems().get(1).getItemSku(), ITEM_SKU4);
    assertTrue(responses.get(1).getItems().get(1).getItemViewConfigs().iterator().next().isBuyable());

    verify(itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU1, ITEM_SKU2), true);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_1);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true);
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product1, items1, IS_PV_ENABLED, IN_ALL_PRODUCT);
    verify(this.productSearchHelperService).setItemCatalogs(STORE_ID, USERNAME, REQUEST_ID, true,
        Collections.singletonList(productAndItemsVo1), new HashMap<>());

    verify(itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
        toSet(ITEM_SKU3, ITEM_SKU4), true);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_2);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_2, true);
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product2, items2, IS_PV_ENABLED, IN_ALL_PRODUCT);
    verify(this.productSearchHelperService).setItemCatalogs(STORE_ID, USERNAME, REQUEST_ID, true,
        Collections.singletonList(productAndItemsVo2), new HashMap<>());
  }

  @Test
  public void findOfflineItemsByProductSkus_oneProductSku_emptyItemViewConfig() throws Exception {
    itemPickupPoint.setItemSku(ITEM_SKU1);
    this.itemPickupPoint.setPrice(Collections.singleton(price));
    productSkus = new ArrayList<>();
    productSkus.add(PRODUCT_SKU_1);

    List<Item> items1 = new ArrayList<>();
    items1.add(createItem(PRODUCT_SKU_1, ITEM_SKU1));
    items1.get(0).setItemViewConfigs(new HashSet<>());

    ProductAndItemsVO productAndItemsVo = new ProductAndItemsVO(product1, items1);

    when(productService.getProduct(STORE_ID, PRODUCT_SKU_1)).thenReturn(product1);
    when(itemService.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true)).thenReturn(items1);
    when(itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
      Collections.singleton(ITEM_SKU1), true))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(masterDataConstructorService.constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product1, items1, IS_PV_ENABLED, IN_ALL_PRODUCT)).thenReturn(productAndItemsVo);

    List<ProductAndItemsVO> responses = offlineItemServiceImpl.findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus);

    assertEquals(responses.size(), 1);

    assertEquals(responses.get(0).getProduct().getProductSku(), PRODUCT_SKU_1);
    assertEquals(responses.get(0).getItems().size(), 1);

    assertEquals(LIST_PRICE_1500,
      responses.get(0).getItems().get(0).getPrice().iterator().next().getListPrice(), 0);
    assertEquals(OFFER_PRICE_1000,
      responses.get(0).getItems().get(0).getPrice().iterator().next().getOfferPrice(), 0);
    assertEquals(responses.get(0).getItems().get(0).getItemSku(), ITEM_SKU1);

    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_1);
    verify(itemService).getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU_1, true);
    verify(itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(STORE_ID,
      Collections.singleton(ITEM_SKU1), true);
    verify(masterDataConstructorService).constructProductAndItemWithMasterData(STORE_ID, USERNAME,
        REQUEST_ID, product1, items1, IS_PV_ENABLED, IN_ALL_PRODUCT);
    verify(this.productSearchHelperService).setItemCatalogs(STORE_ID, USERNAME, REQUEST_ID, true,
        Collections.singletonList(productAndItemsVo), new HashMap<>());
  }

  @Test
  public void findOfflineItemsByProductSkus_oneProductSku_errorProductAndItemNotFound() throws Exception {

    List<Item> items1 = new ArrayList<>();
    items1.add(createItem(PRODUCT_SKU_1, ITEM_SKU1));

    productSkus = new ArrayList<>();
    productSkus.add(PRODUCT_SKU_1);

    when(productService.getProduct(STORE_ID, PRODUCT_SKU_1)).thenReturn(null);

    try {
      List<ProductAndItemsVO> responses =
          offlineItemServiceImpl.findOfflineItemsByProductSkus(STORE_ID, USERNAME, REQUEST_ID, productSkus);
    } catch (RuntimeException e) {
      assertEquals(e.getMessage(), PRODUCT_OR_ITEM_NOT_FOUND);
    }

    verify(productService).getProduct(STORE_ID, PRODUCT_SKU_1);
  }

  private static Item createItem(String productSku, String itemSku) {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<ItemViewConfig>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfigs.add(itemViewConfig);

    Item item = new Item();
    item.setProductSku(productSku);
    item.setItemViewConfigs(itemViewConfigs);
    item.setItemSku(itemSku);

    return item;
  }

  private static OfflineItem createOfflineItem(double listPrice, double offerPrice, String itemSku) {
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setListPrice(listPrice);
    offlineItem.setOfferPrice(offerPrice);
    offlineItem.setUpdatedBy("User");
    offlineItem.setUpdatedDate(new Date());
    offlineItem.setItemSku(itemSku);

    return offlineItem;
  }

  private static ItemPickupPoint createItemPickupPoint(double listPrice, double offerPrice, String itemSku) {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Price price = new Price();
    price.setOfferPrice(offerPrice);
    price.setListPrice(listPrice);
    itemPickupPoint.setPrice(Collections.singleton(price));
    itemPickupPoint.setItemSku(itemSku);
    return itemPickupPoint;
  }

  @Test
  public void findByPickupPointCodeTest_success() throws Exception {
    offlineItemServiceImpl.findByPickupPointCode(STORE_ID, PICKUP_POINT);
    verify(offlineItemRepository).findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT);
  }

  @Test
  public void findByPickupPointCodeTest_blankStoreId() throws Exception {
    try {
      offlineItemServiceImpl.findByPickupPointCode(null, PICKUP_POINT);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByPickupPointCodeTest_blankPickupPointCode() throws Exception {
    try {
      offlineItemServiceImpl.findByPickupPointCode(STORE_ID, null);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void testFindByItemSkuAndPickupPointCode_success() throws Exception {
    when(this.offlineItemRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT)).thenReturn(this.offlineItem);
    offlineItemServiceImpl.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT);
    verify(offlineItemRepository).findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT);
  }

  @Test
  public void testFindByItemSkuAndPickupPointCode_blankStoreId() throws Exception {
    try {
      offlineItemServiceImpl.findByItemSkuAndPickupPointCode(null, ITEM_SKU, PICKUP_POINT);
    } catch (Exception e) {
      assertEquals(
          ErrorCategory.VALIDATION.getMessage() + OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK,
          e.getMessage());
    }
  }

  @Test
  public void testFindByItemSkuAndPickupPointCode_blankItemSku() throws Exception {
    try {
      offlineItemServiceImpl.findByItemSkuAndPickupPointCode(STORE_ID, null, PICKUP_POINT);
    } catch (Exception e) {
      assertEquals(
          ErrorCategory.VALIDATION.getMessage() + OfflineItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK,
          e.getMessage());
    }
  }

  @Test
  public void testFindByItemSkuAndPickupPointCode_blankPickupPointCode() throws Exception {
    try {
      offlineItemServiceImpl.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, null);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage()
          + OfflineItemServiceImpl.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByItemSkusAndMarkForDeleteFalse_blankStoreId() throws Exception {
    try {
      offlineItemServiceImpl.findByItemSkusAndMarkForDeleteFalse(null, ITEM_SKUS_SET);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findByItemSkusAndMarkForDeleteFalse_emptyItemSkus() throws Exception {
    try {
      offlineItemServiceImpl.findByItemSkusAndMarkForDeleteFalse(STORE_ID, null);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.ITEM_SKUS_MUST_NOT_BE_EMPTY, e.getMessage());
    }
  }

  @Test
  public void findByItemSkusAndMarkForDeleteFalse_success() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();

    when(offlineItemRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, ITEM_SKUS_SET))
        .thenReturn(offlineItems);

    List<OfflineItem> result = offlineItemServiceImpl.findByItemSkusAndMarkForDeleteFalse(STORE_ID, ITEM_SKUS_SET);

    verify(offlineItemRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, ITEM_SKUS_SET);
    assertEquals(offlineItems, result);
  }

  @Test
  public void findByItemSkusAndPickupPointCodeAndMarkForDeleteFalse() {
    this.offlineItemServiceImpl.findByItemSkusAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKUS_SET, PICKUP_POINT);
    verify(this.offlineItemRepository)
        .findByStoreIdAndItemSkuInAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKUS_SET,
            PICKUP_POINT);
  }

  @Test
  public void findByItemSkusAndMarkForDeleteFalseWithLimit_success() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();
    Page<OfflineItem> offlineItemPage = new PageImpl<>(offlineItems);

    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LIMIT_OFFLINE_ITEMS)).thenReturn(limitOfflineItemsSystemParameter);
    when(offlineItemRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID), eq(ITEM_SKUS_SET), pageableCaptor.capture()))
        .thenReturn(offlineItemPage);

    List<OfflineItem> result = offlineItemServiceImpl.findByItemSkusAndMarkForDeleteFalseWithLimit(STORE_ID, ITEM_SKUS_SET);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS);
    verify(offlineItemRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID), eq(ITEM_SKUS_SET), pageableCaptor.capture());
    assertEquals(offlineItems, result);
  }

  @Test
  public void findByItemSkusAndMarkForDeleteFalseWithLimitWithoutPagination() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();
    when(offlineItemRepository
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndLimit(STORE_ID, ITEM_SKUS_SET, LIMIT_OFFLINE_ITEMS))
        .thenReturn(offlineItems);
    List<OfflineItem> result = offlineItemServiceImpl
        .findByItemSkusAndMarkForDeleteFalseWithLimitWithoutPagination(STORE_ID, ITEM_SKUS_SET, LIMIT_OFFLINE_ITEMS);
    verify(offlineItemRepository)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndLimit(STORE_ID, ITEM_SKUS_SET, LIMIT_OFFLINE_ITEMS);
    assertEquals(offlineItems, result);
  }

  @Test
  public void findMultipleMerchantsOfflineItemByItemSku_blankStoreId() throws Exception {
    try {
      offlineItemServiceImpl.findMultipleMerchantsOfflineItemByItemSku(null, ITEM_SKU);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.STORE_ID_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findMultipleMerchantsOfflineItemByItemSku_blankItemSku() throws Exception {
    try {
      offlineItemServiceImpl.findMultipleMerchantsOfflineItemByItemSku(STORE_ID, null);
    } catch (Exception e) {
      assertEquals(ErrorCategory.VALIDATION.getMessage() +
          OfflineItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK, e.getMessage());
    }
  }

  @Test
  public void findMultipleMerchantsOfflineItemByItemSku_success_noOfflineItems() throws Exception {
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(null);

    List<ItemPickupPointPriceVo> result =
      offlineItemServiceImpl.findMultipleMerchantsOfflineItemByItemSku(STORE_ID,
      ITEM_SKU);
    assertEquals(0, result.size());

    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findMultipleMerchantsOfflineItemByItemSku_withSynchronizedFalse_success() throws Exception {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setSynchronized(false);

    List<ItemPickupPointPriceVo> itemPickupPointPriceVoList = new ArrayList<>();

    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LIMIT_OFFLINE_ITEMS)).thenReturn(limitOfflineItemsSystemParameter);
    when(itemPickupPointService.findItemPickupPointPriceVoByItemSkus(eq(STORE_ID), eq(ITEM_SKUS_SET),
        pageableCaptor.capture())).thenReturn(itemPickupPointPriceVoList);

    List<ItemPickupPointPriceVo> result = offlineItemServiceImpl.findMultipleMerchantsOfflineItemByItemSku(STORE_ID, ITEM_SKU);
    assertEquals(itemPickupPointPriceVoList, result);

    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS);
    verify(itemPickupPointService).findItemPickupPointPriceVoByItemSkus(eq(STORE_ID),
      eq(ITEM_SKUS_SET), pageableCaptor.capture());

    Pageable pageable = pageableCaptor.getValue();
    Assertions.assertNotNull(pageable);
  }

  @Test
  public void findMultipleMerchantsOfflineItemByItemSku_success_withPristineIdAndPristineIdDataLookupEnabled() throws Exception {
    Set<String> pristineIds = new HashSet<>(Arrays.asList(PRISTINE_ID));
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);

    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setPristineDataItem(pristineDataItem);
    item.setSynchronized(true);

    List<ItemPickupPointPriceVo> itemPickupPointPriceVoList = new ArrayList<>();

    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(Boolean.TRUE.toString());

    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PRISTINE_ID_DATA_LOOKUP_ENABLED))
        .thenReturn(systemParameter);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    when(itemService.getItemSkusByPristineIdsAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, pristineIds))
        .thenReturn(Arrays.asList(item));
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LIMIT_OFFLINE_ITEMS)).thenReturn(limitOfflineItemsSystemParameter);
    when(itemPickupPointService.findItemPickupPointPriceVoByItemSkus(eq(STORE_ID), eq(ITEM_SKUS_SET),
        pageableCaptor.capture())).thenReturn(itemPickupPointPriceVoList);

    List<ItemPickupPointPriceVo> result = offlineItemServiceImpl.findMultipleMerchantsOfflineItemByItemSku(STORE_ID, ITEM_SKU);
    assertEquals(itemPickupPointPriceVoList, result);

    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PRISTINE_ID_DATA_LOOKUP_ENABLED);
    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemService).getItemSkusByPristineIdsAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, pristineIds);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS);
    verify(itemPickupPointService).findItemPickupPointPriceVoByItemSkus(eq(STORE_ID),
      eq(ITEM_SKUS_SET), pageableCaptor.capture());

    Pageable pageable = pageableCaptor.getValue();
    Assertions.assertNotNull(pageable);
  }

  @Test
  public void findMultipleMerchantsOfflineItemByItemSku_success_withPristineIdAndPristineIdDataLookupDisabled() throws Exception {
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);

    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setItemCode(ITEM_CODE);
    item.setPristineDataItem(pristineDataItem);
    item.setSynchronized(true);

    List<ItemPickupPointPriceVo> itemPickupPointPriceVoList = new ArrayList<>();

    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue(Boolean.FALSE.toString());

    when(systemParameterService.findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PRISTINE_ID_DATA_LOOKUP_ENABLED))
        .thenReturn(systemParameter);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    when(itemService.findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LIMIT_OFFLINE_ITEMS)).thenReturn(limitOfflineItemsSystemParameter);
    when(itemPickupPointService.findItemPickupPointPriceVoByItemSkus(eq(STORE_ID), eq(ITEM_SKUS_SET),
      pageableCaptor.capture()))
        .thenReturn(itemPickupPointPriceVoList);

    List<ItemPickupPointPriceVo> result = offlineItemServiceImpl.findMultipleMerchantsOfflineItemByItemSku(STORE_ID, ITEM_SKU);
    assertEquals(itemPickupPointPriceVoList, result);

    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemService).findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(STORE_ID, ITEM_CODE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS);
    verify(itemPickupPointService).findItemPickupPointPriceVoByItemSkus(eq(STORE_ID),
      eq(ITEM_SKUS_SET), pageableCaptor.capture());
    verify(systemParameterService).findValueByStoreIdAndVariable(
        STORE_ID, SystemParameterNames.PRISTINE_ID_DATA_LOOKUP_ENABLED);

    Pageable pageable = pageableCaptor.getValue();
    Assertions.assertNotNull(pageable);
  }

  @Test
  public void findMultipleMerchantsOfflineItemByItemSku_success_withItemCode() throws Exception {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setItemCode(ITEM_CODE);
    item.setSynchronized(true);

    List<ItemPickupPointPriceVo> itemPickupPointPriceVoList = new ArrayList<>();

    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    when(itemService.findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LIMIT_OFFLINE_ITEMS)).thenReturn(limitOfflineItemsSystemParameter);
    when(this.itemPickupPointService.findItemPickupPointPriceVoByItemSkus(eq(STORE_ID), eq(ITEM_SKUS_SET), pageableCaptor.capture()))
        .thenReturn(itemPickupPointPriceVoList);

    List<ItemPickupPointPriceVo> result = offlineItemServiceImpl.findMultipleMerchantsOfflineItemByItemSku(STORE_ID, ITEM_SKU);
    assertEquals(itemPickupPointPriceVoList, result);

    verify(itemService).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemService).findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(STORE_ID, ITEM_CODE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.LIMIT_OFFLINE_ITEMS);
    verify(this.itemPickupPointService).findItemPickupPointPriceVoByItemSkus(eq(STORE_ID),
      eq(ITEM_SKUS_SET), pageableCaptor.capture());
  }

  @Test
  public void findByOfflineItemId() {
    String offlineItemId = "offlineItemId-1";
    OfflineItem offlineItem = new OfflineItem();

    when(offlineItemRepository.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, offlineItemId))
        .thenReturn(offlineItem);
    offlineItemServiceImpl.findByOfflineItemIdAndMarkForDeleteFalse(STORE_ID, offlineItemId);
    verify(offlineItemRepository).findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID, offlineItemId);
  }

  @Test
  public void findByOfflineItemId_StoreId_Blank() {
    String offlineItemId = "offlineItemId-1";
    Assertions.assertThrows(RuntimeException.class, () -> offlineItemServiceImpl.findByOfflineItemIdAndMarkForDeleteFalse(null, offlineItemId));
  }

  @Test
  public void findByOfflineItemId_offlineItemId_Blank() {
    String offlineItemId = "";
    Assertions.assertThrows(RuntimeException.class, () -> offlineItemServiceImpl.findByOfflineItemIdAndMarkForDeleteFalse(STORE_ID, offlineItemId));
  }

  @Test
  public void isOfflineItem_storeIdBlank() throws Exception {
    Assertions.assertThrows(RuntimeException.class, () -> offlineItemServiceImpl.isOfflineItem(null, OFFLINE_ITEM_ID));
  }

  @Test
  public void isOfflineItem_uniqueIdBlank() throws Exception {
    Assertions.assertThrows(RuntimeException.class, () -> offlineItemServiceImpl.isOfflineItem(STORE_ID, null));
  }

  @Test
  public void isOfflineItemTest() throws Exception {
    when(skuValidator.isItemSku(OFFLINE_ITEM_ID)).thenReturn(Boolean.TRUE);
    when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, OFFLINE_ITEM_ID)).thenReturn(
        this.item);
    boolean result = offlineItemServiceImpl.isOfflineItem(STORE_ID, OFFLINE_ITEM_ID);
    verify(skuValidator).isItemSku(OFFLINE_ITEM_ID);
    verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, OFFLINE_ITEM_ID);
  }

  @Test
  public void isOfflineItemTest1() throws Exception {
    when(skuValidator.isItemSku(OFFLINE_ITEM_ID)).thenReturn(Boolean.FALSE);
    when(skuValidator.isItemSkuL4OrL5(OFFLINE_ITEM_ID)).thenReturn(Boolean.TRUE);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    boolean result = offlineItemServiceImpl.isOfflineItem(STORE_ID, OFFLINE_ITEM_ID);
    assertEquals(Boolean.TRUE, result);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
    verify(skuValidator).isItemSkuL4OrL5(OFFLINE_ITEM_ID);
    verify(skuValidator).isItemSku(OFFLINE_ITEM_ID);
  }

  @Test
  public void isOfflineItemTest2() throws Exception {
    when(skuValidator.isItemSku(OFFLINE_ITEM_ID)).thenReturn(Boolean.FALSE);
    when(skuValidator.isItemSkuL4OrL5(OFFLINE_ITEM_ID)).thenReturn(Boolean.FALSE);
    boolean result = offlineItemServiceImpl.isOfflineItem(STORE_ID, OFFLINE_ITEM_ID);
    assertEquals(Boolean.FALSE, result);
    verify(skuValidator).isItemSkuL4OrL5(OFFLINE_ITEM_ID);
    verify(skuValidator).isItemSku(OFFLINE_ITEM_ID);
  }

  @Test
  public void publishOfflineItemsInBatch_byItemSku() throws Exception {

    when(
      this.itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(
        STORE_ID, Collections.singleton(ITEM_SKU), true)).thenReturn(
      Collections.singletonList(itemPickupPoint));

    this.offlineItemServiceImpl.publishOfflineItems(STORE_ID, 10,
      Collections.singletonList(ITEM_SKU));
    verify(
      this.itemPickupPointService).findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(
      STORE_ID, Collections.singleton(ITEM_SKU), true);
    verify(this.saveAndPublishService).publishListOfOfflineItemsWithoutFailsafe(
      Mockito.anyList());
  }

  @Test
  public void publishOfflineItemsInBatch_emptyItemSkus() throws Exception {
    this.offlineItemServiceImpl.publishOfflineItems(STORE_ID, 10, null);
  }

  @Test
  public void republishOfflineItemsByMerchantCodesInBatch_test() throws Exception {

    final List<String> merchantCodes = Arrays.asList(MERCHANT_CODE, MERCHANT_CODE_2);

    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU1);
    offlineItem.setMerchantCode(MERCHANT_CODE);
    OfflineItem offlineItem2 = new OfflineItem();
    offlineItem2.setItemSku(ITEM_SKU2);
    offlineItem2.setMerchantCode(MERCHANT_CODE_2);

    Pageable pageable =
       PageRequest.of(0, 10, Sort.by(Sort.Direction.ASC, SolrFieldNames.CREATED_DATE));
    List<OfflineItem> offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem);
    offlineItems.add(offlineItem2);
    Page<OfflineItem> itemSolrsInPage1 = new PageImpl<>(offlineItems);

    SystemParameter sysParamRunningState = new SystemParameter();
    sysParamRunningState.setValue("false");

    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REPUBLISH_OFFLINE_ITEMS_RUNNING_STATE))
        .thenReturn(sysParamRunningState);

    Mockito.when(offlineItemRepository.findByStoreIdAndMerchantCodeIn(anyString(), anyList(), any())).thenReturn(new PageImpl(new ArrayList()));

    this.offlineItemServiceImpl.republishOfflineItemsByMerchantCodes(STORE_ID, 10, merchantCodes);

    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REPUBLISH_OFFLINE_ITEMS_RUNNING_STATE);

    sysParamRunningState.setValue("true");
    verify(this.systemParameterService, times(2)).update(sysParamRunningState);

    sysParamRunningState.setValue("false");
    verify(this.systemParameterService, times(2)).update(sysParamRunningState);

  }

  @Test
  public void republishOfflineItemsByMerchantCodesInBatch_throwException_test() throws Exception {

    final List<String> merchantCodes = Arrays.asList(MERCHANT_CODE, MERCHANT_CODE_2);

    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU1);
    offlineItem.setMerchantCode(MERCHANT_CODE);
    OfflineItem offlineItem2 = new OfflineItem();
    offlineItem2.setItemSku(ITEM_SKU2);
    offlineItem2.setMerchantCode(MERCHANT_CODE_2);

    Pageable pageable =
       PageRequest.of(0, 10, Sort.by(Sort.Direction.ASC, SolrFieldNames.CREATED_DATE));
    List<OfflineItem> offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem);
    offlineItems.add(offlineItem2);
    Page<OfflineItem> itemSolrsInPage1 = new PageImpl<>(offlineItems);

    SystemParameter sysParamRunningState = new SystemParameter();
    sysParamRunningState.setValue("false");

    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REPUBLISH_OFFLINE_ITEMS_RUNNING_STATE))
        .thenReturn(sysParamRunningState);
    when(offlineItemRepository.findByStoreIdAndMerchantCodeIn(STORE_ID, merchantCodes, pageable))
        .thenThrow(ApplicationRuntimeException.class);
    try {
      this.offlineItemServiceImpl.republishOfflineItemsByMerchantCodes(STORE_ID, 10, merchantCodes);
    } finally {
      verify(this.systemParameterService)
          .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.REPUBLISH_OFFLINE_ITEMS_RUNNING_STATE);

      sysParamRunningState.setValue("true");
      verify(this.systemParameterService, times(2)).update(sysParamRunningState);

      sysParamRunningState.setValue("false");
      verify(this.systemParameterService, times(2)).update(sysParamRunningState);
    }
  }

  @Test
  public void republishOfflineItemsByMerchantCodesInBatch_validateRepublishJobRunning_throwException_test() throws Exception {

    final List<String> merchantCodes = Arrays.asList(MERCHANT_CODE, MERCHANT_CODE_2);

    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU1);
    offlineItem.setMerchantCode(MERCHANT_CODE);
    OfflineItem offlineItem2 = new OfflineItem();
    offlineItem2.setItemSku(ITEM_SKU2);
    offlineItem2.setMerchantCode(MERCHANT_CODE_2);

    Pageable pageable =
        PageRequest.of(2, 10, Sort.by(Sort.Direction.ASC, SolrFieldNames.CREATED_DATE));
    List<OfflineItem> offlineItems = new ArrayList<>();
    offlineItems.add(offlineItem);
    offlineItems.add(offlineItem2);
    Page<OfflineItem> itemSolrsInPage1 = new PageImpl<>(offlineItems);

    SystemParameter sysParamRunningState = new SystemParameter();
    sysParamRunningState.setValue("true");

    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REPUBLISH_OFFLINE_ITEMS_RUNNING_STATE))
        .thenReturn(sysParamRunningState);

    Assertions.assertThrows(Exception.class, () -> this.offlineItemServiceImpl.republishOfflineItemsByMerchantCodes(STORE_ID, 10, merchantCodes));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.REPUBLISH_OFFLINE_ITEMS_RUNNING_STATE);
  }

  @Test
  public void findByItemSkuAndMarkForDeleteFalseTest() {
    OfflineItem offlineItem = new OfflineItem();
    Page<OfflineItem> offlineItemPage = new PageImpl<>(Collections.singletonList(offlineItem));
    when(offlineItemRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, pageable))
        .thenReturn(offlineItemPage);
    Page<OfflineItem> offlineItems =
        offlineItemServiceImpl.findByItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, pageable);
    verify(offlineItemRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, pageable);
    Assertions.assertEquals(offlineItemPage, offlineItems);
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_deferredReindex() throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "excludedUserNames", "ALL");
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
            .thenReturn(enableDeferredSolrReindexSystemParameter);
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE,
        ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    doNothing().when(productAndItemSolrIndexerService).offlineItemPriceAtomicUpdate(Mockito.eq(ITEM_SKU), Mockito.anyList());

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE, updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_normalReindex() throws Exception {
    BeanUtils.copyProperties(offlineItem2, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
            .thenReturn(disableDeferredSolrReindexSystemParameter);
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE,
        ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    doNothing().when(productAndItemSolrIndexerService).offlineItemPriceAtomicUpdate(Mockito.eq(ITEM_SKU), Mockito.anyList());

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE, updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(this.reindexService).reindexPendingL3SolrAndDatabase(
      Collections.singletonList(item.getProductSku()), new HashMap<>());
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_deferredReindex_similarOfferPriceAndNullNewData()
      throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "excludedUserNames", "ALL");
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
            .thenReturn(enableDeferredSolrReindexSystemParameter);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE, updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_deferredReindex_updatedOfferPriceAndFalseNewData()
      throws Exception {
    ReflectionTestUtils.setField(offlineItemServiceImpl, "excludedUserNames", "ALL");
    BeanUtils.copyProperties(offlineItem2, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
            .thenReturn(enableDeferredSolrReindexSystemParameter);
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE,
        ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    doNothing().when(productAndItemSolrIndexerService).offlineItemPriceAtomicUpdate(Mockito.eq(ITEM_SKU), Mockito.anyList());

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE, updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_deferredReindex_nullItem() throws Exception {
    BeanUtils.copyProperties(offlineItem2, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
            .thenReturn(enableDeferredSolrReindexSystemParameter);
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE,
        ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(null);

    doNothing().when(productAndItemSolrIndexerService).offlineItemPriceAtomicUpdate(Mockito.eq(ITEM_SKU), Mockito.anyList());

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(this.reindexService).reindexPendingL3SolrAndDatabase(
      Collections.singletonList(item.getProductSku()), new HashMap<>());
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_normalReindex_emptyOfflineItems()
      throws Exception {
    BeanUtils.copyProperties(offlineItem2, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE,
        ITEM_SKU)).thenReturn(new ArrayList<>());

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(this.reindexService).reindexPendingL3SolrAndDatabase(
      Collections.singletonList(item.getProductSku()), new HashMap<>());
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_throwsException() throws Exception {
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    when(itemPickupPointService.updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
        eq(updateOfflineItemPriceRequest))).thenThrow(ApplicationRuntimeException.class);

    try {
      offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
          updateOfflineItemPriceRequest);
    } catch (Exception ex) {
      verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
      Mockito.verify(itemPickupPointService)
          .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
              Mockito.any(UpdateOfflineItemPriceRequest.class));
    }
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_nullStoreId_throwException() throws Exception {
    mandatoryRequestParam.setStoreId(null);
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest));
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_nullMerchantCode_throwsException()
      throws Exception {
    offlineItem.setMerchantCode(null);
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, null,
        updateOfflineItemPriceRequest));
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_nullItemSku_throwsException() throws Exception {
    offlineItem.setItemSku(null);
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest));
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_nullOfferPrice_throwsException()
      throws Exception {
    offlineItem.setOfferPrice(null);
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_listPriceLessThanMinimumPrice_throwsException()
      throws Exception {
    offlineItem.setListPrice(0d);
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_offerPriceLessThanMinimumPrice_throwsException()
      throws Exception {
    offlineItem.setOfferPrice(0d);
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_listingPriceEqual()
      throws Exception {
    offlineItemList.forEach(offlineItem1 -> offlineItem1.setListPrice(offlineItem2.getListPrice()));
    BeanUtils.copyProperties(offlineItem2, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE)).thenReturn(
        minimumPriceSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(disableDeferredSolrReindexSystemParameter);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(this.reindexService).reindexPendingL3SolrAndDatabase(
      Collections.singletonList(item.getProductSku()), new HashMap<>());
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_listingPriceEqual_MPP()
      throws Exception {
    offlineItemList.forEach(offlineItem1 -> offlineItem1.setListPrice(offlineItem2.getListPrice()));
    BeanUtils.copyProperties(offlineItem2, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE)).thenReturn(
        minimumPriceSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(disableDeferredSolrReindexSystemParameter);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(objectConverterService).convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.any());
    verify(this.reindexService).reindexPendingL3SolrAndDatabase(
        Collections.singletonList(item.getProductSku()), new HashMap<>());
  }



  @Test
  public void testUpdateOfflineItemPriceByItemSku_salePriceEqual()
      throws Exception {
    offlineItemList.forEach(offlineItem1 -> offlineItem1.setOfferPrice(offlineItem2.getOfferPrice()));
    BeanUtils.copyProperties(offlineItem2, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(disableDeferredSolrReindexSystemParameter);

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE, updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(this.reindexService).reindexPendingL3SolrAndDatabase(
      Collections.singletonList(item.getProductSku()), new HashMap<>());
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_bothPricesEqual()
      throws Exception {
    offlineItemList.forEach(offlineItem1 -> offlineItem1.setOfferPrice(offlineItem2.getOfferPrice()));
    offlineItemList.forEach(offlineItem1 -> offlineItem1.setListPrice(offlineItem2.getListPrice()));
    BeanUtils.copyProperties(offlineItem2, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(disableDeferredSolrReindexSystemParameter);

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE, updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(this.reindexService).reindexPendingL3SolrAndDatabase(
      Collections.singletonList(item.getProductSku()), new HashMap<>());
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_bothPricesUnEqual()
      throws Exception {
    offlineItemList.forEach(offlineItem1 -> offlineItem1.setOfferPrice(offlineItem2.getListPrice()));
    offlineItemList.forEach(offlineItem1 -> offlineItem1.setListPrice(offlineItem2.getOfferPrice()));
    BeanUtils.copyProperties(offlineItem2, updateOfflineItemPriceRequest);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(disableDeferredSolrReindexSystemParameter);
    when(offlineItemRepository.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE,
        ITEM_SKU)).thenReturn(offlineItemList);
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU))))
        .thenReturn(Arrays.asList(item));
    doNothing().when(productAndItemSolrIndexerService).offlineItemPriceAtomicUpdate(Mockito.eq(ITEM_SKU), Mockito.anyList());

    offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE, updateOfflineItemPriceRequest);

    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX);
    Mockito.verify(itemPickupPointService)
        .updateItemPickupPointPriceByOfflineItem(Mockito.anyString(), Mockito.anyString(),
            Mockito.any(UpdateOfflineItemPriceRequest.class));
    Mockito.verify(itemService).findByStoreIdAndItemSkus(Mockito.anyString(), Mockito.anySet());
    Mockito.verify(saveAndPublishService)
        .publishListOfOfflineItems(Mockito.anyList(), Mockito.anyMap(),
            Mockito.anyString(), Mockito.anyList());
    Mockito.verify(productAndItemSolrIndexerService).applyItem(Mockito.any(Item.class));
    verify(this.reindexService).reindexPendingL3SolrAndDatabase(
      Collections.singletonList(item.getProductSku()), new HashMap<>());
  }

  @Test
  public void testUpdateOfflineItemPriceByItemSku_nullMinimumPrice_throwException()
      throws Exception {
    BeanUtils.copyProperties(offlineItem, updateOfflineItemPriceRequest);
    minimumPriceSystemParameter.setValue(null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.MINIMUM_PRICE)).thenReturn(minimumPriceSystemParameter);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> offlineItemServiceImpl.updateOfflineItemPriceByItemSku(mandatoryRequestParam, MERCHANT_CODE,
        updateOfflineItemPriceRequest));
  }

  @Test
  public void findByOfflineItemIdsAndMarkForDeleteFalseTest() {
    Mockito.when(this.offlineItemRepository
        .findByStoreIdAndOfflineItemIdIn(STORE_ID, ITEM_SKUS))
        .thenReturn(offlineItemList);
    List<OfflineItem> offlineItemList =
        this.offlineItemServiceImpl.findByOfflineItemIds(STORE_ID, ITEM_SKUS);
    Mockito.verify(this.offlineItemRepository)
        .findByStoreIdAndOfflineItemIdIn(STORE_ID, ITEM_SKUS);
    assertEquals(OFFLINE_ITEM_ID, offlineItemList.get(0).getOfflineItemId());
    assertEquals(ITEM_SKU, offlineItemList.get(0).getItemSku());
    assertEquals(MERCHANT_CODE, offlineItemList.get(0).getMerchantCode());
  }

  @Test
  public void findItemPickupPointByMerchantCodeAndItemSkuTest() {
    this.offlineItemServiceImpl.findItemPickupPointByMerchantCodeAndItemSku(STORE_ID,
      MERCHANT_CODE, ITEM_SKU);
    Mockito.verify(this.itemPickupPointService).findByStoreIdAndMerchantCodeAndItemSku(STORE_ID,
      MERCHANT_CODE, ITEM_SKU);
  }

  @Test
  public void findItemPickupPointByMerchantCodeAndItemSku_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.offlineItemServiceImpl.findItemPickupPointByMerchantCodeAndItemSku(StringUtils.EMPTY,
      MERCHANT_CODE, ITEM_SKU));
  }

  @Test
  public void findItemPickupPointByMerchantCodeAndItemSku_emptyMerchantCodeTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.offlineItemServiceImpl.findItemPickupPointByMerchantCodeAndItemSku(STORE_ID,
      StringUtils.EMPTY, ITEM_SKU));
  }

  @Test
  public void findItemPickupPointByMerchantCodeAndItemSku_emptyItemSkuTest() {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.offlineItemServiceImpl.findItemPickupPointByMerchantCodeAndItemSku(STORE_ID,
      MERCHANT_CODE, StringUtils.EMPTY));
  }

  @Test
  public void findItemPickupPointByOfflineItemIdTest() {
    this.offlineItemServiceImpl.findItemPickupPointByOfflineItemId(STORE_ID,
      OFFLINE_ITEM_ID);
    Mockito.verify(this.itemPickupPointService).findByItemPickupPointPriceVoStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID,
      OFFLINE_ITEM_ID);
  }

  @Test
  public void findItemPickupPointByOfflineItemId_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.offlineItemServiceImpl.findItemPickupPointByOfflineItemId(StringUtils.EMPTY,
      MERCHANT_CODE));
  }

  @Test
  public void findItemPickupPointByOfflineItemId_emptyOfflineItemIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.offlineItemServiceImpl.findItemPickupPointByOfflineItemId(STORE_ID,
      StringUtils.EMPTY));
  }

  @Test
  public void upsertOfflineItemTestItempickupPointNewDataFreeSampleCncTest() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    item.setFreeSample(true);
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(
            this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(null);

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointNewDataFreeSampleDiscoverableTest() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    upsertOfflineItemRequest.setDiscoverable(true);
    upsertOfflineItemRequest.setCncActive(false);
    item.setFreeSample(true);
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(
            this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(null);

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void upsertOfflineItemTestItempickupPointNewDataFreeSampleBuyableTest() throws Exception {
    UpsertOfflineItemPriceResponseVO expectedResult = new UpsertOfflineItemPriceResponseVO();
    expectedResult.setPickupPointCode(PICKUP_POINT);
    expectedResult.setListPrice(LIST_PRICE);
    expectedResult.setOfferPrice(OFFER_PRICE);
    expectedResult.setItemSku(ITEM_SKU);
    expectedResult.setOfflineItemId(OFFLINE_ITEM_ID);
    expectedResult.setSuccess(Boolean.TRUE);
    BeanUtils.copyProperties(offlineItem, upsertOfflineItemRequest);
    upsertOfflineItemRequest.setOfferPrice(1000.0);
    upsertOfflineItemRequest.setListPrice(1000.0);
    upsertOfflineItemRequest.setBuyable(true);
    upsertOfflineItemRequest.setCncActive(false);
    item.setFreeSample(true);
    Mockito.when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(this.item);
    Mockito.when(
        itemService.validatePriceForDeliveryTrueItemPickupPoint(Mockito.anyString(), Mockito.any(ItemPickupPoint.class),
            Mockito.any(UpdateOfflineItemPriceRequest.class))).thenReturn(true);
    Mockito.when(
            this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE))
        .thenReturn(this.minimumPriceSystemParameter);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(null);

    List<UpsertOfflineItemPriceResponseVO> expectedResults = Arrays.asList(expectedResult);

    List<UpsertOfflineItemRequest> upsertOfflineItemRequestList = new ArrayList<>();
    upsertOfflineItemRequestList.add(upsertOfflineItemRequest);
    List<UpsertOfflineItemPriceResponseVO> responses =
        this.offlineItemServiceImpl.upsertOfflineItem(this.mandatoryRequestParam, USERNAME, MERCHANT_CODE,
            upsertOfflineItemRequestList);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MINIMUM_PRICE);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void updatePickupPointCodesAtL3Test() throws Exception {
    Map<String, Set<String>> productSkuAndPickupPointCodeMap = new HashMap<>();
    productSkuAndPickupPointCodeMap.put(PRODUCT_SKU_1, new HashSet<>(Arrays.asList(PICKUP_POINT_2)));
    Map<String, Product> productMap = new HashMap<>();
    product1.setPickupPointCodes(new HashSet<>(Arrays.asList(PICKUP_POINT)));
    productMap.put(PRODUCT_SKU_1, product1);
    offlineItemServiceImpl.updatePickupPointCodesAtL3(productSkuAndPickupPointCodeMap, productMap);
    verify(productService).saveProductWithoutUpdatingSolr(any(Product.class), anyList(), eq(StringUtils.EMPTY));
  }

  @Test
  public void updatePickupPointCodesAtL3SamePPCodeTest() throws Exception {
    Map<String, Set<String>> productSkuAndPickupPointCodeMap = new HashMap<>();
    productSkuAndPickupPointCodeMap.put(PRODUCT_SKU_1, new HashSet<>(Arrays.asList(PICKUP_POINT_2)));
    Map<String, Product> productMap = new HashMap<>();
    product1.setPickupPointCodes(new HashSet<>(Arrays.asList(PICKUP_POINT_2)));
    productMap.put(PRODUCT_SKU_1, product1);
    offlineItemServiceImpl.updatePickupPointCodesAtL3(productSkuAndPickupPointCodeMap, productMap);
  }

  @Test
  public void updatePickupPointCodesAtL3NoProductTest() throws Exception {
    Map<String, Set<String>> productSkuAndPickupPointCodeMap = new HashMap<>();
    productSkuAndPickupPointCodeMap.put(PRODUCT_SKU_1, new HashSet<>(Arrays.asList(PICKUP_POINT_2)));
    offlineItemServiceImpl.updatePickupPointCodesAtL3(productSkuAndPickupPointCodeMap, new HashMap<>());
  }

}
