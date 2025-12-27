package com.gdn.x.product.service.impl;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.BlimartSubscriptionChangeRequest;
import com.gdn.x.product.domain.event.model.PricingPwpPromoEvent;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.rest.web.model.request.AutoCreatePickupPointRequest;
import com.gdn.x.product.rest.web.model.request.CogsUpdateRequest;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointResponse;
import com.gdn.x.product.rest.web.model.response.CogsResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.MinMaxItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.exceptions.ValidationException;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.MDC;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.ArgumentMatchers;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.ItemInfo;
import com.gdn.x.product.dao.api.ItemPickupPointRepository;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.AdjustmentType;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.DeleteOfflineItemVO;
import com.gdn.x.product.model.vo.FieldUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointPriceVo;
import com.gdn.x.product.model.vo.ItemPickupPointRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointSummaryRequestVo;
import com.gdn.x.product.rest.web.model.request.DeleteOfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponse;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointHelperService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PickupPointService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ReindexService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.google.common.collect.ImmutableSet;

public class ItemPickupPointServiceTest {

  private static final String STORE_ID = "storeId";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "ppCode";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final int PAGE = 0;
  private static final int PAGE_SIZE = 10;
  private static final String PAGE_SIZE_IN_STRING = "10";
  private static final String PICKUP_POINT = "pickup-point";
  private static final String PICKUP_POINT_1 = "pickup-point-1";
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final String EXTERNAL_PICKUP_POINT = "external-pickup-point";
  private static final String REQUEST_ID = "requestId";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String USERNAME = "username";
  private static final String ITEM_SKU_2 = "item-sku-2";
  private static final Long COUNT = 1L;
  private static final String PRODUCT_CODE = "PRODUCT-012345";
  private static final String OFFLINE_ITEM_ID = "offline-item-id";
  private static final String X_PRODUCT = "x-product";

  private ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
  private ItemPickupPoint itemPickupPoint2;
  private ItemPickupPoint itemPickupPoint3;

  private SystemParameter enableDeferredSolrReindexSystemParameter;
  private List<DeleteOfflineItemRequest> deleteOfflineItemRequestList;
  private DeleteOfflineItemRequest deleteOfflineItemRequest;
  private ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest;
  private List<DeleteOfflineItemVO> deleteOfflineItemVOs;
  private Item item;
  private List<ItemPickupPoint> itemPickupPointList;
  private Set<ItemViewConfig> itemViewConfigs;
  private Set<ItemViewConfig> itemViewConfigs1;
  private Set<ItemViewConfig> itemViewConfigsCnc;

  private Set<Price> prices;
  private final SystemParameter systemParameter = new SystemParameter();
  private final Pageable pageable = PageRequest.of(0, 10);
  private final Date START_DATE = new Date();
  private Date END_DATE = new Date();
  private AutoCreatePickupPointRequest autoCreatePickupPointRequest;
  private final AutoCreatePickupPointResponse autoCreatePickupPointResponse = new AutoCreatePickupPointResponse();
  private BlimartSubscriptionChangeRequest blimartSubscriptionChangeRequest;

  @InjectMocks
  private ItemPickupPointServiceImpl itemPickupPointService;

  @Mock
  private ItemPickupPointRepository itemPickupPointRepository;

  @Mock
  private PickupPointService pickupPointService;

  @Mock
  private CacheEvictItemService cacheEvictItemService;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private ProductService productService;

  @Mock
  private ChannelService channelService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemService itemService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private ItemPickupPointHelperService pickupPointHelperService;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ReindexService reindexService;

  @Mock
  private ProductCacheableService productCacheableService;

  @Captor
  private ArgumentCaptor<List<OfflineItem>> listArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<Item>> itemListArgumentCaptor;
  @Mock
  private SaveAndPublishServiceImpl saveAndPublishServiceImpl;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Captor
  private ArgumentCaptor<ItemPickupPoint> itemPickupPointArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<FieldUpdateRequestVo>> fieldUpdateRequestListCaptor;

  @Captor
  private ArgumentCaptor<List<ItemPickupPointRequestVo>> itemPickupPointRequestListCaptor;

  private List<String> itemSkuList;
  private ItemRequestV2 itemRequestV2;
  private Page<ItemPickupPoint> itemPickupPointPage;
  private BusinessPartner businessPartnerData;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    ReflectionTestUtils.setField(itemPickupPointService, "itemPickupPointFetchSize", 1);
    ReflectionTestUtils.setField(itemPickupPointService, "itemSkuAndPickupPointInTimeRangeFetchSize", 50);
    ReflectionTestUtils.setField(itemPickupPointService, "validateOnOriginalSellingPriceClientId", "x-bulk");
    ReflectionTestUtils.setField(itemPickupPointService, "validateOnOriginalSellingPrice", false);
    ReflectionTestUtils.setField(itemPickupPointService, "dormantFlowSkipDeletedItems", false);

    itemPickupPoint.setItemSku(ITEM_SKU);
    item = new Item();
    item.setMerchantCode(MERCHANT_CODE);
    item.setItemSku(ITEM_SKU);
    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPickupPointCode(PICKUP_POINT);
    itemViewConfigs = new HashSet<>(Arrays
        .asList(
            new ItemViewConfig(true, false, "DEFAULT", new ItemDiscoverableSchedule(), new ItemBuyableSchedule()),
            new ItemViewConfig(true, false, "B2B", new ItemDiscoverableSchedule(), new ItemBuyableSchedule())
        ));
    itemViewConfigs1 = new HashSet<>(Arrays.asList(
        new ItemViewConfig(true, true, "DEFAULT", new ItemDiscoverableSchedule(), new ItemBuyableSchedule()),
        new ItemViewConfig(true, true, "B2B", new ItemDiscoverableSchedule(), new ItemBuyableSchedule())));

    itemViewConfigsCnc = new HashSet<>(Arrays.asList(
        new ItemViewConfig(true, true, "DEFAULT", new ItemDiscoverableSchedule(), new ItemBuyableSchedule()),
        new ItemViewConfig(true, true, "CNC", new ItemDiscoverableSchedule(), new ItemBuyableSchedule())));

    prices = new HashSet<>(List.of(new Price("$", 10, 10, "", "", new Date())));
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    itemPickupPoint.setPrice(prices);
    itemPickupPoint.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT);
    itemPickupPoint.setMerchantSku(MERCHANT_SKU);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setStoreId(STORE_ID);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);

    deleteOfflineItemVOs = Arrays.asList(
        DeleteOfflineItemVO.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT).build(),
        DeleteOfflineItemVO.builder().itemSku(ITEM_SKU_2).pickupPointCode(PICKUP_POINT).build()
    );

    enableDeferredSolrReindexSystemParameter = new SystemParameter(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX, "true", "");

    itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT);
    itemPickupPoint2.setItemViewConfig(itemViewConfigs);
    itemPickupPoint2.setPrice(prices);
    itemPickupPoint2.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT);
    itemPickupPoint2.setMerchantSku(MERCHANT_SKU);
    itemPickupPoint2.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint2.setStoreId(STORE_ID);
    itemPickupPoint2.setItemSku(ITEM_SKU_2);

    itemPickupPoint3 = new ItemPickupPoint();
    itemPickupPoint3.setPickupPointCode(PICKUP_POINT_1);
    itemPickupPoint3.setItemViewConfig(itemViewConfigs1);
    itemPickupPoint3.setPrice(prices);
    itemPickupPoint3.setExternalPickupPointCode(EXTERNAL_PICKUP_POINT);
    itemPickupPoint3.setMerchantSku(MERCHANT_SKU);
    itemPickupPoint3.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint3.setStoreId(STORE_ID);
    itemPickupPoint3.setItemSku(ITEM_SKU);

    this.deleteOfflineItemRequest = new DeleteOfflineItemRequest();
    this.deleteOfflineItemRequest.setItemSku(ITEM_SKU_2);
    this.deleteOfflineItemRequest.setMerchantSku(MERCHANT_SKU);
    this.deleteOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteOfflineItemRequestList = new ArrayList<>();
    deleteOfflineItemRequestList.add(deleteOfflineItemRequest);

    doNothing().when(cacheEvictHelperService)
        .evictItemPickupPointData(anyString(), any(ItemPickupPoint.class), anyString());
    Mockito.doNothing().when(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,USERNAME);
    itemSkuList = List.of(ITEM_SKU);
    itemRequestV2 = new ItemRequestV2();
    itemRequestV2.setItemSkuList(List.of(ITEM_SKU));
    itemPickupPointViewConfigBaseRequest = new ItemPickupPointViewConfigBaseRequest(false, false, false);
    itemPickupPointPage =  new PageImpl<>(itemPickupPointList, pageable, 0);
    systemParameter.setValue(PAGE_SIZE_IN_STRING);
    Calendar calendar = Calendar.getInstance();
    calendar.setTime(END_DATE);
    calendar.add(Calendar.DATE, 1);
    END_DATE = calendar.getTime();
    List<String> sellerChannel = new ArrayList<>();
    sellerChannel.add(Constants.B2B_SELLER_CHANNEL);
    sellerChannel.add(Constants.B2C_SELLER_CHANNEL);
    businessPartnerData = new BusinessPartner();
    businessPartnerData.setSalesChannel(sellerChannel);
    autoCreatePickupPointRequest = new AutoCreatePickupPointRequest();
    autoCreatePickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    autoCreatePickupPointRequest.setMerchantCode(MERCHANT_CODE);
    autoCreatePickupPointRequest.setWebItemSku(ITEM_SKU);
    autoCreatePickupPointRequest.setWebProductSku(PRODUCT_SKU);
    Mockito.when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT);
    Mockito.when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    blimartSubscriptionChangeRequest = new BlimartSubscriptionChangeRequest();
    blimartSubscriptionChangeRequest.setStoreId(STORE_ID);
    blimartSubscriptionChangeRequest.setItemSku(ITEM_SKU);
    blimartSubscriptionChangeRequest.setPickupPointCode(PICKUP_POINT_CODE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(itemPickupPointRepository);
    Mockito.verifyNoMoreInteractions(saveAndPublishService);
    Mockito.verifyNoMoreInteractions(reindexService);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(businessPartnerService);
    Mockito.verifyNoMoreInteractions(inventoryOutbound);
  }

  @Test
  public void updateItemViewConfigByItemSkuCncActivatedMppTrueTest() {
    Mockito.when(itemPickupPointRepository
            .findByStoreIdAndItemSku(anyString(), anyString()))
        .thenReturn(itemPickupPointList);
    PickupPoint pickupPoint = new PickupPoint();
    pickupPoint.setCncActivated(true);
    pickupPoint.setPickupPointCode(PICKUP_POINT);
    BusinessPartner businessPartner = new BusinessPartner();
    businessPartner.setCncActivated(true);
    Mockito.when(
            businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    Mockito.when(pickupPointService
            .findPickupPointListByPickupPointCodeInAndCncActivatedFalse(anyString(), anyList()))
        .thenReturn(List.of(pickupPoint));
    Mockito.doNothing().when(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    Mockito.doNothing().when(saveAndPublishService).publishListOfOfflineItems(anyList(), Mockito.any());
    itemPickupPointService.updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME,false, item, itemPickupPointList, false);
  }

  @Test
  public void updateItemViewConfigByItemSkuArchiveTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "schedulesAddEditEnabled", true);
    BusinessPartner businessPartner = new BusinessPartner();
    businessPartner.setCncActivated(true);
    Mockito.when(itemPickupPointRepository
            .findByStoreIdAndItemSku(anyString(), anyString()))
        .thenReturn(itemPickupPointList);
    Mockito.when(
            businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    Mockito.doNothing().when(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PAGE_SIZE_TO_UPDATE_L5_DURING_ARCHIVAL)).thenReturn(systemParameter);
    itemPickupPointService.updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME,true, item, itemPickupPointList, false);
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
  }

  @Test
  public void updateItemViewConfigByItemSkuArchiveIsRejectedTrueTest() {
    BusinessPartner businessPartner = new BusinessPartner();
    businessPartner.setCncActivated(true);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSku(anyString(), anyString()))
        .thenReturn(itemPickupPointList);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    Mockito.doNothing().when(itemPickupPointRepository)
        .updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME),
            anyList(), anyList());
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PAGE_SIZE_TO_UPDATE_L5_DURING_ARCHIVAL)).thenReturn(systemParameter);
    itemPickupPointService.updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME, true, item, itemPickupPointList, true);
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME),
        itemPickupPointRequestListCaptor.capture(), fieldUpdateRequestListCaptor.capture());
    assertEquals(6, fieldUpdateRequestListCaptor.getValue().size());
  }

  @Test
  public void updateItemViewConfigByItemSkuArchiveOnlyCncTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    BusinessPartner businessPartner = new BusinessPartner();
    businessPartner.setCncActivated(true);
    itemPickupPoint.setItemViewConfig(new HashSet<>());
    Mockito.when(itemPickupPointRepository
            .findByStoreIdAndItemSku(anyString(), anyString()))
        .thenReturn(itemPickupPointList);
    Mockito.when(
            businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    Mockito.doNothing().when(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PAGE_SIZE_TO_UPDATE_L5_DURING_ARCHIVAL)).thenReturn(systemParameter);
    itemPickupPointService.updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME,true, item, itemPickupPointList, false);
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
  }

  @Test
  public void updateItemViewConfigByItemSkuArchiveOnlyCncIsRejectedTrueTest() {
    BusinessPartner businessPartner = new BusinessPartner();
    businessPartner.setCncActivated(true);
    itemPickupPoint.setItemViewConfig(new HashSet<>());
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSku(anyString(), anyString()))
        .thenReturn(itemPickupPointList);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    Mockito.doNothing().when(itemPickupPointRepository)
        .updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME),
            anyList(), anyList());
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PAGE_SIZE_TO_UPDATE_L5_DURING_ARCHIVAL)).thenReturn(systemParameter);
    itemPickupPointService.updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME, true, item, itemPickupPointList, true);
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME),
        anyList(), anyList());
  }

  @Test
  public void findByItemSkuAndDelivery() {
    Mockito.when(itemPickupPointRepository
            .findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(itemPickupPoint);
    this.itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findItemPickupPointByStoreIdAndItemSkuInAndDeliveryTest() {
    this.itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findByStoreIdAndItemSkuAndMarkForDeleteFalseTest() {
    this.itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, null);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, null);
  }

  @Test
  public void findByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteTest() {
    itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false, null);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false, null);
  }

  @Test
  public void findByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteTestStoreIdEmpty() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, ITEM_SKU, true, false, null));

  }

  @Test
  public void findByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteTestItemSkuEmpty() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, null, true, false, null));
  }

  @Test
  public void findByStoreIdAndItemSkuAndCncBuyableAndMarkForDeleteTest() {
    itemPickupPointService.findByStoreIdAndItemSkuAndCncBuyableTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(STORE_ID, ITEM_SKU, channelService.getCncChannel());
  }

  @Test
  public void findByStoreIdAndPickupPointCodeTest() {
    this.itemPickupPointService.findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndPickupPointCode(STORE_ID, PICKUP_POINT);
  }

  @Test
  public void findByStoreIdAndPickupPointCodeStoreIdEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndPickupPointCode(StringUtils.EMPTY, PICKUP_POINT));
  }

  @Test
  public void findByStoreIdAndPickupPointCodePickupPointCodeEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndPickupPointCode(STORE_ID, StringUtils.EMPTY));
  }

  @Test
  public void findByStoreIdAndPickupPointCodeAndMarkForDeleteFalseTest() {
    this.itemPickupPointService.findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT);
  }

  @Test
  public void findByStoreIdAndPickupPointCodeAndMarkForDeleteFalseStoreIdEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(StringUtils.EMPTY, PICKUP_POINT));
  }

  @Test
  public void findByStoreIdAndPickupPointCodeAndMarkForDeleteFalsePickupPointCodeEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, StringUtils.EMPTY));
  }

  @Test
  public void getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalseTest() {
    Mockito.when(
            itemPickupPointRepository.countByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT_CODE))
        .thenReturn(1L);
    Long result = this.itemPickupPointService.getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointRepository)
        .countByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT_CODE);
    assertEquals(COUNT, result);
  }

  @Test
  public void getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalseStoreIdBlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        this.itemPickupPointService.getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(StringUtils.EMPTY,
            PICKUP_POINT_CODE));
  }

  @Test
  public void getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalsePickupPointCodeBlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getCountByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        StringUtils.EMPTY));
  }

  @Test
  public void findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalseTest() {
    Mockito.when(
            itemPickupPointRepository.findFirstByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    ItemPickupPoint result =
        this.itemPickupPointService.findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
            PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointRepository)
        .findFirstByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PICKUP_POINT_CODE);
  }

  @Test
  public void findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalseStoreIdBlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        this.itemPickupPointService.findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(StringUtils.EMPTY,
            PICKUP_POINT_CODE));
  }

  @Test
  public void findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalsePickupPointCodeBlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        this.itemPickupPointService.findOneByStoreIdAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
            StringUtils.EMPTY));
  }

  @Test
  public void findByItemSkuInAndDeliveryTest() {
    Mockito.when(itemPickupPointRepository
            .findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(STORE_ID, List.of(ITEM_SKU), Boolean.TRUE))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    this.itemPickupPointService.findByItemSkuInAndDelivery(STORE_ID, List.of(ITEM_SKU), Boolean.TRUE);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(STORE_ID, List.of(ITEM_SKU), Boolean.TRUE);
  }

  @Test
  public void findByItemSkusAndDeliveryTest() {
    Mockito.when(itemPickupPointRepository
            .findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(STORE_ID, List.of(ITEM_SKU), Boolean.TRUE))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    this.itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, List.of(ITEM_SKU), Boolean.TRUE);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(STORE_ID, List.of(ITEM_SKU), Boolean.TRUE);
  }


  @Test
  public void findByItemSkusAndDeliveryEmptyResultTest() {
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(STORE_ID,
        List.of(ITEM_SKU), Boolean.TRUE)).thenReturn(new ArrayList<>());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByItemSkusAndDelivery(STORE_ID,
          List.of(ITEM_SKU), Boolean.TRUE));
    } finally {
      Mockito.verify(itemPickupPointRepository)
          .findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(STORE_ID, List.of(ITEM_SKU), Boolean.TRUE);
    }
  }

  @Test
  public void findByItemSkusAndDeliveryEmptyItemSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByItemSkusAndDelivery(STORE_ID, new ArrayList<>(), Boolean.TRUE));
  }

  @Test
  public void findByItemSkusAndDeliveryEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByItemSkusAndDelivery(StringUtils.EMPTY, new ArrayList<>(), Boolean.TRUE));
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndCncActivatedAndMarkForDelete() {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMarkForDelete(false);
    itemPickupPointList.add(itemPickupPoint);
    Mockito.when(this.itemPickupPointRepository
            .findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(STORE_ID, MERCHANT_CODE, Boolean.TRUE, Boolean.FALSE))
        .thenReturn(itemPickupPointList);
    this.itemPickupPointService
        .findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(STORE_ID, MERCHANT_CODE, Boolean.TRUE,
            Boolean.FALSE);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndMerchantCodeAndCncActiveAndMarkForDelete(STORE_ID, MERCHANT_CODE, Boolean.TRUE, Boolean.FALSE);
  }

  @Test
  public void findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDeleteTest() {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    Mockito.when(this.itemPickupPointRepository
            .findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT, true, false))
        .thenReturn(itemPickupPointList);
    this.itemPickupPointService
        .findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT, true, false);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndPickupPointCodeAndCncActiveAndMarkForDelete(STORE_ID, PICKUP_POINT, true, false);
  }

  @Test
  public void findByStoreIdAndPickupPointCodeAndCncBuyableConfigAndMarkForDelete_Test() {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    Mockito.when(
        this.itemPickupPointRepository.findByStoreIdAndPickupPointCodeAndConfigFlagValueAndMarkForDeleteAndChannel(
            STORE_ID, PICKUP_POINT, true, false, "CNC")).thenReturn(itemPickupPointList);
    this.itemPickupPointService.findByStoreIdAndPickupPointCodeAndConfigFlagValuesAndMarkForDeleteAndChannel(
        STORE_ID, PICKUP_POINT, true, false, "CNC");
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndPickupPointCodeAndConfigFlagValueAndMarkForDeleteAndChannel(STORE_ID,
            PICKUP_POINT, true, false, "CNC");
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDelete_Test() {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    Mockito.when(
        this.itemPickupPointRepository.findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDeleteAndChannel(
            STORE_ID, PICKUP_POINT, true, false, "CNC")).thenReturn(itemPickupPointList);
    this.itemPickupPointService.findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDelete(
        STORE_ID, PICKUP_POINT, true, false, "CNC");
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndMerchantCodeAndConfigFlagValueAndMarkForDeleteAndChannel(STORE_ID,
            PICKUP_POINT, true, false, "CNC");
  }

  @Test
  public void updateItemViewConfigForExistingChannelBlankChannelTest() {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(new HashSet<>(List.of(itemViewConfig)));
    itemPickupPointService
        .updateItemViewConfigForExistingChannel(itemPickupPoint, new HashSet<>(List.of(new ItemViewConfig())));
    verify(channelService).getDefaultChannel();
  }

  @Test
  public void updateItemViewConfigForExistingChannelTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService
        .updateItemViewConfigForExistingChannel(null, new HashSet<>(List.of(new ItemViewConfig()))));
  }

  @Test
  public void findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete() {
    itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(STORE_ID, ITEM_SKU, true, false);
  }

  @Test
  public void findByStoreIdAndItemSkuAndMarkForDeleteFalse() {
    this.itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(this.itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
  }

  @Test
  public void findByItemSkuAndPickupPointCodeEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findByItemSkuAndPickupPointCode(StringUtils.EMPTY, ITEM_SKU, PICKUP_POINT));
  }

  @Test
  public void findByItemSkuAndPickupPointCodeEmptyItemSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, StringUtils.EMPTY, PICKUP_POINT));
  }

  @Test
  public void findByItemSkuAndPickupPointCodeEmptyPickupPointTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, StringUtils.EMPTY));
  }

  @Test
  public void saveItemPickupPointTest() {
    itemPickupPointService.saveItemPickupPoint(itemPickupPoint);
    Mockito.verify(this.itemPickupPointRepository).save(itemPickupPoint);
  }

  @Test
  public void findByItemSkuAndPickupPointCodeAndMarkForDeleteFalseTest() {
    Mockito.when(
        this.itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(
            STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPoint =
        itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID,
            ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
            PICKUP_POINT_CODE, false);
    assertEquals(ITEM_SKU, itemPickupPoint.getItemSku());
  }

  @Test
  public void saveItemPickupPointCollectionTest() {
    this.itemPickupPointService.saveItemPickupPointCollection(
        Collections.singletonList(itemPickupPoint));
    Mockito.verify(this.itemPickupPointRepository).saveAll(Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void saveItemPickupPointCollection_emptyListTest() {
    this.itemPickupPointService.saveItemPickupPointCollection(
        Collections.emptyList());
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndMarkForDeleteFalseTest() {
    Mockito.when(
        this.itemPickupPointRepository.findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID,
            MERCHANT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    List<ItemPickupPoint> itemPickupPointList =
        this.itemPickupPointService.findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID,
            MERCHANT_CODE);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndMerchantCodeAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE);
    assertEquals(ITEM_SKU, itemPickupPointList.get(0).getItemSku());
  }

  @Test
  public void updateItemPickupPointPriceByOfflineItemListPriceChangedTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, 20.0, 30.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(10.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(10.0);


    when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, MERCHANT_CODE)).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(itemPickupPointRepository.saveAll(anyList())).thenReturn(itemPickupPointList);

    List<ItemPickupPoint> itemPickupPointList = itemPickupPointService.updateItemPickupPointPriceByOfflineItem(STORE_ID, MERCHANT_CODE, updateOfflineItemPriceRequest);

    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, MERCHANT_CODE);
    verify(itemPickupPointRepository).updatePriceByOfflineItemIds(anyList(),
        Mockito.anyDouble(), Mockito.anyDouble());
    verify(cacheEvictHelperService).evictItemPickupPointCache(any(), any(),
        any());

    assertEquals(20.0, itemPickupPointList.get(0).getPrice().iterator().next().getListPrice(), 0);
    assertEquals(30.0, itemPickupPointList.get(0).getPrice().iterator().next().getOfferPrice(), 0);
  }

  @Test
  public void updateItemPickupPointPriceByOfflineItemListPriceChangedThrowExceptionTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, 20.0, 30.0);
    itemPickupPoint.getPrice().iterator().next().setListPrice(10.0);
    itemPickupPoint.getPrice().iterator().next().setOfferPrice(10.0);
    itemPickupPoint.setDelivery(true);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, MERCHANT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    doThrow(new ApplicationRuntimeException()).when(this.itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID,itemPickupPoint,updateOfflineItemPriceRequest);
    try {
      List<ItemPickupPoint> itemPickupPointList = itemPickupPointService
          .updateItemPickupPointPriceByOfflineItem(STORE_ID, MERCHANT_CODE, updateOfflineItemPriceRequest);
    }finally {
      verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
          ITEM_SKU, MERCHANT_CODE);
      verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID,itemPickupPoint,updateOfflineItemPriceRequest);
    }
  }

  @Test
  public void updateItemPickupPointPriceByOfflineItemListPriceChangedDeliveryTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, 20.0, 10.0);
    itemPickupPoint.setDelivery(true);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, MERCHANT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(itemPickupPointRepository.saveAll(anyList())).thenReturn(itemPickupPointList);
    when(itemService.validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        updateOfflineItemPriceRequest)).thenReturn(true);
    itemPickupPointService.updateItemPickupPointPriceByOfflineItem(STORE_ID, MERCHANT_CODE,
        updateOfflineItemPriceRequest);

    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU, MERCHANT_CODE);
    verify(itemPickupPointRepository).updatePriceByOfflineItemIds(anyList(),
        Mockito.anyDouble(), Mockito.anyDouble());
    verify(cacheEvictHelperService).evictItemPickupPointCache(any(), any(),
        any());
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        updateOfflineItemPriceRequest);
  }

  @Test
  public void updateItemPickupPointPriceByOfflineItemListPriceChangedMppTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, 20.0, 10.0);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, MERCHANT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(itemPickupPointRepository.saveAll(anyList())).thenReturn(itemPickupPointList);
    when(itemService.validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        updateOfflineItemPriceRequest)).thenReturn(true);
    itemPickupPointService.updateItemPickupPointPriceByOfflineItem(STORE_ID, MERCHANT_CODE,
        updateOfflineItemPriceRequest);

    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU, MERCHANT_CODE);
    verify(itemPickupPointRepository).updatePriceByOfflineItemIds(anyList(),
        Mockito.anyDouble(), Mockito.anyDouble());
    verify(cacheEvictHelperService).evictItemPickupPointCache(any(), any(),
        any());
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        updateOfflineItemPriceRequest);
  }

  @Test
  public void updateItemPickupPointPriceByOfflineItemListPriceChangedDeliveryExceptionTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, 20.0, 10.0);
    itemPickupPoint.setDelivery(true);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, MERCHANT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(itemPickupPointRepository.saveAll(anyList())).thenReturn(itemPickupPointList);
    when(itemService.validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        updateOfflineItemPriceRequest)).thenReturn(false);
    itemPickupPointService.updateItemPickupPointPriceByOfflineItem(STORE_ID, MERCHANT_CODE,
        updateOfflineItemPriceRequest);

    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU, MERCHANT_CODE);
    verify(itemService).validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        updateOfflineItemPriceRequest);
    verify(itemPickupPointRepository).updatePriceByOfflineItemIds(anyList(),
        Mockito.anyDouble(), Mockito.anyDouble());
  }

  @Test
  public void updateItemPickupPointPriceByOfflineItemOffferPriceChangedTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, 10.0, 20.0);

    when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, MERCHANT_CODE)).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(itemPickupPointRepository.saveAll(anyList())).thenReturn(itemPickupPointList);

    itemPickupPointService.updateItemPickupPointPriceByOfflineItem(STORE_ID, MERCHANT_CODE, updateOfflineItemPriceRequest);

    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, MERCHANT_CODE);
    verify(itemPickupPointRepository).updatePriceByOfflineItemIds(anyList(),
        Mockito.anyDouble(), Mockito.anyDouble());
    verify(cacheEvictHelperService).evictItemPickupPointCache(any(), any(),
        any());
  }


  @Test
  public void updateItemPickupPointPriceByOfflineItemNoUpdateTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, 10.0, 10.0);

    when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, MERCHANT_CODE)).thenReturn(
        Collections.singletonList(itemPickupPoint));

    itemPickupPointService.updateItemPickupPointPriceByOfflineItem(STORE_ID, MERCHANT_CODE, updateOfflineItemPriceRequest);

    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, MERCHANT_CODE);
  }

  @Test
  public void updateItemPickupPointPriceByOfflineItemStoreIdBlankTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, 10.0, 10.0);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.updateItemPickupPointPriceByOfflineItem(StringUtils.EMPTY, MERCHANT_CODE, updateOfflineItemPriceRequest));

  }

  @Test
  public void updateItemPickupPointPriceByOfflineItemMerchantCodeBlankTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, 10.0, 10.0);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.updateItemPickupPointPriceByOfflineItem(STORE_ID, StringUtils.EMPTY, updateOfflineItemPriceRequest));

  }

  @Test
  public void updateItemPickupPointPriceByOfflineItemSkuBlankTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(StringUtils.EMPTY, 10.0, 10.0);

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.updateItemPickupPointPriceByOfflineItem(STORE_ID, MERCHANT_CODE, updateOfflineItemPriceRequest));
  }

  @Test
  public void updateItemPickupPointPriceByOfflineNoItemPickupPointTest() {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest =
        new UpdateOfflineItemPriceRequest(ITEM_SKU, 10.0, 10.0);
    when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, MERCHANT_CODE)).thenReturn(
        new ArrayList<>());

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.updateItemPickupPointPriceByOfflineItem(STORE_ID, MERCHANT_CODE,
          updateOfflineItemPriceRequest));
    } finally {
      verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, MERCHANT_CODE);
    }
  }

  @Test
  public void updateDiscountPriceInItemPickupPointTable() {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.TRUE);
    itemPickupPointList.add(itemPickupPoint);
    this.itemPickupPointService.updateDiscountPriceInItemPickupPoint(itemPickupPoint);
    Mockito.verify(this.itemPickupPointRepository).updateDiscountPriceInItemPickupPoint(itemPickupPoint);
  }

  @Test
  public void updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    itemPickupPoint.setMarkForDelete(Boolean.TRUE);
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    this.itemPickupPointService.updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(itemPickupPoint);
    Mockito.verify(this.itemPickupPointRepository).updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(itemPickupPoint);
  }

  @Test
  public void updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCodeNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(null));
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndItemSkuTest() {
    Mockito.when(
        this.itemPickupPointRepository.findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(
            STORE_ID, ITEM_SKU, MERCHANT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    List<ItemPickupPointPriceVo> result =
        this.itemPickupPointService.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID, MERCHANT_CODE,
            ITEM_SKU);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndMerchantCodeAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            MERCHANT_CODE);
    Assertions.assertNotNull(result.get(0));
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndItemSku_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndMerchantCodeAndItemSku(StringUtils.EMPTY,
        MERCHANT_CODE, ITEM_SKU));
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndItemSku_emptyMerchantCodeTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID,
        StringUtils.EMPTY, ITEM_SKU));
  }

  @Test
  public void findByStoreIdAndMerchantCodeAndItemSku_emptyItemSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndMerchantCodeAndItemSku(STORE_ID,
        MERCHANT_CODE, StringUtils.EMPTY));
  }

  @Test
  public void findItemPickupPointPriceVoByItemSkusTest() {
    Mockito.when(
            this.itemPickupPointRepository.findByStoreIdAndItemSkuInAndCncActiveTrueAndMarkForDeleteFalse(
                STORE_ID, Collections.singleton(ITEM_SKU), pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(itemPickupPoint)));
    List<ItemPickupPointPriceVo> result =
        this.itemPickupPointService.findItemPickupPointPriceVoByItemSkus(STORE_ID,
            Collections.singleton(ITEM_SKU), pageable);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuInAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
            Collections.singleton(ITEM_SKU), pageable);
    Assertions.assertNotNull(result.get(0));
  }

  @Test
  public void deleteItemPickupPointByStoreIdAndProductSkusTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    Mockito.when(itemPickupPointRepository.deleteByStoreIdAndProductSkuIn(STORE_ID, productSkuSet))
        .thenReturn(new ArrayList<>());
    itemPickupPointService.deleteItemPickupPointByStoreIdAndProductSkus(STORE_ID, productSkuSet);
    Mockito.verify(itemPickupPointRepository).deleteByStoreIdAndProductSkuIn(STORE_ID, productSkuSet);
  }

  @Test
  public void deleteItemPickupPointByStoreIdAndProductSkusStoreIdEmptyTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.deleteItemPickupPointByStoreIdAndProductSkus(StringUtils.EMPTY, productSkuSet));
  }

  @Test
  public void deleteItemPickupPointByStoreIdAndProductSkusProductSkuSetEmptyTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.deleteItemPickupPointByStoreIdAndProductSkus(STORE_ID, productSkuSet));
  }

  @Test
  public void findItemPickupPointPriceVoByItemSkus_nullRepositoryResponseTest() {
    Mockito.when(
            this.itemPickupPointRepository.findByStoreIdAndItemSkuInAndCncActiveTrueAndMarkForDeleteFalse(
                STORE_ID, Collections.singleton(ITEM_SKU), pageable))
        .thenReturn(null);
    List<ItemPickupPointPriceVo> result =
        this.itemPickupPointService.findItemPickupPointPriceVoByItemSkus(STORE_ID,
            Collections.singleton(ITEM_SKU), pageable);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuInAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
            Collections.singleton(ITEM_SKU), pageable);
    assertTrue(CollectionUtils.isEmpty(result));
  }

  @Test
  public void findItemPickupPointPriceVoByItemSkus_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findItemPickupPointPriceVoByItemSkus(StringUtils.EMPTY,
        Collections.singleton(ITEM_SKU), pageable));
  }

  @Test
  public void findItemPickupPointPriceVoByItemSkus_emptyItemSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findItemPickupPointPriceVoByItemSkus(STORE_ID,
        Collections.emptySet(), pageable));
  }

  @Test
  public void findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalseTest() {
    this.itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(
        STORE_ID, Collections.singleton(ITEM_SKU), true);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuInAndCncActiveAndMarkForDeleteFalse(STORE_ID,
            Collections.singleton(ITEM_SKU), true);
  }

  @Test
  public void findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse_emptyItemSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(
        STORE_ID, Collections.emptySet(), true));
  }

  @Test
  public void findByItemSkuAndDeliveryTest() {
    this.itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU, Boolean.TRUE);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndDeliveryAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, Boolean.TRUE);
  }

  @Test
  public void getL5sCreatedInTimeRangeTest() throws Exception {
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    itemPickupPoints.add(new ItemPickupPoint());
    Mockito.when(itemPickupPointRepository.findByStoreIdAndCreatedDateBetweenAndMarkForDeleteFalseAndForceReviewFalse(STORE_ID, START_DATE, END_DATE,
        PageRequest.of(PAGE, PAGE_SIZE))).thenReturn(new PageImpl<>(itemPickupPoints));
    this.itemPickupPointService.getL5sCreatedInTimeRange(STORE_ID, START_DATE, END_DATE, PAGE, PAGE_SIZE, REQUEST_ID);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndCreatedDateBetweenAndMarkForDeleteFalseAndForceReviewFalse(STORE_ID, START_DATE, END_DATE,
        PageRequest.of(PAGE, PAGE_SIZE));
  }

  @Test
  public void getL5sCreatedInTimeRangeListEmptyTest() throws Exception {
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Mockito.when(itemPickupPointRepository.findByStoreIdAndCreatedDateBetweenAndMarkForDeleteFalseAndForceReviewFalse(STORE_ID, START_DATE, END_DATE,
        PageRequest.of(PAGE, PAGE_SIZE))).thenReturn(new PageImpl<>(itemPickupPoints));
    this.itemPickupPointService.getL5sCreatedInTimeRange(STORE_ID, START_DATE, END_DATE, PAGE, PAGE_SIZE, REQUEST_ID);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndCreatedDateBetweenAndMarkForDeleteFalseAndForceReviewFalse(STORE_ID, START_DATE, END_DATE,
        PageRequest.of(PAGE, PAGE_SIZE));
  }

  @Test
  public void getL5sCreatedInTimeRangePageSizeExceededTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getL5sCreatedInTimeRange(STORE_ID, START_DATE, END_DATE, PAGE, 100, REQUEST_ID));
  }

  @Test
  public void getL5sCreatedInTimeRangePageSizeStoreIdBlankTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getL5sCreatedInTimeRange(StringUtils.EMPTY, START_DATE, END_DATE, PAGE, PAGE_SIZE, REQUEST_ID));
  }

  @Test
  public void getL5sCreatedInTimeRangePageSizeSameDateTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getL5sCreatedInTimeRange(StringUtils.EMPTY, START_DATE, START_DATE, PAGE, PAGE_SIZE, REQUEST_ID));
  }

  @Test
  public void getL5sCreatedInTimeRangePageSizeStartDateNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getL5sCreatedInTimeRange(STORE_ID, null, END_DATE, PAGE, PAGE_SIZE, REQUEST_ID));
  }

  @Test
  public void getL5sCreatedInTimeRangePageSizeEndDateNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getL5sCreatedInTimeRange(STORE_ID, START_DATE, null, PAGE, PAGE_SIZE, REQUEST_ID));
  }

  @Test
  public void getL5sPriceUpdatedInTimeRangeTest() throws Exception {
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    itemPickupPoints.add(new ItemPickupPoint());
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndPriceUpdatedDateBetweenAndMarkForDeleteFalse(STORE_ID, START_DATE,
            END_DATE, PageRequest.of(PAGE, PAGE_SIZE))).thenReturn(new PageImpl<>(itemPickupPoints));
    this.itemPickupPointService.getL5sPriceUpdatedInTimeRange(STORE_ID, START_DATE, END_DATE, PAGE, PAGE_SIZE,
        REQUEST_ID);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndPriceUpdatedDateBetweenAndMarkForDeleteFalse(STORE_ID, START_DATE, END_DATE,
            PageRequest.of(PAGE, PAGE_SIZE));
  }

  @Test
  public void findByItemSkuAndPickupPointCodeTest() {
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT, false)).thenReturn(itemPickupPoint);
    this.itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT, false);
  }

  @Test
  public void findByItemSkuAndPickupPointCodeFromDbTest() {
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU,
        PICKUP_POINT, false)).thenReturn(itemPickupPoint);
    itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT, false);
  }

  @Test
  public void findByStoreIdAndProductSkuAndDeliveryTest() {
    this.itemPickupPointService
        .findByStoreIdAndProductSkuAndDelivery(STORE_ID, PRODUCT_SKU, Boolean.TRUE);
    Mockito.verify(this.itemPickupPointRepository)
        .findItemPickupPointByStoreIdAndProductSkuAndDeliveryAndMarkForDeleteFalse(STORE_ID,
            PRODUCT_SKU, Boolean.TRUE);
  }

  @Test
  public void findByStoreIdAndOfflineItemIdAndMarkForDeleteFalseTest() {
    Mockito.when(
        this.itemPickupPointRepository.findByStoreIdAndOfflineItemIdAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
            OFFLINE_ITEM_ID)).thenReturn(itemPickupPoint);
    ItemPickupPointPriceVo result =
        this.itemPickupPointService.findByItemPickupPointPriceVoStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID,
            OFFLINE_ITEM_ID);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndOfflineItemIdAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, OFFLINE_ITEM_ID);
    assertEquals(ITEM_SKU, result.getItemSku());
  }

  @Test
  public void findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse_nullEntityTest() {
    Mockito.when(
        this.itemPickupPointRepository.findByStoreIdAndOfflineItemIdAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
            OFFLINE_ITEM_ID)).thenReturn(null);
    ItemPickupPointPriceVo result =
        this.itemPickupPointService.findByItemPickupPointPriceVoStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID,
            OFFLINE_ITEM_ID);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndOfflineItemIdAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, OFFLINE_ITEM_ID);
    Assertions.assertNull(result);
  }

  @Test
  public void findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByItemPickupPointPriceVoStoreIdAndOfflineItemIdAndMarkForDeleteFalse(StringUtils.EMPTY,
        ITEM_SKU));
  }

  @Test
  public void findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse_emptyItemSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByItemPickupPointPriceVoStoreIdAndOfflineItemIdAndMarkForDeleteFalse(STORE_ID,
        StringUtils.EMPTY));
  }

  @Test
  public void updateMerchantPromoDiscountFlagTest() {
    Mockito.when(itemPickupPointRepository.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, false)).thenReturn(new ArrayList<>());
    boolean result = this.itemPickupPointService.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, false);
    Mockito.verify(this.itemPickupPointRepository).updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, false);
    assertFalse(result);
  }

  @Test
  public void updateFieldByItemSkusAndDeliveryTest() {
    Mockito.when(itemPickupPointRepository.updateFieldByItemSkusAndDelivery(STORE_ID, List.of(ITEM_SKU),
        ProductFieldNames.PROMO_BUNDLING, false, false)).thenReturn(Collections.singletonList(itemPickupPoint));
    List<ItemPickupPoint> itemPickupPoints =
        this.itemPickupPointService.updateFieldByItemSkusAndDelivery(STORE_ID, List.of(ITEM_SKU),
            ProductFieldNames.PROMO_BUNDLING, false, false);
    Mockito.verify(this.itemPickupPointRepository).updateFieldByItemSkusAndDelivery(STORE_ID, List.of(ITEM_SKU),
        ProductFieldNames.PROMO_BUNDLING, false, false);
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointData(anyString(), any(ItemPickupPoint.class), anyString());
    Assertions.assertNotNull(itemPickupPoints);
    assertEquals(1, itemPickupPoints.size());
  }

  @Test
  public void updateFieldByItemSkusAndPPCodeTest() {
    List<ItemPickupPoint> itemPickupPoints = this.itemPickupPointService.updateFieldByItemSkusAndPPCode(STORE_ID,
        List.of(new ItemInfo(ITEM_SKU, PICKUP_POINT_CODE, OFFLINE_ITEM_ID)), ProductFieldNames.PROMO_BUNDLING,
        true);
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    verify(itemPickupPointRepository).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        Collections.singletonList(new ItemPickupPointRequestVo(ITEM_SKU,PICKUP_POINT_CODE)), false);
    Assertions.assertNotNull(itemPickupPoints);
    assertEquals(0, itemPickupPoints.size());
  }

  @Test
  public void updateActivePromoBundling() {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPointResponse =
        this.itemPickupPointService.updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE, false);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU);
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(STORE_ID, itemPickupPoint, itemPickupPoint.getPickupPointCode());
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    assertFalse(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertTrue(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE));
  }

  @Test
  public void updateActivePromoBundlingByPPCode() {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPointResponse =
        this.itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, ITEM_SKU,
            Constants.WHOLESALE, false, PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(STORE_ID, itemPickupPoint, itemPickupPoint.getPickupPointCode());
    assertFalse(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertTrue(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE));
  }

  @Test
  public void updateActivePromoBundlingByPPCodeInitiallyNullIPP() {
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(null);
    this.itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, ITEM_SKU, Constants.WHOLESALE,
        false, PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE, false);
  }

  @Test
  public void updateActivePromoBundlingInitiallyNullPB() {
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPointResponse =
        this.itemPickupPointService.updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE,
            false);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU);
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(STORE_ID, itemPickupPoint, itemPickupPoint.getPickupPointCode());
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    assertFalse(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertTrue(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE));
  }

  @Test
  public void updateActivePromoBundlingInitiallyNullIPP() {
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(null);
    this.itemPickupPointService.updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE, false);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU);
  }

  @Test
  public void updateActivePromoBundlingWHPrice() {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPointResponse =
        this.itemPickupPointService.updateActivePromoBundling(STORE_ID, ITEM_SKU,
            Constants.WHOLESALE_PRICE, false);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU);
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(STORE_ID, itemPickupPoint, itemPickupPoint.getPickupPointCode());
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    assertTrue(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertFalse(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE));
  }

  @Test
  public void updateActivePromoBundlingEmptyPB() {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPointResponse =
        this.itemPickupPointService.updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE_PRICE, true);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU);
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(STORE_ID, itemPickupPoint, itemPickupPoint.getPickupPointCode());
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    assertFalse(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
  }

  @Test
  public void updateActivePromoBundlingInitiallyNullPBWHPrice() {
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPointResponse =
        this.itemPickupPointService.updateActivePromoBundling(STORE_ID,
            ITEM_SKU, Constants.WHOLESALE_PRICE, false);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU);
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(STORE_ID, itemPickupPoint, itemPickupPoint.getPickupPointCode());

    assertTrue(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertFalse(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE));
  }

  @Test
  public void updateActivePromoBundlingWHPriceAndComboTest() {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPointResponse =
        this.itemPickupPointService.updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.COMBO,
            false);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU);
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(STORE_ID, itemPickupPoint, itemPickupPoint.getPickupPointCode());
    assertFalse(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertTrue(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE));
    assertTrue(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.COMBO));
  }

  @Test
  public void updateActivePromoBundlingWithWHPriceAndComboViceVersaTest() {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    itemPickupPoint.getActivePromoBundlings().add(Constants.COMBO);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPointResponse =
        this.itemPickupPointService.updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE,
            false);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU);
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(STORE_ID, itemPickupPoint, itemPickupPoint.getPickupPointCode());
    assertFalse(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertTrue(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE));
    assertTrue(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.COMBO));
  }

  @Test
  public void updateActivePromoBundlingWithWHPriceAndComboClearTestTest() {
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());
    itemPickupPoint.getActivePromoBundlings().add(Constants.COMBO);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPointResponse =
        this.itemPickupPointService.updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE,
            true);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU);
    verify(itemPickupPointRepository).updateFieldsByItemSkuAndPickupPointCodes(eq(STORE_ID), eq(USERNAME), anyList(), anyList());
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(STORE_ID, itemPickupPoint, itemPickupPoint.getPickupPointCode());
    assertFalse(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertFalse(itemPickupPointResponse.getActivePromoBundlings().contains(Constants.WHOLESALE));
  }

  @Test
  public void findByStoreIdAndOfflineItemId() {
    this.itemPickupPointService.findByStoreIdAndOfflineItemId(STORE_ID, ITEM_SKU);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndOfflineItemIdAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findByStoreIdAndOfflineItemId_Exception() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndOfflineItemId(null, ITEM_SKU));
  }

  @Test
  public void findByStoreIdAndOfflineItemId_Exception1() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndOfflineItemId(STORE_ID, null));
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCode() {
    this.itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICKUP_POINT);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICKUP_POINT, false);
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCode_Exception() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(null, ITEM_SKU, PICKUP_POINT));
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCode_Exception1() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, null, PICKUP_POINT));
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCode_Exception2() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, null));
  }

  @Test
  public void findByItemSkuAndDeliveryUnCached() {
    this.itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findByItemSkuAndDeliveryUnCached_Exception() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByItemSkuAndDelivery(null, ITEM_SKU));
  }

  @Test
  public void findByItemSkuAndDeliveryUnCached_Exception1() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, null));
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalseTest() {
    this.itemPickupPointService
        .findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKU, PICKUP_POINT, Boolean.TRUE);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKU, PICKUP_POINT, Boolean.TRUE);
  }

  @Test
  public void delete_nullStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.delete(null, MERCHANT_CODE, deleteOfflineItemRequestList, USERNAME));
  }

  @Test
  public void delete_nullMerchantCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.delete(STORE_ID, null, deleteOfflineItemRequestList, USERNAME));
  }

  @Test
  public void delete_emptyOfflineItems() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.delete(STORE_ID, MERCHANT_CODE, new ArrayList<>(), USERNAME));
  }

  @Test
  public void delete_nullItemSku() throws Exception {
    this.deleteOfflineItemRequest.setItemSku(null);
    this.deleteOfflineItemRequestList.add(this.deleteOfflineItemRequest);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.delete(STORE_ID, MERCHANT_CODE, deleteOfflineItemRequestList, USERNAME));
  }

  @Test
  public void delete_withOtherExistedOfflineItem_success() throws Exception {
    itemPickupPoint.setDelivery(false);
    when(this.itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKU_2,PICKUP_POINT_CODE,
            Boolean.TRUE)).thenReturn(itemPickupPoint);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableDeferredSolrReindexSystemParameter);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID,
        new HashSet<>(List.of(ITEM_SKU)))).thenReturn(Collections.singletonList(item));
    when(itemPickupPointRepository
        .getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet()
        ))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    itemPickupPointService.delete(STORE_ID, MERCHANT_CODE, deleteOfflineItemRequestList, USERNAME);
    verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKU_2,PICKUP_POINT_CODE, Boolean.TRUE);
    doNothing().when(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME), eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(1));
    verify(itemPickupPointRepository)
        .getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet()
        );
    verify(this.itemPickupPointRepository).saveAll(anyList());
    verify(this.saveAndPublishService)
        .publishListOfItemsPickupPoints(this.itemPickupPointList, null);
    verify(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME),
        eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(1));
  }

  @Test
  public void delete_withOtherExistedOfflineItemProductArchive() throws Exception {
    itemPickupPoint.setDelivery(false);
    when(this.itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKU_2,PICKUP_POINT_CODE,
            Boolean.TRUE)).thenReturn(itemPickupPoint);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableDeferredSolrReindexSystemParameter);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID,
        new HashSet<>(Collections.singletonList(ITEM_SKU)))).thenReturn(Collections.singletonList(item));
    when(itemPickupPointRepository.getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID),
        eq(itemPickupPoint.getProductSku()), Mockito.anySet())).thenReturn(new ArrayList<>());
    itemPickupPointService.delete(STORE_ID, MERCHANT_CODE, deleteOfflineItemRequestList, USERNAME);
    verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKU_2,PICKUP_POINT_CODE, Boolean.TRUE);
    verify(itemPickupPointRepository)
        .getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet());
    verify(this.itemPickupPointRepository).saveAll(anyList());
    verify(productService).toggleArchiveProduct(STORE_ID, USERNAME, PRODUCT_SKU, true, StringUtils.EMPTY);
    verify(this.saveAndPublishService)
        .publishListOfItemsPickupPoints(this.itemPickupPointList, null);
  }

  @Test
  public void bulkDelete_emptyOfflineItems() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.bulkDelete(STORE_ID, MERCHANT_CODE, Collections.emptyList(), USERNAME));
  }

  @Test
  public void bulkDeleteAllSucceededMppTrueTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "inventoryDeleteBatchSize", 10);
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    itemPickupPointList.add(itemPickupPoint2);
    itemPickupPoint2.setProductSku(PRODUCT_SKU);
    Map<String, Item> itemMap = new HashMap<>();
    itemMap.put(ITEM_SKU, item);
    itemMap.put(ITEM_SKU_2, item);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false))
        .thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint2));
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2, false))
        .thenReturn(Collections.singletonList(itemPickupPoint2));
    when(this.itemPickupPointRepository
        .findFirstByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            Boolean.TRUE)).thenReturn(null);
    when(itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICKUP_POINT, false)).thenReturn(itemPickupPoint);
    when(itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2, PICKUP_POINT, false)).thenReturn(itemPickupPoint2);
    when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, Boolean.FALSE, USERNAME))
        .thenReturn(item);
    when(itemService.fetchItemMapByItemSkus(eq(STORE_ID), any())).thenReturn(itemMap);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableDeferredSolrReindexSystemParameter);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID,
        new HashSet<>(List.of(ITEM_SKU)))).thenReturn(Collections.singletonList(item));
    when(
        itemPickupPointRepository.getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID),
            eq(itemPickupPoint.getProductSku()), Mockito.anySet())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(any(), any())).thenReturn(BusinessPartner.builder().build());
    doNothing().when(productService)
        .updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME), eq(itemPickupPoint.getProductSku()),
            Mockito.anySet(), eq(2));
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);
    List<DeleteOfflineItemVO> result =
        itemPickupPointService.bulkDelete(STORE_ID, MERCHANT_CODE, deleteOfflineItemVOs, USERNAME);
    verify(this.itemPickupPointRepository).saveAll(anyList());
    verify(itemPickupPointRepository).getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(
        eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet());
    verify(this.saveAndPublishService)
        .publishListOfItemsPickupPoints(this.itemPickupPointList, null);
    verify(itemPickupPointRepository, times(2)).findByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(STORE_ID), any(), eq(false));
    verify(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME),
        eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(2));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(anyString(),anyString());
    verify(this.inventoryOutbound, times(1)).deleteByItemSkuAndPickupPointCode(anyString(),
        anyString(), anyList());
    int succeededOfflineItemCount =
        (int) result.stream().filter(DeleteOfflineItemVO::isSuccess).count();
    int failedOfflineItemCount = (int) result.stream().filter(e -> !e.isSuccess()).count();
    assertEquals(1, succeededOfflineItemCount);
    assertEquals(1, failedOfflineItemCount);
  }

  @Test
  public void bulkDeleteAllSucceededMppTrueExceptionTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "inventoryDeleteBatchSize", 10);
    ReflectionTestUtils.setField(itemPickupPointService, "ranchIntegrationEnabled", true);
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    itemPickupPointList.add(itemPickupPoint2);
    itemPickupPoint2.setProductSku(PRODUCT_SKU);
    WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO webItemSkuAndPickupPointCodeDTO =
        new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO();
    webItemSkuAndPickupPointCodeDTO.setWebItemSku(ITEM_SKU);
    webItemSkuAndPickupPointCodeDTO.setPickupPointCode(PICKUP_POINT);
    webItemSkuAndPickupPointCodeDTO.setWebMerchantCode(MERCHANT_CODE);
    Map<String, Item> itemMap = new HashMap<>();
    itemMap.put(ITEM_SKU, item);
    itemMap.put(ITEM_SKU_2, item);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    itemSkus.add(ITEM_SKU_2);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false)).thenReturn(
        Arrays.asList(itemPickupPoint, itemPickupPoint2));
    when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2, false)).thenReturn(
        Collections.singletonList(itemPickupPoint2));
    when(
        this.itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            Boolean.TRUE)).thenReturn(null);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        PICKUP_POINT, false)).thenReturn(itemPickupPoint);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2,
        PICKUP_POINT, false)).thenReturn(itemPickupPoint2);
    when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, Boolean.FALSE, USERNAME)).thenReturn(item);
    when(inventoryOutbound.deleteByItemSkuAndPickupPointCode(X_PRODUCT, USERNAME,
        Collections.singletonList(webItemSkuAndPickupPointCodeDTO))).thenThrow(new ApplicationRuntimeException());
    when(itemService.fetchItemMapByItemSkus(eq(STORE_ID), eq(itemSkus))).thenReturn(itemMap);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX)).thenReturn(
        enableDeferredSolrReindexSystemParameter);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU)))).thenReturn(
        Collections.singletonList(item));
    when(
        itemPickupPointRepository.getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID),
            eq(itemPickupPoint.getProductSku()), Mockito.anySet())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    doNothing().when(productService)
        .updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME), eq(itemPickupPoint.getProductSku()),
            Mockito.anySet(), eq(2));
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(STORE_ID, MERCHANT_CODE))
        .thenReturn(businessPartnerData);
    try {
      List<DeleteOfflineItemVO> result =
          itemPickupPointService.bulkDelete(STORE_ID, MERCHANT_CODE, deleteOfflineItemVOs, USERNAME);
    } finally {
      verify(this.itemPickupPointRepository).saveAll(anyList());
      verify(itemPickupPointRepository).getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(
          eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet());
      verify(this.saveAndPublishService).publishListOfItemsPickupPoints(this.itemPickupPointList, null);
      verify(itemPickupPointRepository, times(1)).findByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(STORE_ID),
          eq(ITEM_SKU), eq(false));
      verify(itemPickupPointRepository, times(1)).findByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(STORE_ID),
          eq(ITEM_SKU_2), eq(false));
      verify(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME),
          eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(2));
      verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(STORE_ID, MERCHANT_CODE);
      verify(this.inventoryOutbound, times(1)).deleteByItemSkuAndPickupPointCode(X_PRODUCT, USERNAME,
          Collections.singletonList(webItemSkuAndPickupPointCodeDTO));
    }
  }

  @Test
  public void bulkDeleteDistributionL5ExceptionTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "inventoryDeleteBatchSize", 10);
    ReflectionTestUtils.setField(itemPickupPointService, "ranchIntegrationEnabled", true);
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    itemPickupPointList.add(itemPickupPoint2);
    itemPickupPoint2.setProductSku(PRODUCT_SKU);
    WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO webItemSkuAndPickupPointCodeDTO =
        new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO();
    webItemSkuAndPickupPointCodeDTO.setWebItemSku(ITEM_SKU);
    webItemSkuAndPickupPointCodeDTO.setPickupPointCode(PICKUP_POINT);
    webItemSkuAndPickupPointCodeDTO.setWebMerchantCode(MERCHANT_CODE);
    Map<String, Item> itemMap = new HashMap<>();
    itemMap.put(ITEM_SKU, item);
    itemMap.put(ITEM_SKU_2, item);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    itemSkus.add(ITEM_SKU_2);
    itemPickupPoint2.setDistribution(true);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false)).thenReturn(
        Arrays.asList(itemPickupPoint, itemPickupPoint2));
    when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2, false)).thenReturn(
        Collections.singletonList(itemPickupPoint2));
    when(
        this.itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            Boolean.TRUE)).thenReturn(null);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        PICKUP_POINT, false)).thenReturn(itemPickupPoint);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2,
        PICKUP_POINT, false)).thenReturn(itemPickupPoint2);
    when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, Boolean.FALSE, USERNAME)).thenReturn(item);
    when(inventoryOutbound.deleteByItemSkuAndPickupPointCode(X_PRODUCT, USERNAME,
        Collections.singletonList(webItemSkuAndPickupPointCodeDTO))).thenThrow(new ApplicationRuntimeException());
    when(itemService.fetchItemMapByItemSkus(eq(STORE_ID), eq(itemSkus))).thenReturn(itemMap);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX)).thenReturn(
        enableDeferredSolrReindexSystemParameter);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU)))).thenReturn(
        Collections.singletonList(item));
    when(
        itemPickupPointRepository.getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID),
            eq(itemPickupPoint.getProductSku()), Mockito.anySet())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    doNothing().when(productService)
        .updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME), eq(itemPickupPoint.getProductSku()),
            Mockito.anySet(), eq(2));
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(STORE_ID, MERCHANT_CODE))
        .thenReturn(businessPartnerData);
    itemPickupPointService.bulkDelete(STORE_ID, MERCHANT_CODE, deleteOfflineItemVOs, USERNAME);
      verify(this.itemPickupPointRepository).saveAll(anyList());
      verify(itemPickupPointRepository).getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(
          eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet());
      verify(this.saveAndPublishService).publishListOfItemsPickupPoints(
          Collections.singletonList(this.itemPickupPointList.get(0)), null);
      verify(itemPickupPointRepository, times(1)).findByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(STORE_ID),
          eq(ITEM_SKU), eq(false));
      verify(itemPickupPointRepository, times(1)).findByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(STORE_ID),
          eq(ITEM_SKU_2), eq(false));
      verify(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME),
          eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(1));
      verify(this.inventoryOutbound, times(1)).deleteByItemSkuAndPickupPointCode(X_PRODUCT, USERNAME,
          Collections.singletonList(webItemSkuAndPickupPointCodeDTO));
  }

  @Test
  public void bulkDeleteAllSucceededMppTrueWithOfflineUpdatesTest() throws Exception {
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    itemPickupPoint2.setCncActive(true);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().get().setBuyable(Boolean.FALSE);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().get().setDiscoverable(Boolean.TRUE);
    itemPickupPointList.add(itemPickupPoint2);
    itemPickupPoint2.setProductSku(PRODUCT_SKU);
    Map<String, Item> itemMap = new HashMap<>();
    itemMap.put(ITEM_SKU, item);
    itemMap.put(ITEM_SKU_2, item);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false))
        .thenReturn(null);
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2, false))
        .thenReturn(Collections.singletonList(itemPickupPoint2));
    when(this.itemPickupPointRepository
        .findFirstByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            Boolean.TRUE)).thenReturn(null);
    when(itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICKUP_POINT, false)).thenReturn(itemPickupPoint);
    when(itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_2, PICKUP_POINT, false)).thenReturn(itemPickupPoint2);
    when(itemService.updateCncActivated(STORE_ID, ITEM_SKU, Boolean.FALSE, USERNAME))
        .thenReturn(item);
    when(itemService.fetchItemMapByItemSkus(eq(STORE_ID), any())).thenReturn(itemMap);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableDeferredSolrReindexSystemParameter);
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID,
        new HashSet<>(List.of(ITEM_SKU)))).thenReturn(Collections.singletonList(item));
    when(itemPickupPointRepository
        .getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    doNothing().when(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME), eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(2));
    BusinessPartner businessPartner = BusinessPartner.builder().build();
    businessPartner.setInternationalFlag(true);
    when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(any(), any())).thenReturn(businessPartner);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    List<DeleteOfflineItemVO> result =
        itemPickupPointService.bulkDelete(STORE_ID, MERCHANT_CODE, deleteOfflineItemVOs, USERNAME);
    verify(this.itemPickupPointRepository).saveAll(anyList());
    verify(itemPickupPointRepository).getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(
        eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet());
    verify(this.saveAndPublishService)
        .publishListOfItemsPickupPoints(any(), eq(null));
    verify(itemPickupPointRepository, times(2)).findByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(STORE_ID), any(), eq(false));
    verify(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME), eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(2));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(anyString(),anyString());
    int succeededOfflineItemCount =
        (int) result.stream().filter(DeleteOfflineItemVO::isSuccess).count();
    int failedOfflineItemCount = (int) result.stream().filter(e -> !e.isSuccess()).count();
    assertEquals(2, failedOfflineItemCount);
  }

  @Test
  public void delete_ExceptionTest() throws Exception {
    itemPickupPoint.setDelivery(false);
    when(this.itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKU_2,PICKUP_POINT_CODE , Boolean.TRUE)).thenReturn(itemPickupPoint);
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableDeferredSolrReindexSystemParameter);
    doThrow(RuntimeException.class).when(cacheEvictItemService).evictUniqueIdTypeCheck(eq(STORE_ID),
        eq(ITEM_SKU));
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableDeferredSolrReindexSystemParameter);
    when(
        itemPickupPointRepository.getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID),
            eq(itemPickupPoint.getProductSku()), Mockito.anySet())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID,
        new HashSet<>(List.of(ITEM_SKU)))).thenReturn(Collections.singletonList(item));
    doNothing().when(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME), eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(1));
    itemPickupPointService.delete(STORE_ID, MERCHANT_CODE, deleteOfflineItemRequestList, USERNAME);
    verify(this.itemPickupPointRepository).saveAll(itemPickupPointList);
    verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeAndCncActiveAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKU_2,
            PICKUP_POINT_CODE,Boolean.TRUE);
    verify(this.saveAndPublishService)
        .publishListOfItemsPickupPoints(itemPickupPointList, null);
    verify(itemPickupPointRepository).getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(
        eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet());
    verify(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME),
        eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(1));
  }

  @Test
  public void validateAndUpdateProductItemPickupPointCncActiveEmptyListTest() throws Exception {
    itemPickupPointService.validateAndUpdateProductItemPickupPointCncActive(STORE_ID, USERNAME, new ArrayList<>());
  }

  @Test
  public void validateAndUpdateCncActivatedTrueTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "excludedUserNames", "ALL");
    when(this.itemPickupPointRepository
        .findFirstByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            Boolean.TRUE)).thenReturn(itemPickupPoint);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableDeferredSolrReindexSystemParameter);
    when(
        itemPickupPointRepository.getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID),
            eq(itemPickupPoint.getProductSku()), Mockito.anySet())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU))))
        .thenReturn(Collections.singletonList(item));
    doNothing().when(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME), eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(1));
    itemPickupPointService.validateAndUpdateProductItemPickupPointCncActive(STORE_ID, USERNAME, itemPickupPointList);
    verify(itemPickupPointRepository).getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(
        eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet());
    verify(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME), eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(1));
  }

  @Test
  public void validateAndUpdateCncActivatedTrueTest_exception() throws Exception {
    enableDeferredSolrReindexSystemParameter = new SystemParameter(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX, "false", "");
    when(this.itemPickupPointRepository
        .findFirstByStoreIdAndItemSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            Boolean.TRUE)).thenReturn(itemPickupPoint);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_OFFLINE_DEFERRED_SOLR_REINDEX))
        .thenReturn(enableDeferredSolrReindexSystemParameter);
    when(
        itemPickupPointRepository.getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(eq(STORE_ID),
            eq(itemPickupPoint.getProductSku()), Mockito.anySet())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    doNothing().when(productService)
        .updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME), eq(itemPickupPoint.getProductSku()),
            Mockito.anySet(), eq(1));
    Mockito.when(itemService.getItemsByStoreIdAndItemSkus(STORE_ID, new HashSet<>(List.of(ITEM_SKU))))
        .thenReturn(Collections.singletonList(item));
    Mockito.doThrow(Exception.class).when(productAndItemSolrIndexerService).applyItem(any(Item.class));
    itemPickupPointService.validateAndUpdateProductItemPickupPointCncActive(STORE_ID, USERNAME, itemPickupPointList);
    verify(itemPickupPointRepository).getItemPickupPointsByStoreIdAndProductSkuAndMarkForDeleteFalseAndIdNotIn(
        eq(STORE_ID), eq(itemPickupPoint.getProductSku()), Mockito.anySet());
    verify(productService).updateDistinctPickupPointCodesAndL5Count(eq(STORE_ID), eq(USERNAME),
        eq(itemPickupPoint.getProductSku()), Mockito.anySet(), eq(1));
  }

  @Test
  public void updateFieldByItemSkuTest() throws Exception{
    String fieldName = "field-name";
    Item savedItem = new Item();
    savedItem.setStoreId(STORE_ID);
    itemPickupPointService.updateFieldByItemSku(STORE_ID,ITEM_SKU, fieldName, savedItem);
    verify(itemPickupPointRepository).updateFieldByItemSku(STORE_ID,ITEM_SKU,fieldName,savedItem);
  }

  @Test
  public void updateFieldsByItemSkuTest() throws Exception{
    String fieldName = "field-name";
    Item savedItem = new Item();
    savedItem.setStoreId(STORE_ID);
    itemPickupPointService.updateFieldsByItemSku(STORE_ID,ITEM_SKU, fieldName, savedItem);
    verify(itemPickupPointRepository).updateFieldsByItemSku(STORE_ID,ITEM_SKU,fieldName,savedItem);
  }

  @Test
  public void saveItemPickupPointListTest() {
    Mockito.when(itemPickupPointRepository.saveAll(anyList())).thenReturn(itemPickupPointList);
    itemPickupPointService.saveItemPickupPoint(itemPickupPointList);
    Mockito.verify(itemPickupPointRepository).saveAll(itemPickupPointList);
  }

  @Test
  public void saveItemPickupPointAndItemListTest() {
    Mockito.when(itemPickupPointRepository.saveAll(anyList())).thenReturn(itemPickupPointList);
    itemPickupPointService.saveItemPickupPoint(itemPickupPointList, List.of(item));
    Mockito.verify(itemPickupPointRepository).saveAll(itemPickupPointList);
    Mockito.verify(cacheEvictHelperService).evictItemPickupPointData(itemPickupPointList, List.of(item));
  }

  @Test
  public void findByStoreIdAndItemSkuTest() {
    itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findByStoreIdAndItemSkuSkuNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, null));
  }

  @Test
  public void findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalseTest() {
    Mockito.when(
            this.itemPickupPointRepository.findByStoreIdAndOfflineItemIdInAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
                Collections.singletonList(ITEM_SKU)))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    List<ItemPickupPoint> result =
        this.itemPickupPointService.findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(STORE_ID,
            Collections.singletonList(ITEM_SKU));
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndOfflineItemIdInAndCncActiveTrueAndMarkForDeleteFalse(STORE_ID,
            Collections.singletonList(ITEM_SKU));
    assertEquals(ITEM_SKU, result.get(0).getItemSku());
    assertEquals(EXTERNAL_PICKUP_POINT, result.get(0).getExternalPickupPointCode());
    assertEquals(PICKUP_POINT, result.get(0).getPickupPointCode());
  }

  @Test
  public void findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(
        StringUtils.EMPTY, Collections.singletonList(ITEM_SKU)));
  }

  @Test
  public void findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse_emptySkuListTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(STORE_ID,
        Collections.emptyList()));
  }

  @Test
  public void getActivePromoBundlingByItemSkusTest() {
    Mockito.when(
            this.itemPickupPointRepository.getActivePromoBundlingsByItemSkus(STORE_ID,
                Collections.singletonList(ITEM_SKU)))
        .thenReturn(new HashSet<>(Collections.singletonList(itemPickupPoint.getItemSku())));
    Set<String> result =
        this.itemPickupPointService.getActivePromoBundlingByItemSkus(STORE_ID, Collections.singletonList(ITEM_SKU));
    Mockito.verify(this.itemPickupPointRepository)
        .getActivePromoBundlingsByItemSkus(STORE_ID, Collections.singletonList(ITEM_SKU));
    assertEquals(itemPickupPoint.getItemSku(), result.stream().findFirst().get());
  }

  @Test
  public void getActivePromoBundlingByItemSkus_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getActivePromoBundlingByItemSkus(
        StringUtils.EMPTY, Collections.singletonList(ITEM_SKU)));
  }

  @Test
  public void getActivePromoBundlingByItemSkus_emptySkuListTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getActivePromoBundlingByItemSkus(STORE_ID, Collections.emptyList()));
  }

  @Test
  public void getActivePromoBundlingByItemSkusAndPickupPointCodeTest() {
    Mockito.when(
            this.itemPickupPointRepository.getActivePromoBundlingsByItemSkusAndMarkForDeleteFalse(STORE_ID,
                Collections.singletonList(ITEM_SKU), PICKUP_POINT_CODE))
        .thenReturn(new HashSet<>(Collections.singletonList(itemPickupPoint.getItemSku())));
    Set<String> result = this.itemPickupPointService.getActivePromoBundlingByItemSkusAndPickupPointCode(STORE_ID,
        Collections.singletonList(ITEM_SKU), PICKUP_POINT_CODE);
    Mockito.verify(this.itemPickupPointRepository)
        .getActivePromoBundlingsByItemSkusAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(ITEM_SKU), PICKUP_POINT_CODE);
    assertEquals(itemPickupPoint.getItemSku(), result.stream().findFirst().get());
  }

  @Test
  public void getActivePromoBundlingByItemSkusAndPickupPointCode_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getActivePromoBundlingByItemSkusAndPickupPointCode(
        StringUtils.EMPTY, Collections.singletonList(ITEM_SKU), PICKUP_POINT_CODE));
  }

  @Test
  public void getActivePromoBundlingByItemSkusAndPickupPointCode_emptySkuListTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getActivePromoBundlingByItemSkusAndPickupPointCode(STORE_ID, Collections.emptyList(), PICKUP_POINT_CODE));
  }

  @Test
  public void getItemPickupPointsByProductSkuAndMarkForDeleteFalseTest() {
    Mockito.when(
        this.itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
            PRODUCT_SKU, false)).thenReturn(Collections.singletonList(itemPickupPoint));
    List<ItemPickupPoint> result =
        itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID,
            PRODUCT_SKU);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    assertEquals(itemPickupPoint, result.get(0));
  }

  @Test
  public void getItemPickupPointsByProductSkuAndMarkForDeleteFalse_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(StringUtils.EMPTY,
        PRODUCT_SKU));
  }

  @Test
  public void getItemPickupPointsByProductSkuAndMarkForDeleteFalse_emptyProductSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID,
        StringUtils.EMPTY));
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPPcode() {
    Mockito.when(
        this.itemPickupPointRepository.findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKU, PICKUP_POINT)).thenReturn(Collections.singletonList(itemPickupPoint));
    List<ItemPickupPoint> result =
        this.itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(STORE_ID, ITEM_SKU, PICKUP_POINT);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICKUP_POINT);
    assertEquals(ITEM_SKU, result.get(0).getItemSku());
    assertEquals(EXTERNAL_PICKUP_POINT, result.get(0).getExternalPickupPointCode());
    assertEquals(PICKUP_POINT, result.get(0).getPickupPointCode());
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPPCodeWithShowDeletedTest() {
    itemPickupPointService.findItemPickupPointsByProductSkuAndPPCodeWithShowDeleted(STORE_ID, PRODUCT_SKU,
        PICKUP_POINT_CODE, false);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, PICKUP_POINT_CODE);
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPPCodeWithShowDeletedShowDeletedFalseTest() {
    itemPickupPointService.findItemPickupPointsByProductSkuAndPPCodeWithShowDeleted(STORE_ID, PRODUCT_SKU,
        PICKUP_POINT_CODE, true);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndProductSkuAndPickupPointCode(STORE_ID, PRODUCT_SKU, PICKUP_POINT_CODE);
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPickupPointCodesStoreIdEmpty() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findItemPickupPointsByProductSkuAndPickupPointCodes(null, null, null));
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPickupPointCodesProductSkuEmpty() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, null, null));
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPickupPointCodesEmptyList() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU, null));
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPickupPointCodesTest() {
    itemPickupPointService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT));
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndProductSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
            Collections.singletonList(PICKUP_POINT), false);
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPPcode_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(StringUtils.EMPTY, PRODUCT_SKU,
        PICKUP_POINT_CODE));
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPPcode_emptySkuListTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(STORE_ID,
        null, PICKUP_POINT));
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPPcode_emptyPPCodeTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(STORE_ID,
        ITEM_SKU, null));
  }


  @Test
  public void findPickUpPointCodeByItemSkuInAndDeliveryTest() {
    Mockito.when(
        this.itemPickupPointRepository.findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(STORE_ID,
            Collections.singletonList(ITEM_SKU), true)).thenReturn(itemPickupPointList);
    List<ItemSkuPickupPointCodeResponse> result =
        this.itemPickupPointService.findPickUpPointCodeByItemSkuInAndDelivery(STORE_ID, Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(this.itemPickupPointRepository)
        .findByStoreIdAndItemSkuInAndDeliveryAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(ITEM_SKU), true);
    assertEquals(itemPickupPointList.get(0).getItemSku(), result.stream().findFirst().get().getItemSku());
    assertEquals(itemPickupPointList.get(0).getPickupPointCode(), result.stream().findFirst().get().getPickupPointCode());
  }

  @Test
  public void findPickUpPointCodeByItemSkuInAndDelivery_emptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findPickUpPointCodeByItemSkuInAndDelivery(StringUtils.EMPTY, Collections.singletonList(ITEM_SKU), true));
  }

  @Test
  public void findPickUpPointCodeByItemSkuInAndDelivery_emptySkuListTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.findPickUpPointCodeByItemSkuInAndDelivery(STORE_ID, Collections.emptyList(), false));
  }

  @Test
  public void findItemPickupPointForSummaryListingTest() {
    when(itemPickupPointRepository.getItemPickupPointListing(eq(STORE_ID), eq(0), eq(1),
        any(ItemPickupPointSummaryRequestVo.class))).thenReturn(new PageImpl<>(
        Collections.singletonList(itemPickupPoint)));
    itemPickupPointService.findItemPickupPointForSummaryListing(STORE_ID, 0, 1, new ItemPickupPointSummaryRequest(), new HashSet<>());
    verify(itemPickupPointRepository).getItemPickupPointListing(eq(STORE_ID), eq(0), eq(1),
        any(ItemPickupPointSummaryRequestVo.class));
  }

  @Test
  public void findItemPickupPointForSummaryListingEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findItemPickupPointForSummaryListing(StringUtils.EMPTY, 0, 1, new ItemPickupPointSummaryRequest(), new HashSet<>()));
  }

  @Test
  public void findItemPickupPointForSummaryListingEmptyRequestTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findItemPickupPointForSummaryListing(STORE_ID, 0, 1, null, new HashSet<>()));
  }

  @Test
  public void fetchItemPickupPointsByItemSkuAndPickupPointCodeTest() {
    ItemPickupPointRequest itemPickupPointRequest1 = new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT);
    ItemPickupPointRequest itemPickupPointRequest2 = new ItemPickupPointRequest(ITEM_SKU_2, PICKUP_POINT_1);

    when(itemPickupPointRepository.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        ProductAndItemsUtil.toItemPickupPointRequestVo(List.of(itemPickupPointRequest1)), false)).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(itemPickupPointRepository.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        ProductAndItemsUtil.toItemPickupPointRequestVo(List.of(itemPickupPointRequest2)), false)).thenReturn(
        Collections.singletonList(itemPickupPoint2));

    itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        Arrays.asList(itemPickupPointRequest1, itemPickupPointRequest2), false);

    verify(itemPickupPointRepository).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        ProductAndItemsUtil.toItemPickupPointRequestVo(List.of(itemPickupPointRequest1)), false);
    verify(itemPickupPointRepository).fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID,
        ProductAndItemsUtil.toItemPickupPointRequestVo(List.of(itemPickupPointRequest2)), false);
  }

  @Test
  public void fetchItemPickupPointsByItemSkuAndPickupPointCodeEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(StringUtils.EMPTY, new ArrayList<>(), false));
  }

  @Test
  public void fetchItemPickupPointsByItemSkuAndPickupPointCodeEmptyRequestTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(STORE_ID, new ArrayList<>(), false));
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusMPPflagTrueTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointList.get(0).getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(true).build());
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false)).thenReturn(itemPickupPointList);

    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(anyString(), anyString())).thenReturn(new ArrayList<>());
    Mockito.when(productService.getProduct(anyString(), anyString())).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.any())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);

    itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
        itemPickupPointViewConfigBaseRequest);

    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(anyString(), anyString(), eq(false));

    Mockito.verify(itemPickupPointRepository).saveAll(anyList());

    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).saveItems(anyList());
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(anyList());
    Mockito.verify(businessPartnerService,times(2)).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Mockito.verify(productService)
        .getProduct(any(), any());
    Mockito.verify(saveOperationService).saveProduct(any());
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusMPPflagViewConfigTrueTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointList.get(0).getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    ReflectionTestUtils.setField(itemPickupPointService, "newL5DataChangeTypeEnabled", true);
    ReflectionTestUtils.setField(itemPickupPointService,
        "newlyAddedItemPickupPointDataChangeEventTypes",
        ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName().concat(",")
            .concat(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
    itemPickupPointList.get(0).getAllItemViewConfigs().forEach(itemViewConfig -> {
      itemViewConfig.setBuyable(true);
      itemViewConfig.setDiscoverable(true);
    });
    itemPickupPointList.get(0).setCncActive(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false)).thenReturn(itemPickupPointList);

    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(anyString(), anyString())).thenReturn(new ArrayList<>());
    Mockito.when(productService.getProduct(anyString(), anyString())).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.any())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);

    itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
        itemPickupPointViewConfigBaseRequest);

    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(anyString(), anyString(), eq(false));

    Mockito.verify(itemPickupPointRepository).saveAll(anyList());

    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).saveItems(anyList());
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(anyList());

    Mockito.verify(productService)
        .getProduct(any(), any());
    Mockito.verify(saveOperationService).saveProduct(any());
    Mockito.verify(businessPartnerService,times(2)).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatus_cncForWarehouseTrue() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(itemPickupPointService,
        "newlyAddedItemPickupPointDataChangeEventTypes",
        ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName().concat(",")
            .concat(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
    itemPickupPointList.get(0).setItemViewConfig(itemViewConfigsCnc);
    itemPickupPointList.get(0).getAllItemViewConfigs().forEach(itemViewConfig -> {
      itemViewConfig.setBuyable(true);
      itemViewConfig.setDiscoverable(true);
    });
    itemPickupPointList.get(0).setCncActive(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false)).thenReturn(itemPickupPointList);

    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(anyString(), anyString())).thenReturn(new ArrayList<>());
    Mockito.when(productService.getProduct(anyString(), anyString())).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        any())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);

    itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
        itemPickupPointViewConfigBaseRequest);

    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(anyString(), anyString(), Mockito.eq(false));

    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());

    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).saveItems(Mockito.anyList());
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());

    Mockito.verify(productService)
        .getProduct(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveOperationService).saveProduct(Mockito.any());
    Mockito.verify(businessPartnerService, times(2)).getBusinessPartnerByBusinessPartnerCode(null, null);
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatus_cncForWarehouseTrueTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(itemPickupPointService,
        "newlyAddedItemPickupPointDataChangeEventTypes",
        ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName().concat(",")
            .concat(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
    itemPickupPointList.get(0).setItemViewConfig(itemViewConfigsCnc);
    itemPickupPointViewConfigBaseRequest.setBuyable(false);
    itemPickupPointViewConfigBaseRequest.setCncActivated(true);
    itemPickupPointList.get(0).getAllItemViewConfigs().forEach(itemViewConfig -> {
      itemViewConfig.setBuyable(false);
      itemViewConfig.setDiscoverable(false);
    });
    itemPickupPointList.get(0).setCncActive(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false)).thenReturn(itemPickupPointList);

    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(anyString(), anyString())).thenReturn(new ArrayList<>());
    Mockito.when(productService.getProduct(anyString(), anyString())).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        any()))
        .thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);

    itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
        itemPickupPointViewConfigBaseRequest);

    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(anyString(), anyString(), Mockito.eq(false));

    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());

    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).saveItems(Mockito.anyList());
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    Mockito.verify(productService)
        .getProduct(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveOperationService).saveProduct(Mockito.any());
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatus_cncForWarehouseTrue_noCncConfigsTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(itemPickupPointService,
        "newlyAddedItemPickupPointDataChangeEventTypes",
        ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName().concat(",")
            .concat(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
    itemPickupPointViewConfigBaseRequest.setBuyable(false);
    itemPickupPointViewConfigBaseRequest.setCncActivated(true);
    itemPickupPointList.get(0).getAllItemViewConfigs().forEach(itemViewConfig -> {
      itemViewConfig.setBuyable(false);
      itemViewConfig.setDiscoverable(false);
    });
    itemPickupPointList.get(0).setCncActive(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false)).thenReturn(itemPickupPointList);

    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(anyString(), anyString())).thenReturn(new ArrayList<>());
    Mockito.when(productService.getProduct(anyString(), anyString())).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
            Mockito.anyBoolean()))
        .thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);

    itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
        itemPickupPointViewConfigBaseRequest);

    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(anyString(), anyString(), Mockito.eq(false));

    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());

    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).saveItems(Mockito.anyList());
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());

    Mockito.verify(productService)
        .getProduct(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveOperationService).saveProduct(Mockito.any());
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatus_cncForWarehouseFalseTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", false);
    ReflectionTestUtils.setField(itemPickupPointService,
        "newlyAddedItemPickupPointDataChangeEventTypes",
        ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName().concat(",")
            .concat(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
    itemPickupPointList.get(0).setItemViewConfig(itemViewConfigsCnc);
    itemPickupPointViewConfigBaseRequest.setBuyable(false);
    itemPickupPointViewConfigBaseRequest.setDiscoverable(false);
    itemPickupPointList.get(0).getAllItemViewConfigs().forEach(itemViewConfig -> {
      itemViewConfig.setBuyable(false);
      itemViewConfig.setDiscoverable(false);
    });
    itemPickupPointList.get(0).setCncActive(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false)).thenReturn(itemPickupPointList);

    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(anyString(), anyString())).thenReturn(new ArrayList<>());
    Mockito.when(productService.getProduct(anyString(), anyString())).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
            Mockito.anyBoolean()))
        .thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);

    itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
        itemPickupPointViewConfigBaseRequest);

    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(anyString(), anyString(), Mockito.eq(false));

    Mockito.verify(itemPickupPointRepository).saveAll(Mockito.anyList());

    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).saveItems(Mockito.anyList());
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(null, null);

    Mockito.verify(productService)
        .getProduct(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveOperationService).saveProduct(Mockito.any());
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusMPPflagTrueViewConfigFalseTest() throws Exception {
    itemPickupPointList.get(0).getAllItemViewConfigs().forEach(itemViewConfig -> {
      itemViewConfig.setBuyable(false);
      itemViewConfig.setDiscoverable(false);
    });
    itemPickupPointList.get(0).setCncActive(false);
    itemPickupPointList.get(0).setOfflineItemId(OFFLINE_ITEM_ID);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false)).thenReturn(itemPickupPointList);

    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(anyString(), anyString())).thenReturn(new ArrayList<>());
    Mockito.when(productService.getProduct(anyString(), anyString())).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
            Mockito.anyBoolean()))
        .thenReturn(new ItemPickupPointDataChangeEventModel());

    itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
        itemPickupPointViewConfigBaseRequest);

    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(anyString(), anyString(), eq(false));

    Mockito.verify(itemPickupPointRepository).saveAll(anyList());

    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).saveItems(anyList());
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(anyList());

    Mockito.verify(productService)
        .getProduct(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveOperationService).saveProduct(any());
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusMPPflagTrueBuyableFalseTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService,
        "newlyAddedItemPickupPointDataChangeEventTypes",
        ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName().concat(",")
            .concat(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
    itemPickupPointList.get(0).getAllItemViewConfigs().forEach(itemViewConfig -> {
      itemViewConfig.setBuyable(false);
      itemViewConfig.setDiscoverable(true);
    });
    itemPickupPointList.get(0).setCncActive(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false)).thenReturn(itemPickupPointList);

    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(anyString(), anyString())).thenReturn(new ArrayList<>());
    Mockito.when(productService.getProduct(anyString(), anyString())).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
            Mockito.anyBoolean()))
        .thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);

    itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
        itemPickupPointViewConfigBaseRequest);

    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(anyString(), anyString(), eq(false));

    Mockito.verify(itemPickupPointRepository).saveAll(anyList());

    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).saveItems(anyList());
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(anyList());
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());

    Mockito.verify(productService)
        .getProduct(any(), any());
    Mockito.verify(saveOperationService).saveProduct(any());
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusExceptionTest() throws Exception {
    Mockito.when(itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false))
        .thenThrow(new ApplicationRuntimeException());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID,
          PRODUCT_SKU, itemPickupPointViewConfigBaseRequest));
    } finally {
      Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    }
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusDormantFlowSkipDeletedItemsTrueTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(itemPickupPointService, "dormantFlowSkipDeletedItems", true);
    itemPickupPointList.get(0).getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(true).build());
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false)).thenReturn(itemPickupPointList);

    Mockito.when(itemService.getItemsByProductSkuFromCacheOrElseDB(anyString(), anyString())).thenReturn(new ArrayList<>());
    Mockito.when(productService.getProduct(anyString(), anyString())).thenReturn(new Product());
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.any())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);

    itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
        itemPickupPointViewConfigBaseRequest);

    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(anyString(), anyString(), eq(false));

    Mockito.verify(itemPickupPointRepository).saveAll(anyList());

    Mockito.verify(itemService).findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemService).saveItems(anyList());
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(anyList());
    Mockito.verify(businessPartnerService,times(2)).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Mockito.verify(productService)
        .getProduct(any(), any());
    Mockito.verify(saveOperationService).saveProduct(any());
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusDormantFlowSkipDeletedItemsTrueEmptyListTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(itemPickupPointService, "dormantFlowSkipDeletedItems", true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false)).thenReturn(new ArrayList<>());
    itemPickupPointService.updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
        itemPickupPointViewConfigBaseRequest);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(anyString(), anyString(), eq(false));
  }

  @Test
  public void findItemPickupPointByItemSkusDescTest() {
    itemRequestV2.setSortField(Constants.ITEM_SKU);
    itemRequestV2.setSortOrder(Constants.DESC);
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Sort.Direction.DESC, Constants.ITEM_SKU, Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByItemSkusAscTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "excludeDistributionL5", true);
    itemRequestV2.setSortField(Constants.ITEM_SKU);
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, true);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndDistributionFalseOrMissingAndMarkForDeleteFalse(
        STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Sort.Direction.ASC, Constants.ITEM_SKU, Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByItemSkusAscSwitchOffTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "excludeDistributionL5", false);
    itemRequestV2.setSortField(Constants.ITEM_SKU);
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, true);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Sort.Direction.ASC, Constants.ITEM_SKU, Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByItemSkusAscSwitchOnParamFalseTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "excludeDistributionL5", true);
    itemRequestV2.setSortField(Constants.ITEM_SKU);
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Sort.Direction.ASC, Constants.ITEM_SKU, Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByItemSkusCreatedDateDescTest() {
    itemRequestV2.setSortField(Constants.CREATED_DATE);
    itemRequestV2.setSortOrder(Constants.DESC);
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Sort.Direction.DESC, Constants.CREATED_DATE, Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByItemSkusCreatedDateAscTest() {
    itemRequestV2.setSortField(Constants.CREATED_DATE);
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Sort.Direction.ASC, Constants.CREATED_DATE, Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByItemSkusTest() {
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        itemSkuList, PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByProductSkusOrItemSkusTest() {
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, List.of(PRODUCT_SKU),
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)))).thenReturn(itemPickupPointPage);
    itemPickupPointService.findItemPickupPointByProductSkusOrItemSkus(STORE_ID, List.of(PRODUCT_SKU), itemSkuList, PAGE, PAGE_SIZE);
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, List.of(PRODUCT_SKU), PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findAllItemPickupPointByProductSkusOrItemSkuswithproductSkuNullTest() {
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndItemSkuIn(STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)))).thenReturn(itemPickupPointPage);
    itemPickupPointService.findAllItemPickupPointByProductSkusOrItemSkus(STORE_ID,
        null, itemSkuList, PAGE, PAGE_SIZE);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuIn(STORE_ID,
        itemSkuList, PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findAllItemPickupPointByProductSkusOrItemSkusTest() {
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndProductSkuIn(STORE_ID, List.of(PRODUCT_SKU),
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)))).thenReturn(itemPickupPointPage);
    itemPickupPointService.findAllItemPickupPointByProductSkusOrItemSkus(STORE_ID, List.of(PRODUCT_SKU), itemSkuList, PAGE, PAGE_SIZE);
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuIn(STORE_ID,
        List.of(PRODUCT_SKU), PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByProductSkusOrItemSkusEmptyItemSkuTest() {
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)))).thenReturn(itemPickupPointPage);
    itemPickupPointService.findItemPickupPointByProductSkusOrItemSkus(STORE_ID,
        null, itemSkuList, PAGE, PAGE_SIZE);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        itemSkuList, PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByProductSkusOrItemSkusForFbbTest() {
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository
        .findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndFbbActivated(STORE_ID, List.of(PRODUCT_SKU),
            PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)), true)).thenReturn(itemPickupPointPage);
    itemPickupPointService
        .findItemPickupPointByProductSkusOrItemSkusForFbb(STORE_ID, List.of(PRODUCT_SKU), itemSkuList, true, PAGE,
            PAGE_SIZE);
    verify(itemPickupPointRepository)
        .findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndFbbActivated(STORE_ID, List.of(PRODUCT_SKU),
            PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)), true);
  }

  @Test
  public void findItemPickupPointByProductSkusOrItemSkusForFbbEmptyItemSkuTest() {
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)), true)).thenReturn(itemPickupPointPage);
    itemPickupPointService
        .findItemPickupPointByProductSkusOrItemSkusForFbb(STORE_ID, null, itemSkuList, true, PAGE, PAGE_SIZE);
    verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(STORE_ID, itemSkuList,
            PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)), true);
  }

  @Test
  public void findItemPickupPointByProductSkusMPPEnabledTest() {
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, List.of(PRODUCT_SKU))).thenReturn(itemPickupPoints);
    itemPickupPointService.findItemPickupPointByProductSkus(STORE_ID, List.of(PRODUCT_SKU));
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, List.of(PRODUCT_SKU));
  }

  @Test
  public void findItemPickupPointByProductSkusAndPickupPointCodesTest() {
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID,
        List.of(PRODUCT_SKU), List.of(PICKUP_POINT_1))).thenReturn(itemPickupPoints);
    itemPickupPointService.findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID, List.of(PRODUCT_SKU),
        List.of(PICKUP_POINT_1));
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuInAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID,
        List.of(PRODUCT_SKU), List.of(PICKUP_POINT_1));
  }

  @Test
  public void findItemPickupPointByProductSkusAndEmptyPickupPointCodesTest() {
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID,
        List.of(PRODUCT_SKU), List.of(PICKUP_POINT_1))).thenReturn(itemPickupPoints);
    when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, List.of(PRODUCT_SKU))).thenReturn(itemPickupPoints);
    itemPickupPointService.findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID, List.of(PRODUCT_SKU),
        new ArrayList<>());
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, List.of(PRODUCT_SKU));
  }

  @Test
  public void findItemPickupPointByProductSkusAndPickupPointCodesEmptyProductSkuTest() {
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    itemPickupPointService.findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID, new ArrayList<>(),
        List.of(PICKUP_POINT_1));
  }

  @Test
  public void findItemPickupPointByEmptyProductSkusTest() {
    List<ItemPickupPoint> itemPickupPointByProductSkus =
        itemPickupPointService.findItemPickupPointByProductSkus(STORE_ID, List.of());
    assertTrue(itemPickupPointByProductSkus.isEmpty());
  }

  @Test
  public void findItemPickupPointByProductSkuOrItemSkuTest() {
    Pageable pageable = PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU, pageable)).thenReturn(itemPickupPointPage);
    itemPickupPointService.findItemPickupPointByProductSkuOrItemSku(STORE_ID,
        PRODUCT_SKU, ITEM_SKU, PAGE, PAGE_SIZE);
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU, pageable);
  }

  @Test
  public void findItemPickupPointByProductSkuOrItemSkuProductSkuNullTest() {
    Pageable pageable = PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, pageable)).thenReturn(itemPickupPointPage);
    itemPickupPointService.findItemPickupPointByProductSkuOrItemSku(STORE_ID,
        null, ITEM_SKU, PAGE, PAGE_SIZE);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, pageable);
  }

  @Test
  public void validateAndRepublishItemPickupPointToAgpTest() {
    List<ItemPickupPointRequest> itemPickupPointRequestListMock = new ArrayList<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequestListMock.add(itemPickupPointRequest);
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();

    List<ItemPickupPointRequest> itemPickupPointRequestList = new ArrayList<>();
    itemPickupPointRequestList.add(itemPickupPointRequest);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
            STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    try {
      this.itemPickupPointService.republishItemPickupPointToAgp(itemPickupPointRequestList,
          STORE_ID, true);
      itemPickupPoints.add(itemPickupPoint);
    } finally {
      assertFalse(itemPickupPointRequestList.isEmpty());
      verify(saveAndPublishService).publishItemPickupPointDataChangeEventForAGP(
          itemPickupPoint);
      Mockito.verify(itemPickupPointRepository)
          .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
              PICKUP_POINT_CODE, false);
      assertTrue(CollectionUtils.isNotEmpty(itemPickupPoints));
    }
  }

  @Test
  public void validateAndRepublishItemPickupPointToAgpFalseTest() {
    List<ItemPickupPointRequest> itemPickupPointRequestListMock = new ArrayList<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequestListMock.add(itemPickupPointRequest);
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();

    List<ItemPickupPointRequest> itemPickupPointRequestList = new ArrayList<>();
    itemPickupPointRequestList.add(itemPickupPointRequest);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    this.itemPickupPointService.republishItemPickupPointToAgp(itemPickupPointRequestList, STORE_ID, false);
    itemPickupPoints.add(itemPickupPoint);
    assertFalse(itemPickupPointRequestList.isEmpty());
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(Collections.singletonList(itemPickupPoint),
        new ArrayList<>(), new HashMap<>());
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    assertTrue(CollectionUtils.isNotEmpty(itemPickupPoints));
  }

  @Test
  public void validateAndRepublishItemPickupPointToAgp_EmptyResponseTest() {
    List<ItemPickupPointRequest> itemPickupPointRequestListMock = new ArrayList<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequestListMock.add(itemPickupPointRequest);
    List<ItemPickupPointRequest> itemPickupPointRequestList = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.republishItemPickupPointToAgp(itemPickupPointRequestList, STORE_ID,
          true));
    } finally {
      assertTrue(itemPickupPointRequestList.isEmpty());
      assertTrue(itemPickupPoints.isEmpty());
    }
  }

  @Test
  public void validateAndRepublishItemPickupPointToAgp_EmptyListTest() {
    List<ItemPickupPointRequest> itemPickupPointRequestListMock = new ArrayList<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequestListMock.add(itemPickupPointRequest);
    List<ItemPickupPoint> itemPickupPoints = Collections.EMPTY_LIST;
    this.itemPickupPoint = null;

    List<ItemPickupPointRequest> itemPickupPointRequestList = new ArrayList<>();
    itemPickupPointRequestList.add(itemPickupPointRequest);
    Mockito.when(itemPickupPointRepository
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(null);
    try {
      this.itemPickupPointService.republishItemPickupPointToAgp(itemPickupPointRequestList, STORE_ID, true);

    } finally {
      assertFalse(itemPickupPointRequestList.isEmpty());
      Mockito.verify(itemPickupPointRepository)
          .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
      assertTrue(CollectionUtils.isEmpty(itemPickupPoints));
    }
  }

  @Test
  public void validatePriceChangePriceDisabledExceptionTest() {
    when(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint)).thenReturn(true);
    Price price = new Price();
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    discountPriceList.add(
        new DiscountPrice(200, new Date(1654418263000L), new Date(1685954263000L), StringUtils.EMPTY,
            AdjustmentType.MERCHANT, "TEST_CAMPAIGN"));
    price.setListPrice(700);
    price.setOfferPrice(600);
    price.setChannel("DEFAULT");
    price.setListOfDiscountPrices(discountPriceList);
    price.setMerchantPromoDiscountPrice(discountPriceList.get(0));
    Set<Price> prices = new HashSet<Price>();
    prices.add(price);
    itemPickupPoint.setPrice(prices);
    itemPickupPoint.setMerchantPromoDiscount(true);
    when(pickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices))
        .thenReturn(true);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.validatePriceChangeAndSet(itemPickupPoint, prices, USERNAME));
    verify(pickupPointHelperService).isItemPickupPointPriceChange(itemPickupPoint, prices);
    assertFalse(itemPickupPoint.isMerchantPromoDiscount());
  }

  @Test
  public void validatePriceChangeTest() {
    when(pickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices))
        .thenReturn(true);
    when(pickupPointHelperService.setItemPriceByChannel(itemPickupPoint, prices, USERNAME))
        .thenReturn(itemPickupPoint2);
    this.itemPickupPointService.validatePriceChangeAndSet(itemPickupPoint, prices, USERNAME);
    verify(pickupPointHelperService).isItemPickupPointPriceChange(itemPickupPoint, prices);
    verify(pickupPointHelperService).setItemPriceByChannel(itemPickupPoint, prices, USERNAME);
  }

  @Test
  public void validatePriceChangeOriginalSellingPriceTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "validateOnOriginalSellingPrice", true);
    when(pickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices))
        .thenReturn(true);
    when(pickupPointHelperService.setItemPriceByChannel(itemPickupPoint, prices, USERNAME))
        .thenReturn(itemPickupPoint2);
    this.itemPickupPointService.validatePriceChangeAndSet(itemPickupPoint, prices, USERNAME);
    verify(pickupPointHelperService).isItemPickupPointPriceChange(itemPickupPoint, prices);
    verify(pickupPointHelperService).setItemPriceByChannel(itemPickupPoint, prices, USERNAME);
  }

  @Test
  public void validatePriceChangeOriginalSellingPriceTest2() {
    org.slf4j.MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, "x-bulk");
    ReflectionTestUtils.setField(itemPickupPointService, "validateOnOriginalSellingPrice", true);
    when(pickupPointHelperService.isItemPickupPointPriceChangeForOriginalSellingPrice(itemPickupPoint, prices))
        .thenReturn(true);
    when(pickupPointHelperService.setItemPriceByChannel(itemPickupPoint, prices, USERNAME))
        .thenReturn(itemPickupPoint2);
    this.itemPickupPointService.validatePriceChangeAndSet(itemPickupPoint, prices, USERNAME);
    verify(pickupPointHelperService).isItemPickupPointPriceChangeForOriginalSellingPrice(itemPickupPoint, prices);
    verify(pickupPointHelperService).setItemPriceByChannel(itemPickupPoint, prices, USERNAME);
  }

  @Test
  public void publishItemPickupPointChangeEventTest() {
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    when(objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false))
        .thenReturn(eventModel);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);
    this.itemPickupPointService.publishItemPickupPointChangeEvent(itemPickupPoint);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME),anyString(),eq(eventModel));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void publishItemPickupPointChangeEventTestSwitchOff() {
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    when(objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false)).thenReturn(
        eventModel);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);
    this.itemPickupPointService.publishItemPickupPointChangeEvent(itemPickupPoint);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(),any());
  }

  @Test
  public void findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalseTest() {
    when(itemPickupPointRepository
        .findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(itemPickupPoint);
    this.itemPickupPointService
        .findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalseTest() {
    Set<String> itemSku = new HashSet<>();
    itemSku.add(ITEM_SKU);
    when(itemPickupPointRepository
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndAndCncActiveAndMarkForDeleteFalse(
            STORE_ID, itemSku, true, PICKUP_POINT_CODE)).thenReturn(itemPickupPointList);
    this.itemPickupPointService
        .findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalse(
            STORE_ID, itemSku, true, PICKUP_POINT_CODE);
    verify(itemPickupPointRepository)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndAndCncActiveAndMarkForDeleteFalse(
            STORE_ID, itemSku, true, PICKUP_POINT_CODE);
  }

  @Test
  public void validatePriceChangeAndSetEmptyPriceTest() {
    boolean result = this.itemPickupPointService
        .validatePriceChangeAndSet(itemPickupPoint, new HashSet<>(), USERNAME);
    assertFalse(result);
  }

  @Test
  public void validatePriceChangeAndSetFalseTest() {
    when(pickupPointHelperService.isItemPickupPointPriceChange(itemPickupPoint, prices))
        .thenReturn(false);
    boolean result =
        this.itemPickupPointService.validatePriceChangeAndSet(itemPickupPoint, prices, USERNAME);
    assertFalse(result);
  }

  @Test
  public void isPriceEditDisabled_MerchantPromoEnabledTest() {
    when(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint)).thenReturn(true);
    prices = new HashSet<>();
    itemPickupPoint.setPrice(prices);
    itemPickupPoint.setMerchantPromoDiscount(true);
    assertTrue(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint));
  }

  @Test
  public void isPriceEditDisabled_CampaignTest() {
    when(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint)).thenReturn(true);
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(PRODUCT_CODE);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(List.of(discountPrice));
    itemPickupPoint.setPrice(new HashSet<>(List.of(price)));
    itemPickupPoint.setMerchantPromoDiscount(false);
    assertTrue(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint));
  }

  @Test
  public void isPriceEditDisabled_BlibliSubsidyCampaignTest() {
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(List.of(discountPrice));
    itemPickupPoint.setPrice(new HashSet<>(List.of(price)));
    itemPickupPoint.setMerchantPromoDiscount(false);
    assertFalse(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint));
  }

  @Test
  public void isPriceEditDisabled_BlibliSubsidyPlusMPDTest() {
    when(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint)).thenReturn(true);
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(List.of(discountPrice));
    itemPickupPoint.setPrice(new HashSet<>(List.of(price)));
    itemPickupPoint.setMerchantPromoDiscount(true);
    assertTrue(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint));
  }

  @Test
  public void isPriceEditDisabled_MultipleCampaignsTest() {
    when(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint)).thenReturn(true);
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    itemPickupPoint.setPrice(new HashSet<>(List.of(price)));
    itemPickupPoint.setMerchantPromoDiscount(false);
    assertTrue(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint));
  }

  @Test
  public void isPriceEditDisabledEmptyDiscountPricesFalseTest() {
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-2))));
    itemPickupPoint.setPrice(new HashSet<>(List.of(price)));
    itemPickupPoint.setMerchantPromoDiscount(false);
    assertFalse(pickupPointHelperService.isPriceEditDisabled(itemPickupPoint));
  }

  @Test
  public void getItemPickupPointListingByProductSkuTest() {
    ItemPickupPointListingRequestVo itemPickupPointListingRequestVo = new ItemPickupPointListingRequestVo();
    when(itemPickupPointRepository
        .getItemPickupPointListingByProductSku(STORE_ID, 0, 1, itemPickupPointListingRequestVo))
        .thenReturn(new PageImpl<>(Collections.singletonList(itemPickupPoint)));
    itemPickupPointService.getItemPickupPointListingByProductSku(STORE_ID, 0, 1, itemPickupPointListingRequestVo);
    verify(itemPickupPointRepository)
        .getItemPickupPointListingByProductSku(STORE_ID, 0, 1, itemPickupPointListingRequestVo);
  }

  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseStoreIdNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(null, null));
  }

  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseItemSkuNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, null));
  }

  @Test
  public void findCountByStoreIdAndProductSkuAndMarkForDeleteFalseTest() {
    itemPickupPointService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemPickupPointRepository).countByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
  }

  @Test
  public void findCountByStoreIdAndProductSkuAndMarkForDeleteFalseStoreIdNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalse(null, null));
  }

  @Test
  public void findCountByStoreIdAndProductSkuAndMarkForDeleteFalseItemSkuNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, null));
  }

  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseTest() {
    itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(itemPickupPointRepository).countByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
  }

  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActiveStoreIdNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(null, null, false));
  }

  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActiveItemSkuNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(STORE_ID, null, false));
  }


  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncNotOfflineTest() {
    itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(STORE_ID, ITEM_SKU,
        Constants.CNC);
    verify(itemPickupPointRepository).countByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(STORE_ID, ITEM_SKU,
        Constants.CNC, false);
  }

  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncNotOffline_storeIdNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(null, ITEM_SKU,
        Constants.CNC));
  }

  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncNotOffline_itemSkuNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(STORE_ID, null,
        Constants.CNC));
  }

  @Test
  public void existsountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncNotOfflineTest() {
    itemPickupPointService.existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(STORE_ID, ITEM_SKU,
        Constants.CNC);
    verify(itemPickupPointRepository).existsByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(STORE_ID, ITEM_SKU,
        Constants.CNC);
  }

  @Test
  public void existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncNotOffline_storeIdNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(null, ITEM_SKU,
        Constants.CNC));
  }

  @Test
  public void existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncNotOffline_itemSkuNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.existsCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncBuyable(STORE_ID, null,
        Constants.CNC));
  }

  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActiveTest() {
    itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(STORE_ID, ITEM_SKU, false);
    verify(itemPickupPointRepository)
        .countByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(STORE_ID, ITEM_SKU, false, false);
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalseStoreIdNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(null, null, null));
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalseItemSkuNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, null, null));
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalsePPNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, null));
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalseTest() {
    itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        Collections.singletonList(PICKUP_POINT));
    verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            Collections.singletonList(PICKUP_POINT));
  }

  @Test
  public void getItemPickupPointByItemSkuInAndPickupPointCodeTest() {
    when(itemPickupPointRepository.findByStoreIdAndItemSkuInAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ImmutableSet.of(ITEM_SKU), PICKUP_POINT_CODE)).thenReturn(Collections.singletonList(itemPickupPoint));
    itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ImmutableSet.of(ITEM_SKU), PICKUP_POINT_CODE);
  }

  @Test
  public void getItemPickupPointByItemSkuInAndPickupPointCodeStoreIdBlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(StringUtils.EMPTY, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE));
  }

  @Test
  public void getItemPickupPointByItemSkuInAndPickupPointCodeItemSkusEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, new HashSet<>(),
        PICKUP_POINT_CODE));
  }

  @Test
  public void getItemPickupPointByItemSkuInAndPickupPointCodePickupPointBodeBlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        StringUtils.EMPTY));
  }

  @Test
  public void findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivatedTest() {
    when(
        itemPickupPointRepository.countByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(STORE_ID, PRODUCT_SKU,
            true)).thenReturn(1L);
    itemPickupPointService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(STORE_ID, PRODUCT_SKU,
        true);
    verify(itemPickupPointRepository).countByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(STORE_ID,
        PRODUCT_SKU, true);
  }

  @Test
  public void findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivatedProductSkuEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(STORE_ID,
        StringUtils.EMPTY, true));
  }

  @Test
  public void findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivatedStoreIdEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndFbbActivated(StringUtils.EMPTY,
        PRODUCT_SKU, true));
  }

  @Test
  public void getItemPickupPointByProductSkuListTest() {
    when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, List.of(PRODUCT_SKU))).thenReturn(Collections.singletonList(itemPickupPoint));
    itemPickupPointService.getItemPickupPointByProductSkuList(STORE_ID, List.of(PRODUCT_SKU));
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, List.of(PRODUCT_SKU));
  }

  @Test
  public void getItemPickupPointByProductSkuListBlankStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointByProductSkuList(StringUtils.EMPTY,
        List.of(PRODUCT_SKU)));
  }

  @Test
  public void getItemPickupPointByProductSkuListBlankProductSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointByProductSkuList(STORE_ID, new ArrayList<>()));
  }

  @Test
  public void getItemPickupPointByProductSkuListAndDeliveryTrueTest() {
    when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndDeliveryTrue(STORE_ID,
        List.of(PRODUCT_SKU))).thenReturn(Collections.singletonList(itemPickupPoint));
    itemPickupPointService.getItemPickupPointByProductSkuListAndDeliveryTrue(STORE_ID, List.of(PRODUCT_SKU));
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndDeliveryTrue(STORE_ID,
        List.of(PRODUCT_SKU));
  }

  @Test
  public void getItemPickupPointByProductSkuListAndDeliveryTrueBlankStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointByProductSkuListAndDeliveryTrue(StringUtils.EMPTY,
        List.of(PRODUCT_SKU)));
  }

  @Test
  public void getItemPickupPointByProductSkuListAndDeliveryTrueBlankProductSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointByProductSkuListAndDeliveryTrue(STORE_ID, new ArrayList<>()));
  }

  @Test
  public void findOneByStoreIdAndProductSkuAndCncActiveAndMarkForDeleteFalseExceptionTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(null, null, true));
  }

  @Test
  public void findOneByStoreIdAndProductSkuAndCncActiveAndMarkForDeleteFalseExeTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, null, true));
  }

  @Test
  public void findOneByStoreIdAndProductSkuAndCncActiveAndMarkForDeleteFalseTest() {
    itemPickupPointService.findOneByProductSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, true);
    Mockito.verify(itemPickupPointRepository).findFirstByStoreIdAndProductSkuAndCncActiveAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, true);
  }

  @Test
  public void findOneByStoreIdAndProductSkuAndMarkForDeleteFalseStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByStoreIdAndProductSkuAndMarkForDeleteFalse(null, null));
  }

  @Test
  public void findOneByStoreIdAndProductSkuAndMarkForDeleteFalseProductSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, null));
  }

  @Test
  public void findOneByStoreIdAndProductSkuAndMarkForDeleteFalseTest() {
    itemPickupPointService.findOneByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointRepository).findFirstByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void getItemPickupPointByItemSkuInAndDeliveryTrueTest() {
    when(itemPickupPointRepository.findByStoreIdAndItemSkuInAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        List.of(ITEM_SKU))).thenReturn(itemPickupPointList);
    itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, List.of(ITEM_SKU));
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndDeliveryTrueAndMarkForDeleteFalse(STORE_ID,
        List.of(ITEM_SKU));
  }

  @Test
  public void getItemPickupPointByItemSkuInAndDeliveryTrueStoreIdEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(StringUtils.EMPTY,
        List.of(ITEM_SKU)));
  }

  @Test
  public void getItemPickupPointByItemSkuInAndDeliveryTrueItemSkuListEmptyTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointByItemSkuInAndDeliveryTrue(STORE_ID, new ArrayList<>()));
  }

  @Test
  public void getItemPickupPointByStoreIdAndProductSkuInAndDeliveryTrueTest() {
    Pageable pageable = PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.ITEM_SKU));
    when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndDeliveryTrue(
        STORE_ID, PRODUCT_SKU, pageable)).thenReturn(new PageImpl<>(itemPickupPointList));
    itemPickupPointService.getItemPickupPointByStoreIdAndProductSkuInAndDeliveryTrue(STORE_ID,
        PRODUCT_SKU, PAGE, PAGE_SIZE);
    verify(
        itemPickupPointRepository).findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndDeliveryTrue(
        STORE_ID, PRODUCT_SKU, pageable);
  }

  @Test
  public void getItemPickupPointByItemSkuInAndDeliveryTrueStoreIdEmptyProductSku() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getItemPickupPointByStoreIdAndProductSkuInAndDeliveryTrue(STORE_ID, "",
        PAGE, PAGE_SIZE));
  }

  @Test
  public void getBasicItemDetailsTest() throws Exception {
    itemPickupPointList = new ArrayList<>();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPointList.add(itemPickupPoint);
    Product product = new Product();
    product.setStoreId(STORE_ID);
    product.setProductSku(PRODUCT_SKU);
    Page<ItemPickupPoint> itemPickupPoints = new PageImpl<>(itemPickupPointList,
        PageRequest.of(1,1), 0);
    Pageable pageable = PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        pageable)).thenReturn(new PageImpl<>(itemPickupPointList));
    when(productService.getProduct(STORE_ID, PRODUCT_SKU)).thenReturn(new Product());
    itemPickupPointService.getBasicItemDetails(STORE_ID, PAGE, PAGE_SIZE, ITEM_SKU);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, pageable);
    verify(productService).getProduct(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void getBasicItemDetailsEmptyTest() {
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.getBasicItemDetails(STORE_ID, PAGE, PAGE_SIZE, ITEM_SKU));
    } finally {
      Pageable pageable = PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
      verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, pageable);
    }
  }

  @Test
  public void deleteItemPickupPointsTest() {
    Mockito.doNothing().when(itemPickupPointRepository).deleteItemPickupPointByItemSku(STORE_ID, ITEM_SKU);
    itemPickupPointService.deleteItemPickupPoints(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointRepository).deleteItemPickupPointByItemSku(STORE_ID, ITEM_SKU);
  }

  @Test
  public void deleteItemPickupPointsStoreIdNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.deleteItemPickupPoints(null, ITEM_SKU));
  }

  @Test
  public void deleteItemPickupPointsItemSkuNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.deleteItemPickupPoints(STORE_ID, null));
  }

  @Test
  public void getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgpTest() {
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(
        itemPickupPointRepository.findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, PRODUCT_SKU, null,
            false)).thenReturn(itemPickupPoint);
    List<ProductSkuPickupPointResponse> productSkuPickupPointResponses = itemPickupPointService
        .getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgp(STORE_ID, List.of(PRODUCT_SKU), true);
    Mockito.verify(itemPickupPointRepository)
        .findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, PRODUCT_SKU, null, false);
    Mockito.verify(saveAndPublishService).publishItemPickupPointDataChangeEventForAGP(itemPickupPoint);
    assertEquals(PICKUP_POINT_CODE, productSkuPickupPointResponses.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU, productSkuPickupPointResponses.get(0).getProductSku());
  }

  @Test
  public void getL5BasedOnProductSkuListAndOnlineOrCncFlagAndNoRepublishToAgpTest() {
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(
        itemPickupPointRepository.findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, PRODUCT_SKU, null,
            false)).thenReturn(itemPickupPoint);
    List<ProductSkuPickupPointResponse> productSkuPickupPointResponses = itemPickupPointService
        .getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgp(STORE_ID, List.of(PRODUCT_SKU), false);
    Mockito.verify(itemPickupPointRepository)
        .findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, PRODUCT_SKU, null, false);
    assertEquals(PICKUP_POINT_CODE, productSkuPickupPointResponses.get(0).getPickupPointCode());
    assertEquals(PRODUCT_SKU, productSkuPickupPointResponses.get(0).getProductSku());
  }

  @Test
  public void getL5BasedOnProductSkuListAndOnlineOrCncFlagAndNoRepublishToAgpNoDataTest() {
    Mockito.when(
        itemPickupPointRepository.findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, PRODUCT_SKU, null,
            false)).thenReturn(null);
    List<ProductSkuPickupPointResponse> productSkuPickupPointResponses = itemPickupPointService
        .getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgp(STORE_ID, List.of(PRODUCT_SKU), false);
    Mockito.verify(itemPickupPointRepository)
        .findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, PRODUCT_SKU, null, false);
    assertEquals(0, productSkuPickupPointResponses.size());
  }

  @Test
  public void getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgpTest() {
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(
        itemPickupPointRepository.findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, null, ITEM_SKU,
            false)).thenReturn(itemPickupPoint);
    List<ItemSkuPickupPointResponse> itemSkuPickupPointResponses = itemPickupPointService
        .getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgp(STORE_ID, List.of(ITEM_SKU), true);
    Mockito.verify(itemPickupPointRepository)
        .findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, null, ITEM_SKU, false);
    Mockito.verify(saveAndPublishService).publishItemPickupPointDataChangeEventForAGP(itemPickupPoint);
    assertEquals(PICKUP_POINT_CODE, itemSkuPickupPointResponses.get(0).getPickupPointCode());
    assertEquals(ITEM_SKU, itemSkuPickupPointResponses.get(0).getItemSku());
  }

  @Test
  public void getL5BasedOnItemSkuListAndOnlineOrCncFlagAndNoRepublishToAgpTest() {
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(
        itemPickupPointRepository.findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, null, ITEM_SKU,
            false)).thenReturn(itemPickupPoint);
    List<ItemSkuPickupPointResponse> itemSkuPickupPointResponses = itemPickupPointService
        .getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgp(STORE_ID, List.of(ITEM_SKU), false);
    Mockito.verify(itemPickupPointRepository)
        .findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, null, ITEM_SKU, false);
    assertEquals(PICKUP_POINT_CODE, itemSkuPickupPointResponses.get(0).getPickupPointCode());
    assertEquals(ITEM_SKU, itemSkuPickupPointResponses.get(0).getItemSku());
  }

  @Test
  public void getL5BasedOnItemSkuListAndOnlineOrCncFlagAndNoRepublishToAgpNoDataTest() {
    Mockito.when(
        itemPickupPointRepository.findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, null, ITEM_SKU,
            false)).thenReturn(null);
    List<ItemSkuPickupPointResponse> itemSkuPickupPointResponses = itemPickupPointService
        .getL5BasedOnItemSkuListAndOnlineOrCncFlagAndRepublishToAgp(STORE_ID, List.of(ITEM_SKU), false);
    Mockito.verify(itemPickupPointRepository)
        .findL5BasedOnSkuAndOnlineOrCncFlag(STORE_ID, null, ITEM_SKU, false);
    assertEquals(0, itemSkuPickupPointResponses.size());
  }

  @Test
  public void updateActivePromoBundlingByPPCodeStoreIdNull() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(null, ITEM_SKU, Constants.WHOLESALE,
        false, PICKUP_POINT_CODE));
  }

  @Test
  public void updateActivePromoBundlingByPPCodeItemSkuNull() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, null, Constants.WHOLESALE,
        false, PICKUP_POINT_CODE));
  }

  @Test
  public void updateActivePromoBundlingByPPCodeNull() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, ITEM_SKU, Constants.WHOLESALE,
        false, null));
  }

  @Test
  public void getProductSkuListByBusinessPartnerAndPickupPointCodeBpCodeNullTest(){
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getProductSkuListByBusinessPartnerAndPickupPointCode(STORE_ID,
        null,PICKUP_POINT_CODE,0,1));
  }

  @Test
  public void getProductSkuListByBusinessPartnerAndPickupPointCodeNullTest(){
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemPickupPointService.getProductSkuListByBusinessPartnerAndPickupPointCode(STORE_ID,
        MERCHANT_CODE,null,0,1));
  }

  @Test
  public void getProductSkuListByBusinessPartnerAndPickupPointCodeTest() {
    Mockito.when(itemPickupPointRepository
            .findByStoreIdAndMerchantCodeAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE,
                PICKUP_POINT_CODE, PageRequest.of(0, 1, Sort.by(Constants.ITEM_SKU))))
        .thenReturn(new PageImpl<>(itemPickupPointList));
    Page<ProductSkuPickupPointResponseV2> result = this.itemPickupPointService
        .getProductSkuListByBusinessPartnerAndPickupPointCode(STORE_ID, MERCHANT_CODE,
            PICKUP_POINT_CODE, 0, 1);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndMerchantCodeAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, MERCHANT_CODE,
            PICKUP_POINT_CODE, PageRequest.of(0, 1, Sort.by(Constants.ITEM_SKU)));
    assertEquals(result.getTotalElements(), itemPickupPointList.size());
  }

  @Test
  public void getMinAndMaxOfferPrice(){
    when(productService.findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE)).thenReturn(
        Collections.singletonList(new Product(PRODUCT_SKU)));
    when(this.itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        PRODUCT_SKU, false)).thenReturn(itemPickupPointList);
    MinMaxItemPriceResponse minAndMaxOfferPrice =
        this.itemPickupPointService.getMinAndMaxOfferPrice(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    verify(productService).findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID,
        PRODUCT_CODE);
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
        PRODUCT_SKU, false);
    assertEquals(Optional.of(prices.iterator().next().getOfferPrice()).get(),
        minAndMaxOfferPrice.getMaxPrice());
  }

  @Test
  public void createFbbPickupPointAlreadyExistsTest() {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = getCreateFbbPickupPointRequest();
    CreateFbbPickupPointResponse createFbbPickupPointResponse = new CreateFbbPickupPointResponse();
    createFbbPickupPointResponse.setItemSku(createFbbPickupPointRequest.getItemSku());
    createFbbPickupPointResponse.setPickupPointCode(createFbbPickupPointRequest.getPickupPointCode());
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID,
            createFbbPickupPointRequest.getItemSku(), createFbbPickupPointRequest.getPickupPointCode(), false))
        .thenReturn(itemPickupPoint);
    createFbbPickupPointResponse =
        itemPickupPointService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest, item,
            createFbbPickupPointResponse);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, createFbbPickupPointRequest.getItemSku(),
            createFbbPickupPointRequest.getPickupPointCode(), false);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(STORE_ID,
        createFbbPickupPointRequest.getItemSku());
    assertEquals(ErrorMessages.FBB_PICKUP_POINT_ALREADY_EXISTS, createFbbPickupPointResponse.getReason());
  }

  @Test
  public void createFbbPickupPointAlreadyExistsTest1() {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = getCreateFbbPickupPointRequest();
    CreateFbbPickupPointResponse createFbbPickupPointResponse = new CreateFbbPickupPointResponse();
    createFbbPickupPointResponse.setItemSku(createFbbPickupPointRequest.getItemSku());
    createFbbPickupPointResponse.setPickupPointCode(createFbbPickupPointRequest.getPickupPointCode());
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(STORE_ID,
        createFbbPickupPointRequest.getItemSku())).thenReturn(itemPickupPoint);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID,
            createFbbPickupPointRequest.getItemSku(), createFbbPickupPointRequest.getPickupPointCode(), false))
        .thenReturn(itemPickupPoint);
    createFbbPickupPointResponse =
        itemPickupPointService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest, item,
            createFbbPickupPointResponse);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(STORE_ID,
        createFbbPickupPointRequest.getItemSku());
    assertEquals(ErrorMessages.FBB_PICKUP_POINT_ALREADY_EXISTS, createFbbPickupPointResponse.getReason());
  }

  @Test
  public void createFbbPickupPointExistingPickupPointMfdTrueTest() {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = getCreateFbbPickupPointRequest();
    CreateFbbPickupPointResponse createFbbPickupPointResponse = new CreateFbbPickupPointResponse();
    createFbbPickupPointResponse.setItemSku(createFbbPickupPointRequest.getItemSku());
    createFbbPickupPointResponse.setPickupPointCode(createFbbPickupPointRequest.getPickupPointCode());
    itemPickupPoint.setMarkForDelete(true);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID,
            createFbbPickupPointRequest.getItemSku(), createFbbPickupPointRequest.getPickupPointCode(), false))
        .thenReturn(itemPickupPoint);
    Mockito.when(itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        createFbbPickupPointRequest.getItemSku())).thenReturn(itemPickupPoint2);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);
    try {
      Assertions.assertThrows(ValidationException.class, () ->
          itemPickupPointService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest, item,
              createFbbPickupPointResponse));
    } finally {
      Mockito.verify(itemPickupPointRepository)
          .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, createFbbPickupPointRequest.getItemSku(),
              createFbbPickupPointRequest.getPickupPointCode(), false);
      Mockito.verify(itemPickupPointRepository)
          .findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(STORE_ID,
              createFbbPickupPointRequest.getItemSku());
      Mockito.verify(itemPickupPointRepository).findFirstByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
      Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
      assertNull(createFbbPickupPointResponse.getReason());
    }
  }

  @Test
  public void createFbbPickupPointTest() {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = getCreateFbbPickupPointRequest();
    CreateFbbPickupPointResponse createFbbPickupPointResponse = new CreateFbbPickupPointResponse();
    createFbbPickupPointResponse.setItemSku(createFbbPickupPointRequest.getItemSku());
    createFbbPickupPointResponse.setPickupPointCode(createFbbPickupPointRequest.getPickupPointCode());
    itemPickupPoint.setMarkForDelete(true);
    List<ItemPickupPoint> existingL5s = new ArrayList<>();
    ItemPickupPoint pickupPoint = new ItemPickupPoint();
    Price price = new Price();
    Set<Price> priceSet = new HashSet<>();
    price.setChannel(Constants.DEFAULT_CHANNEL);
    price.setListPrice(20.0);
    price.setOfferPrice(20.0);
    priceSet.add(price);
    pickupPoint.setPrice(priceSet);
    existingL5s.add(pickupPoint);
    existingL5s.add(itemPickupPoint);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false))
        .thenReturn(existingL5s);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID,
            createFbbPickupPointRequest.getItemSku(), createFbbPickupPointRequest.getPickupPointCode(), false))
        .thenReturn(null);
    Mockito.when(itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        createFbbPickupPointRequest.getItemSku())).thenReturn(itemPickupPoint2);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Mockito.when(itemPickupPointRepository.save(any())).thenReturn(itemPickupPoint);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);
    createFbbPickupPointResponse =
        itemPickupPointService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest, item,
            createFbbPickupPointResponse);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, createFbbPickupPointRequest.getItemSku(),
            createFbbPickupPointRequest.getPickupPointCode(), false);
    Mockito.verify(itemPickupPointRepository).findFirstByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(STORE_ID,
        createFbbPickupPointRequest.getItemSku());
    Mockito.verify(itemPickupPointRepository).save(any());
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointData(anyString(), any(ItemPickupPoint.class), anyString());
    Mockito.verify(objectConverterService)
        .convertToItemPickupPointChangeEventModel(any(), Mockito.anyBoolean());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), any(),
        any());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(),any());
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), any());
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    assertNull(createFbbPickupPointResponse.getReason());
  }

  @Test
  public void findItemPickupPointByItemSkusFbbActivatedDescTest() {
    itemRequestV2.setSortField(Constants.FBB_ACTIVATED);
    itemRequestV2.setSortOrder(Constants.DESC);
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Sort.Direction.DESC, Constants.FBB_ACTIVATED,
            Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByItemSkusFbbActivatedAscTest() {
    itemRequestV2.setSortField(Constants.FBB_ACTIVATED);
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    itemPickupPointService.findItemPickupPointByItemSkus(STORE_ID, itemRequestV2, PAGE, PAGE_SIZE, false);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkuList,
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Sort.Direction.ASC, Constants.FBB_ACTIVATED,
            Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalseTest() {
    itemPickupPointService.findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(STORE_ID,ITEM_SKU);
  }

  private static CreateFbbPickupPointRequest getCreateFbbPickupPointRequest() {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = new CreateFbbPickupPointRequest();
    createFbbPickupPointRequest.setItemSku(ITEM_SKU);
    createFbbPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    createFbbPickupPointRequest.setBusinessPartnerCode(MERCHANT_CODE);
    return createFbbPickupPointRequest;
  }

  @Test
  public void updateFbbFlagTest() {
    String id = UUID.randomUUID().toString();
    ItemPickupPoint itemPickupPoint1 = ItemPickupPoint.builder().itemSku(ITEM_SKU).fbbActivated(true).build();
    itemPickupPoint1.setId(id);
    ItemPickupPoint itemPickupPoint2 = ItemPickupPoint.builder().itemSku(ITEM_SKU).fbbActivated(false).build();
    ItemPickupPoint itemPickupPoint3 = ItemPickupPoint.builder().itemSku(ITEM_SKU).fbbActivated(false).build();
    itemPickupPoint3.setId(id);
    ItemPickupPoint itemPickupPoint4 = ItemPickupPoint.builder().itemSku(ITEM_SKU_2).fbbActivated(true).build();

    when(itemPickupPointRepository.findByItemSkuInAndFbbActivatedTrueAndMarkForDeleteFalse(
        ImmutableSet.of(ITEM_SKU))).thenReturn(new ArrayList<>(Arrays.asList(itemPickupPoint3, itemPickupPoint4)));

    List<ItemPickupPoint> itemPickupPointList = itemPickupPointService.updateFbbFlag(Arrays.asList(itemPickupPoint1, itemPickupPoint2));

    verify(itemPickupPointRepository).findByItemSkuInAndFbbActivatedTrueAndMarkForDeleteFalse(
        ImmutableSet.of(ITEM_SKU));

    assertEquals(1, itemPickupPointList.size());
    assertEquals(ITEM_SKU_2, itemPickupPointList.get(0).getItemSku());
    assertTrue(itemPickupPointList.get(0).isMarkForDelete());
  }

  @Test
  public void updateFbbFlagNoExistingFbbTruePPCodeTest() {
    String id = UUID.randomUUID().toString();
    ItemPickupPoint itemPickupPoint1 = ItemPickupPoint.builder().itemSku(ITEM_SKU).fbbActivated(true).build();
    itemPickupPoint1.setId(id);
    ItemPickupPoint itemPickupPoint2 = ItemPickupPoint.builder().itemSku(ITEM_SKU).fbbActivated(false).build();

    when(itemPickupPointRepository.findByItemSkuInAndFbbActivatedTrueAndMarkForDeleteFalse(
        ImmutableSet.of(ITEM_SKU))).thenReturn(new ArrayList<>());

    List<ItemPickupPoint> itemPickupPointList = itemPickupPointService.updateFbbFlag(Arrays.asList(itemPickupPoint1, itemPickupPoint2));

    verify(itemPickupPointRepository).findByItemSkuInAndFbbActivatedTrueAndMarkForDeleteFalse(
        ImmutableSet.of(ITEM_SKU));

    assertTrue(itemPickupPointList.isEmpty());
  }

  @Test
  public void updateFbbFlagNoNewFbbTruePPCodeTest() {
    String id = UUID.randomUUID().toString();
    ItemPickupPoint itemPickupPoint1 = ItemPickupPoint.builder().itemSku(ITEM_SKU).fbbActivated(false).build();
    itemPickupPoint1.setId(id);
    ItemPickupPoint itemPickupPoint2 = ItemPickupPoint.builder().itemSku(ITEM_SKU).fbbActivated(false).build();

    List<ItemPickupPoint> itemPickupPointList = itemPickupPointService.updateFbbFlag(Arrays.asList(itemPickupPoint1, itemPickupPoint2));

    assertTrue(itemPickupPointList.isEmpty());
  }

  @Test
  public void pickupPointsAddedToOtherVariantsTest() {
    Mockito.when(
        itemPickupPointRepository.findByProductSkuAndOfflineItemIdNotInAndMarkForDeleteFalse(
            PRODUCT_SKU, ImmutableSet.of(OFFLINE_ITEM_ID))).thenReturn(Collections.singletonList(itemPickupPoint));
    itemPickupPointService.pickupPointsAddedToOtherVariants(PRODUCT_SKU, ImmutableSet.of(OFFLINE_ITEM_ID));
    Mockito.verify(itemPickupPointRepository).findByProductSkuAndOfflineItemIdNotInAndMarkForDeleteFalse(
        PRODUCT_SKU, ImmutableSet.of(OFFLINE_ITEM_ID));
  }

  @Test
  public void pickupPointsAddedToOtherVariantsEmptyTest() {
    Set<String> result =  itemPickupPointService.pickupPointsAddedToOtherVariants(PRODUCT_SKU, new HashSet<>());
    assertTrue(result.isEmpty());
  }

  @Test
  public void findItemPickupPointByProductSkusForCncTrue_Test() {
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndCncActiveTrue(
        STORE_ID, List.of(PRODUCT_SKU),
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)))).thenReturn(
        itemPickupPointPage);
    itemPickupPointService.findItemPickupPointByProductSkus(STORE_ID, List.of(PRODUCT_SKU),
        true, PAGE, PAGE_SIZE);
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuInAndMarkForDeleteFalseAndCncActiveTrue(
        STORE_ID, List.of(PRODUCT_SKU),
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void findItemPickupPointByProductSkusForCncTrue_cncForWarehouseSwitchTrueTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncBuyable(
        STORE_ID, List.of(PRODUCT_SKU), "CNC", PAGE, PAGE_SIZE,
        Sort.by(Constants.OFFLINE_ITEM_ID))).thenReturn(itemPickupPointPage);
    itemPickupPointService.findItemPickupPointByProductSkus(STORE_ID, List.of(PRODUCT_SKU),
        true, PAGE, PAGE_SIZE);
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncBuyable(
        STORE_ID, List.of(PRODUCT_SKU), "CNC", PAGE, PAGE_SIZE,
        Sort.by(Constants.OFFLINE_ITEM_ID));
  }

  @Test
  public void findItemPickupPointByProductSkusForCnc_Test() {
    PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID));
    when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, List.of(PRODUCT_SKU),
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)))).thenReturn(
        itemPickupPointPage);
    itemPickupPointService.findItemPickupPointByProductSkus(STORE_ID, List.of(PRODUCT_SKU),
        false, PAGE, PAGE_SIZE);
    verify(itemPickupPointRepository).findByStoreIdAndProductSkuInAndMarkForDeleteFalse(
        STORE_ID, List.of(PRODUCT_SKU),
        PageRequest.of(PAGE, PAGE_SIZE, Sort.by(Constants.OFFLINE_ITEM_ID)));
  }

  @Test
  public void autoCreateL5NewlyCreatedWithSameRegionTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "skipAutoCreateForExistingL5", true);
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.TRUE);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    autoCreatePickupPointRequest.setPpCodeInRegion(Collections.singleton(PICKUP_POINT_1));
    Mockito.when(
            itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(null);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setBuyable(false);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setDiscoverable(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            List.of(PICKUP_POINT_1))).thenReturn(List.of(itemPickupPoint3, itemPickupPoint2));
    AutoCreatePickupPointResponse autoCreatePickupPointResponseResult = autoCreatePickupPointResponse;
    autoCreatePickupPointResponseResult.setPrice(10);
    Mockito.when(
        pickupPointHelperService.getAutoCreatePickupPointResponse(any(),
            any())).thenReturn(autoCreatePickupPointResponseResult);
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        eq(false))).thenReturn(eventModel);
    item.setMerchantSku(MERCHANT_SKU);
    itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
        autoCreatePickupPointResponse, ProductType.BOPIS);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, List.of(PICKUP_POINT_1));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(null, null);
    verify(itemPickupPointRepository).save(itemPickupPointArgumentCaptor.capture());
    verify(pickupPointHelperService).getAutoCreatePickupPointResponse(any(), any());
    Assertions.assertEquals(MERCHANT_SKU, itemPickupPointArgumentCaptor.getValue().getMerchantSku());
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(),Mockito.any(),Mockito.anyString());
    assertFalse(autoCreatePickupPointResponse.isCncBuyable());
    assertTrue(autoCreatePickupPointResponse.isNewlyCreated());
    assertFalse(autoCreatePickupPointResponse.isBuyable());
    assertEquals(prices.stream().findFirst().orElse(new Price()).getOfferPrice(),
        autoCreatePickupPointResponse.getPrice(),0);
  }

  @Test
  public void autoCreateL5NewlyCreatedWithSameRegionTestSwitchOn() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "skipAutoCreateForExistingL5", true);
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.TRUE);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    autoCreatePickupPointRequest.setPpCodeInRegion(Collections.singleton(PICKUP_POINT_1));
    Mockito.when(
            itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(null);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(any(), any()))
        .thenReturn(businessPartnerData);
    AutoCreatePickupPointResponse autoCreatePickupPointResponseResult = autoCreatePickupPointResponse;
    autoCreatePickupPointResponseResult.setPrice(10);
    Mockito.when(
        pickupPointHelperService.getAutoCreatePickupPointResponse(any(),
            any())).thenReturn(autoCreatePickupPointResponseResult);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setBuyable(false);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setDiscoverable(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            List.of(PICKUP_POINT_1))).thenReturn(List.of(itemPickupPoint3, itemPickupPoint2));
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        Mockito.anyBoolean())).thenReturn(eventModel);
    item.setMerchantSku(MERCHANT_SKU);
    itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
        autoCreatePickupPointResponse, ProductType.BOPIS);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, List.of(PICKUP_POINT_1));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(itemPickupPointRepository).save(itemPickupPointArgumentCaptor.capture());
    verify(pickupPointHelperService).getAutoCreatePickupPointResponse(any(), any());
    Assertions.assertEquals(MERCHANT_SKU, itemPickupPointArgumentCaptor.getValue().getMerchantSku());
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(),Mockito.any(),Mockito.anyString());
    assertEquals(MERCHANT_SKU, itemPickupPointArgumentCaptor.getValue().getMerchantSku());
    verify(cacheEvictHelperService).evictItemPickupPointData(anyString(), any(), anyString());
    assertTrue(autoCreatePickupPointResponse.isNewlyCreated());
    assertFalse(autoCreatePickupPointResponse.isBuyable());
    assertFalse(autoCreatePickupPointResponse.isCncBuyable());
    assertEquals(prices.stream().findFirst().orElse(new Price()).getOfferPrice(),
        autoCreatePickupPointResponse.getPrice(),0);
  }

  @Test
  public void autoCreateL5NewlyCreatedWithSameRegionTestSwitchOnDeliveryFalse() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "skipAutoCreateForExistingL5", true);
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.FALSE);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    autoCreatePickupPointRequest.setPpCodeInRegion(Collections.singleton(PICKUP_POINT_1));
    Mockito.when(
            itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(null);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setBuyable(false);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setDiscoverable(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            List.of(PICKUP_POINT_1))).thenReturn(List.of(itemPickupPoint3, itemPickupPoint2));
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        eq(false))).thenReturn(eventModel);
    item.setMerchantSku(MERCHANT_SKU);
    itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
        autoCreatePickupPointResponse, ProductType.BOPIS);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(null, null);
    verify(itemPickupPointRepository).save(itemPickupPointArgumentCaptor.capture());
    Assertions.assertEquals(MERCHANT_SKU, itemPickupPointArgumentCaptor.getValue().getMerchantSku());
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(),Mockito.any(),Mockito.anyString());
    assertTrue(autoCreatePickupPointResponse.isNewlyCreated());
    assertFalse(autoCreatePickupPointResponse.isBuyable());
    assertFalse(autoCreatePickupPointResponse.isCncBuyable());
    assertEquals(prices.stream().findFirst().orElse(new Price()).getOfferPrice(),
        autoCreatePickupPointResponse.getPrice(),10);
  }

  @Test
  public void autoCreateL5NewlyCreatedWithSameRegion2Test() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "skipAutoCreateForExistingL5", true);
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.TRUE);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    autoCreatePickupPointRequest.setPpCodeInRegion(Collections.singleton(PICKUP_POINT_1));
    itemPickupPoint3.setMarkForDelete(true);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
            autoCreatePickupPointRequest.getWebItemSku(), autoCreatePickupPointRequest.getPickupPointCode(), false))
        .thenReturn(null);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(any(), any()))
        .thenReturn(businessPartnerData);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setBuyable(false);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setDiscoverable(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            List.of(PICKUP_POINT_1))).thenReturn(List.of(itemPickupPoint3, itemPickupPoint2));
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        Mockito.anyBoolean())).thenReturn(eventModel);
    AutoCreatePickupPointResponse autoCreatePickupPointResponseResult = autoCreatePickupPointResponse;
    autoCreatePickupPointResponseResult.setPrice(10);
    Mockito.when(
        pickupPointHelperService.getAutoCreatePickupPointResponse(any(),
            any())).thenReturn(autoCreatePickupPointResponseResult);
    itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
        autoCreatePickupPointResponse, ProductType.BOPIS);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, List.of(PICKUP_POINT_1));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(itemPickupPointRepository).save(any());
    verify(cacheEvictHelperService).evictItemPickupPointData(anyString(), any(), anyString());
    verify(pickupPointHelperService).getAutoCreatePickupPointResponse(any(), any());
    assertTrue(autoCreatePickupPointResponse.isNewlyCreated());
    assertFalse(autoCreatePickupPointResponse.isBuyable());
    assertFalse(autoCreatePickupPointResponse.isCncBuyable());
    assertEquals(prices.stream().findFirst().orElse(new Price()).getOfferPrice(),
        autoCreatePickupPointResponse.getPrice(),0);
  }

  @Test
  public void autoCreateL5NewlyCreatedWithSameRegion2DeliveryFalseTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "skipAutoCreateForExistingL5", true);
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.FALSE);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    autoCreatePickupPointRequest.setPpCodeInRegion(Collections.singleton(PICKUP_POINT_1));
    itemPickupPoint3.setMarkForDelete(true);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
            autoCreatePickupPointRequest.getWebItemSku(), autoCreatePickupPointRequest.getPickupPointCode(), false))
        .thenReturn(null);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setBuyable(false);
    itemPickupPoint2.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setDiscoverable(true);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            List.of(PICKUP_POINT_1))).thenReturn(List.of(itemPickupPoint3, itemPickupPoint2));
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        eq(false))).thenReturn(eventModel);
    itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
        autoCreatePickupPointResponse, ProductType.BOPIS);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(null, null);
    verify(itemPickupPointRepository).save(Mockito.any());
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(),Mockito.any(),Mockito.anyString());
    assertTrue(autoCreatePickupPointResponse.isNewlyCreated());
    assertFalse(autoCreatePickupPointResponse.isBuyable());
    assertFalse(autoCreatePickupPointResponse.isCncBuyable());
    assertEquals(prices.stream().findFirst().orElse(new Price()).getOfferPrice(),
        autoCreatePickupPointResponse.getPrice(),10);
  }

  @Test
  public void autoCreateForExistingL5Test() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointService, "skipAutoCreateForExistingL5", true);
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.TRUE);
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    itemPickupPoint3.setMarkForDelete(false);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
            autoCreatePickupPointRequest.getWebItemSku(), autoCreatePickupPointRequest.getPickupPointCode(), false))
        .thenReturn(itemPickupPoint3);
    AutoCreatePickupPointResponse autoCreatePickupPointResponseResult = autoCreatePickupPointResponse;
    autoCreatePickupPointResponseResult.setPrice(10);
    Mockito.when(
        pickupPointHelperService.getAutoCreatePickupPointResponse(any(),
            any())).thenReturn(autoCreatePickupPointResponseResult);
    AutoCreatePickupPointResponse autoCreatePickupPointResponse1 =
        itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
            autoCreatePickupPointResponse, ProductType.BOPIS);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        autoCreatePickupPointRequest.getWebItemSku(), autoCreatePickupPointRequest.getPickupPointCode(), false);
    assertFalse(autoCreatePickupPointResponse1.isNewlyCreated());
    verify(pickupPointHelperService).getAutoCreatePickupPointResponse(any(), any());
    Assertions.assertFalse(autoCreatePickupPointResponse1.isNewlyCreated());
    assertFalse(autoCreatePickupPointResponse.isCncBuyable());
  }

  @Test
  public void autoCreateL5NewlyCreatedWithSameRegionOfflineTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    autoCreatePickupPointRequest.setPpCodeInRegion(Collections.singleton(PICKUP_POINT_1));
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.TRUE);
    Mockito.when(
            itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(any(), any()))
        .thenReturn(businessPartnerData);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            List.of(PICKUP_POINT_1))).thenReturn(List.of(itemPickupPoint2));
    itemPickupPoint3.getPrice().stream().findFirst().orElse(new Price()).setOfferPrice(40);
    itemPickupPointList.add(itemPickupPoint3);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false))
        .thenReturn(itemPickupPointList);
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        Mockito.anyBoolean())).thenReturn(eventModel);
    AutoCreatePickupPointResponse autoCreatePickupPointResponseResult = autoCreatePickupPointResponse;
    autoCreatePickupPointResponseResult.setPrice(40);
    Mockito.when(
        pickupPointHelperService.getAutoCreatePickupPointResponse(any(),
            any())).thenReturn(autoCreatePickupPointResponseResult);
    itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
        autoCreatePickupPointResponse, ProductType.BOPIS);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, List.of(PICKUP_POINT_1));
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(itemPickupPointRepository).save(any());
    verify(pickupPointHelperService).getAutoCreatePickupPointResponse(any(), any());
    verify(cacheEvictHelperService).evictItemPickupPointData(anyString(), any(), anyString());
    assertFalse(autoCreatePickupPointResponse.isNewlyCreated());
    assertFalse(autoCreatePickupPointResponse.isBuyable());
    assertFalse(autoCreatePickupPointResponse.isCncBuyable());
    assertEquals(40, autoCreatePickupPointResponse.getPrice(), 0);
  }

  @Test
  public void autoCreateL5NewlyCreatedWithOfflineTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.TRUE);
    Mockito.when(
            itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(null);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            List.of(PICKUP_POINT_1))).thenReturn(List.of(itemPickupPoint2));
    AutoCreatePickupPointResponse autoCreatePickupPointResponseResult = autoCreatePickupPointResponse;
    autoCreatePickupPointResponseResult.setPrice(40);
    Mockito.when(
        pickupPointHelperService.getAutoCreatePickupPointResponse(any(),
            any())).thenReturn(autoCreatePickupPointResponseResult);
    itemPickupPoint3.getPrice().stream().findFirst().orElse(new Price()).setOfferPrice(40);
    itemPickupPointList.add(itemPickupPoint3);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false))
        .thenReturn(itemPickupPointList);
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        eq(false))).thenReturn(eventModel);
    itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
        autoCreatePickupPointResponse, ProductType.BOPIS);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(null, null);
    verify(itemPickupPointRepository).save(Mockito.any());
    verify(pickupPointHelperService).getAutoCreatePickupPointResponse(any(), any());
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(),Mockito.any(),Mockito.anyString());
    assertTrue(autoCreatePickupPointResponse.isNewlyCreated());
    assertFalse(autoCreatePickupPointResponse.isBuyable());
    assertFalse(autoCreatePickupPointResponse.isCncBuyable());
    assertEquals(40, autoCreatePickupPointResponse.getPrice(), 0);
  }

  @Test
  public void autoCreateL5NewlyCreatedWithOfflineTestCncTrue() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.FALSE);
    Mockito.when(
            itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(null);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(any(), any()))
        .thenReturn(businessPartnerData);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            List.of(PICKUP_POINT_1))).thenReturn(List.of(itemPickupPoint2));
    itemPickupPoint3.getPrice().stream().findFirst().orElse(new Price()).setOfferPrice(40);
    itemPickupPoint3.setCncActive(Boolean.TRUE);
    itemPickupPoint3.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setBuyable(false);
    itemPickupPoint3.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig()).setDiscoverable(false);
    itemPickupPointList.add(itemPickupPoint3);
    AutoCreatePickupPointResponse autoCreatePickupPointResponseResult = autoCreatePickupPointResponse;
    autoCreatePickupPointResponseResult.setPrice(40);
    Mockito.when(
        pickupPointHelperService.getAutoCreatePickupPointResponse(any(),
            any())).thenReturn(autoCreatePickupPointResponseResult);
    Mockito.when(itemPickupPointRepository.findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false))
        .thenReturn(itemPickupPointList);
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        Mockito.anyBoolean())).thenReturn(eventModel);
    itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
        autoCreatePickupPointResponse, ProductType.BOPIS);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(itemPickupPointRepository).save(any());
    verify(cacheEvictHelperService).evictItemPickupPointData(anyString(), any(), anyString());
    verify(pickupPointHelperService).getAutoCreatePickupPointResponse(any(), any());
    assertTrue(autoCreatePickupPointResponse.isNewlyCreated());
    assertFalse(autoCreatePickupPointResponse.isBuyable());
    assertFalse(autoCreatePickupPointResponse.isCncBuyable());
    assertEquals(40, autoCreatePickupPointResponse.getPrice(), 0);
  }

  @Test
  public void autoCreateL5NewlyCreatedWithSameRegionRegularProductTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.TRUE);
    autoCreatePickupPointRequest.setPpCodeInRegion(Collections.singleton(PICKUP_POINT_1));
    Mockito.when(
            itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(null);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(any(), any()))
        .thenReturn(businessPartnerData);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            List.of(PICKUP_POINT_1))).thenReturn(List.of(itemPickupPoint3));
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        Mockito.anyBoolean())).thenReturn(eventModel);
    AutoCreatePickupPointResponse autoCreatePickupPointResponseResult = autoCreatePickupPointResponse;
    autoCreatePickupPointResponseResult.setPrice(10);
    autoCreatePickupPointResponseResult.setBuyable(true);
    Mockito.when(
        pickupPointHelperService.getAutoCreatePickupPointResponse(any(),
            any())).thenReturn(autoCreatePickupPointResponseResult);
    AutoCreatePickupPointResponse autoCreatePickupPointResponse1 = itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
        autoCreatePickupPointResponse, ProductType.REGULAR);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, List.of(PICKUP_POINT_1));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(null, null);
    verify(itemPickupPointRepository).save(Mockito.any());
    verify(pickupPointHelperService).getAutoCreatePickupPointResponse(any(), any());
    verify(cacheEvictHelperService).evictItemPickupPointData(Mockito.anyString(),Mockito.any(),Mockito.anyString());
    assertTrue(autoCreatePickupPointResponse1.isNewlyCreated());
    assertTrue(autoCreatePickupPointResponse1.isBuyable());
    assertFalse(autoCreatePickupPointResponse1.isCncBuyable());
    assertEquals(prices.stream().findFirst().orElse(new Price()).getOfferPrice(),
        autoCreatePickupPointResponse1.getPrice(),0);
  }

  @Test
  public void autoCreateL5NewlyCreatedWithSameRegionRegularProductTestSwitchOn() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, REQUEST_ID, REQUEST_ID, REQUEST_ID, USERNAME,
            USERNAME);
    ReflectionTestUtils.setField(itemPickupPointService, "cncForWarehouseFeatureSwitch", true);
    autoCreatePickupPointRequest.setDeliveryFlag(Boolean.TRUE);
    autoCreatePickupPointRequest.setPpCodeInRegion(Collections.singleton(PICKUP_POINT_1));
    Mockito.when(
            itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(null);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartnerData);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
            List.of(PICKUP_POINT_1))).thenReturn(List.of(itemPickupPoint3));
    ItemPickupPointDataChangeEventModel eventModel = new ItemPickupPointDataChangeEventModel();
    AutoCreatePickupPointResponse autoCreatePickupPointResponseResult = autoCreatePickupPointResponse;
    autoCreatePickupPointResponseResult.setPrice(10);
    autoCreatePickupPointResponseResult.setBuyable(true);
    Mockito.when(
        pickupPointHelperService.getAutoCreatePickupPointResponse(any(),
            any())).thenReturn(autoCreatePickupPointResponseResult);
    Mockito.when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        eq(false))).thenReturn(eventModel);
    AutoCreatePickupPointResponse autoCreatePickupPointResponse1 = itemPickupPointService.autoCreateL5(autoCreatePickupPointRequest, mandatoryRequestParam, item,
        autoCreatePickupPointResponse, ProductType.REGULAR);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, false);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, List.of(PICKUP_POINT_1));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), anyString(),
        eq(eventModel));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(null, null);
    verify(pickupPointHelperService).getAutoCreatePickupPointResponse(any(), any());
    verify(itemPickupPointRepository).save(any());
    verify(cacheEvictHelperService).evictItemPickupPointData(anyString(), any(), anyString());
    assertTrue(autoCreatePickupPointResponse1.isNewlyCreated());
    assertTrue(autoCreatePickupPointResponse1.isBuyable());
    assertFalse(autoCreatePickupPointResponse1.isCncBuyable());
    assertEquals(prices.stream().findFirst().orElse(new Price()).getOfferPrice(),
        autoCreatePickupPointResponse1.getPrice(),0);
  }

  @Test
  public void findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalseTest() {
    Mockito.when(
        itemPickupPointRepository.findFirstByStoreIdAndProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(
            STORE_ID, PRODUCT_SKU, true, ImmutableSet.of(PICKUP_POINT_CODE))).thenReturn(itemPickupPoint);
    itemPickupPointService.findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU, true, ImmutableSet.of(PICKUP_POINT_CODE));
    Mockito.verify(itemPickupPointRepository)
        .findFirstByStoreIdAndProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
            true, ImmutableSet.of(PICKUP_POINT_CODE));
  }

  @Test
  public void findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(null,
        PRODUCT_SKU, true, ImmutableSet.of(PICKUP_POINT_CODE)));
  }

  @Test
  public void findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyProductSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID, null,
        true, ImmutableSet.of(PICKUP_POINT_CODE)));
  }

  @Test
  public void findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyPickupPointCodesTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByProductSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU, true, new HashSet<>()));
  }

  @Test
  public void findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalseTest() {
    Mockito.when(
        itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(
            STORE_ID, ITEM_SKU, true, ImmutableSet.of(PICKUP_POINT_CODE))).thenReturn(itemPickupPoint);
    itemPickupPointService.findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        true, ImmutableSet.of(PICKUP_POINT_CODE));
    Mockito.verify(itemPickupPointRepository)
        .findFirstByStoreIdAndItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true,
            ImmutableSet.of(PICKUP_POINT_CODE));
  }

  @Test
  public void findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(null, ITEM_SKU,
        true, ImmutableSet.of(PICKUP_POINT_CODE)));
  }

  @Test
  public void findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyItemSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID, null,
        true, ImmutableSet.of(PICKUP_POINT_CODE)));
  }

  @Test
  public void findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyPickupPointCodesTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByItemSkuAndCncActiveAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        true, new HashSet<>()));
  }

  @Test
  public void findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalseTest() {
    Mockito.when(
            itemPickupPointRepository.findFirstByItemSkuAndCncBuyableConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
                STORE_ID, ITEM_SKU, ImmutableSet.of(PICKUP_POINT_CODE), true))
        .thenReturn(itemPickupPoint);
    itemPickupPointService.findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU, ImmutableSet.of(PICKUP_POINT_CODE), true);
    Mockito.verify(itemPickupPointRepository)
        .findFirstByItemSkuAndCncBuyableConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKU, ImmutableSet.of(PICKUP_POINT_CODE), true);
  }

  @Test
  public void findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        null, ITEM_SKU, ImmutableSet.of(PICKUP_POINT_CODE), true));
  }

  @Test
  public void findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyItemSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        STORE_ID, null, ImmutableSet.of(PICKUP_POINT_CODE), true));
  }

  @Test
  public void findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyPickupPointCodesTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByItemSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU, new HashSet<>(), true));
  }

  @Test
  public void findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalseTest() {
    Mockito.when(
            itemPickupPointRepository.findFirstByProductSkuAndCncBuyableConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
                STORE_ID, PRODUCT_SKU, ImmutableSet.of(PICKUP_POINT_CODE), true))
        .thenReturn(itemPickupPoint);
    itemPickupPointService.findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        STORE_ID, PRODUCT_SKU, ImmutableSet.of(PICKUP_POINT_CODE), true);
    Mockito.verify(itemPickupPointRepository)
        .findFirstByProductSkuAndCncBuyableConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
            STORE_ID, PRODUCT_SKU, ImmutableSet.of(PICKUP_POINT_CODE), true);
  }

  @Test
  public void findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        null, PRODUCT_SKU, ImmutableSet.of(PICKUP_POINT_CODE), true));
  }

  @Test
  public void findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyProductSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        STORE_ID, null, ImmutableSet.of(PICKUP_POINT_CODE), true));
  }

  @Test
  public void findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalseEmptyPickupPointCodesTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findOneByProductSkuAndCncActiveConfigAndPickupPointCodeNotInAndMarkForDeleteFalse(
        STORE_ID, PRODUCT_SKU, new HashSet<>(), true));
  }

  @Test
  public void findFbbTrueOnlinePickupPointsAndItemSkusInTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "itemPickupPointMaxFetchSize", 10);
    Mockito.when(itemPickupPointRepository.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
        Collections.singletonList(ITEM_SKU))).thenReturn(itemPickupPointList);
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList =
        itemPickupPointService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
            Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointRepository)
        .findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, Collections.singletonList(ITEM_SKU));
    assertEquals(ITEM_SKU, itemSkuPickupPointCodeResponseList.get(0).getItemSku());
  }

  @Test
  public void findFbbTrueOnlinePickupPointsAndItemSkusInBuyableScheduleTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "itemPickupPointMaxFetchSize", 10);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().setBuyable(false);
    Mockito.when(itemPickupPointRepository.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
        Collections.singletonList(ITEM_SKU))).thenReturn(itemPickupPointList);
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList =
        itemPickupPointService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
            Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointRepository)
        .findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, Collections.singletonList(ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(itemSkuPickupPointCodeResponseList));
  }

  @Test
  public void findFbbTrueOnlinePickupPointsAndItemSkusInBuyableScheduleNullTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "itemPickupPointMaxFetchSize", 10);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().setItemBuyableSchedules(null);
    Mockito.when(itemPickupPointRepository.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
        Collections.singletonList(ITEM_SKU))).thenReturn(itemPickupPointList);
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList =
        itemPickupPointService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
            Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointRepository)
        .findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, Collections.singletonList(ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(itemSkuPickupPointCodeResponseList));
  }

  @Test
  public void findFbbTrueOnlinePickupPointsAndItemSkusInBuyableScheduleNullEndDateTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "itemPickupPointMaxFetchSize", 10);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().getItemBuyableSchedules()
        .setStartDateTime(new Date());
    Mockito.when(itemPickupPointRepository.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
        Collections.singletonList(ITEM_SKU))).thenReturn(itemPickupPointList);
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList =
        itemPickupPointService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
            Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointRepository)
        .findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, Collections.singletonList(ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(itemSkuPickupPointCodeResponseList));
  }

  @Test
  public void findFbbTrueOnlinePickupPointsAndItemSkusInBuyableScheduleFalseTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "itemPickupPointMaxFetchSize", 10);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().getItemBuyableSchedules()
        .setStartDateTime(Date.from(Instant.now().plus(Duration.ofDays(-2))));
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().getItemBuyableSchedules()
        .setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(2))));
    Mockito.when(itemPickupPointRepository.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
        Collections.singletonList(ITEM_SKU))).thenReturn(itemPickupPointList);
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList =
        itemPickupPointService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
            Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointRepository)
        .findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, Collections.singletonList(ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(itemSkuPickupPointCodeResponseList));
  }

  @Test
  public void findFbbTrueOnlinePickupPointsAndItemSkusInBuyableScheduleBeforeStartDateTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "itemPickupPointMaxFetchSize", 10);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().getItemBuyableSchedules()
        .setStartDateTime(Date.from(Instant.now().plus(Duration.ofDays(2))));
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().getItemBuyableSchedules()
        .setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(2))));
    Mockito.when(itemPickupPointRepository.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
        Collections.singletonList(ITEM_SKU))).thenReturn(itemPickupPointList);
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList =
        itemPickupPointService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
            Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointRepository)
        .findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, Collections.singletonList(ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(itemSkuPickupPointCodeResponseList));
  }

  @Test
  public void findFbbTrueOnlinePickupPointsAndItemSkusInBuyableScheduleAfterEndDateTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "itemPickupPointMaxFetchSize", 10);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().getItemBuyableSchedules()
        .setStartDateTime(Date.from(Instant.now().plus(Duration.ofDays(-2))));
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().getItemBuyableSchedules()
        .setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(-2))));
    Mockito.when(itemPickupPointRepository.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
        Collections.singletonList(ITEM_SKU))).thenReturn(itemPickupPointList);
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList =
        itemPickupPointService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
            Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointRepository)
        .findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, Collections.singletonList(ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(itemSkuPickupPointCodeResponseList));
  }

  @Test
  public void findFbbTrueOnlinePickupPointsAndItemSkusInBuyableScheduleTrueTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "itemPickupPointMaxFetchSize", 10);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().getItemBuyableSchedules()
        .setStartDateTime(Date.from(Instant.now().plus(Duration.ofDays(-2))));
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().getItemBuyableSchedules()
        .setEndDateTime(Date.from(Instant.now().plus(Duration.ofDays(2))));
    itemPickupPointList.get(0).getItemViewConfig().iterator().next().getItemBuyableSchedules().setBuyable(true);
    Mockito.when(itemPickupPointRepository.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
        Collections.singletonList(ITEM_SKU))).thenReturn(itemPickupPointList);
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointCodeResponseList =
        itemPickupPointService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
            Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointRepository)
        .findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, Collections.singletonList(ITEM_SKU));
    assertEquals(ITEM_SKU, itemSkuPickupPointCodeResponseList.get(0).getItemSku());
  }

  @Test
  public void findFbbTrueOnlinePickupPointsAndItemSkusInExceptionTest() {
    ReflectionTestUtils.setField(itemPickupPointService, "itemPickupPointMaxFetchSize", 0);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, Collections.singletonList(ITEM_SKU)));
  }

  @Test
  public void findByItemSkuAndPickupPointCodeReadFromPrimaryTest() {
    itemPickupPointService.findByItemSkuAndPickupPointCodeReadFromPrimary(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, true);
  }

  @Test
  public void findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalseReadFromPrimaryTest() {
    itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalseReadFromPrimary(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE, true);
  }

  @Test
  public void findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimaryTest() {
    itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true);
  }

  @Test
  public void findByStoreIdAndProductSkuTest() {
    itemPickupPointService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false);
  }

  @Test
  public void findByStoreIdAndProductSkuReadFromPrimaryTest() {
    itemPickupPointService.findByStoreIdAndProductSkuReadFromPrimary(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointRepository).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, true);
  }

  @Test
  public void findByStoreIdAndItemSkuInAndMarkForDeleteFalseTest() {
    itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(ITEM_SKU), false);
  }

  @Test
  public void findByStoreIdAndItemSkuInAndMarkForDeleteFalseReadFromPrimaryTest() {
    itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalseReadFromPrimary(STORE_ID,
        Collections.singletonList(ITEM_SKU));
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, Collections.singletonList(ITEM_SKU), true);
  }

  @Test
  public void findItemPickupPointsByProductSkuAndPickupPointCodesReadFromPrimaryTest() {
    itemPickupPointService.findItemPickupPointsByProductSkuAndPickupPointCodesReadFromPrimary(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT));
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndProductSkuAndPickupPointCodeInAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
            Collections.singletonList(PICKUP_POINT), true);
  }

  @Test
  public void getItemPickupPointsByProductSkuAndMarkForDeleteFalseReadFromPrimaryTest() {
    itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalseReadFromPrimary(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, true);
  }

  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimaryTest() {
    itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointRepository).countByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true);
  }

  @Test
  public void findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivatedReadFromPrimaryTest() {
    itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivatedReadFromPrimary(STORE_ID,
        Collections.singleton(ITEM_SKU), false);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuInAndFbbActivatedAndMarkForDeleteFalse(STORE_ID, Collections.singleton(ITEM_SKU), false,
            true);
  }

  @Test
  public void findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActiveReadFromPrimaryTest() {
    itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActiveReadFromPrimary(STORE_ID,
        ITEM_SKU, true);
    Mockito.verify(itemPickupPointRepository)
        .countByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(STORE_ID, ITEM_SKU, true, true);
  }

  @Test
  public void deleteItemPickupPointsFromInventoryTest() {
    List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> request =
        CommonUtil.getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOs(Collections.singletonList(itemPickupPoint));
    Mockito.when(
        inventoryOutbound.deleteByItemSkuAndPickupPointCode(GdnMandatoryRequestParameterUtil.getRequestId(), USERNAME,
            request)).thenReturn(true);
    itemPickupPointService.deleteItemPickupPointsFromInventory(Collections.singletonList(itemPickupPoint));
    Mockito.verify(inventoryOutbound)
        .deleteByItemSkuAndPickupPointCode(GdnMandatoryRequestParameterUtil.getRequestId(), USERNAME, request);
    assertEquals(itemPickupPoint.getItemSku(), request.get(0).getWebItemSku());
    assertEquals(itemPickupPoint.getPickupPointCode(), request.get(0).getPickupPointCode());
  }

  @Test
  public void deleteItemPickupPointsFromInventoryErrorTest() {
    List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> request =
        CommonUtil.getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTOs(Collections.singletonList(itemPickupPoint));
    Mockito.when(
        inventoryOutbound.deleteByItemSkuAndPickupPointCode(GdnMandatoryRequestParameterUtil.getRequestId(), USERNAME,
            request)).thenThrow(ApplicationRuntimeException.class);
    itemPickupPointService.deleteItemPickupPointsFromInventory(Collections.singletonList(itemPickupPoint));
    Mockito.verify(inventoryOutbound)
        .deleteByItemSkuAndPickupPointCode(GdnMandatoryRequestParameterUtil.getRequestId(), USERNAME, request);
  }

  @Test
  public void getItemSkuFbbMapTest() {
    String storeId = "123";
    Set<String> itemSkus = new HashSet<>(Arrays.asList("itemSku1", "itemSku2"));
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setFbbActivated(true);
    itemPickupPoint1.setItemSku("itemSku1");
    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setFbbActivated(false);
    itemPickupPoint2.setItemSku("itemSku2");
    when(itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(storeId,
        "itemSku1")).thenReturn(itemPickupPoint1);
    when(itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(storeId,
        "itemSku2")).thenReturn(itemPickupPoint2);
    Map<String, Boolean> result = itemPickupPointService.getItemSkuFbbMap(storeId, itemSkus);
    assertTrue(result.get("itemSku1"));
    assertFalse(result.get("itemSku2"));
    verify(itemPickupPointRepository).findFirstByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(
        storeId, "itemSku1");
    verify(itemPickupPointRepository).findFirstByStoreIdAndItemSkuAndFbbActivatedTrueAndMarkForDeleteFalse(
        storeId, "itemSku2");
  }

  @Test
  public void updateSubscriptionFlagItemPickupPointNullTest() {
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.updateSubscriptionFlag(blimartSubscriptionChangeRequest));
    } finally {
      Mockito.verify(itemPickupPointRepository)
          .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, true);
    }
  }

  @Test
  public void updateSubscriptionFlagItemNullTest() {
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
            true)).thenReturn(itemPickupPoint);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.updateSubscriptionFlag(blimartSubscriptionChangeRequest));
    } finally {
      Mockito.verify(itemPickupPointRepository)
          .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, true);
      Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    }
  }

  @Test
  public void updateSubscriptionFlagItemTrueTest() {
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
            true)).thenReturn(itemPickupPoint);
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(item);
    blimartSubscriptionChangeRequest.setSubscribable(true);
    itemPickupPointService.updateSubscriptionFlag(blimartSubscriptionChangeRequest);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, true);
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(itemService).saveItems(itemListArgumentCaptor.capture());
    Mockito.verify(itemPickupPointRepository).save(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(cacheEvictHelperService).evictItemCache(item.getStoreId(), item);
    Mockito.verify(cacheEvictHelperService)
        .evictItemPickupPointData(item.getStoreId(), itemPickupPoint, itemPickupPoint.getPickupPointCode());
    assertTrue(itemListArgumentCaptor.getValue().get(0).isSubscribable());
    assertTrue(itemPickupPointArgumentCaptor.getValue().isSubscribable());
  }

  @Test
  public void updateSubscriptionFlagItemTrueSavedItemTrueTest() {
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
            true)).thenReturn(itemPickupPoint);
    item.setSubscribable(true);
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(item);
    blimartSubscriptionChangeRequest.setSubscribable(true);
    itemPickupPointService.updateSubscriptionFlag(blimartSubscriptionChangeRequest);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, true);
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointRepository).save(itemPickupPointArgumentCaptor.capture());
    assertTrue(itemPickupPointArgumentCaptor.getValue().isSubscribable());
  }

  @Test
  public void updateSubscriptionFlagItemFalseTest() {
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
            true)).thenReturn(itemPickupPoint);
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(item);
    blimartSubscriptionChangeRequest.setSubscribable(false);
    itemPickupPointService.updateSubscriptionFlag(blimartSubscriptionChangeRequest);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, true);
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(itemService).saveItems(itemListArgumentCaptor.capture());
    Mockito.verify(itemPickupPointRepository).save(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndSubscribableAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true, true);
    assertFalse(itemListArgumentCaptor.getValue().get(0).isSubscribable());
    assertFalse(itemPickupPointArgumentCaptor.getValue().isSubscribable());
  }

  @Test
  public void updateSubscriptionFlagItemFalseNotItemSaveTest() {
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
            true)).thenReturn(itemPickupPoint);
    Mockito.when(itemService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(item);
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndItemSkuAndSubscribableAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true,
            true)).thenReturn(itemPickupPointList);
    blimartSubscriptionChangeRequest.setSubscribable(false);
    itemPickupPointService.updateSubscriptionFlag(blimartSubscriptionChangeRequest);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE, true);
    Mockito.verify(itemService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(itemPickupPointRepository).save(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndItemSkuAndSubscribableAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, true, true);
    assertFalse(itemPickupPointArgumentCaptor.getValue().isSubscribable());
  }

  @Test
  public void findOneForEachItemSkuInTest() {
    when(itemPickupPointRepository.findFirstByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(
        itemPickupPoint);
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.findOneForEachItemSkuIn(STORE_ID, List.of(ITEM_SKU));
    verify(itemPickupPointRepository).findFirstByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    assertEquals(ITEM_SKU, itemPickupPoints.get(0).getItemSku());
  }
  //  @Override
  //  public List<ItemPickupPoint> fetchBasicDetailsByItemSkuAndPickupPointCodeList(String storeId,
  //    List<ItemPickupPointRequest> itemPickupPointRequest) {
  //    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
  //    checkArgument(CollectionUtils.isNotEmpty(itemPickupPointRequest),
  //      ErrorMessages.CANNOT_FIND_ITEM_PICKUP_POINT_FOR_THE_REQUEST);
  //    List<ItemPickupPointRequestVo> itemPickupPointRequestVoList =
  //      ProductAndItemsUtil.toItemPickupPointRequestVo(itemPickupPointRequest);
  //    return Lists.partition(itemPickupPointRequestVoList, itemPickupPointFetchSize).stream().flatMap(
  //      itemPickupPointRequestVoSubList -> itemPickupPointRepository.fetchBasicDataByItemSkuAndPickupPointCode(
  //        storeId, itemPickupPointRequestVoSubList).stream()).collect(Collectors.toList());
  //  }

  @Test
  public void fetchBasicDetailsByItemSkuAndPickupPointCodeListTest(){
    List<ItemPickupPointRequest> itemPickupPointRequestList = new ArrayList<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointRequestList.add(itemPickupPointRequest);
    ItemPickupPointRequestVo itemPickupPointRequestVo = new ItemPickupPointRequestVo();
    BeanUtils.copyProperties(itemPickupPointRequest, itemPickupPointRequestVo);
    Mockito.when(itemPickupPointRepository.fetchBasicDataByItemSkuAndPickupPointCode(
            Constants.DEFAULT_STORE_ID, Collections.singletonList(itemPickupPointRequestVo)))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.fetchBasicDetailsByItemSkuAndPickupPointCodeList(
            Constants.DEFAULT_STORE_ID, itemPickupPointRequestList);
    assertTrue(CollectionUtils.isNotEmpty(itemPickupPoints));
    Mockito.verify(itemPickupPointRepository).fetchBasicDataByItemSkuAndPickupPointCode(
        Constants.DEFAULT_STORE_ID, Collections.singletonList(itemPickupPointRequestVo));
  }

  @Test
  public void fetchBasicDetailsByItemSkuAndPickupPointCodeListWithEmptyListTest(){
    List<ItemPickupPointRequest> itemPickupPointRequestList = new ArrayList<>();
    ItemPickupPointRequestVo itemPickupPointRequestVo = new ItemPickupPointRequestVo();
    Mockito.when(itemPickupPointRepository.fetchBasicDataByItemSkuAndPickupPointCode(
            Constants.DEFAULT_STORE_ID, Collections.singletonList(itemPickupPointRequestVo)))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        itemPickupPointService.fetchBasicDetailsByItemSkuAndPickupPointCodeList(
            Constants.DEFAULT_STORE_ID, itemPickupPointRequestList));
  }

  @Test
  public void fetchBasicDetailsByItemSkuAndPickupPointCodeListWithStoreIdEmptyTest(){
    List<ItemPickupPointRequest> itemPickupPointRequestList = new ArrayList<>();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointRequestList.add(itemPickupPointRequest);
    ItemPickupPointRequestVo itemPickupPointRequestVo = new ItemPickupPointRequestVo();
    BeanUtils.copyProperties(itemPickupPointRequest, itemPickupPointRequestVo);
    Mockito.when(itemPickupPointRepository.fetchBasicDataByItemSkuAndPickupPointCode(
            Constants.DEFAULT_STORE_ID, Collections.singletonList(itemPickupPointRequestVo)))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        itemPickupPointService.fetchBasicDetailsByItemSkuAndPickupPointCodeList(
            StringUtils.EMPTY, itemPickupPointRequestList));
  }

  @Test
  public void findByStoreIdAndOfflineItemIdsAndcncBuyableAndMarkForDeleteFalseAndLimitTest() {
    Mockito.when(
        itemPickupPointRepository.findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(any(), anySet(),
            anyBoolean(), eq(Constants.CNC), anyInt())).thenReturn(Collections.singletonList(itemPickupPoint));
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(STORE_ID,
            Collections.singleton(OFFLINE_ITEM_ID), true, Constants.CNC, 10);
    Assertions.assertEquals(itemPickupPoints.size(), 1);
    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndOfflineItemIdsAndBuyableAndMarkForDeleteFalseAndLimit(any(), anySet(), anyBoolean(),
            eq(Constants.CNC), anyInt());
  }

  @Test
  public void findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalseTest() {
    Mockito.when(
        itemPickupPointRepository.findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
            true)).thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPoint =
        itemPickupPointService.findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(STORE_ID,
            PRODUCT_SKU, true);
    Assertions.assertNotNull(itemPickupPoint);
    Mockito.verify(itemPickupPointRepository)
        .findFirstByProductSkuAndCncBuyableConfigAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, true);
  }

  @Test
  public void updatePwpFlagsByItemSkuAndPickupPointCodeSuccessTest() {
    PricingPwpPromoEvent pricingPwpPromoEvent =
        PricingPwpPromoEvent.builder().storeId(STORE_ID).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .isPartOfPendingPWPAsMain(true).isPartOfActivePWPAsMain(false).isPartOfPendingPWPAsAdditional(true)
            .isPartOfActivePWPAsAdditional(false).build();

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(STORE_ID);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setActivePromoBundlings(new HashSet<>());

    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
        true)).thenReturn(itemPickupPoint);
    when(itemPickupPointRepository.save(itemPickupPoint)).thenReturn(itemPickupPoint);
    when(productService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(new Product());
    itemPickupPointService.updatePwpFlagsByItemSkuAndPickupPointCode(pricingPwpPromoEvent);

    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
        true);
    verify(itemPickupPointRepository).save(itemPickupPoint);
    verify(productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(new Product(), new ArrayList<>(), false);
    assertTrue(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_MAIN_PENDING));
    assertFalse(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_MAIN_ACTIVE));
    assertTrue(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_ADDITIONAL_PENDING));
    assertFalse(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_ADDITIONAL_ACTIVE));
  }

  @Test
  public void updatePwpFlagsByItemSkuAndPickupPointCodeRemoveExistingFlagsTest() {
    PricingPwpPromoEvent pricingPwpPromoEvent =
        PricingPwpPromoEvent.builder().storeId(STORE_ID).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .isPartOfPendingPWPAsMain(false).isPartOfActivePWPAsMain(false).isPartOfPendingPWPAsAdditional(false)
            .isPartOfActivePWPAsAdditional(false).build();

    Set<String> existingPromoBundlings = new HashSet<>();
    existingPromoBundlings.add(Constants.PWP_MAIN_PENDING);
    existingPromoBundlings.add(Constants.PWP_MAIN_ACTIVE);
    existingPromoBundlings.add(Constants.PWP_ADDITIONAL_PENDING);
    existingPromoBundlings.add(Constants.PWP_ADDITIONAL_ACTIVE);

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(STORE_ID);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setActivePromoBundlings(existingPromoBundlings);

    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
        true)).thenReturn(itemPickupPoint);
    when(itemPickupPointRepository.save(itemPickupPoint)).thenReturn(itemPickupPoint);
    when(productService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(new Product());
    itemPickupPointService.updatePwpFlagsByItemSkuAndPickupPointCode(pricingPwpPromoEvent);

    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
        true);
    verify(itemPickupPointRepository).save(itemPickupPoint);
    verify(productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(new Product(), new ArrayList<>(), false);
    assertFalse(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_MAIN_PENDING));
    assertFalse(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_MAIN_ACTIVE));
    assertFalse(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_ADDITIONAL_PENDING));
    assertFalse(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_ADDITIONAL_ACTIVE));
  }

  @Test
  public void updatePwpFlagsByItemSkuAndPickupPointCodeRemoveExistingTrueFlagsTest() {
    PricingPwpPromoEvent pricingPwpPromoEvent =
        PricingPwpPromoEvent.builder().storeId(STORE_ID).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .isPartOfPendingPWPAsMain(null).isPartOfActivePWPAsMain(null).isPartOfPendingPWPAsAdditional(null)
            .isPartOfActivePWPAsAdditional(null).build();

    Set<String> existingPromoBundlings = new HashSet<>();
    existingPromoBundlings.add(Constants.PWP_MAIN_PENDING);
    existingPromoBundlings.add(Constants.PWP_MAIN_ACTIVE);
    existingPromoBundlings.add(Constants.PWP_ADDITIONAL_PENDING);
    existingPromoBundlings.add(Constants.PWP_ADDITIONAL_ACTIVE);

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(STORE_ID);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setActivePromoBundlings(existingPromoBundlings);

    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
        true)).thenReturn(itemPickupPoint);
    when(itemPickupPointRepository.save(itemPickupPoint)).thenReturn(itemPickupPoint);
    when(productService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(new Product());
    itemPickupPointService.updatePwpFlagsByItemSkuAndPickupPointCode(pricingPwpPromoEvent);

    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
        true);
    verify(itemPickupPointRepository).save(itemPickupPoint);
    verify(productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(new Product(), new ArrayList<>(), false);
    assertTrue(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_MAIN_PENDING));
    assertTrue(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_MAIN_ACTIVE));
    assertTrue(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_ADDITIONAL_PENDING));
    assertTrue(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_ADDITIONAL_ACTIVE));
  }

  @Test
  public void updatePwpFlagsByItemSkuAndPickupPointCodeNullActivePromoBundlingsTest() {
    PricingPwpPromoEvent pricingPwpPromoEvent =
        PricingPwpPromoEvent.builder().storeId(STORE_ID).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .isPartOfPendingPWPAsMain(true).isPartOfActivePWPAsMain(true).isPartOfPendingPWPAsAdditional(true)
            .isPartOfActivePWPAsAdditional(true).build();

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(STORE_ID);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setActivePromoBundlings(null);

    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
        true)).thenReturn(itemPickupPoint);
    when(itemPickupPointRepository.save(itemPickupPoint)).thenReturn(itemPickupPoint);
    when(productService.findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(new Product());
    itemPickupPointService.updatePwpFlagsByItemSkuAndPickupPointCode(pricingPwpPromoEvent);

    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE,
        true);
    verify(itemPickupPointRepository).save(itemPickupPoint);
    verify(productService).findByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(productAndItemSolrIndexerService).updateProductAndItemDetailsInSolr(new Product(), new ArrayList<>(), false);
    Assertions.assertNotNull(itemPickupPoint.getActivePromoBundlings());
    assertTrue(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_MAIN_PENDING));
    assertTrue(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_MAIN_ACTIVE));
    assertTrue(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_ADDITIONAL_PENDING));
    assertTrue(itemPickupPoint.getActivePromoBundlings().contains(Constants.PWP_ADDITIONAL_ACTIVE));
  }

  @Test
  void testUpdateInsuredAmountInItemPickupPoint_WhenInsuredAmountsAreDifferent_ShouldUpdateAndPublishEvent() {
    ReflectionTestUtils.setField(itemPickupPointService, "cogsUpdateMaxRequestSize", 10);
    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().offlineItemId(OFFLINE_ITEM_ID).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .insuredAmount(100.0).build();
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(itemPickupPointRepository.saveAll(anyList())).thenReturn(Collections.singletonList(itemPickupPoint));
    CogsUpdateRequest cogsUpdateRequest =
        CogsUpdateRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).insuredAmount(150.0).build();
    itemPickupPointService.updateInsuredAmountInItemPickupPoint(STORE_ID, Collections.singletonList(cogsUpdateRequest));
    assertEquals(150.0, itemPickupPoint.getInsuredAmount());
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE, false);
    verify(itemPickupPointRepository).saveAll(anyList());
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(anyList(),
        eq(Collections.singletonList(ItemPickupPointChangeEventType.INSURED_AMOUNT_CHANGE)),
      eq(Collections.EMPTY_MAP));
  }

  @Test
  void testUpdateInsuredAmountInItemPickupPointMaxSizeExceeded() {
    ReflectionTestUtils.setField(itemPickupPointService, "cogsUpdateMaxRequestSize", 1);
    CogsUpdateRequest cogsUpdateRequest =
        CogsUpdateRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).insuredAmount(150.0).build();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointService.updateInsuredAmountInItemPickupPoint(STORE_ID,
        Arrays.asList(cogsUpdateRequest, cogsUpdateRequest)));
  }

  @Test
  void testUpdateInsuredAmountInItemPickupPoint_WhenInsuredAmountsAreSame_ShouldNotUpdateOrPublishEvent() {
    ReflectionTestUtils.setField(itemPickupPointService, "cogsUpdateMaxRequestSize", 10);
    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().offlineItemId(OFFLINE_ITEM_ID).itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .insuredAmount(100.0).build();
    when(itemPickupPointRepository.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    CogsUpdateRequest cogsUpdateRequest =
        CogsUpdateRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).insuredAmount(100.0).build();
    itemPickupPointService.updateInsuredAmountInItemPickupPoint(STORE_ID, Collections.singletonList(cogsUpdateRequest));
    assertEquals(100.0, itemPickupPoint.getInsuredAmount());
    verify(itemPickupPointRepository).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        PICKUP_POINT_CODE, false);
  }

  @Test
  void testGetCogsData_WithDifferentPageSizes_ShouldHandleCorrectly() {
    String storeId = "STORE123";
    String productSku = "PRODUCT-SKU-123";
    int page = 0;
    int size = 20;

    ItemPickupPoint itemPickupPoint =
        ItemPickupPoint.builder().itemSku("ITEM-001").pickupPointCode("PP001").insuredAmount(99.99).build();

    List<ItemPickupPoint> itemPickupPoints = Collections.singletonList(itemPickupPoint);
    Page<ItemPickupPoint> pageResult = new PageImpl<>(itemPickupPoints, PageRequest.of(page, size), 1L);

    when(itemPickupPointRepository.findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku,
        PageRequest.of(page, size, Sort.by(Constants.OFFLINE_ITEM_ID)))).thenReturn(pageResult);

    Page<CogsResponse> result = itemPickupPointService.getCogsData(storeId, productSku, page, size);

    assertTrue(result.hasContent());
    assertEquals(1, result.getContent().size());
    assertEquals(1L, result.getTotalElements());
    assertEquals(0, result.getNumber());
    assertEquals(20, result.getSize());

    verify(itemPickupPointRepository).findByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku,
        PageRequest.of(page, size, Sort.by(Constants.OFFLINE_ITEM_ID)));
  }
  @Test
  public void publishItemPickupPointDataChangeEventWithPureCncStatusChange_blacklistedMerchant_shouldSkipPublish() {
    ItemPickupPointDataChangeEventModel model = new ItemPickupPointDataChangeEventModel();
    model.setMerchantCode("MERCHANT_CODE");
    model.setStoreId("STORE_ID");
    model.setItemSku("ITEM_SKU");
    model.setPickupPointCode("PICKUP_POINT_CODE");

    Map<String, Set<String>> blacklist = new HashMap<>();
    blacklist.put(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME,
            new HashSet<>(Collections.singletonList("MERCHANT_CODE")));

    itemPickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(model, blacklist);

    verifyNoInteractions(businessPartnerService);
    verifyNoInteractions(kafkaProducer);

  }
  @Test
  public void publishItemPickupPointDataChangeEventWithPureCncStatusChange_nonBlacklistedMerchant_shouldPublish() {
    ItemPickupPointDataChangeEventModel model = new ItemPickupPointDataChangeEventModel();
    model.setMerchantCode("MERCHANT_CODE");
    model.setStoreId("STORE_ID");
    model.setItemSku("ITEM_SKU");
    model.setPickupPointCode("PICKUP_POINT_CODE");

    Map<String, Set<String>> blacklist = new HashMap<>();
    blacklist.put(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME,
            new HashSet<>(Collections.singletonList("OTHER_MERCHANT")));

    BusinessPartner bp = new BusinessPartner();
    when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(eq(model.getStoreId()), eq(model.getMerchantCode())))
            .thenReturn(bp);

    ArgumentCaptor<String> topicCaptor = ArgumentCaptor.forClass(String.class);
    ArgumentCaptor<String> keyCaptor = ArgumentCaptor.forClass(String.class);
    ArgumentCaptor<ItemPickupPointDataChangeEventModel> modelCaptor = ArgumentCaptor.forClass(ItemPickupPointDataChangeEventModel.class);

    itemPickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(model, blacklist);
    verify(businessPartnerService, times(1))
            .getBusinessPartnerByBusinessPartnerCode(eq(model.getStoreId()), eq(model.getMerchantCode()));
    verify(kafkaProducer, times(1))
            .send(topicCaptor.capture(), keyCaptor.capture(), modelCaptor.capture());

    assertEquals(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME, topicCaptor.getValue());
    assertNotNull(keyCaptor.getValue());
    assertNotNull(modelCaptor.getValue());
    assertEquals(model.getItemSku(), modelCaptor.getValue().getItemSku());
    assertEquals(model.getPickupPointCode(), modelCaptor.getValue().getPickupPointCode());
  }
  @Test
  public void publishItemPickupPointDataChangeEventWithPureCncStatusChange_nullBlacklist_shouldPublish() {

    ItemPickupPointDataChangeEventModel model = new ItemPickupPointDataChangeEventModel();
    model.setMerchantCode("MERCHANT_CODE");
    model.setStoreId("STORE_ID");
    model.setItemSku("ITEM_SKU");
    model.setPickupPointCode("PICKUP_POINT_CODE");

    Map<String, Set<String>> blacklist = null;

    BusinessPartner bp = new BusinessPartner();
    when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(
            eq(model.getStoreId()), eq(model.getMerchantCode())))
            .thenReturn(bp);

    itemPickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(model, blacklist);

    verify(businessPartnerService, times(1))
            .getBusinessPartnerByBusinessPartnerCode(eq(model.getStoreId()), eq(model.getMerchantCode()));

    verify(kafkaProducer, times(1))
            .send(
                    eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME),
                    anyString(),
                    eq(model)
            );
  }

  @Test
  void publishItemPickupPointDataChangeEventWithPureCncStatusChange_eventKeyHasEmptySet_publishesEvent()
    throws Exception {

    ItemPickupPointDataChangeEventModel model = new ItemPickupPointDataChangeEventModel();
    model.setStoreId(STORE_ID);
    model.setMerchantCode(MERCHANT_CODE);
    model.setItemSku(ITEM_SKU);
    model.setPickupPointCode(PICKUP_POINT_CODE);

    Map<String, Set<String>> eventToBlackListedSellersMap = new HashMap<>();
    eventToBlackListedSellersMap.put(
      ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME, Collections.emptySet());

    BusinessPartner bp = new BusinessPartner();
    bp.setBusinessPartnerCode(MERCHANT_CODE);

    when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(STORE_ID,
      MERCHANT_CODE)).thenReturn(bp);

    try (MockedStatic<CommonUtil> mockedCommonUtil = Mockito.mockStatic(CommonUtil.class)) {
      mockedCommonUtil.when(() -> CommonUtil.generateSellerChannelFromBusinessPartner(
          ArgumentMatchers.any(BusinessPartner.class)))
        .thenReturn(Collections.singletonList("ONLINE"));

      itemPickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(model,
        eventToBlackListedSellersMap);

      String expectedTopic = ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME;
      String expectedKey = ITEM_SKU + "-" + PICKUP_POINT_CODE;

      verify(kafkaProducer, times(1)).send(eq(expectedTopic), eq(expectedKey), eq(model));
    }

    verify(businessPartnerService, times(1)).getBusinessPartnerByBusinessPartnerCode(STORE_ID,
      MERCHANT_CODE);
  }
}
