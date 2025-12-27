package com.gdn.x.product.service.impl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.apache.log4j.MDC;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.AddDeleteVariantRequestVo;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import com.gdn.x.product.model.vo.B2bFieldsVo;
import com.gdn.x.product.model.vo.ItemPickupPointDeleteRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.model.vo.ProductAttributeDetailVo;
import com.gdn.x.product.model.vo.ProductScoreVo;
import com.gdn.x.product.outbound.api.InventoryOutbound;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.ProductAndItemPickupPointDTO;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.DataSourceWrapperService;
import com.gdn.x.product.service.api.ItemPickupPointHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;

public class ItemPickupPointSummaryServiceImplTest {

  private static final String PRODUCT_SKU = "PRODUCT-SKU";

  private static final String PRODUCT_CODE = "MTA-100001";

  private static final boolean IS_LATE_FULFILLMENT = true;

  private static final String PICKUP_POINT_CODE = "pickup-point-code";

  private static final String PICKUP_POINT_CODE_1 = "pickup-point-code-1";

  private static final String PICKUP_POINT_CODE_2 = "pickup-point-code-2";

  private static final String MERCHANT_SKU = "merchant-sku";

  private static final String MERCHANT_CODE = "merchant-code";

  private static final String ITEM_SKU = "ITEM-SKU";

  private static final String ITEM_SKU_2 = "ITEM-SKU-2";

  private static final String USERNAME = "username";

  private static final String REQUEST_ID = "request-id";

  private static final String STORE_ID = "store-id";

  private static final String MERCHANT_SKU_2 = "merchant-sku-2";
  private static final String OFFLINE_ITEM_ID_1 = "offline-item-id-1";
  private static final String OFFLINE_ITEM_ID_2 = "offline-item-id-2";

  @InjectMocks
  private ItemPickupPointSummaryServiceImpl itemPickupPointSummaryService;

  @Mock
  private ProductService productService;

  @Mock
  private ItemService itemService;

  @Mock
  private ItemPickupPointService pickupPointService;

  @Mock
  private ItemPickupPointHelperService pickupPointHelperService;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private ObjectConverterServiceImpl objectConverterService;

  @Mock
  private ChannelService channelService;

  @Mock
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Mock
  private InventoryOutbound inventoryOutbound;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<List<ItemPickupPoint>> itemPickupPointListCaptor;

  @Captor
  private ArgumentCaptor<List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO>>  WebInventoryDeleteCaptor;

  @Captor
  private ArgumentCaptor<ItemPickupPoint> itemPickupPointCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<Item>> itemListArgumentCaptor;

  @Mock
  private ItemPriceServiceImpl itemPriceService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private DataSourceWrapperService dataSourceWrapperService;

  private Set<Price> prices;
  private Set<ItemViewConfig> itemViewConfigs;
  private Product product;
  private ItemPickupPointListingUpdateRequestVo itemListingUpdateRequestVo;
  private Item item;
  private ItemPickupPoint itemPickupPoint;
  private ItemPickupPoint itemPickupPoint2;
  private ItemPickupPoint itemPickupPoint1;
  private MandatoryRequestParam mandatoryRequestParam;
  private ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo;
  private ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo;
  private ItemPickupPointDeleteRequestVo itemPickupPointDeleteRequestVo;
  private ItemPickupPointDeleteRequestVo itemPickupPointDeleteRequestVo1;
  private ItemPickupPoint deleteItemPickupPoint;
  private ItemPickupPoint deleteItemPickupPoint1;
  private ItemPickupPoint deleteItemPickupPoint2;
  private SystemParameter systemParameter;
  private EditItemResponse editItemResponse = new EditItemResponse();
  private AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
  private AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
  private BusinessPartner businessPartner = new BusinessPartner();
  private EditProductDetailDTO editProductDetailDTO;
  private Map.Entry<String, List<String>> itemSkuAndPickupPointListMap;
  private Map<String, List<ItemPickupPoint>> itemSkuToItemPickupPointMap;
  private Map.Entry<String, ItemPickupPointListingUpdateRequestVo> itemSkuAndMerchantSkuMap;
  private BusinessPartnerPickupPoint businessPartnerPickupPoint ;

  @BeforeEach
  public void init() throws Exception {
    openMocks(this);

    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);

    this.prices = new HashSet<>();
    this.prices.add(new Price());

    this.itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    this.itemViewConfigs.add(itemViewConfig);

    this.product = new Product();
    this.product.setProductCode(PRODUCT_CODE);
    this.product.setProductType(ProductType.REGULAR);
    this.product.setProductSku(ItemPickupPointSummaryServiceImplTest.PRODUCT_SKU);
    this.product.setMerchantCode(ItemPickupPointSummaryServiceImplTest.MERCHANT_CODE);
    this.product.setMasterDataProduct(generateMasterDataProduct());

    this.item = new Item();
    this.item.setSynchronized(false);
    this.item.setItemSku(ItemPickupPointSummaryServiceImplTest.ITEM_SKU);
    this.item.setProductSku(PRODUCT_SKU);
    this.item.setPrice(this.prices);
    this.item.setItemViewConfigs(this.itemViewConfigs);
    this.item.setPromoBundling(true);
    this.item.setPickupPointCode(ItemPickupPointSummaryServiceImplTest.PICKUP_POINT_CODE);
    this.item.setMerchantSku(ItemPickupPointSummaryServiceImplTest.MERCHANT_SKU);
    this.item.setLateFulfillment(ItemPickupPointSummaryServiceImplTest.IS_LATE_FULFILLMENT);
    this.item.setActivePromoBundlings(new HashSet<>());
    this.item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    this.item.setItemChangeEventTypes(new ArrayList<>());

    this.itemPickupPoint = new ItemPickupPoint();
    this.itemPickupPoint.setItemViewConfig(this.itemViewConfigs);
    this.itemPickupPoint.setItemSku(ITEM_SKU);
    this.itemPickupPoint.setPrice(this.prices);
    this.itemPickupPoint.setMarkForDelete(false);
    this.itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    this.itemPickupPoint.setOfflineItemId(ITEM_SKU + Constants.DASH + PICKUP_POINT_CODE);
    this.itemPickupPoint.setMerchantSku(ItemPickupPointSummaryServiceImplTest.MERCHANT_SKU);

    this.itemPickupPoint2 = new ItemPickupPoint();
    this.itemPickupPoint2.setItemViewConfig(this.itemViewConfigs);
    this.itemPickupPoint2.setItemSku(ITEM_SKU);
    this.itemPickupPoint2.setPrice(this.prices);
    this.itemPickupPoint2.setMarkForDelete(false);
    this.itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE);
    this.itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE);
    this.itemPickupPoint2.setOfflineItemId(ITEM_SKU + Constants.DASH + PICKUP_POINT_CODE);

    this.itemPickupPoint1 = new ItemPickupPoint();
    this.itemPickupPoint1.setItemViewConfig(this.itemViewConfigs);
    this.itemPickupPoint1.setItemSku(ITEM_SKU);
    this.itemPickupPoint1.setPrice(this.prices);
    this.itemPickupPoint1.setMarkForDelete(false);
    this.itemPickupPoint1.setOfflineItemId(ITEM_SKU + Constants.DASH + PICKUP_POINT_CODE_1);
    this.itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE_1);

    itemListingUpdateRequestVo = new ItemPickupPointListingUpdateRequestVo();
    itemListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU);
    itemListingUpdateRequestVo.setOff2OnChannelActive(true);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE);
    itemListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemListingUpdateRequestVo.setItemViewConfigs(itemViewConfigs);

    mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, REQUEST_ID);
    itemPickupPointUpdateRequestVo = new ItemPickupPointUpdateRequestVo();
    itemPickupPointUpdateRequestVo.setProductSku(PRODUCT_SKU);
    itemPickupPointUpdateRequestVo.setProductType(ProductType.BIG_PRODUCT);

    addVariantRequestVo.setItemSku(ITEM_SKU);
    List<AddVariantRequestVo> addVariantRequestVos = new ArrayList<>();
    addVariantRequestVos.add(addVariantRequestVo);

    addDeleteVariantRequestVo.setAddVariantsList(addVariantRequestVos);
    addDeleteVariantRequestVo.setDeleteVariantsList(Arrays.asList(ITEM_SKU));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);


    itemPickupPointListingUpdateRequestVo =
        new ItemPickupPointListingUpdateRequestVo();
    itemPickupPointListingUpdateRequestVo.setCncActivated(false);
    itemPickupPointListingUpdateRequestVo.setItemSku(ITEM_SKU);
    itemPickupPointListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(Collections.singleton(itemViewConfig1));
    itemSkuToItemPickupPointMap = new HashMap<>();

    systemParameter  = new SystemParameter(STORE_ID, null, "false", null);

    itemPickupPointDeleteRequestVo = new ItemPickupPointDeleteRequestVo();
    itemPickupPointDeleteRequestVo.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointDeleteRequestVo.setItemSku(ITEM_SKU);

    itemPickupPointDeleteRequestVo1 = new ItemPickupPointDeleteRequestVo();
    itemPickupPointDeleteRequestVo1.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPointDeleteRequestVo1.setItemSku(ITEM_SKU);

    deleteItemPickupPoint = new ItemPickupPoint();
    deleteItemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_1);
    deleteItemPickupPoint.setItemSku(ITEM_SKU);

    deleteItemPickupPoint1 = new ItemPickupPoint();
    deleteItemPickupPoint1.setItemSku(ITEM_SKU);
    deleteItemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE_2);

    deleteItemPickupPoint2 = new ItemPickupPoint();
    deleteItemPickupPoint2.setItemSku(PRODUCT_SKU);
    deleteItemPickupPoint2.setPickupPointCode(PRODUCT_SKU);
    when(saveOperationService.saveProductWithoutUpdatingSolr(Mockito.any(Product.class), Mockito.anyList(),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(Mockito.any(), Mockito.any()))
        .thenReturn(businessPartner);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        Mockito.anyString(), Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean())).thenReturn(new ArrayList<>());
    businessPartner.setSalesChannel(Arrays.asList(Constants.B2C_SELLER_CHANNEL, Constants.B2B_SELLER_CHANNEL));
    editProductDetailDTO = new EditProductDetailDTO();
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "includeVariantUpdate", true);
    itemSkuAndPickupPointListMap = Map.entry(ITEM_SKU, Arrays.asList(PICKUP_POINT_CODE));
    itemSkuAndMerchantSkuMap = Map.entry(ITEM_SKU, new ItemPickupPointListingUpdateRequestVo());
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "missingEntitySwitch", true);
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "distributionSellerList",
        Set.of(MERCHANT_CODE));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productService,this.itemService);
    verifyNoMoreInteractions(this.itemService,this.pickupPointService);
    verifyNoMoreInteractions(this.saveAndPublishService,this.saveOperationService,this.systemParameterService);
    verifyNoMoreInteractions(channelService);
    verifyNoMoreInteractions(businessPartnerPickupPointService);
    verifyNoMoreInteractions(inventoryOutbound, businessPartnerService);
  }

  @Test
  public void updateItemPickupPointListingWholeSaleActivatedFalseTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
      itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
      itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
      .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
      .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
        any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
      itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
      .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), true, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
      itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
      .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingPriceUpdatest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(true);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), null, true,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, true, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveAndPublishService).publishOfflineItemChangeSellerEvent(Mockito.any(OfflineItemChange.class));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingOnlineFlagFalseTest() throws Exception {
    itemPickupPointUpdateRequestVo.setB2cActivated(true);
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(true);
    itemListingUpdateRequestVo.getItemViewConfigs().stream().findFirst().get().setChannel(Constants.DEFAULT_CHANNEL);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(pickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), false, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishOfflineItemChangeSellerEvent(Mockito.any(OfflineItemChange.class));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingNoItemDataChangeEventTest() throws Exception {
    itemPickupPointUpdateRequestVo.setB2cActivated(true);
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "instoreNewFlowEnabled", true);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(null);
    itemPickupPoint.setPrice(prices);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(pickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(false);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), false, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingCncFlagFalseTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(false);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(true);
    product.setCncActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false)).thenReturn(
        product);
    when(cacheItemHelperService
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(item));
    when(pickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(
        itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), false, false, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingCncFlagFalseAddDeleteItemTest() throws Exception {
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addVariantRequestVo.setItemSku(ITEM_SKU_2);
    ProductAttributeDetailVo productAttributeDetailVo = new ProductAttributeDetailVo();
    addVariantRequestVo.setDefiningAttributes(Collections.singletonList(productAttributeDetailVo));
    addDeleteVariantRequestVo.setAddVariantsList(Collections.singletonList(addVariantRequestVo));

    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(false);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(true);
    product.setCncActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false)).thenReturn(
        product);
    when(pickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(
        itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), true, false, editItemResponse, false, null, false, addDeleteVariantRequestVo, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingFbbFlagFalseTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setFbbActivated(false);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemPickupPoint.setFbbActivated(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);

    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo1 = new ItemPickupPointListingUpdateRequestVo();
    itemPickupPointListingUpdateRequestVo1.setMerchantSku(MERCHANT_SKU);
    itemPickupPointListingUpdateRequestVo1.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingUpdateRequestVo1.setItemSku(ITEM_SKU);
    itemPickupPointListingUpdateRequestVo1.setPrice(prices);
    itemPickupPointListingUpdateRequestVo1.setFbbActivated(false);
    itemPickupPointListingUpdateRequestVo1.setWholesalePriceActivated(false);
    itemPickupPointListingUpdateRequestVo1.setItemViewConfigs(itemViewConfigs);
    itemPickupPoint2.setActivePromoBundlings(promoBundling);
    itemPickupPoint2.setPrice(prices);
    itemPickupPoint2.setItemSku(ITEM_SKU);
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint2.setFbbActivated(true);

    product.setOnline(true);
    product.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false)).thenReturn(
        product);
    when(cacheItemHelperService
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(item));
    when(pickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint2));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyBoolean())).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(
        itemPickupPoint);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemPickupPointListingUpdateRequestVo1.getItemSku(),
        itemPickupPointListingUpdateRequestVo1.getPickupPointCode(), false)).thenReturn(itemPickupPoint2);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());

    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo, itemPickupPointListingUpdateRequestVo1), new HashSet<>(),
        new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), false, false, editItemResponse, true, null, false,
        null, itemPickupPointUpdateRequestVo, null, false, false);

    verify(pickupPointService, times(2)).validatePriceChangeAndSet(Mockito.any(), Mockito.any(), Mockito.anyString());
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemPickupPointListingUpdateRequestVo1.getItemSku(),
        itemPickupPointListingUpdateRequestVo1.getPickupPointCode(), false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint, itemPickupPoint2));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingFbbFlagFalseProductFbbFalseTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setFbbActivated(false);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemPickupPoint.setFbbActivated(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);

    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo1 = new ItemPickupPointListingUpdateRequestVo();
    itemPickupPointListingUpdateRequestVo1.setMerchantSku(MERCHANT_SKU);
    itemPickupPointListingUpdateRequestVo1.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingUpdateRequestVo1.setItemSku(ITEM_SKU);
    itemPickupPointListingUpdateRequestVo1.setPrice(prices);
    itemPickupPointListingUpdateRequestVo1.setFbbActivated(false);
    itemPickupPointListingUpdateRequestVo1.setWholesalePriceActivated(false);
    itemPickupPointListingUpdateRequestVo1.setItemViewConfigs(itemViewConfigs);
    itemPickupPoint2.setActivePromoBundlings(promoBundling);
    itemPickupPoint2.setPrice(prices);
    itemPickupPoint2.setItemSku(ITEM_SKU);
    itemPickupPoint2.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint2.setFbbActivated(true);

    product.setOnline(true);
    product.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false)).thenReturn(
        product);
    when(cacheItemHelperService
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(item));

    when(pickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint2));

    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyBoolean())).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(
        itemPickupPoint);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemPickupPointListingUpdateRequestVo1.getItemSku(),
        itemPickupPointListingUpdateRequestVo1.getPickupPointCode(), false)).thenReturn(itemPickupPoint2);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    Set<String> fbbActivatedPickupPoints = new HashSet<>();
    fbbActivatedPickupPoints.add(PICKUP_POINT_CODE_2);

    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo, itemPickupPointListingUpdateRequestVo1), new HashSet<>(),
        new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), false, false, editItemResponse, true, fbbActivatedPickupPoints, false,
        null, itemPickupPointUpdateRequestVo, null, false, false);

    verify(pickupPointService, times(2)).validatePriceChangeAndSet(Mockito.any(), Mockito.any(), Mockito.anyString());
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemPickupPointListingUpdateRequestVo1.getItemSku(),
        itemPickupPointListingUpdateRequestVo1.getPickupPointCode(), false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint, itemPickupPoint2));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingCncFlagFalse2Test() throws Exception {
    Item item1 = new Item();
    item.setItemSku(PRODUCT_SKU);
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(false);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(true);
    product.setCncActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false)).thenReturn(
        product);
    item.setCncActivated(true);
    item1.setCncActivated(true);
    when(cacheItemHelperService
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(item, item1));
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(
        itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), false, false, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item1, item));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingCncFlagFalse3Test() throws Exception {
    Item item1 = new Item();
    item.setItemSku(PRODUCT_SKU);
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(false);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setCncActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false)).thenReturn(
        product);
    itemPickupPoint1.setCncActive(true);
    when(cacheItemHelperService
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(item, item1));
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(
        itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), null, false, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(
        Collections.singletonList(item));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingCncFlagFalse1Test() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(false);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(true);
    product.setCncActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false)).thenReturn(
        product);
    when(itemService.getItemsByProductSku(Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(item));
    when(pickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(
        itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), false, true, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingCncFlagTrueTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(false);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(true);
    product.setCncActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(itemService.getItemsByProductSku(Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(item));
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), itemListingUpdateRequestVo.getItemSku(),
        itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), false, true, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), itemListingUpdateRequestVo.getItemSku(),
        itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(),
        eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)), eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingOnlineFlagFalse2Test() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(true);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Arrays.asList(itemPickupPoint, itemPickupPoint1));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), false, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    itemPickupPoint1.getItemViewConfig().iterator().next().setDiscoverable(false);
    itemPickupPoint1.getItemViewConfig().iterator().next().setBuyable(false);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint, itemPickupPoint1));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishOfflineItemChangeSellerEvent(Mockito.any(OfflineItemChange.class));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingOnlineFlagFalse1Test() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(true);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(pickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), false, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveAndPublishService).publishOfflineItemChangeSellerEvent(Mockito.any(OfflineItemChange.class));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingOnlineFlagTrueTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(true);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(pickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), true, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(saveAndPublishService).publishOfflineItemChangeSellerEvent(Mockito.any(OfflineItemChange.class));
    Assertions.assertTrue(productArgumentCaptor.getValue().isOnline());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingOnlineFlagTrue1Test() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(true);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(pickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), true, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveAndPublishService).publishOfflineItemChangeSellerEvent(Mockito.any(OfflineItemChange.class));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingAddPickupPointTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService
        .updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
            new HashSet<>(), new HashMap<>(), new HashMap<>(), Collections.singleton(PICKUP_POINT_CODE),
            new HashSet<>(), null, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
          false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertTrue(productArgumentCaptor.getValue().getPickupPointCodes().contains(PICKUP_POINT_CODE));
    Assertions.assertEquals(1, productArgumentCaptor.getValue().getPickupPointCodes().size());
  }

  @Test
  public void updateItemPickupPointListingDeletePickupPointTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(
        dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
            itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(),
            false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService
        .updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
            new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
            Collections.singleton(PICKUP_POINT_CODE), null, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
          false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(0, productArgumentCaptor.getValue().getPickupPointCodes().size());
  }

  @Test
  public void updateItemPickupPointListingDeletePickupPoint1Test() {
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService
        .updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
            new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
            Collections.singleton(PICKUP_POINT_CODE), null, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
          false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(0, productArgumentCaptor.getValue().getPickupPointCodes().size());
  }

  @Test
  public void updateItemPickupPointListingWholeSaleActivatedTrueTest() throws Exception {
    item.setArchived(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemListingUpdateRequestVo.setPickupPointCode("");
    itemListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU_2);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
      itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
      itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "true", null);
    when(systemParameterService
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
      .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
      .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
        any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
      itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService
      .findByStoreIdAndItemSku(STORE_ID,ITEM_SKU)).thenReturn(Arrays.asList(itemPickupPoint2));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
      .thenReturn(Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), null, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
      .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
      itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).saveItemPickupPoint(Mockito.anyList());
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService)
      .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
        any(ReindexType.class), Mockito.anyBoolean());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingNoChangeTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "includeB2bFieldChange", true);
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    ItemViewConfig itemViewConfig= new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(Constants.B2B);
    b2bFieldsVo.setB2bItemViewConfigs(new HashSet<>(Collections.singletonList(itemViewConfig)));
    itemListingUpdateRequestVo.setB2bFieldsVo(b2bFieldsVo);
    itemListingUpdateRequestVo.setWholesalePriceActivated(null);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
      itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint)))
        .thenReturn(Arrays.asList(itemPickupPoint));
    when(objectConverterService
        .convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean()))
        .thenReturn(new ItemPickupPointDataChangeEventModel());
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
      itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(false);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(saveOperationService)
      .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
        any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
      itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), null, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
      itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
      itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).saveItemPickupPoint(Collections.singletonList(itemPickupPoint));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void updateItemPickupPointListingNoChangeTest1() throws Exception {
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setBasePrice(200.0);
    ItemViewConfig itemViewConfig= new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(Constants.B2B);
    b2bFieldsVo.setB2bItemViewConfigs(new HashSet<>(Collections.singletonList(itemViewConfig)));
    itemListingUpdateRequestVo.setB2bFieldsVo(b2bFieldsVo);
    B2bFields b2bFields = new B2bFields();
    b2bFields.setBasePrice(200.0);
    ItemViewConfig itemViewConfig1= new ItemViewConfig();
    itemViewConfig1.setBuyable(false);
    itemViewConfig1.setDiscoverable(false);
    itemViewConfig1.setChannel(Constants.B2B);
    itemPickupPoint.getAllItemViewConfigs().add(itemViewConfig1);
    itemPickupPoint.setB2bFields(b2bFields);
    itemListingUpdateRequestVo.setWholesalePriceActivated(null);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(false);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), null, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).saveItemPickupPoint(Collections.singletonList(itemPickupPoint));
    verify(saveOperationService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingNoChangeTest3() throws Exception {
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setBasePrice(200.0);
    ItemViewConfig itemViewConfig= new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(Constants.B2B);
    b2bFieldsVo.setB2bItemViewConfigs(new HashSet<>(Collections.singletonList(itemViewConfig)));
    itemListingUpdateRequestVo.setB2bFieldsVo(b2bFieldsVo);
    B2bFields b2bFields = new B2bFields();
    b2bFields.setBasePrice(200.0);
    ItemViewConfig itemViewConfig1= new ItemViewConfig();
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    itemViewConfig1.setChannel(Constants.B2B);
    Set<ItemViewConfig> itemViewConfig2 = itemPickupPoint.getItemViewConfig();
    itemViewConfig2.add(itemViewConfig1);
    itemPickupPoint.setItemViewConfig(itemViewConfig2);
    itemPickupPoint.setB2bFields(b2bFields);
    itemListingUpdateRequestVo.setWholesalePriceActivated(null);
    product.setB2bActivated(true);
    product.setB2cActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(false);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    itemPickupPointUpdateRequestVo.setB2cActivated(Boolean.FALSE);
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo),
        new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), null, null,
        editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null, false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingMerchantSkuTest() throws Exception {
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(null);
    itemListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU_2);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(false);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), anyList(),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Collections.singletonList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(),
        new HashSet<>(), new HashSet<>(), null, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).saveItemPickupPoint(Collections.singletonList(itemPickupPoint));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Collections.singletonList(item));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingMerchantSku2Test() throws Exception {
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setCncActive(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(null);
    itemListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU_2);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(false);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    itemPickupPoint.setItemSku(PRODUCT_SKU);
    when(pickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        new ArrayList<>(Collections.singletonList(itemPickupPoint)));
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Collections.singletonList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(),
        new HashSet<>(), new HashSet<>(), null, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Collections.singletonList(item));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingMerchantSkuAlreadyUpdatedAtL5Test() throws Exception {
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint2.setMerchantSku(MERCHANT_SKU_2);
    itemListingUpdateRequestVo.setWholesalePriceActivated(null);
    itemListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU_2);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(false);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), anyList(),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    when(pickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(new ArrayList<>(Arrays.asList(itemPickupPoint2)));
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Collections.singletonList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(),
        new HashSet<>(), new HashSet<>(), null, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).saveItemPickupPoint(Collections.singletonList(itemPickupPoint));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Collections.singletonList(item));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemListingEmptyItemListingUpdateRequestVoTest() {
    product.setProductType(ProductType.BOPIS);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
      .thenReturn(product);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(itemListArgumentCaptor.capture(), Mockito
      .any(), anyString()))
      .thenReturn(Arrays.asList(item));
    itemPickupPointSummaryService
      .updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR, new ArrayList<>(), new HashSet<>(),
          new HashMap<>(), new HashMap<>(), new HashSet<>(), new HashSet<>(), null, null, editItemResponse, false, null,
          false, null, itemPickupPointUpdateRequestVo, product, false, false);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(any(), anyList(), eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointListingMerhcnatSkuChangeTest() throws Exception {
    itemPickupPoint.setMerchantSku(MERCHANT_SKU);
    itemListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU_2);
    item.setArchived(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
      itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        new ArrayList<>(Collections.singletonList(itemPickupPoint2)));
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
      itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "true", null);
    when(systemParameterService
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
      .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), anyList(),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
      .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
        any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
      itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
      Arrays.asList(itemListingUpdateRequestVo),new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), null, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
      itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
      .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
      itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService)
      .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
        any(ReindexType.class), Mockito.anyBoolean());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID,ITEM_SKU);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  private MasterDataProduct generateMasterDataProduct() {
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setShippingWeight(34);
    return masterDataProduct;
  }

  @Test
  public void updateItemPickupPointProductNullTest() throws Exception {
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
          new HashMap<>(), false, false));
    } finally {
      verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    }
  }

  @Test
  public void updateItemPickupPointOnlyProductUpdateTest() throws Exception {
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void validateForOneFbbTruePickupPointPerItemAddAndEditPickupPointTest() throws Exception {
    ItemPickupPointListingUpdateRequestVo addPickupPoint = new ItemPickupPointListingUpdateRequestVo();
    addPickupPoint.setFbbActivated(true);
    addPickupPoint.setItemSku(ITEM_SKU);
    addPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(Arrays.asList(addPickupPoint));
    ItemPickupPointListingUpdateRequestVo editPickupPoint = new ItemPickupPointListingUpdateRequestVo();
    editPickupPoint.setFbbActivated(false);
    editPickupPoint.setItemSku(ITEM_SKU);
    editPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPointUpdateRequestVo.setQuickEditUpdateRequests(Arrays.asList(editPickupPoint));
    ItemPickupPoint existingPickupPoint = new ItemPickupPoint();
    existingPickupPoint.setFbbActivated(true);
    existingPickupPoint.setItemSku(ITEM_SKU);
    existingPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_2);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false)).thenReturn(
        Arrays.asList(existingPickupPoint));
    itemPickupPointSummaryService.validateForOneFbbTruePickupPointPerItem(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
      editItemResponse, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false);
  }

  @Test
  public void validateForOneFbbTruePickupPointPerItemAddPickupPointTest() throws Exception {
    ItemPickupPointListingUpdateRequestVo addPickupPoint = new ItemPickupPointListingUpdateRequestVo();
    addPickupPoint.setFbbActivated(true);
    addPickupPoint.setItemSku(ITEM_SKU);
    addPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(Arrays.asList(addPickupPoint));
    ItemPickupPoint existingPickupPoint = new ItemPickupPoint();
    existingPickupPoint.setFbbActivated(true);
    existingPickupPoint.setItemSku(ITEM_SKU);
    existingPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_1);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false)).thenReturn(
        Arrays.asList(existingPickupPoint));
    itemPickupPointSummaryService.validateForOneFbbTruePickupPointPerItem(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
      editItemResponse, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false);
  }

  @Test
  public void validateForOneFbbTruePickupPointPerItemDeletePickupPointTest() throws Exception {
    ItemPickupPointDeleteRequestVo deletePickupPoint = new ItemPickupPointDeleteRequestVo();
    deletePickupPoint.setItemSku(ITEM_SKU);
    deletePickupPoint.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointUpdateRequestVo.setDeletePickupPointRequests(Arrays.asList(deletePickupPoint));
    ItemPickupPoint existingPickupPoint = new ItemPickupPoint();
    existingPickupPoint.setFbbActivated(true);
    existingPickupPoint.setItemSku(ITEM_SKU);
    existingPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_1);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false)).thenReturn(
        Arrays.asList(existingPickupPoint));
    itemPickupPointSummaryService.validateForOneFbbTruePickupPointPerItem(mandatoryRequestParam,
        itemPickupPointUpdateRequestVo, editItemResponse, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false);
  }

  @Test
  public void validateForOneFbbTruePickupPointPerItemDeletePickupPointNotMatchTest() throws Exception {
    ItemPickupPointDeleteRequestVo deletePickupPoint = new ItemPickupPointDeleteRequestVo();
    deletePickupPoint.setItemSku(ITEM_SKU);
    deletePickupPoint.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPointUpdateRequestVo.setDeletePickupPointRequests(Arrays.asList(deletePickupPoint));
    ItemPickupPoint existingPickupPoint = new ItemPickupPoint();
    existingPickupPoint.setFbbActivated(true);
    existingPickupPoint.setItemSku(ITEM_SKU);
    existingPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_1);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false)).thenReturn(
        Arrays.asList(existingPickupPoint));
    itemPickupPointSummaryService.validateForOneFbbTruePickupPointPerItem(mandatoryRequestParam,
        itemPickupPointUpdateRequestVo, editItemResponse, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false);
  }

  @Test
  public void validateForOneFbbTruePickupPointPerItemMoreThanOneFbbPickupPointTest() throws Exception {
    ItemPickupPointListingUpdateRequestVo addPickupPoint = new ItemPickupPointListingUpdateRequestVo();
    addPickupPoint.setFbbActivated(true);
    addPickupPoint.setItemSku(ITEM_SKU);
    addPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(Arrays.asList(addPickupPoint));
    ItemPickupPoint existingPickupPoint = new ItemPickupPoint();
    existingPickupPoint.setFbbActivated(true);
    existingPickupPoint.setItemSku(ITEM_SKU);
    existingPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_1);
    ItemPickupPoint existingPickupPoint1 = new ItemPickupPoint();
    existingPickupPoint1.setFbbActivated(false);
    existingPickupPoint1.setItemSku(ITEM_SKU);
    existingPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE_2);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false)).thenReturn(
        Arrays.asList(existingPickupPoint, existingPickupPoint1));
      itemPickupPointSummaryService.validateForOneFbbTruePickupPointPerItem(mandatoryRequestParam,
          itemPickupPointUpdateRequestVo, editItemResponse, false);
      verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
          mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false);
  }

  @Test
  public void validateForOneFbbTruePickupPointPerItemMoreThanOneFbbPickupPointCombinedEditTest() throws Exception {
    ItemPickupPointListingUpdateRequestVo addPickupPoint = new ItemPickupPointListingUpdateRequestVo();
    addPickupPoint.setFbbActivated(true);
    addPickupPoint.setItemSku(ITEM_SKU);
    addPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(Arrays.asList(addPickupPoint));
    ItemPickupPoint existingPickupPoint = new ItemPickupPoint();
    existingPickupPoint.setFbbActivated(true);
    existingPickupPoint.setItemSku(ITEM_SKU);
    existingPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID_1);
    existingPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_1);
    ItemPickupPoint existingPickupPoint1 = new ItemPickupPoint();
    existingPickupPoint1.setFbbActivated(false);
    existingPickupPoint1.setItemSku(ITEM_SKU);
    existingPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE_2);
    existingPickupPoint1.setOfflineItemId(OFFLINE_ITEM_ID_2);
    editProductDetailDTO.getAllItemPickupPointMap().put(OFFLINE_ITEM_ID_1, existingPickupPoint);
    editProductDetailDTO.getAllItemPickupPointMap().put(OFFLINE_ITEM_ID_2, existingPickupPoint1);
    itemPickupPointSummaryService.validateForOneFbbTruePickupPointPerItem(mandatoryRequestParam,
        itemPickupPointUpdateRequestVo, editItemResponse, false);
  }

  @Test
  public void updateItemPickupPointAddNonCncL5UpdateTest() throws Exception {
    itemPickupPointUpdateRequestVo
        .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(pickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU,
        Arrays.asList(PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2))).thenReturn(
        Collections.singletonList(itemPickupPoint1));
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(dataSourceWrapperService)
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertTrue(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertTrue(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getValue().get(0).getPickupPointCode());
    verify(channelService).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "updatedMerchantSkuFromNewlyAddedL5", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo
        .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    itemPickupPointUpdateRequestVo.setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        GdnMandatoryRequestParameterUtil.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(PICKUP_POINT_CODE));
    item.setCncActivated(true);
    item.setMerchantSku("merchant-sku-1");
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        true)).thenReturn(item);
    when(pickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU,
        Arrays.asList(PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2))).thenReturn(
        Collections.singletonList(itemPickupPoint1));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(channelService, times(2)).getDefaultChannel();
    verify(businessPartnerPickupPointService, times(2)).getBusinessPartnerPickupPointByPickupPointCodes(anyString(), anyList());
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    verify(pickupPointService, times(2)).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(Mockito.any(), Mockito.any(), Mockito.any());
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
   // verify(businessPartnerPickupPointService).getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertFalse(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertFalse(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertEquals(MERCHANT_SKU, itemPickupPointListCaptor.getValue().get(0).getMerchantSku());

  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateNullItemTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "updatedMerchantSkuFromNewlyAddedL5", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo
      .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    itemPickupPointUpdateRequestVo.setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
      false)).thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
      .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
      GdnMandatoryRequestParameterUtil.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
      Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE)))
      .thenReturn(Collections.singletonList(PICKUP_POINT_CODE));
    item.setCncActivated(true);
    item.setMerchantSku("merchant-sku-1");
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
      true)).thenReturn(null);
    when(pickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(
      mandatoryRequestParam.getStoreId(), ITEM_SKU,
      Arrays.asList(PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2))).thenReturn(
      Collections.singletonList(itemPickupPoint1));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
      false)).thenReturn(null);
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam,
        itemPickupPointUpdateRequestVo, new HashMap<>(), false, false));
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(channelService).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(anyString(), anyList());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
      itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
      false);
  }

  @Test
  public void updateItemPickupPointAddFbbL5UpdateTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVoNew =
        new ItemPickupPointListingUpdateRequestVo();
    itemPickupPointListingUpdateRequestVoNew.setItemSku(ITEM_SKU);
    itemPickupPointListingUpdateRequestVoNew.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingUpdateRequestVoNew.setFbbActivated(false);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemPickupPointListingUpdateRequestVoNew.setMerchantSku("merchant-sku");
    itemPickupPointListingUpdateRequestVoNew.setItemViewConfigs(Collections.singleton(itemViewConfig));
    itemPickupPointListingUpdateRequestVoNew.getItemViewConfigs().iterator().next()
        .setChannel(Constants.DEFAULT_CHANNEL);
    List<ItemPickupPointListingUpdateRequestVo> itemPickupPointListingUpdateRequestVoList = new ArrayList<>();
    itemPickupPointListingUpdateRequestVoList.add(itemPickupPointListingUpdateRequestVo);
    itemPickupPointListingUpdateRequestVoList.add(itemPickupPointListingUpdateRequestVoNew);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(itemPickupPointListingUpdateRequestVoList);
    itemPickupPointUpdateRequestVo.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false)).thenReturn(
        product);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setFbbActivated(true);
    itemPickupPoint.setMerchantSku("merchant-sku");
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(itemPickupPoint);
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointList = new ArrayList<>();
    pickupPointList.add(PICKUP_POINT_CODE);
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        Collections.singleton(PICKUP_POINT_CODE))).thenReturn(pickupPointList);
    item.setCncActivated(true);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
        ITEM_SKU)).thenReturn(item);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(pickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU,
        Arrays.asList(PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2))).thenReturn(
        Collections.singletonList(itemPickupPoint1));

    EditItemResponse editItemResponse = itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam,
        itemPickupPointUpdateRequestVo, new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(businessPartnerPickupPointService).getFbbActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(dataSourceWrapperService, times(2)).findItemPickupPointByItemSkuAndPickupPointCode(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());

    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertTrue(
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertTrue(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getValue().get(0).getItemSku());
    Assertions.assertTrue(editItemResponse.isFbbFlagChangedAtL3Level());
    verify(channelService, times(2)).getDefaultChannel();
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    pickupPointCodes.add(PICKUP_POINT_CODE_1);
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updatingFbbAtL3AndL5Test() throws Exception {
    List<ItemPickupPoint> allItemPickupPoints = new ArrayList<>();
    itemPickupPoint.setFbbActivated(false);
    product.setFbbActivated(true);
    allItemPickupPoints.add(itemPickupPoint);
    boolean isFbbChangedAtL3 =
        itemPickupPointSummaryService.updatingFbbAtL3AndL5(new HashSet<>(),  true, allItemPickupPoints, false, product);
    Assertions.assertTrue(isFbbChangedAtL3);
  }

  @Test
  public void updatingFbbAtL3AndL5FbbTrueAtL5Test() throws Exception {
    List<ItemPickupPoint> allItemPickupPoints = new ArrayList<>();
    itemPickupPoint.setFbbActivated(true);
    product.setFbbActivated(true);
    allItemPickupPoints.add(itemPickupPoint);
    boolean isFbbChangedAtL3 =
        itemPickupPointSummaryService.updatingFbbAtL3AndL5(new HashSet<>(), true, allItemPickupPoints, false, product);
    Assertions.assertFalse(isFbbChangedAtL3);
  }

  @Test
  public void updatingFbbAtL3AndL5FbbFalseAtL3Test() throws Exception {
    List<ItemPickupPoint> allItemPickupPoints = new ArrayList<>();
    itemPickupPoint.setFbbActivated(false);
    product.setFbbActivated(false);
    allItemPickupPoints.add(itemPickupPoint);
    boolean isFbbChangedAtL3 =
        itemPickupPointSummaryService.updatingFbbAtL3AndL5(new HashSet<>(), true, allItemPickupPoints, false, product);
    Assertions.assertFalse(isFbbChangedAtL3);
  }

  @Test
  public void updatingFbbAtL3AndL5AllItemPickupPointListEmptyTest() throws Exception {
    List<ItemPickupPoint> allItemPickupPoints = new ArrayList<>();
    itemPickupPoint.setFbbActivated(false);
    product.setFbbActivated(true);
    allItemPickupPoints.add(itemPickupPoint);
    boolean isFbbChangedAtL3 =
        itemPickupPointSummaryService.updatingFbbAtL3AndL5(new HashSet<>(),  true, new ArrayList<>(), false, product);
    Assertions.assertFalse(isFbbChangedAtL3);
  }

  @Test
  public void updateItemPickupPointAddFbbL5UpdateProductFbbTrueTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVoNew =
        new ItemPickupPointListingUpdateRequestVo();
    itemPickupPointListingUpdateRequestVoNew.setItemSku(ITEM_SKU);
    itemPickupPointListingUpdateRequestVoNew.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingUpdateRequestVoNew.setFbbActivated(false);
    itemPickupPointListingUpdateRequestVoNew.setMerchantSku("merchant-sku");
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemPickupPointListingUpdateRequestVoNew.setItemViewConfigs(Collections.singleton(itemViewConfig));
    itemPickupPointListingUpdateRequestVoNew.getItemViewConfigs().iterator().next()
        .setChannel(Constants.DEFAULT_CHANNEL);
    List<ItemPickupPointListingUpdateRequestVo> itemPickupPointListingUpdateRequestVoList = new ArrayList<>();
    itemPickupPointListingUpdateRequestVoList.add(itemPickupPointListingUpdateRequestVo);
    itemPickupPointListingUpdateRequestVoList.add(itemPickupPointListingUpdateRequestVoNew);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(itemPickupPointListingUpdateRequestVoList);
    itemPickupPointUpdateRequestVo.setFbbActivated(true);
    product.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false)).thenReturn(
        product);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setFbbActivated(true);
    itemPickupPoint.setMerchantSku("merchant-sku");
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(pickupPointService.findByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString())).thenReturn(itemPickupPoint);
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointList = new ArrayList<>();
    pickupPointList.add(PICKUP_POINT_CODE);
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        Collections.singleton(PICKUP_POINT_CODE))).thenReturn(pickupPointList);
    item.setCncActivated(true);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
        ITEM_SKU)).thenReturn(item);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(pickupPointService.findByStoreIdAndItemSku(mandatoryRequestParam.getStoreId(), ITEM_SKU)).thenReturn(
        Collections.singletonList(itemPickupPoint1));

    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);

    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(businessPartnerPickupPointService).getFbbActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    verify(dataSourceWrapperService, times(2)).findItemPickupPointByItemSkuAndPickupPointCode(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean());
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertTrue(
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertTrue(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getValue().get(0).getItemSku());
    verify(channelService, times(2)).getDefaultChannel();
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    pickupPointCodes.add(PICKUP_POINT_CODE_1);
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddFbbL5NotMatchWithRequestPPTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "updatedMerchantSkuFromNewlyAddedL5", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(
        Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setFbbActivated(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        Collections.singleton(PICKUP_POINT_CODE))).thenReturn(Collections.singletonList(PICKUP_POINT_CODE_1));
    item.setCncActivated(true);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);

    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(channelService).getDefaultChannel();
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);

    verify(businessPartnerPickupPointService).getFbbActivatedPickupPointCodes(Mockito.anySet());
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        Mockito.anyString(), Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void updateItemPickupPointAddFbbL5NotFoundTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(
        Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false)).thenReturn(
        product);
    itemPickupPoint.setCncActive(false);
    itemPickupPoint.setFbbActivated(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        Collections.singleton(PICKUP_POINT_CODE))).thenReturn(new ArrayList<>());
    item.setCncActivated(true);
    when(itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
        ITEM_SKU)).thenReturn(item);

    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(businessPartnerPickupPointService).getFbbActivatedPickupPointCodes(Mockito.anySet());
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService).getDefaultChannel();
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddCncL5NoCncPPFoundTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo
        .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(PICKUP_POINT_CODE_1));
    item.setCncActivated(true);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
          new HashMap<>(), false, false));
    } finally {
      verify(channelService).getDefaultChannel();
      List<String> pickupPointCodes = new ArrayList<>();
      pickupPointCodes.add(PICKUP_POINT_CODE);
      verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);

      verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
      verify(dataSourceWrapperService)
          .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
      verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
          Mockito.anyString(), Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
      verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
      verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    }
  }

  @Test
  public void updateItemPickupPointAddCncL5NPPNonCncTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "updatedMerchantSkuFromNewlyAddedL5", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo
        .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    item.setCncActivated(true);
    item.setMerchantSku("merchant-sku-1");
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(pickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        new ArrayList<>(Collections.singletonList(itemPickupPoint)));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
          new HashMap<>(), false, false));
    } finally {
      verify(channelService).getDefaultChannel();
      List<String> pickupPointCodes = new ArrayList<>();
      pickupPointCodes.add(PICKUP_POINT_CODE);
      verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
      verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
          false);
      verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
      verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
          itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
      verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
          Mockito.anyString(), Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
      verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
      verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    }
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    itemPickupPointUpdateRequestVo
        .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMerchantSku("merchant-sku");
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(PICKUP_POINT_CODE));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(channelService).getDefaultChannel();
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);

    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Collections.singletonList(item));
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertTrue(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertTrue(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getValue().get(0).getPickupPointCode());
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEditCncTrueTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(itemListArgumentCaptor.capture());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertTrue(itemListArgumentCaptor.getValue().get(0).isCncActivated());
    verify(this.channelService, times(1)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }


  @Test
  public void updateItemPickupPointAddL5UpdateL4UpdateQuickEditFbbTrueTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setFbbActivated(false);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        Collections.singleton(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(PICKUP_POINT_CODE));

    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(1)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddL5UpdateL4UpdateQuickEditFbbTrueProductFbbFalseTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    product.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setFbbActivated(false);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(PICKUP_POINT_CODE));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        Collections.singleton(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(PICKUP_POINT_CODE));
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(false);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    itemPickupPointUpdateRequestVo.setFbbActivated(true);
    EditItemResponse editItemResponse = itemPickupPointSummaryService.updateItemPickupPoint(
        mandatoryRequestParam, itemPickupPointUpdateRequestVo, new HashMap<>(), false, false);

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertFalse(editItemResponse.isFbbFlagChangedAtL3Level());
    verify(this.channelService).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddL5UpdateL4UpdateQuickEditFbbTrueProductByPassOneFbbCheckTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "mppForWhEnabled", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    product.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setFbbActivated(false);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(PICKUP_POINT_CODE));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        Collections.singleton(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(PICKUP_POINT_CODE));
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    itemPickupPointUpdateRequestVo.setFbbActivated(true);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(1)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddL5UpdateL4UpdateQuickEditFbbTrueProductTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    product.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setFbbActivated(false);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        Collections.singleton(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(PICKUP_POINT_CODE));
    when(itemService.updatePickupPoints(any(), any(), eq(false))).thenReturn(
        new ProductAndItemPickupPointDTO(null, new ArrayList<>()));
    itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests().get(0).setPpCodeChangedForNonMppSeller(true);

    itemPickupPointUpdateRequestVo.setFbbActivated(true);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(itemService).updatePickupPoints(any(), any(), eq(false));
    verify(this.channelService, times(1)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddL5UpdateL4UpdateQuickEditFbbTrueProductTestWithMissingItem() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
      .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    product.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
      .thenReturn(product);
    itemPickupPoint.setFbbActivated(false);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
      .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, false)).thenReturn(item);
    when(systemParameterService
      .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
      .thenReturn(systemParameter);
    when(dataSourceWrapperService
      .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
      .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
      Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService
      .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
        itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
      Collections.singleton(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(PICKUP_POINT_CODE));
    when(itemService.updatePickupPoints(any(), any(), eq(false))).thenReturn(
      new ProductAndItemPickupPointDTO(null, new ArrayList<>()));
    itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests().get(0).setPpCodeChangedForNonMppSeller(true);

    itemPickupPointUpdateRequestVo.setFbbActivated(true);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
      new HashMap<>(), false, false);

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
      .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
      Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(itemService).updatePickupPoints(any(), any(), eq(false));
    verify(this.channelService, times(1)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddL5UpdateL4UpdateQuickEditFbbTrueAndFalseTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);

    itemPickupPointUpdateRequestVo.setQuickEditUpdateRequests(
        Arrays.asList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setFbbActivated(false);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(mandatoryRequestParam.getStoreId()),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(item);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        Mockito.any(), Mockito.any(), Mockito.any(), Mockito.anyBoolean())).thenReturn(itemPickupPoint);
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        Collections.singleton(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(PICKUP_POINT_CODE));
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService,
        times(1)).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
    verify(pickupPointService, times(1)).validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(dataSourceWrapperService, times(1)).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        eq(mandatoryRequestParam.getStoreId()), Mockito.anyString(), Mockito.anyBoolean());
    verify(this.channelService, times(1)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddL5UpdateL4UpdateQuickEditFbbTrueUpdateTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setFbbActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setFbbActivated(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(Arrays.asList(itemPickupPoint));
    when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        Collections.singleton(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(PICKUP_POINT_CODE));

    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(1)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddAndDeleteVariantsincludeVariantUpdateTrueAndDeleteSkusNonNullFalse() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "includeVariantUpdate", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(
        Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addVariantRequestVo.setItemSku(itemPickupPointListingUpdateRequestVo.getItemSku());
    addDeleteVariantRequestVo.setAddVariantsList(new ArrayList<>());
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(Collections.singletonList(itemPickupPointDeleteRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMerchantSku("merchant-sku");
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(Long.valueOf(2));
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false))
        .thenReturn(Collections.singletonList(deleteItemPickupPoint));
    itemPickupPointUpdateRequestVo.setOnline(false);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService, times(2)).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService, times(2)).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(channelService).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Collections.singletonList(item));
    verify(pickupPointService).deleteItemPickupPointsFromInventory(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertFalse(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertFalse(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getOfflineItemId());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getAllValues().get(0).get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, itemPickupPointListCaptor.getAllValues().get(1).get(0).getPickupPointCode());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(1).get(0).getItemSku());
    Assertions.assertTrue(itemPickupPointListCaptor.getAllValues().get(1).get(0).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListCaptor.getAllValues().get(1).get(0).isCncActive());
  }

  @Test
  public void updateItemPickupPointAddAndDeleteIncludeVariantTrueAnddeleteSKusNonNullTrueAndNotContainsSameItemsinDeleteList() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "includeVariantUpdate", true);
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "missingEntitySwitch", false);
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "ranchIntegrationEnabled", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    itemPickupPointListingUpdateRequestVo.setDistribution(true);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(
        Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addDeleteVariantRequestVo.setDeleteVariantsList(Arrays.asList(ITEM_SKU_2));
    addVariantRequestVo.setItemSku(ITEM_SKU_2);
    addDeleteVariantRequestVo.setAddVariantsList(Collections.singletonList(addVariantRequestVo));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(Collections.singletonList(itemPickupPointDeleteRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(Long.valueOf(2));
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false))
        .thenReturn(Collections.singletonList(deleteItemPickupPoint));
    itemPickupPointUpdateRequestVo.setOnline(false);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService, times(2)).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService, times(2)).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(channelService).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Collections.singletonList(item));
    verify(pickupPointService).deleteItemPickupPointsFromInventory(itemPickupPointListCaptor.capture());
verify(dataSourceWrapperService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertFalse(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertFalse(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getOfflineItemId());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getAllValues().get(0).get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, itemPickupPointListCaptor.getAllValues().get(1).get(0).getPickupPointCode());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(1).get(0).getItemSku());
    Assertions.assertTrue(itemPickupPointListCaptor.getAllValues().get(1).get(0).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListCaptor.getAllValues().get(1).get(0).isCncActive());
  }

  @Test
  public void updateItemPickupPointAddAndDeleteVariantsincludeVariantUpdateFalseDeleteSkusNonNullTrue() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "includeVariantUpdate", false);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(
        Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addVariantRequestVo.setItemSku(itemPickupPointListingUpdateRequestVo.getItemSku());
    addDeleteVariantRequestVo.setAddVariantsList(Collections.singletonList(addVariantRequestVo));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(Collections.singletonList(itemPickupPointDeleteRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMerchantSku("merchant-sku");
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(Long.valueOf(2));
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false))
        .thenReturn(Collections.singletonList(deleteItemPickupPoint));
    itemPickupPointUpdateRequestVo.setOnline(false);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService, times(2)).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService, times(2)).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(channelService).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Collections.singletonList(item));
    verify(pickupPointService).deleteItemPickupPointsFromInventory(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertFalse(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertFalse(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getOfflineItemId());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getAllValues().get(0).get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, itemPickupPointListCaptor.getAllValues().get(1).get(0).getPickupPointCode());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(1).get(0).getItemSku());
    Assertions.assertTrue(itemPickupPointListCaptor.getAllValues().get(1).get(0).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListCaptor.getAllValues().get(1).get(0).isCncActive());
  }


  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEditCncFalseTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(false);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(1)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEditCncFalseTest2() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(false);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    itemPickupPoint.setCncActive(true);
    when(dataSourceWrapperService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(null);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointSummaryService
          .updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo, new HashMap<>(),
            false, false));
    } finally {
      verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
          mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
      verify(dataSourceWrapperService)
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false);
      verify(this.channelService, times(1)).getDefaultChannel();
      verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    }
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEditCncFalseTest3() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService,"missingEntitySwitch", false);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(false);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    itemPickupPoint.setCncActive(true);
    when(dataSourceWrapperService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(null);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    try {
      Assertions.assertThrows(Exception.class, () -> itemPickupPointSummaryService
          .updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo, new HashMap<>(),
            false, false));
    } finally {
      verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
          mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
      verify(dataSourceWrapperService)
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false);
      verify(this.channelService, times(1)).getDefaultChannel();
      verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    }
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEditCncFalse2Test() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(false);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    item.setCncActivated(true);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(1)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEditCncFalse3Test() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(false);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(false);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(1)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4AndL3UpdateTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    itemPickupPointUpdateRequestVo
        .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    product.setCncActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMerchantSku("merchant-sku");
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(channelService).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Collections.singletonList(item));
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertTrue(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertTrue(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getValue().get(0).getPickupPointCode());
  }

  @Test
  public void checkFbbForDeleteRequestTest() throws Exception {
    itemPickupPointUpdateRequestVo.setProductSku(PRODUCT_SKU);
    itemPickupPointUpdateRequestVo.setFbbActivated(false);
    product.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint2.setMarkForDelete(false);
    itemPickupPoint2.setFbbActivated(true);
    when(pickupPointService.findByStoreIdAndProductSku(mandatoryRequestParam.getStoreId(),
        product.getProductSku())).thenReturn(Arrays.asList(itemPickupPoint2));
    itemPickupPointSummaryService.checkFbbForDeleteRequest(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
      false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndProductSku(mandatoryRequestParam.getStoreId(),
        product.getProductSku(), false);
  }

  @Test
  public void checkFbbForDeleteRequestMfdTrueTest() throws Exception {
    itemPickupPointUpdateRequestVo.setProductSku(PRODUCT_SKU);
    itemPickupPointUpdateRequestVo.setFbbActivated(false);
    product.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint2.setMarkForDelete(true);
    itemPickupPoint2.setFbbActivated(false);
    when(pickupPointService.findByStoreIdAndProductSku(mandatoryRequestParam.getStoreId(),
        product.getProductSku())).thenReturn(Arrays.asList(itemPickupPoint2));
    itemPickupPointSummaryService.checkFbbForDeleteRequest(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
      false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndProductSku(mandatoryRequestParam.getStoreId(),
        product.getProductSku(), false);
  }

  @Test
  public void checkFbbForDeleteRequestMfdTrueAndFbbTrueTest() throws Exception {
    itemPickupPointUpdateRequestVo.setProductSku(PRODUCT_SKU);
    itemPickupPointUpdateRequestVo.setFbbActivated(false);
    product.setFbbActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint2.setMarkForDelete(true);
    itemPickupPoint2.setFbbActivated(true);
    when(pickupPointService.findByStoreIdAndProductSku(mandatoryRequestParam.getStoreId(),
        product.getProductSku())).thenReturn(Arrays.asList(itemPickupPoint2));
    itemPickupPointSummaryService.checkFbbForDeleteRequest(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
      false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndProductSku(mandatoryRequestParam.getStoreId(),
        product.getProductSku(), false);
  }

  @Test
  public void updateItemPickupPointAddAndDeleteUpdateOnlineFalseTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(
        Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addVariantRequestVo.setItemSku(itemPickupPointListingUpdateRequestVo.getItemSku());
    addDeleteVariantRequestVo.setAddVariantsList(Collections.singletonList(addVariantRequestVo));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(Collections.singletonList(itemPickupPointDeleteRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMerchantSku("merchant-sku");
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);

    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(Long.valueOf(2));
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false))
        .thenReturn(Collections.singletonList(deleteItemPickupPoint));
    itemPickupPointUpdateRequestVo.setOnline(false);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService, times(2)).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService, times(2)).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(channelService).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Collections.singletonList(item));
    verify(pickupPointService).deleteItemPickupPointsFromInventory(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertFalse(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertFalse(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getOfflineItemId());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getAllValues().get(0).get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, itemPickupPointListCaptor.getAllValues().get(1).get(0).getPickupPointCode());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(1).get(0).getItemSku());
    Assertions.assertTrue(itemPickupPointListCaptor.getAllValues().get(1).get(0).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListCaptor.getAllValues().get(1).get(0).isCncActive());
  }

  @Test
  public void updateItemPickupPointAddAndDeleteUpdateOnlineFalseTest1() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    ItemPickupPointListingUpdateRequestVo itemPickupPointListingUpdateRequestVo1 =
        new ItemPickupPointListingUpdateRequestVo();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT_CHANNEL);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemPickupPointListingUpdateRequestVo1.setItemViewConfigs(Collections.singleton(itemViewConfig));
    itemPickupPointListingUpdateRequestVo1.setItemSku(itemPickupPointListingUpdateRequestVo.getItemSku());
    itemPickupPointListingUpdateRequestVo1.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo1.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo1.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo1.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointListingUpdateRequestVo1.setMerchantSku("merchant-sku");
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(
        Arrays.asList(itemPickupPointListingUpdateRequestVo, itemPickupPointListingUpdateRequestVo1));
    AddDeleteVariantRequestVo addDeleteVariantRequestVo = new AddDeleteVariantRequestVo();
    AddVariantRequestVo addVariantRequestVo = new AddVariantRequestVo();
    addVariantRequestVo.setItemSku(itemPickupPointListingUpdateRequestVo.getItemSku());
    addDeleteVariantRequestVo.setAddVariantsList(Collections.singletonList(addVariantRequestVo));
    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(addDeleteVariantRequestVo);
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(Collections.singletonList(itemPickupPointDeleteRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setMerchantSku("merchant-sku");
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    pickupPointCodes.add(PICKUP_POINT_CODE_1);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE_1);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(Long.valueOf(2));
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false))
        .thenReturn(Collections.singletonList(deleteItemPickupPoint));
    itemPickupPointUpdateRequestVo.setOnline(false);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService)
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, PICKUP_POINT_CODE_1, false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService, times(2)).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService, times(2)).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(channelService, times(2)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(pickupPointService).deleteItemPickupPointsFromInventory(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(Mockito.anyString(),
        Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertFalse(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertFalse(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getOfflineItemId());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getAllValues().get(0).get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, itemPickupPointListCaptor.getAllValues().get(1).get(0).getPickupPointCode());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(1).get(0).getItemSku());
    Assertions.assertTrue(itemPickupPointListCaptor.getAllValues().get(1).get(0).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListCaptor.getAllValues().get(1).get(0).isCncActive());
  }

  @Test
  public void updateItemPickupPointNoAddAndMultiDeleteUpdate1Test() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(Arrays.asList(itemPickupPointDeleteRequestVo, itemPickupPointDeleteRequestVo1));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setCncActive(true);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    itemViewConfig1.setChannel(Constants.CNC);
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setBuyable(false);
    itemViewConfig2.setDiscoverable(true);
    itemViewConfig2.setChannel(Constants.CNC);
    deleteItemPickupPoint1.setItemViewConfig(Set.of(itemViewConfig2));
    deleteItemPickupPoint.setItemViewConfig(Set.of(itemViewConfig1));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    item.setCncActivated(true);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(Long.valueOf(3));
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(), ITEM_SKU,
            true, false)).thenReturn(Long.valueOf(5));
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1), false))
        .thenReturn(Arrays.asList(deleteItemPickupPoint, deleteItemPickupPoint1));
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Mockito.anyList());
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService)
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(), ITEM_SKU,
            true, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        Mockito.anyString(), Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).deleteItemPickupPointsFromInventory(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1), false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, itemPickupPointListCaptor.getAllValues().get(0).get(0).getPickupPointCode());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemSku());
    Assertions.assertTrue(itemPickupPointListCaptor.getAllValues().get(0).get(0).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListCaptor.getAllValues().get(0).get(0).isCncActive());
  }

  @Test
  public void updateItemPickupPoint_Deletion_Not_Allowed_For_Distribution_PP_Test()
      throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next()
        .setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo.setDeletePickupPointRequests(
        Arrays.asList(itemPickupPointDeleteRequestVo, itemPickupPointDeleteRequestVo1));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU, false)).thenReturn(product);
    itemPickupPoint.setCncActive(true);
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setBuyable(true);
    itemViewConfig1.setDiscoverable(true);
    itemViewConfig1.setChannel(Constants.CNC);
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setBuyable(false);
    itemViewConfig2.setDiscoverable(true);
    itemViewConfig2.setChannel(Constants.CNC);
    deleteItemPickupPoint1.setItemViewConfig(Set.of(itemViewConfig2));
    deleteItemPickupPoint.setItemViewConfig(Set.of(itemViewConfig1));
    deleteItemPickupPoint.setDistribution(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
        Constants.STORE_ID, pickupPointCodes)).thenReturn(
        Arrays.asList(businessPartnerPickupPoint));
    item.setCncActivated(true);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, false)).thenReturn(item);
    when(
        dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
            mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(
        itemPickupPoint);
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID,
        PRODUCT_SKU, Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1), false)).thenReturn(
        Arrays.asList(deleteItemPickupPoint, deleteItemPickupPoint1));
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam,
              itemPickupPointUpdateRequestVo, new HashMap<>(), false, false));
  }

  @Test
  public void updateItemPickupPointNoAddAndMultiDeleteUpdate1Test2() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(Arrays.asList(itemPickupPointDeleteRequestVo, itemPickupPointDeleteRequestVo1));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.CNC);
    deleteItemPickupPoint.setItemViewConfig(Set.of(itemViewConfig));
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(Constants.CNC);
    deleteItemPickupPoint.setItemViewConfig(Set.of(itemViewConfig));
    deleteItemPickupPoint2.setItemViewConfig(Set.of(itemViewConfig2));
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    item.setCncActivated(true);
    when(dataSourceWrapperService
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
            false)).thenReturn(Long.valueOf(3));
    when(dataSourceWrapperService.findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, true, false)).thenReturn(Long.valueOf(5));
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1), false))
        .thenReturn(Arrays.asList(deleteItemPickupPoint, deleteItemPickupPoint1));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointSummaryService
          .updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo, new HashMap<>(),
            false, false));
    } finally {
      verify(dataSourceWrapperService)
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false);
      verify(dataSourceWrapperService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
          Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1), false);
    }
  }

  @Test()
  public void setDeletedL5ForCombinedRequestInDTOTest() {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "includeVariantUpdate", false);
    this.itemPickupPointSummaryService.setDeletedL5ForCombinedRequestInDTO(Arrays.asList(itemPickupPoint),
        new ArrayList<>(), editProductDetailDTO);
  }

  @Test()
  public void setDeletedL5ForCombinedRequestInDTOEmptyItemSkusToBeDeletedTest() {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "includeVariantUpdate", true);
    this.itemPickupPointSummaryService.setDeletedL5ForCombinedRequestInDTO(Arrays.asList(itemPickupPoint),
        new ArrayList<>(), editProductDetailDTO);
  }

  @Test()
  public void setDeletedL5ForCombinedRequestInDTONonEmptyItemSkusToBeDeletedTest() {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "includeVariantUpdate", true);
    this.itemPickupPointSummaryService.setDeletedL5ForCombinedRequestInDTO(Arrays.asList(itemPickupPoint),
        Arrays.asList(ITEM_SKU_2), editProductDetailDTO);
  }

  @Test
  public void updateItemPickupPointNoAddAndMultiDeleteExceptionTest() throws Exception {
    itemPickupPointUpdateRequestVo = new ItemPickupPointUpdateRequestVo();
    itemPickupPointUpdateRequestVo.setProductSku(PRODUCT_SKU);
    itemPickupPointUpdateRequestVo.setProductType(ProductType.BIG_PRODUCT);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(Arrays.asList(itemPickupPointDeleteRequestVo, itemPickupPointDeleteRequestVo1));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    item.setCncActivated(true);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1), false))
        .thenReturn(Arrays.asList(deleteItemPickupPoint, deleteItemPickupPoint1));
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(Long.valueOf(2));
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(), ITEM_SKU,
            true, false)).thenReturn(Long.valueOf(5));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
          new HashMap<>(), false, false));
    } finally {
      verify(dataSourceWrapperService).findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(
          mandatoryRequestParam.getStoreId(), ITEM_SKU, false);
      verify(dataSourceWrapperService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
          Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1), false);
      verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
          Mockito.anyString(), Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    }
  }


  @Test
  public void validateAndUpdateItemPickupPointListingTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService).getDefaultChannel();
  }

  @Test
  public void validateAndUpdateItemPickupPointListingRemoveDuplicateTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "removeDuplicateL5BeforeSave", true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService).getDefaultChannel();
  }

  @Test
  public void validateAndUpdateItemPickupPointListingRemoveDuplicateTest1() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "removeDuplicateL5BeforeSave", true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo, itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService, times(2)).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService, times(2))
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService, times(2)).publishViewConfigChange(itemPickupPoint);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService, times(2)).getDefaultChannel();
  }

  @Test
  public void validateAndUpdateItemPickupPointListingCncTrueTest() throws Exception {
    itemListingUpdateRequestVo.setCncActivated(true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(any())).thenReturn(Arrays.asList(PICKUP_POINT_CODE,
        PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishOfflineItemChangeSellerEvent(Mockito.any(OfflineItemChange.class));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService).getDefaultChannel();
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE_2);
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void validateAndUpdateItemPickupPointListingCncTrueItemNullTest() throws Exception {
    itemListingUpdateRequestVo.setCncActivated(true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(null);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE_2);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE_2);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
          ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo)));
    } finally {
      verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
          mandatoryRequestParam.getStoreId(), ITEM_SKU, false);
      verify(this.channelService, times(1)).getDefaultChannel();
      verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    }
  }

  @Test
  public void validateAndUpdateItemPickupPointListingNotSettingCncFlagAtL4Test() throws Exception {
    itemListingUpdateRequestVo.setCncActivated(true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    item.setCncActivated(true);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(any())).thenReturn(Arrays.asList(PICKUP_POINT_CODE,
        PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(channelService).getDefaultChannel();
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE_2);
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(saveAndPublishService).publishOfflineItemChangeSellerEvent(Mockito.any(OfflineItemChange.class));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
  }

  @Test
  public void validateAndUpdateItemPickupPointListingItemCncActivatedFalseTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    item.setCncActivated(true);
    item.setStoreId(STORE_ID);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);

    when(pickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, true)).thenReturn(3L);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(pickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(), ITEM_SKU, true);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService).getDefaultChannel();
  }

  @Test
  public void validateAndUpdateItemPickupPointListingItemCncActivatedFalseItemPickupPointCncActiveTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    item.setCncActivated(true);
    item.setStoreId(STORE_ID);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setCncActive(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);

    when(pickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, true)).thenReturn(3L);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(pickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(), ITEM_SKU, true);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService).getDefaultChannel();
  }

  @Test
  public void validateAndUpdateItemPickupPointListingItemCncActivatedFalseL5TrueCountZeroTest() throws Exception {
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    item.setCncActivated(true);
    item.setStoreId(STORE_ID);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setCncActive(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);

    when(pickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, true)).thenReturn(1L);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(pickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(), ITEM_SKU, true);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService).getDefaultChannel();
  }

  @Test
  public void validateFbbForNonMppSellerTest() throws Exception {
    ItemPickupPointUpdateRequestVo requestVo = new ItemPickupPointUpdateRequestVo();
    ItemPickupPointListingUpdateRequestVo editRequest = new ItemPickupPointListingUpdateRequestVo();
    editRequest.setFbbActivated(true);
    editRequest.setPickupPointCode(PICKUP_POINT_CODE);
    requestVo.setQuickEditUpdateRequests(Arrays.asList(editRequest));
    Mockito.when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        new HashSet<>(Arrays.asList(PICKUP_POINT_CODE)))).thenReturn(Arrays.asList(PICKUP_POINT_CODE));
    itemPickupPointSummaryService.validateFbbForNonMppSeller(requestVo, mandatoryRequestParam);
    Mockito.verify(businessPartnerPickupPointService)
        .getFbbActivatedPickupPointCodes(new HashSet<>(Arrays.asList(PICKUP_POINT_CODE)));
  }

  @Test
  public void validateFbbForNonMppSellerExceptionTest() throws Exception {
    ItemPickupPointUpdateRequestVo requestVo = new ItemPickupPointUpdateRequestVo();
    ItemPickupPointListingUpdateRequestVo editRequest = new ItemPickupPointListingUpdateRequestVo();
    editRequest.setFbbActivated(true);
    editRequest.setPickupPointCode(PICKUP_POINT_CODE);
    requestVo.setQuickEditUpdateRequests(Arrays.asList(editRequest));
    Mockito.when(businessPartnerPickupPointService.getFbbActivatedPickupPointCodes(
        new HashSet<>(Arrays.asList(PICKUP_POINT_CODE)))).thenReturn(new ArrayList<>());
    try {
      itemPickupPointSummaryService.validateFbbForNonMppSeller(requestVo, mandatoryRequestParam);
    } finally {
      Mockito.verify(businessPartnerPickupPointService)
          .getFbbActivatedPickupPointCodes(new HashSet<>(Arrays.asList(PICKUP_POINT_CODE)));
    }
  }

  @Test
  public void updateItemPickupPointAddAndDeleteUpdateAddDeleteNullRequestTest() throws Exception {
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointListingUpdateRequestVo.setMerchantSku("merchant-sku");
    itemPickupPointUpdateRequestVo
        .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    product.setFbbActivated(true);
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(Collections.singletonList(itemPickupPointDeleteRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);

    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndProductSku(mandatoryRequestParam.getStoreId(),
        product.getProductSku(), false)).thenReturn(Arrays.asList(itemPickupPoint2));

    itemPickupPoint1.setCncActive(true);
    itemPickupPoint1.setMerchantSku("merchant-sku");
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(Long.valueOf(2));
    deleteItemPickupPoint.setCncActive(true);
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false)).thenReturn(
        Collections.singletonList(deleteItemPickupPoint));
    itemPickupPointUpdateRequestVo.setFbbActivated(false);

    itemPickupPointUpdateRequestVo.setAddDeleteVariantRequestVo(null);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo, new HashMap<>(),
      false, false);

    verify(dataSourceWrapperService, times(2)).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU, false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndProductSku(mandatoryRequestParam.getStoreId(),
        product.getProductSku(), false);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService, times(2)).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService, times(2)).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(channelService).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Collections.singletonList(item));
    verify(pickupPointService).deleteItemPickupPointsFromInventory(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Collections.singletonList(PICKUP_POINT_CODE_1), false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        Mockito.anyString(), Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertTrue(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertTrue(
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU.concat(Constants.HYPHEN).concat(PICKUP_POINT_CODE),
        itemPickupPointListCaptor.getAllValues().get(0).get(0).getOfflineItemId());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getAllValues().get(0).get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, itemPickupPointListCaptor.getAllValues().get(1).get(0).getPickupPointCode());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(1).get(0).getItemSku());
    Assertions.assertTrue(itemPickupPointListCaptor.getAllValues().get(1).get(0).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListCaptor.getAllValues().get(1).get(0).isCncActive());

  }
  @Test
  public void moreThan1FbbEditResponseTest() throws Exception {
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
      .thenReturn(product);
    ItemPickupPointListingUpdateRequestVo addPickupPoint = new ItemPickupPointListingUpdateRequestVo();
    addPickupPoint.setFbbActivated(true);
    addPickupPoint.setItemSku(ITEM_SKU);
    addPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointUpdateRequestVo.setAddPickupPointRequests(Arrays.asList(addPickupPoint));
    ItemPickupPointListingUpdateRequestVo editPickupPoint = new ItemPickupPointListingUpdateRequestVo();
    editPickupPoint.setFbbActivated(false);
    editPickupPoint.setItemSku(ITEM_SKU);
    editPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPointUpdateRequestVo.setQuickEditUpdateRequests(Arrays.asList(editPickupPoint));
    ItemPickupPoint existingPickupPoint = new ItemPickupPoint();
    existingPickupPoint.setFbbActivated(true);
    existingPickupPoint.setItemSku(ITEM_SKU);
    existingPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_2);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
      mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false)).thenReturn(
      Arrays.asList(existingPickupPoint));
    EditItemResponse response =
      itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam,
      itemPickupPointUpdateRequestVo,
      new HashMap<>(), true, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        mandatoryRequestParam.getStoreId(), new HashSet<>(Arrays.asList(ITEM_SKU)), Boolean.TRUE, false);
    Assertions.assertEquals(ApiErrorCode.FBB_PICKUP_POINT_ALREADY_EXISTS,
      response.getApiErrorCode());
  }

  @Test
  public void publishPreorderEventAndDeleteL5FromInventoryTest() {
    itemPickupPoint.setMarkForDelete(true);
    editProductDetailDTO.setProductScoreChanged(true);
    editProductDetailDTO.setExistingProductScore(new ProductScoreVo());
    itemPickupPointSummaryService.publishPreorderEventAndDeleteL5FromInventory(new Product(), editProductDetailDTO,
        Arrays.asList(itemPickupPoint));
    verify(pickupPointService).deleteItemPickupPointsFromInventory(itemPickupPointListCaptor.capture());
    verify(objectConverterService).toProductScore(any());
    verify(productService).saveHistoryForProductScoreUpdate(any(), any(), any());
  }

  @Test
  public void publishPreorderEventAndDeleteL5FromInventoryExceptionTest() {
    itemPickupPoint.setMarkForDelete(true);
    editProductDetailDTO.setProductScoreChanged(true);
    editProductDetailDTO.setExistingProductScore(new ProductScoreVo());
    WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO webItemSkuAndPickupPointCodeDTO =
        new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO();
    webItemSkuAndPickupPointCodeDTO.setWebItemSku(ITEM_SKU);
    webItemSkuAndPickupPointCodeDTO.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(
        inventoryOutbound.deleteByItemSkuAndPickupPointCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            Collections.singletonList(webItemSkuAndPickupPointCodeDTO))).thenThrow(ApplicationRuntimeException.class);
    try {
      itemPickupPointSummaryService.publishPreorderEventAndDeleteL5FromInventory(new Product(), editProductDetailDTO,
          Arrays.asList(itemPickupPoint));
    } finally {
      verify(pickupPointService).deleteItemPickupPointsFromInventory(itemPickupPointListCaptor.capture());
      verify(objectConverterService).toProductScore(new ProductScoreVo());
      verify(productService).saveHistoryForProductScoreUpdate(new Product(), null, null);
    }
  }

  @Test
  public void setCncActiveAtProductLevelTest() {
    product.setCncActivated(true);
    item.setCncActivated(false);
    editProductDetailDTO.getAllItemMap().put(ITEM_SKU, item);
    editProductDetailDTO.setCncItemCount(1L);
    boolean cncChanged =
        itemPickupPointSummaryService.setCncActiveAtProductLevel(PRODUCT_SKU, Set.of(item), product,
          false);
    Assertions.assertTrue(cncChanged);
  }

  @Test
  public void setCncActiveAtProductLevelProductCncFalseTest() {
    product.setCncActivated(false);
    item.setCncActivated(false);
    editProductDetailDTO.getAllItemMap().put(ITEM_SKU, item);
    boolean cncChanged =
        itemPickupPointSummaryService.setCncActiveAtProductLevel(PRODUCT_SKU, Set.of(item), product,
          false);
    Assertions.assertFalse(cncChanged);
  }

  @Test
  public void updateItemPickupPoint_cncSwitchTrueTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemListingUpdateRequestVo.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).build());
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(false);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.getAllItemViewConfigs().removeIf(itemViewConfig -> itemViewConfig.getChannel().equals(Constants.CNC));
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(true);
    product.setCncActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(itemService.getItemsByProductSku(Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(item));
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), itemListingUpdateRequestVo.getItemSku(),
        itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), false, true, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), itemListingUpdateRequestVo.getItemSku(),
        itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(2)).getCncChannel();
  }

  @Test
  public void updateItemPickupPoint_cncSwitchTrue_noViewConfigChangeTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemListingUpdateRequestVo.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).build());
    itemPickupPoint.getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).build());
    itemListingUpdateRequestVo.setMerchantSku(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.setPrice(prices);
    itemListingUpdateRequestVo.setCncActivated(false);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    prices.iterator().next().setListPrice(9999);
    prices.iterator().next().setOfferPrice(9999);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.setPrice(prices);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    product.setOnline(true);
    product.setCncActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(itemService.getItemsByProductSku(Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(item));
    when(dataSourceWrapperService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean())).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), itemListingUpdateRequestVo.getItemSku(),
        itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo), new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), false, true, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), itemListingUpdateRequestVo.getItemSku(),
        itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(dataSourceWrapperService).getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(2)).getCncChannel();
  }

  @Test
  public void updateItemPickupPoint_cncSwitchTrue_noViewConfigChangeTest2() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemListingUpdateRequestVo.setItemViewConfigs(new HashSet<>(itemListingUpdateRequestVo.getItemViewConfigs()));
    itemListingUpdateRequestVo.getItemViewConfigs().add(ItemViewConfig.builder().isDiscoverable(true).channel(Constants.CNC).build());
    itemPickupPoint.getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).build());
    itemPickupPoint.setMerchantSku(MERCHANT_SKU);
    itemListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU_2);
    item.setArchived(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        new ArrayList<>(Collections.singletonList(itemPickupPoint2)));
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "true", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), anyList(),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo),new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), null, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
      false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID,ITEM_SKU);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(2)).getCncChannel();
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
  }


  @Test
  public void validateAndUpdateItemPickupPointListing_cncWarehouseSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(this.channelService, times(6)).getCncChannel();
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService, times(2)).getDefaultChannel();
  }

  @Test
  public void validateAndUpdateItemPickupPointListing_cncWarehouseSwitchOn_notOfflineTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isBuyable(true).build());
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.getAllItemViewConfigs().forEach(itemViewConfig -> itemViewConfig.setBuyable(true));
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(this.channelService, times(6)).getCncChannel();
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService, times(4)).getDefaultChannel();
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE_2);
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void validateAndUpdateItemPickupPointListing_cncWarehouseSwitchOn_setOfflineTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    item.setCncActivated(true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).build());
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    when(pickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, Constants.CNC)).thenReturn(3L);
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(this.channelService, times(11)).getCncChannel();
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(pickupPointService).findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(null,
        ITEM_SKU, Constants.CNC);
    verify(channelService, times(4)).getDefaultChannel();
  }

  @Test
  public void validateAndUpdateItemPickupPointListing_cncWarehouseSwitchOn_setTeaserTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(true).build());
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.getAllItemViewConfigs().forEach(itemViewConfig -> itemViewConfig.setBuyable(true));
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(this.channelService, times(6)).getCncChannel();
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService, times(4)).getDefaultChannel();
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE_2);
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEditCncTrue_switchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>(itemPickupPointListingUpdateRequestVo.getItemViewConfigs());
    itemViewConfigSet.iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemViewConfigSet.add(ItemViewConfig.builder().channel(Constants.CNC).build());
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(itemViewConfigSet);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(this.channelService, times(6)).getCncChannel();
    verify(this.channelService, times(4)).getDefaultChannel();
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEditCncTrue_switchOn_Diff_Merchant_Code_For_Distribution_Status_Test()
      throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>(itemPickupPointListingUpdateRequestVo.getItemViewConfigs());
    itemViewConfigSet.iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemViewConfigSet.add(ItemViewConfig.builder().channel(Constants.CNC).build());
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(itemViewConfigSet);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    product.setMerchantCode("MERCHANT_CODE");
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(this.channelService, times(6)).getCncChannel();
    verify(this.channelService, times(4)).getDefaultChannel();
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEdit_l5CncTrue_switchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>(itemPickupPointListingUpdateRequestVo.getItemViewConfigs());
    itemViewConfigSet.iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemViewConfigSet.add(ItemViewConfig.builder().channel(Constants.CNC).build());
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(4)).getCncChannel();
    verify(this.channelService, times(2)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEdit_l3CncTrue_switchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>(itemPickupPointListingUpdateRequestVo.getItemViewConfigs());
    itemViewConfigSet.iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemViewConfigSet.add(ItemViewConfig.builder().channel(Constants.CNC).build());
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(itemViewConfigSet);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(6)).getCncChannel();
    verify(this.channelService, times(4)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEdit_requestCncTrue_switchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>(itemPickupPointListingUpdateRequestVo.getItemViewConfigs());
    itemViewConfigSet.iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemViewConfigSet.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.CNC).build());
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(itemViewConfigSet);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(4)).getCncChannel();
    verify(this.channelService, times(4)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Mockito.anyList());
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEdit_requestAndEntityCncTrue_switchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>(itemPickupPointListingUpdateRequestVo.getItemViewConfigs());
    itemViewConfigSet.iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemViewConfigSet.add(ItemViewConfig.builder().isBuyable(true).isDiscoverable(true).channel(Constants.CNC).build());
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(itemViewConfigSet);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
     List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(4)).getCncChannel();
    verify(this.channelService, times(4)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID, pickupPointCodes);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Mockito.anyList());
  }

  @Test
  public void validateAndUpdateItemPickupPointListing_cncWarehouseSwitchOn_deliveryFlagOFF() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemListingUpdateRequestVo.getItemViewConfigs()
        .add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(true).build());
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.getAllItemViewConfigs().forEach(itemViewConfig -> itemViewConfig.setBuyable(true));
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, false)).thenReturn(item);
    when(
        dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
            itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(),
            false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX)).thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(),
        eq(new ArrayList<>()), eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class), any(ReindexType.class),
            Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE_2);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE_2);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));
    verify(this.channelService, times(10)).getCncChannel();
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        STORE_ID, itemListingUpdateRequestVo.getItemSku(), itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(),
        USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService, times(6)).getDefaultChannel();
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        pickupPointCodes);
  }

  @Test
  public void updateItemPickupPointAddCncL5UpdateL4UpdateQuickEditCncTeaser_switchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>(itemPickupPointListingUpdateRequestVo.getItemViewConfigs());
    itemViewConfigSet.iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemViewConfigSet.add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(true).build());
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(itemViewConfigSet);
    itemPickupPointListingUpdateRequestVo.setPrice(prices);
    itemPickupPointUpdateRequestVo
        .setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,false))
        .thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(PICKUP_POINT_CODE));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService
        .findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false))
        .thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService)
        .findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(),
            itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(this.channelService, times(4)).getCncChannel();
    verify(this.channelService, times(4)).getDefaultChannel();
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Mockito.anyList());
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(Collections.singletonList(PICKUP_POINT_CODE)));
  }

  @Test
  public void updateItemPickupPointAddOfflineCncL5Update_cncSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "updatedMerchantSkuFromNewlyAddedL5", true);
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU);
    ItemViewConfig defaultItemViewConfig = ItemViewConfig.builder().channel(Constants.DEFAULT_CHANNEL).build();
    ItemViewConfig cncItemViewConfig = ItemViewConfig.builder().channel(Constants.CNC).build();
    HashSet<ItemViewConfig> updatedItemViewConfigs =
        new HashSet<>(Arrays.asList(defaultItemViewConfig, cncItemViewConfig));
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(updatedItemViewConfigs);
    itemPickupPointUpdateRequestVo
        .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    itemPickupPointUpdateRequestVo.setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setItemViewConfig(updatedItemViewConfigs);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        GdnMandatoryRequestParameterUtil.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(PICKUP_POINT_CODE));
    item.setCncActivated(true);
    item.setMerchantSku("merchant-sku-1");
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        true)).thenReturn(item);
    when(pickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU,
        Arrays.asList(PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2))).thenReturn(
        Collections.singletonList(itemPickupPoint1));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(channelService, times(8)).getDefaultChannel();
    verify(channelService, times(10)).getCncChannel();
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    verify(pickupPointService, times(2)).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(Mockito.any(), Mockito.any(), Mockito.any());
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    // verify(businessPartnerPickupPointService).getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertFalse(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertFalse(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertEquals(MERCHANT_SKU, itemPickupPointListCaptor.getValue().get(0).getMerchantSku());
  }

  @Test
  public void updateItemPickupPointAddCncBuyableL5Update_cncSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "updatedMerchantSkuFromNewlyAddedL5", true);
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU);
    ItemViewConfig defaultItemViewConfig = ItemViewConfig.builder().channel(Constants.DEFAULT_CHANNEL).build();
    ItemViewConfig cncItemViewConfig = ItemViewConfig.builder().channel(Constants.CNC).isBuyable(true).build();
    HashSet<ItemViewConfig> updatedItemViewConfigs =
        new HashSet<>(Arrays.asList(defaultItemViewConfig, cncItemViewConfig));
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(updatedItemViewConfigs);
    itemPickupPointUpdateRequestVo
        .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    itemPickupPointUpdateRequestVo.setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setItemViewConfig(updatedItemViewConfigs);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        GdnMandatoryRequestParameterUtil.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(PICKUP_POINT_CODE));
    item.setCncActivated(true);
    item.setMerchantSku("merchant-sku-1");
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        true)).thenReturn(item);
    when(pickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU,
        Arrays.asList(PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2))).thenReturn(
        Collections.singletonList(itemPickupPoint1));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(channelService, times(8)).getDefaultChannel();
    verify(channelService, times(6)).getCncChannel();
    verify(businessPartnerPickupPointService, times(2)).getBusinessPartnerPickupPointByPickupPointCodes(anyString(), anyList());
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    verify(pickupPointService, times(2)).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(Mockito.any(), Mockito.any(), Mockito.any());
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    // verify(businessPartnerPickupPointService).getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertFalse(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertFalse(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertEquals(MERCHANT_SKU, itemPickupPointListCaptor.getValue().get(0).getMerchantSku());
  }

  @Test
  public void updateItemPickupPointAddCncTeasereL5Update_cncSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "updatedMerchantSkuFromNewlyAddedL5", true);
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU);
    ItemViewConfig defaultItemViewConfig = ItemViewConfig.builder().channel(Constants.DEFAULT_CHANNEL).build();
    ItemViewConfig cncItemViewConfig = ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(true).build();
    HashSet<ItemViewConfig> updatedItemViewConfigs =
        new HashSet<>(Arrays.asList(defaultItemViewConfig, cncItemViewConfig));
    itemPickupPointListingUpdateRequestVo.setItemViewConfigs(updatedItemViewConfigs);
    itemPickupPointUpdateRequestVo
        .setAddPickupPointRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    itemPickupPointUpdateRequestVo.setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointListingUpdateRequestVo));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setCncActive(true);
    itemPickupPoint.setItemViewConfig(updatedItemViewConfigs);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        GdnMandatoryRequestParameterUtil.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(new ItemPickupPointDataChangeEventModel());
    when(businessPartnerPickupPointService.getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE)))
        .thenReturn(Collections.singletonList(PICKUP_POINT_CODE));
    item.setCncActivated(true);
    item.setMerchantSku("merchant-sku-1");
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        true)).thenReturn(item);
    when(pickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeInAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU,
        Arrays.asList(PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2))).thenReturn(
        Collections.singletonList(itemPickupPoint1));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    verify(channelService, times(8)).getDefaultChannel();
    verify(channelService, times(6)).getCncChannel();
    verify(businessPartnerPickupPointService, times(2)).getBusinessPartnerPickupPointByPickupPointCodes(anyString(), anyList());
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(anyList());
    verify(pickupPointService, times(2)).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(mandatoryRequestParam.getStoreId(),
        itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode(), false);
    verify(pickupPointService).validatePriceChangeAndSet(Mockito.any(), Mockito.any(), Mockito.any());
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    // verify(businessPartnerPickupPointService).getCncActivatedPickupPointCodes(Collections.singleton(PICKUP_POINT_CODE));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    Assertions.assertEquals(Constants.DEFAULT_CHANNEL,
        itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().getChannel());
    Assertions.assertFalse(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isDiscoverable());
    Assertions.assertFalse(itemPickupPointListCaptor.getValue().get(0).getItemViewConfig().iterator().next().isBuyable());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getValue().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointListCaptor.getValue().get(0).getPickupPointCode());
    Assertions.assertEquals(MERCHANT_SKU, itemPickupPointListCaptor.getValue().get(0).getMerchantSku());
  }

  @Test
  public void updateItemPickupPointNoAddAndMultiDeleteUpdate_cncSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemPickupPointListingUpdateRequestVo.setWholesalePriceActivated(true);
    itemPickupPointListingUpdateRequestVo.setCncActivated(true);
    itemPickupPointListingUpdateRequestVo.getItemViewConfigs().iterator().next().setChannel(Constants.DEFAULT_CHANNEL);
    itemPickupPointUpdateRequestVo
        .setDeletePickupPointRequests(Arrays.asList(itemPickupPointDeleteRequestVo, itemPickupPointDeleteRequestVo1));
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    itemPickupPoint.setCncActive(true);
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    List<String> pickupPointCodes = new ArrayList<>();
    pickupPointCodes.add(PICKUP_POINT_CODE);
    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(Constants.STORE_ID, pickupPointCodes)).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    item.setCncActivated(true);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(
        mandatoryRequestParam.getStoreId(), ITEM_SKU, PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    when(dataSourceWrapperService
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU, false))
        .thenReturn(Long.valueOf(3));
    when(dataSourceWrapperService.findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(
        mandatoryRequestParam.getStoreId(), ITEM_SKU)).thenReturn(Long.valueOf(5));
    when(dataSourceWrapperService.findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1), false))
        .thenReturn(Arrays.asList(deleteItemPickupPoint, deleteItemPickupPoint1));
    itemPickupPointSummaryService.updateItemPickupPoint(mandatoryRequestParam, itemPickupPointUpdateRequestVo,
        new HashMap<>(), false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setCncActivated(true);
    verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Mockito.anyList());
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(pickupPointService).publishItemPickupPointChangeEvent(itemPickupPointCaptor.capture());
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService)
        .findItemPickupPointCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(mandatoryRequestParam.getStoreId(), ITEM_SKU);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuInAndMarkForDeleteFalseAndFbbActivated(
        Mockito.anyString(), Mockito.anySet(), Mockito.anyBoolean(), Mockito.anyBoolean());
    verify(pickupPointService).saveItemPickupPoint(itemPickupPointListCaptor.capture());
    verify(pickupPointService).deleteItemPickupPointsFromInventory(itemPickupPointListCaptor.capture());
    verify(dataSourceWrapperService).findItemPickupPointsByProductSkuAndPickupPointCodes(STORE_ID, PRODUCT_SKU,
        Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1), false);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, itemPickupPointListCaptor.getAllValues().get(0).get(0).getPickupPointCode());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointListCaptor.getAllValues().get(0).get(0).getItemSku());
    Assertions.assertTrue(itemPickupPointListCaptor.getAllValues().get(0).get(0).isMarkForDelete());
    Assertions.assertFalse(itemPickupPointListCaptor.getAllValues().get(0).get(0).isCncActive());
  }

  @Test
  public void validateAndUpdateItemPickupPointListing_singleL5ItemCncActivatedFalseFromTeaserTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setItemViewConfigs(
        Collections.singleton(ItemViewConfig.builder().channel(Constants.CNC).build()));
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    item.setCncActivated(true);
    item.setStoreId(STORE_ID);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(true).build());
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);

    when(pickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, true)).thenReturn(3L);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(pickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(mandatoryRequestParam.getStoreId(), ITEM_SKU, Constants.CNC);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService, times(2)).getDefaultChannel();
    verify(channelService, times(7)).getCncChannel();
  }

  @Test
  public void validateAndUpdateItemPickupPointListing_singleL5ItemCncActivatedFalseFromB2BTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setItemViewConfigs(
        Collections.singleton(ItemViewConfig.builder().channel(Constants.CNC).build()));
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    item.setCncActivated(true);
    item.setStoreId(STORE_ID);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isBuyable(true).build());
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);

    when(pickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, true)).thenReturn(3L);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(pickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(mandatoryRequestParam.getStoreId(), ITEM_SKU, Constants.CNC);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService, times(2)).getDefaultChannel();
    verify(channelService, times(7)).getCncChannel();
  }

  @Test
  public void validateAndUpdateItemPickupPointListing_singleL5ItemCncActivatedFalseFromOfflineTest() throws Exception {
    ReflectionTestUtils.setField(itemPickupPointSummaryService, "cncForWarehouseFeatureSwitch", true);
    itemListingUpdateRequestVo.setMerchantSku(null);
    itemListingUpdateRequestVo.setItemViewConfigs(
        Collections.singleton(ItemViewConfig.builder().channel(Constants.CNC).build()));
    itemListingUpdateRequestVo.setPickupPointCode(PICKUP_POINT_CODE_2);
    item.setCncActivated(true);
    item.setStoreId(STORE_ID);
    Set<String> promoBundling = new HashSet<>();
    promoBundling.add(Constants.WHOLESALE_PRICE);
    itemPickupPoint.setActivePromoBundlings(promoBundling);
    itemPickupPoint.getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isBuyable(true).build());
    itemListingUpdateRequestVo.setWholesalePriceActivated(false);

    when(pickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncActive(mandatoryRequestParam.getStoreId(),
        ITEM_SKU, true)).thenReturn(3L);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "false", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(true);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(pickupPointService.saveItemPickupPoint(Mockito.anyList()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    when(channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT_CHANNEL);
    when(channelService.getCncChannel()).thenReturn(Constants.CNC);
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());

    itemPickupPointSummaryService.validateAndUpdateItemPickupPointListing(mandatoryRequestParam, PRODUCT_SKU,
        ProductType.REGULAR, Arrays.asList(itemListingUpdateRequestVo));

    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(pickupPointService)
        .validatePriceChangeAndSet(itemPickupPoint, itemListingUpdateRequestVo.getPrice(), USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(pickupPointService)
        .findCountByStoreIdAndItemSkuAndMarkForDeleteFalseAndCncViewConfig(mandatoryRequestParam.getStoreId(), ITEM_SKU, Constants.CNC);
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    verify(channelService, times(2)).getDefaultChannel();
    verify(channelService, times(7)).getCncChannel();
  }

  @Test
  public void updateItemPickupPointListingOnlineFlagChange() {
    product.setOnline(true);
    itemPickupPoint.setMerchantSku(MERCHANT_SKU);
    itemListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU_2);
    item.setArchived(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        new ArrayList<>(Collections.singletonList(itemPickupPoint2)));
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "true", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), anyList(),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo),new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), false, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
        false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID,ITEM_SKU);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertFalse(productArgumentCaptor.getValue().isOnline());
  }

  @Test
  public void updateItemPickupPointListingOnlineTrueChange() {
    product.setOnline(false);
    itemPickupPoint.setMerchantSku(MERCHANT_SKU);
    itemListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU_2);
    item.setArchived(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        new ArrayList<>(Collections.singletonList(itemPickupPoint2)));
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "true", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), anyList(),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo),new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), true, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
        false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID,ITEM_SKU);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertTrue(productArgumentCaptor.getValue().isOnline());
  }

  @Test
  public void updateItemPickupPointListingOnlineTrueWithB2cFalseChange() {
    product.setOnline(false);
    itemPickupPoint.setMerchantSku(MERCHANT_SKU);
    itemListingUpdateRequestVo.setMerchantSku(MERCHANT_SKU_2);
    item.setArchived(true);
    itemListingUpdateRequestVo.setWholesalePriceActivated(true);
    when(dataSourceWrapperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false)).thenReturn(product);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false)).thenReturn(itemPickupPoint);
    when(pickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        new ArrayList<>(Collections.singletonList(itemPickupPoint2)));
    when(pickupPointHelperService.isItemViewConfigChangeForExistingChannel(itemPickupPoint,
        itemListingUpdateRequestVo.getItemViewConfigs())).thenReturn(true);
    SystemParameter systemParameter = new SystemParameter(STORE_ID, null, "true", null);
    when(systemParameterService
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX))
        .thenReturn(systemParameter);
    product.setB2cActivated(true);
    when(saveOperationService.saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), anyList(),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP))).thenReturn(product);
    doNothing().when(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    when(pickupPointService.validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME)).thenReturn(false);
    when(pickupPointService.saveItemPickupPoint(Arrays.asList(itemPickupPoint))).thenReturn(Arrays.asList(itemPickupPoint));
    when(
        objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class), Mockito.anyBoolean())).thenReturn(
        new ItemPickupPointDataChangeEventModel());
    itemPickupPointUpdateRequestVo.setB2cActivated(false);
    itemPickupPointSummaryService.updateItemPickupPointListing(PRODUCT_SKU, ProductType.REGULAR,
        Arrays.asList(itemListingUpdateRequestVo),new HashSet<>(), new HashMap<>(), new HashMap<>(), new HashSet<>(),
        new HashSet<>(), true, null, editItemResponse, false, null, false, null, itemPickupPointUpdateRequestVo, null,
        false, false);
    verify(dataSourceWrapperService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU,
        false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(mandatoryRequestParam.getStoreId(), ITEM_SKU,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        itemListingUpdateRequestVo.getItemSku(),itemListingUpdateRequestVo.getPickupPointCode(), false);
    verify(saveOperationService)
        .saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(Arrays.asList(item));
    verify(systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.ENABLE_DEFERRED_SOLR_REINDEX);
    verify(pickupPointService).validatePriceChangeAndSet(itemPickupPoint,
        itemListingUpdateRequestVo.getPrice(),USERNAME);
    verify(pickupPointService).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        Mockito.any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishItemDataChangeEvent(Mockito.anyList());
    verify(pickupPointService).saveItemPickupPoint(Arrays.asList(itemPickupPoint));
    verify(saveAndPublishService).publishViewConfigChange(itemPickupPoint);
    verify(saveOperationService)
        .deferredReindexItems(eq(STORE_ID), any(List.class), any(Boolean.class),
            any(ReindexType.class), Mockito.anyBoolean());
    verify(pickupPointService).findByStoreIdAndItemSku(STORE_ID,ITEM_SKU);
    verify(saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(), eq(Collections.singletonList(ProductChangeEventType.PICKUP_POINT_CHANGE)),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(any(), any());
    Assertions.assertFalse(productArgumentCaptor.getValue().isOnline());
  }


}
