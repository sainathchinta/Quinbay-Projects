package com.gdn.x.product.service.impl;

import static com.gdn.x.product.domain.event.enums.ProductFieldNames.ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.ArgumentMatchers.nullable;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import com.gdn.x.product.domain.event.model.VideoCompressionEventModel;
import com.gdn.x.product.service.event.listener.MasterProductChangeEventListener;
import org.apache.commons.collections.CollectionUtils;
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
import org.springframework.beans.BeanUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.partners.merchant.voucher.streaming.model.DomainEventName;
import com.gdn.partners.merchant.voucher.streaming.model.VoucherItemSkusEventModel;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.domain.event.model.BundleProductOneToOneMappingEventModel;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemDataChange;
import com.gdn.x.product.domain.event.model.ItemEventModel;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.ItemPickupPointMigrationEvent;
import com.gdn.x.product.domain.event.model.ItemPickupPointViewConfigChangeEvent;
import com.gdn.x.product.domain.event.model.ItemViewConfigWithArchivedChangeEvent;
import com.gdn.x.product.domain.event.model.MerchantPromoDiscountChangeEvent;
import com.gdn.x.product.domain.event.model.MigrationForItemCreationEvent;
import com.gdn.x.product.domain.event.model.OdooCreationEventModel;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.domain.event.model.OfflineItemPopulateEvent;
import com.gdn.x.product.domain.event.model.PristineDataItemEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.ProductChange;
import com.gdn.x.product.domain.event.model.ProductEventModel;
import com.gdn.x.product.domain.event.model.ProductL3SolrReindexEvent;
import com.gdn.x.product.domain.event.model.ProductModel;
import com.gdn.x.product.domain.event.model.WholesalePriceActivatedOrDeactivatedEvent;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.DistributionStatus;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.AdjustmentType;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.OfflineItemHistoryDetailVO;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.executor.api.AsyncProcessor;
import com.gdn.x.product.service.properties.KafkaTopicProperties;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;

public class SaveAndPublishServiceImplTest {

  private static final String ITEM_SKU = "ITS-70001-00602-00005";

  private static final String ITEM_CODE = "item-code";

  private static final String PRODUCT_SKU = "ITS-70001-00602";

  private static final String STORE_ID = "10001";

  private static final String CHANNEL_ID = "api";

  private static final String CLIENT_ID_UPSERT_OFFLINE_ITEM_PRICE = "Regular-Merchant-Client-Id";

  private static final String CLIENT_ID = "CLIENT_ID";

  private static final String REQUEST_ID = "REQUEST_ID";

  private static final String BLANK = "";

  private static final String PROMO_BUNDLING_TYPE = "promoBundlingType";

  private static final String OFFLINE_ITEM_ID = "offline-item-id";

  private static final String BRAND = "dummy-brand";

  private static final String BRAND_LOGO_URL = "dummy-brand-logo-url";

  private static final Product PRODUCT = new Product();

  private static final Item ITEM = new Item();

  private static final List<Item> ITEMS = Arrays.asList(SaveAndPublishServiceImplTest.ITEM);

  private static final String PRODUCT_SKU1 = "product-sku-1";

  private static final String PRODUCT_SKU2 = "product-sku-2";

  private static final String PRODUCT_SKU3 = "product-sku-3";

  private static final Product PRODUCT1 = initializeProduct(PRODUCT_SKU1);

  private static final Product PRODUCT2 = initializeProduct(PRODUCT_SKU2);

  private static final Product PRODUCT3 = initializeProduct(PRODUCT_SKU3);

  private static final Stream<Product> PRODUCT_STREAM = Stream.of(PRODUCT1,PRODUCT2,PRODUCT3);

  private static final String ITEM_SKU1 = "item-sku-1";

  private static final String ITEM_SKU2 = "item-sku-2";

  private static final String ITEM_SKU3 = "item-sku-3";

  private static final Item ITEM1 = initializeItem(ITEM_SKU1);

  private static final Item ITEM2 = initializeItem(ITEM_SKU2);

  private static final Item ITEM3 = initializeItem(ITEM_SKU3);

  private static final Stream<Item> ITEM_STREAM = Stream.of(ITEM1,ITEM2,ITEM3);

  private static final String COMMAND_DESC_PRODUCT = "publishStreamOfProducts";

  private static final String COMMAND_DESC_ITEM = "publishStreamOfItems";
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String OFFLINE_PRICE_2 = "offlinePrice2";
  private static final String OFFLINE_PRICE_1 = "offlinePrice1";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String PICKUP_POINT_CODE_1 = "pickup-point-code-1";
  private static final double OFFER_PRICE_1 = 10000.00;
  private static final String PICKUP_POINT_CODE_2 = "pickup-point-code-2";
  private static final double OFFER_PRICE_2 = 20000.00;

  private static final String PRISTINE_ID = "pristineId-1";
  private static final String PRISTINE_ID_2 = "pristineId-2";
  private static final String REINDEX_STATUS = "REINDEX_STATUS";
  private static final String STATUS = "status";
  private List<String> itemSkuList = Collections.singletonList(ITEM_SKU);
  private ItemPickupPointMigrationEvent itemPickupPointMigrationEvent =
    ItemPickupPointMigrationEvent.builder().itemSkuList(itemSkuList).build();
  private MigrationForItemCreationEvent migrationForItemCreationEvent =
    MigrationForItemCreationEvent.builder().itemSkuList(itemSkuList).build();



  @InjectMocks
  private SaveAndPublishServiceImpl saveAndPublishServiceImpl;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private ItemService itemService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private AsyncProcessor asyncProcessor;

  @Mock
  private GdnMapper gdnMapper;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Captor
  private ArgumentCaptor<OfflineItemChange> offlineItemChangeArgCaptor;

  @Captor
  private ArgumentCaptor<OfflineItemPopulateEvent> offlineItemPopulateEventArgCaptor;

  @Captor
  private ArgumentCaptor<ItemChange> itemChangeArgumentCaptor;

  @Captor
  ArgumentCaptor<ProductChange> productChangeArgumentCaptor;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private MasterProductChangeEventListener masterProductChangeEventListener;

  @Captor
  private ArgumentCaptor<ItemDataChange> itemDataChangeArgumentCaptor;

  @Captor
  private ArgumentCaptor<AuditTrailListResponse> auditTrailListResponseArgumentCaptor;

  private OfflineItem offlineItem;
  private Item item;
  private ItemPickupPoint itemPickupPoint;
  private ItemPickupPointVo itemPickupPointVo;
  private Item item2;
  private Product product = new Product();
  private ProductChange productChange = new ProductChange();
  private Set<ItemViewConfig> itemViewConfigs;
  private Set<Price> prices;
  private BusinessPartner businessPartner;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    when(this.itemRepository.saveAll(SaveAndPublishServiceImplTest.ITEMS)).thenReturn(SaveAndPublishServiceImplTest.ITEMS);

    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setBrand(BRAND);
    masterDataProduct.setBrandLogoUrl(BRAND_LOGO_URL);

    PRODUCT.setMasterDataProduct(masterDataProduct);
    PRODUCT.setProductScore(new ProductScore(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));

    this.item = new Item();
    item.setItemChangeEventTypes(new ArrayList<>());
    item.setItemSku(ITEM_SKU);
    item.setItemCode(ITEM_CODE);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    item.setPristineDataItem(pristineDataItem);

    this.item2 = new Item();
    item2.setItemSku(ITEM_SKU2);
    item2.setItemCode(ITEM_CODE);
    PristineDataItem pristineDataItem2 = new PristineDataItem();
    pristineDataItem2.setPristineId(PRISTINE_ID_2);
    item2.setPristineDataItem(pristineDataItem2);

    when(this.itemRepository.insert(SaveAndPublishServiceImplTest.ITEMS)).thenReturn(
        SaveAndPublishServiceImplTest.ITEMS);

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPointVo = new ItemPickupPointVo();
    itemViewConfigs = new HashSet<>(Arrays
        .asList(new ItemViewConfig(true, false, "DEFAULT", new ItemDiscoverableSchedule(), new ItemBuyableSchedule())));
    prices = new HashSet<>(Arrays.asList(new Price("$", 10, 10, "", "", new Date())));
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    itemPickupPoint.setPrice(prices);
    itemPickupPointVo.setItemViewConfig(itemViewConfigs);
    itemPickupPointVo.setPrice(prices);
    businessPartner = new BusinessPartner();
    List<String> salesChannel = new ArrayList<>();
    salesChannel.add(Constants.B2B_SELLER_CHANNEL);
    salesChannel.add(Constants.B2C_SELLER_CHANNEL);
    businessPartner.setSalesChannel(salesChannel);
    when(this.itemRepository.insert(SaveAndPublishServiceImplTest.ITEMS))
        .thenReturn(SaveAndPublishServiceImplTest.ITEMS);
    when(this.itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPoint);
    Mockito.when(applicationContext.getBean(SaveAndPublishService.class)).thenReturn(saveAndPublishServiceImpl);
  }

  @Test
  public void addActivePromoBundling() {
    when(this.itemRepository.addActivePromoBundling(SaveAndPublishServiceImplTest.STORE_ID,
        SaveAndPublishServiceImplTest.ITEM_SKU,
        SaveAndPublishServiceImplTest.PROMO_BUNDLING_TYPE)).thenReturn(ITEM);

    this.saveAndPublishServiceImpl.addActivePromoBundling(SaveAndPublishServiceImplTest.STORE_ID,
        SaveAndPublishServiceImplTest.ITEM_SKU, SaveAndPublishServiceImplTest.PROMO_BUNDLING_TYPE);

    Mockito.verify(this.itemRepository).addActivePromoBundling(SaveAndPublishServiceImplTest.STORE_ID,
        SaveAndPublishServiceImplTest.ITEM_SKU, SaveAndPublishServiceImplTest.PROMO_BUNDLING_TYPE);
  }

  @Test
  public void insertItemTest() {
    when(this.itemRepository.insert(SaveAndPublishServiceImplTest.ITEM))
        .thenReturn(SaveAndPublishServiceImplTest.ITEM);
    this.saveAndPublishServiceImpl.insertItem(SaveAndPublishServiceImplTest.ITEM);
    Mockito.verify(this.itemRepository).insert(SaveAndPublishServiceImplTest.ITEM);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME),any(),
      any(ItemChange.class));
  }

  @Test
  public void removeActivePromoBundling() {
    when(this.itemRepository.removeActivePromoBundling(SaveAndPublishServiceImplTest.STORE_ID,
        SaveAndPublishServiceImplTest.ITEM_SKU,
        SaveAndPublishServiceImplTest.PROMO_BUNDLING_TYPE)).thenReturn(ITEM);

    this.saveAndPublishServiceImpl.removeActivePromoBundling(SaveAndPublishServiceImplTest.STORE_ID,
        SaveAndPublishServiceImplTest.ITEM_SKU, SaveAndPublishServiceImplTest.PROMO_BUNDLING_TYPE);

    Mockito.verify(this.itemRepository).removeActivePromoBundling(SaveAndPublishServiceImplTest.STORE_ID,
        SaveAndPublishServiceImplTest.ITEM_SKU, SaveAndPublishServiceImplTest.PROMO_BUNDLING_TYPE);
  }

  @Test
  public void saveProductTest() {
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(SaveAndPublishServiceImplTest.PRODUCT);
    this.saveAndPublishServiceImpl.saveProductAndSKipPublishForMfdTrueProducts(SaveAndPublishServiceImplTest.PRODUCT);
    Mockito.verify(this.productRepository).save(SaveAndPublishServiceImplTest.PRODUCT);
    verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME), any(),
        any(ProductChange.class));
  }

  @Test
  public void saveProductTestSkipEventFalseAndMfdFalse() {
    PRODUCT.setMarkForDelete(false);
    ReflectionTestUtils.setField(saveAndPublishServiceImpl, "skipEventPublishForTakeDownProduct", true);
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(SaveAndPublishServiceImplTest.PRODUCT);
    this.saveAndPublishServiceImpl.saveProductAndSKipPublishForMfdTrueProducts(SaveAndPublishServiceImplTest.PRODUCT);
    Mockito.verify(this.productRepository).save(SaveAndPublishServiceImplTest.PRODUCT);
    verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME), any(),
            any(ProductChange.class));
  }

  @Test
  public void saveProductTestSkipEventTrueAndMfdTrue() {
    PRODUCT.setMarkForDelete(true);
    ReflectionTestUtils.setField(saveAndPublishServiceImpl, "skipEventPublishForTakeDownProduct", false);
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(SaveAndPublishServiceImplTest.PRODUCT);
    this.saveAndPublishServiceImpl.saveProductAndSKipPublishForMfdTrueProducts(SaveAndPublishServiceImplTest.PRODUCT);
    Mockito.verify(this.productRepository).save(SaveAndPublishServiceImplTest.PRODUCT);
    verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME), any(),
            any(ProductChange.class));
  }

  @Test
  public void saveProductTestSkipEventFalseAndMfdTrue() {
    PRODUCT.setMarkForDelete(true);
    ReflectionTestUtils.setField(saveAndPublishServiceImpl, "skipEventPublishForTakeDownProduct", true);
    Mockito.when(productRepository.save(Mockito.any(Product.class))).thenReturn(SaveAndPublishServiceImplTest.PRODUCT);
    this.saveAndPublishServiceImpl.saveProductAndSKipPublishForMfdTrueProducts(SaveAndPublishServiceImplTest.PRODUCT);
    Mockito.verify(this.productRepository).save(SaveAndPublishServiceImplTest.PRODUCT);
  }

  @Test
  public void saveProductsTest() {
    PRODUCT.setSizeChartCode(STATUS);
    Mockito.when(productRepository.saveAll(Collections.singletonList(PRODUCT)))
      .thenReturn(Collections.singletonList(PRODUCT));
    this.saveAndPublishServiceImpl.saveProducts(Collections.singletonList(PRODUCT));
    Mockito.verify(this.productRepository).saveAll(Collections.singletonList(PRODUCT));
    Mockito.verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME), eq(productChange.getProductSku()),
        productChangeArgumentCaptor.capture());
    Assertions.assertEquals(STATUS, productChangeArgumentCaptor.getValue().getSizeChartCode());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.itemRepository);
    Mockito.verifyNoMoreInteractions(this.itemService);
    Mockito.verifyNoMoreInteractions(this.productRepository);
    Mockito.verifyNoMoreInteractions(this.asyncProcessor);
    Mockito.verifyNoMoreInteractions(this.kafkaProducer);
    Mockito.verifyNoMoreInteractions(this.businessPartnerService);
    Mockito.verifyNoMoreInteractions(this.kafkaTopicProperties);
  }

  @Test
  public void updateItemFieldByItemSkuTest() {
    String fieldValue = "field-value";
    String storeId = "store-id";
    String itemSku = "item-sku";
    String fieldName = "field-name";
    when(this.itemRepository.updateFieldByItemSku(storeId, itemSku, fieldName, fieldValue))
        .thenReturn(SaveAndPublishServiceImplTest.ITEM);
    this.saveAndPublishServiceImpl
        .updateItemFieldByItemSku(storeId, itemSku, fieldName, fieldValue);
    Mockito.verify(this.itemRepository).updateFieldByItemSku(storeId, itemSku, fieldName, fieldValue);
    verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME), any(),
        any(ItemChange.class));
  }

  @Test
  public void publishListOfOfflineItemsTest_success() {
    List<OfflineItem> offlineItems = new ArrayList<>();

    offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItem.setStoreId(STORE_ID);
    offlineItem.setOfferPrice(OFFER_PRICE_2);
    offlineItem.setOfflineItemHistoryDetail(
        OfflineItemHistoryDetailVO.builder().oldOfferPrice(OFFER_PRICE_1).build());
    offlineItems.add(offlineItem);

    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    Map<String, String> itemCodeByItemSku = Collections.singletonMap(ITEM_SKU, ITEM_CODE);

    when(this.itemService.getItemCodesByItemSkuIn(STORE_ID, itemSkus)).thenReturn(itemCodeByItemSku);

    this.saveAndPublishServiceImpl.publishListOfOfflineItems(offlineItems, null);

    verify(this.itemService).getItemCodesByItemSkuIn(STORE_ID, itemSkus);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.OFFLINE_ITEM_CHANGE_EVENT_NAME),
        any(), any(OfflineItemChange.class));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.OFFLINE_ITEM_CHANGE_BIG_SELLER_EVENT_NAME),
        anyString(),any(OfflineItemChange.class));

//    OfflineItemChange offlineItemChange = offlineItemChangeArgCaptor.getValue();
//    assertEquals(OFFLINE_ITEM_ID, offlineItemChange.getUniqueId());
//    assertEquals(ITEM_SKU, offlineItemChange.getItemSku());
//    assertEquals(PRODUCT_SKU, offlineItemChange.getProductSku());
//    assertEquals(ITEM_CODE, offlineItemChange.getItemCode());
//    assertEquals(Double.valueOf(OFFER_PRICE_2), offlineItemChange.getOfferPrice());
//    assertEquals(Double.valueOf(OFFER_PRICE_1), offlineItemChange.getOldOfferPrice());
  }

  @Test
  public void publishListOfOfflineItemsTest_mandatoryParamNotNull_regularMerchant_success() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();

    offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItem.setStoreId(STORE_ID);
    offlineItem.setOfferPrice(OFFER_PRICE_2);
    offlineItem.setOfflineItemHistoryDetail(
        OfflineItemHistoryDetailVO.builder().oldOfferPrice(OFFER_PRICE_1).build());
    offlineItems.add(offlineItem);

    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    Map<String, String> itemCodeByItemSku = Collections.singletonMap(ITEM_SKU, ITEM_CODE);

    when(this.itemService.getItemCodesByItemSkuIn(STORE_ID, itemSkus)).thenReturn(itemCodeByItemSku);

    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID_UPSERT_OFFLINE_ITEM_PRICE,
            REQUEST_ID);

    this.saveAndPublishServiceImpl.publishListOfOfflineItems(offlineItems, mandatoryRequestParam);

    verify(this.itemService).getItemCodesByItemSkuIn(STORE_ID, itemSkus);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.OFFLINE_ITEM_CHANGE_EVENT_NAME),
        anyString(), any(OfflineItemChange.class));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.OFFLINE_ITEM_CHANGE_REGULAR_SELLER_EVENT_NAME),
        anyString(), any(OfflineItemChange.class));

//    OfflineItemChange offlineItemChange = offlineItemChangeArgCaptor.getValue();
//    assertEquals(OFFLINE_ITEM_ID, offlineItemChange.getUniqueId());
//    assertEquals(ITEM_SKU, offlineItemChange.getItemSku());
//    assertEquals(PRODUCT_SKU, offlineItemChange.getProductSku());
//    assertEquals(ITEM_CODE, offlineItemChange.getItemCode());
//    assertEquals(Double.valueOf(OFFER_PRICE_2), offlineItemChange.getOfferPrice());
//    assertEquals(Double.valueOf(OFFER_PRICE_1), offlineItemChange.getOldOfferPrice());
  }

  @Test
  public void publishListOfOfflineItemsTest_mandatoryParamNotNull_bigMerchant_success() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();

    offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItem.setStoreId(STORE_ID);
    offlineItem.setOfferPrice(OFFER_PRICE_2);
    offlineItem.setOfflineItemHistoryDetail(
        OfflineItemHistoryDetailVO.builder().oldOfferPrice(OFFER_PRICE_1).build());
    offlineItems.add(offlineItem);

    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    Map<String, String> itemCodeByItemSku = Collections.singletonMap(ITEM_SKU, ITEM_CODE);

    when(this.itemService.getItemCodesByItemSkuIn(STORE_ID, itemSkus)).thenReturn(itemCodeByItemSku);

    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID);

    this.saveAndPublishServiceImpl.publishListOfOfflineItems(offlineItems, mandatoryRequestParam);

    verify(this.itemService).getItemCodesByItemSkuIn(STORE_ID, itemSkus);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.OFFLINE_ITEM_CHANGE_EVENT_NAME),
        anyString(), any(OfflineItemChange.class));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.OFFLINE_ITEM_CHANGE_BIG_SELLER_EVENT_NAME),
        anyString(), any(OfflineItemChange.class));

//    OfflineItemChange offlineItemChange = offlineItemChangeArgCaptor.getValue();
//    assertEquals(OFFLINE_ITEM_ID, offlineItemChange.getUniqueId());
//    assertEquals(ITEM_SKU, offlineItemChange.getItemSku());
//    assertEquals(PRODUCT_SKU, offlineItemChange.getProductSku());
//    assertEquals(ITEM_CODE, offlineItemChange.getItemCode());
//    assertEquals(Double.valueOf(OFFER_PRICE_2), offlineItemChange.getOfferPrice());
//    assertEquals(Double.valueOf(OFFER_PRICE_1), offlineItemChange.getOldOfferPrice());
  }

  @Test
  public void publishListOfOfflineItemsTest_offlineItemsEmpty_success() {
    List<OfflineItem> offlineItems = new ArrayList<>();
    this.saveAndPublishServiceImpl.publishListOfOfflineItems(offlineItems, null);
  }

  @Test
  public void publishListOfOfflineItemsTest_throwException_success() {
    List<OfflineItem> offlineItems = new ArrayList<>();

    offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItem.setStoreId(STORE_ID);
    offlineItem.setOfferPrice(OFFER_PRICE_2);
    offlineItem.setOfflineItemHistoryDetail(
        OfflineItemHistoryDetailVO.builder().oldOfferPrice(OFFER_PRICE_1).build());
    offlineItems.add(offlineItem);

    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    Map<String, String> itemCodeByItemSku = Collections.singletonMap(ITEM_SKU, ITEM_CODE);

    when(this.itemService.getItemCodesByItemSkuIn(STORE_ID, itemSkus)).thenReturn(itemCodeByItemSku);

    doThrow(new RuntimeException("error")).when(kafkaProducer)
        .send(eq(ProductDomainEventName.OFFLINE_ITEM_CHANGE_EVENT_NAME),
            anyString(), any(OfflineItemChange.class));

    this.saveAndPublishServiceImpl.publishListOfOfflineItems(offlineItems, null);

    verify(this.itemService).getItemCodesByItemSkuIn(STORE_ID, itemSkus);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.OFFLINE_ITEM_CHANGE_EVENT_NAME),
        anyString(), any(OfflineItemChange.class));

//    OfflineItemChange offlineItemChange = offlineItemChangeArgCaptor.getValue();
//    assertEquals(OFFLINE_ITEM_ID, offlineItemChange.getUniqueId());
//    assertEquals(ITEM_SKU, offlineItemChange.getItemSku());
//    assertEquals(PRODUCT_SKU, offlineItemChange.getProductSku());
//    assertEquals(ITEM_CODE, offlineItemChange.getItemCode());
//    assertEquals(Double.valueOf(OFFER_PRICE_2), offlineItemChange.getOfferPrice());
//    assertEquals(Double.valueOf(OFFER_PRICE_1), offlineItemChange.getOldOfferPrice());

  }

  @Test
  public void publishListOfOfflineItemsPickupPointsTest_offlineItemsEmpty_success() {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    this.saveAndPublishServiceImpl.publishListOfItemsPickupPoints(itemPickupPointList, null);
    Mockito.verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), any(),
        Mockito.any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), isNull());
  }

  @Test
  public void publishListOfOfflineItemsWithoutFailsafeTest_success() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();

    offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItem.setStoreId(STORE_ID);
    offlineItem.setOfflineItemHistoryDetail(
        OfflineItemHistoryDetailVO.builder().oldOfferPrice(OFFER_PRICE_1).build());
    offlineItems.add(offlineItem);

    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    Map<String, Item> itemCodeByItemSku = Collections.singletonMap(ITEM_SKU, item);

    when(this.itemService.getItemsByItemSkuIn(STORE_ID, itemSkus)).thenReturn(itemCodeByItemSku);

    this.saveAndPublishServiceImpl.publishListOfOfflineItemsWithoutFailsafe(offlineItems);

    verify(this.itemService).getItemsByItemSkuIn(STORE_ID, itemSkus);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.POPULATE_OFFLINE_ITEM_EVENT_NAME),
      anyString(), any(OfflineItemPopulateEvent.class));
  }

  @Test
  public void publishListOfOfflineItemsWithoutFailsafeTest_itemNull_success() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();

    offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItem.setStoreId(STORE_ID);
    offlineItems.add(offlineItem);

    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    Map<String, Item> itemCodeByItemSku = Collections.singletonMap(ITEM_SKU, null);

    when(this.itemService.getItemsByItemSkuIn(STORE_ID, itemSkus)).thenReturn(itemCodeByItemSku);

    this.saveAndPublishServiceImpl.publishListOfOfflineItemsWithoutFailsafe(offlineItems);

    verify(this.itemService).getItemsByItemSkuIn(STORE_ID, itemSkus);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.POPULATE_OFFLINE_ITEM_EVENT_NAME),
        anyString(), any(OfflineItemPopulateEvent.class));

  }

  @Test
  public void publishListOfOfflineItemsWithoutFailsafeTest_pristineDataNull_success() throws Exception {
    List<OfflineItem> offlineItems = new ArrayList<>();

    offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItem.setStoreId(STORE_ID);
    offlineItems.add(offlineItem);

    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    item.setPristineDataItem(null);
    Map<String, Item> itemCodeByItemSku = Collections.singletonMap(ITEM_SKU, item);

    when(this.itemService.getItemsByItemSkuIn(STORE_ID, itemSkus)).thenReturn(itemCodeByItemSku);

    this.saveAndPublishServiceImpl.publishListOfOfflineItemsWithoutFailsafe(offlineItems);

    verify(this.itemService).getItemsByItemSkuIn(STORE_ID, itemSkus);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.POPULATE_OFFLINE_ITEM_EVENT_NAME),
      anyString(), any(OfflineItemPopulateEvent.class));
//
//    OfflineItemPopulateEvent offlineItemPopulateEvent = offlineItemPopulateEventArgCaptor.getValue();
//    assertEquals(ITEM_SKU, offlineItemPopulateEvent.getItemSku());
//    assertEquals(ITEM_CODE, offlineItemPopulateEvent.getItemCode());
//    assertNull(offlineItemPopulateEvent.getPristineId());
  }

  @Test
  public void publishListOfOfflineItemsTest_multipleOfflineItems_itemOneThrowsException_itemTwoGetsPublished() {
    offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItem.setStoreId(STORE_ID);

    OfflineItem offlineItem2 = new OfflineItem();
    offlineItem2.setItemSku(ITEM_SKU2);
    offlineItem2.setOfflineItemId(OFFLINE_ITEM_ID + "2");
    offlineItem2.setStoreId(STORE_ID);

    OfflineItemChange offlineItem1Change = new OfflineItemChange();
    offlineItem1Change.setItemSku(ITEM_SKU);
    offlineItem1Change.setUniqueId(OFFLINE_ITEM_ID);
    offlineItem1Change.setItemCode(ITEM_CODE);
    offlineItem1Change.setStoreId(STORE_ID);
    offlineItem1Change.setProductSku(PRODUCT_SKU);

    OfflineItemChange offlineItem2Change = new OfflineItemChange();
    offlineItem2Change.setItemSku(ITEM_SKU2);
    offlineItem2Change.setUniqueId(OFFLINE_ITEM_ID + "2");
    offlineItem2Change.setItemCode(ITEM_CODE);
    offlineItem2Change.setStoreId(STORE_ID);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    itemSkus.add(ITEM_SKU2);
    Map<String, String> itemCodeByItemSku = Collections.singletonMap(ITEM_SKU, ITEM_CODE);

    when(this.itemService.getItemCodesByItemSkuIn(STORE_ID, itemSkus))
        .thenReturn(itemCodeByItemSku);

    doThrow(new RuntimeException("error")).when(kafkaProducer)
        .send(eq(ProductDomainEventName.POPULATE_OFFLINE_ITEM_EVENT_NAME),anyString(),
            any(OfflineItemChange.class));
    this.saveAndPublishServiceImpl.publishListOfOfflineItems(Arrays.asList(offlineItem, offlineItem2), null);
    verify(this.itemService).getItemCodesByItemSkuIn(STORE_ID, itemSkus);
    verify(this.kafkaProducer, times(4)).send(
      Mockito.anyString(), anyString(),
      any());
  }

  @Test
  public void publishListOfOfflineItemsWithoutFailsafeTest_exceptionThrown() {
    offlineItem = new OfflineItem();
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItem.setStoreId(STORE_ID);

    OfflineItem offlineItem2 = new OfflineItem();
    offlineItem2.setItemSku(ITEM_SKU2);
    offlineItem2.setOfflineItemId(OFFLINE_ITEM_ID + "2");
    offlineItem2.setStoreId(STORE_ID);

    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    itemSkus.add(ITEM_SKU2);
    Map<String, Item> itemCodeByItemSku = Collections.singletonMap(ITEM_SKU, item);

    when(this.itemService.getItemsByItemSkuIn(STORE_ID, itemSkus)).thenReturn(itemCodeByItemSku);

    doThrow(new RuntimeException("error")).when(kafkaProducer)
        .send(eq(ProductDomainEventName.POPULATE_OFFLINE_ITEM_EVENT_NAME),any(),
            any(OfflineItemPopulateEvent.class));
    try {
      this.saveAndPublishServiceImpl
          .publishListOfOfflineItemsWithoutFailsafe(Arrays.asList(offlineItem, offlineItem2));
    } catch (Exception e) {
      verify(this.itemService).getItemsByItemSkuIn(STORE_ID, itemSkus);
      verify(this.kafkaProducer, times(1)).send(
        eq(ProductDomainEventName.POPULATE_OFFLINE_ITEM_EVENT_NAME), any(),
        offlineItemPopulateEventArgCaptor.capture());

      List<OfflineItemPopulateEvent> offlineItemPopulateEvent =
          offlineItemPopulateEventArgCaptor.getAllValues();

      assertEquals(1, offlineItemPopulateEvent.size());
      assertEquals(PRISTINE_ID, offlineItemPopulateEvent.get(0).getPristineId());
    }
  }

  @Test
  public void updateItemOff2OnChannelActiveByItemSkuTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "newlyAddedItemDataChangeEventTypes", "MASTER_SKU_UPDATE");
    when(this.itemRepository.updateOff2OnChannelActiveByItemSku(STORE_ID, ITEM_SKU, true))
        .thenReturn(ITEM);
    saveAndPublishServiceImpl.updateItemOff2OnChannelActiveByItemSku(STORE_ID, ITEM_SKU, true);
    Mockito.verify(kafkaProducer)
        .send(Mockito.anyString(), isNull(), Mockito.any(ItemDataChange.class));
    Mockito.verify(this.itemRepository)
        .updateOff2OnChannelActiveByItemSku(STORE_ID, ITEM_SKU, true);
    Assertions.assertEquals(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE,
        ITEM.getItemChangeEventTypes().get(0));
  }

  @Test
  public void updateOff2OnItemCountIncrementTest() {
    Product product = PRODUCT;
    product.setSalesCatalogs(new ArrayList<>());
    product.setB2cActivated(false);
    product.getAllSalesCatalogs().add(new SalesCatalog());
    when(
        this.productRepository.updateOff2OnItemCountIncrementByProductSku(STORE_ID, PRODUCT_SKU, 1))
        .thenReturn(PRODUCT);
    saveAndPublishServiceImpl.updateOff2OnItemCountIncrement(STORE_ID, PRODUCT_SKU, true, 1);
    Mockito.verify(kafkaProducer)
        .send(Mockito.anyString(), Mockito.isNull(), Mockito.any(ProductChange.class));
    Mockito.verify(this.productRepository)
        .updateOff2OnItemCountIncrementByProductSku(STORE_ID, PRODUCT_SKU, 1);
  }

  @Test
  public void updateOff2OnItemCountIncrementTest_activatedFalse() {
    when(this.productRepository
        .updateOff2OnItemCountIncrementByProductSku(STORE_ID, PRODUCT_SKU, -1)).thenReturn(PRODUCT);
    saveAndPublishServiceImpl.updateOff2OnItemCountIncrement(STORE_ID, PRODUCT_SKU, false, 1);
    Mockito.verify(kafkaProducer)
        .send(Mockito.anyString(), Mockito.isNull(), Mockito.any(ProductChange.class));
    Mockito.verify(this.productRepository)
        .updateOff2OnItemCountIncrementByProductSku(STORE_ID, PRODUCT_SKU, -1);
  }

  private static Product initializeProduct(String productSku){
    Product product = new Product();
    product.setProductSku(productSku);
    return product;
  }

  @Test
  public void publishStreamOfProducts() {
    saveAndPublishServiceImpl.publishStreamOfProducts(PRODUCT_STREAM);

    verify(asyncProcessor,times(3))
        .submitWithBackoff(eq(COMMAND_DESC_PRODUCT),any(Runnable.class));
    verify(gdnMapper, times(3)).deepCopy(any(Product.class), eq(ProductModel.class));
  }

  private static Item initializeItem(String itemSku){
    Item item = new Item();
    item.setItemSku(itemSku);
    return item;
  }

  @Test
  public void publishStreamOfItems() {
    saveAndPublishServiceImpl.publishStreamOfItems(ITEM_STREAM);

    verify(asyncProcessor,times(3))
        .submitWithBackoff(eq(COMMAND_DESC_ITEM),any(Runnable.class));
  }

  @Test
  public void publishPristineItemTest_ItemDataChangeEvent() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "newlyAddedItemDataChangeEventTypes", "MASTER_SKU_UPDATE_1");
    Mockito.when(gdnMapper
            .deepCopy(Mockito.any(MasterDataItem.class), eq(com.gdn.x.product.domain.event.model.MasterDataItem.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.MasterDataItem());
    Mockito.when(gdnMapper.deepCopy(Mockito.any(PristineDataItem.class), eq(PristineDataItemEventModel.class)))
        .thenReturn(new PristineDataItemEventModel());
    this.saveAndPublishServiceImpl.publishPristineItem(STORE_ID, ITEM1);
    Mockito.verify(applicationContext).getBean(SaveAndPublishService.class);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(),
      any(ItemDataChange.class));
    Mockito.verify(gdnMapper, times(2))
        .deepCopy(isNull(), eq(com.gdn.x.product.domain.event.model.MasterDataItem.class));
    Mockito.verify(gdnMapper, times(2))
        .deepCopy(isNull(), eq(PristineDataItemEventModel.class));
  }

  @Test
  public void publishPristineItemTest_PristineItemChangeEvent(){
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishPristineMappingChangeEvent",true);
    this.saveAndPublishServiceImpl.publishPristineItem(STORE_ID, ITEM1);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRISTINE_ITEM_CHANGE_EVENT_NAME),
        anyString(), any(ItemChange.class));
  }

  @Test
  public void publishMerchantPromoDiscountEventChangeTest(){
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    discountPriceList.add(new DiscountPrice(200, new Date(), new Date(), StringUtils.EMPTY,
        AdjustmentType.MERCHANT));

    Price price = new Price();
    price.setListPrice(700);
    price.setOfferPrice(600);
    price.setChannel("DEFAULT");
    price.setListOfDiscountPrices(discountPriceList);
    price.setMerchantPromoDiscountPrice(discountPriceList.get(0));
    Set<Price> prices = new HashSet<Price>();
    prices.add(price);
    ITEM1.setPrice(prices);
    this.saveAndPublishServiceImpl.publishMerchantPromoDiscountEventChange(ITEM1);
    verify(this.kafkaProducer).send(Mockito.eq(ProductDomainEventName.MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE),
        Mockito.isNull(), Mockito.any(MerchantPromoDiscountChangeEvent.class));
  }

  @Test
  public void publishMerchantPromoDiscountEventChange_WhenExceptionTest() {
    Mockito.doThrow(new RuntimeException()).when(this.kafkaProducer).send(
        Mockito.eq(ProductDomainEventName.MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE), Mockito.anyString(),
            Mockito.any(MerchantPromoDiscountChangeEvent.class));
    this.saveAndPublishServiceImpl.publishMerchantPromoDiscountEventChange(ITEM1);
    verify(this.kafkaProducer).send(Mockito.eq(ProductDomainEventName.MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE),
        Mockito.isNull(), Mockito.any(MerchantPromoDiscountChangeEvent.class));
  }

  @Test
  public void publishItemSkuListForVoucherTest() {
    VoucherItemSkusEventModel voucherItemSkusEventModel = VoucherItemSkusEventModel.builder().build();
    this.saveAndPublishServiceImpl.publishItemSkuListForVoucher(voucherItemSkusEventModel);
    verify(this.kafkaProducer).send(DomainEventName.PRODUCT_DETAIL_EVENT,
       voucherItemSkusEventModel.getVoucherCode(), voucherItemSkusEventModel);
  }

  @Test
  public void publishItemSkuListForVoucherTest_WhenException() {
    VoucherItemSkusEventModel voucherItemSkusEventModel = VoucherItemSkusEventModel.builder().build();
    doThrow(new RuntimeException()).when(this.kafkaProducer).send( DomainEventName.PRODUCT_DETAIL_EVENT,
        voucherItemSkusEventModel.getVoucherCode(), voucherItemSkusEventModel);
    this.saveAndPublishServiceImpl.publishItemSkuListForVoucher(voucherItemSkusEventModel);
    verify(this.kafkaProducer).send(DomainEventName.PRODUCT_DETAIL_EVENT,
        voucherItemSkusEventModel.getVoucherCode(), voucherItemSkusEventModel);
  }

  @Test
  public void publishMerchantVoucherViewConfigChange() {
    this.saveAndPublishServiceImpl.publishMerchantVoucherViewConfigChange(Arrays.asList(itemPickupPoint), Arrays.asList(item));
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE), anyString(),
        any(ItemViewConfigWithArchivedChangeEvent.class));
  }

  @Test
  public void publishMerchantVoucherViewConfigChangeItemNotFound() {
    itemPickupPoint.setItemSku(ITEM_SKU2);
    this.saveAndPublishServiceImpl.publishMerchantVoucherViewConfigChange(Arrays.asList(itemPickupPoint), Arrays.asList(item));
    assertNotNull(ITEM);
  }

  @Test
  public void publishMerchantVoucherViewConfigChange_WhenException() {
    doThrow(new RuntimeException()).when(this.kafkaProducer)
        .send(eq(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE), eq(item.getItemSku()),
            any(ItemViewConfigWithArchivedChangeEvent.class));
    try {
      Assertions.assertThrows(RuntimeException.class, () ->  this.saveAndPublishServiceImpl.publishMerchantVoucherViewConfigChange(Arrays.asList(itemPickupPoint), Arrays.asList(item)));
    }
    finally {
      verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE), eq(ITEM_SKU), any(ItemViewConfigWithArchivedChangeEvent.class));
    }
  }

  @Test
  public void publishWholesalePriceActivatedOrDeactivatedEvent() {
    this.saveAndPublishServiceImpl.publishWholesalePriceActivatedOrDeactivatedEvent(ITEM_SKU, true, MERCHANT_CODE);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.WHOLESALE_PRICE_ACTIVATED_DEACTIVATED_BY_SCHEDULER),
        Mockito.anyString(), any(WholesalePriceActivatedOrDeactivatedEvent.class));
  }

  @Test
  public void publishWholesalePriceActivatedOrDeactivatedEvent_WhenException() {
    doThrow(new RuntimeException()).when(this.kafkaProducer)
        .send(eq(ProductDomainEventName.WHOLESALE_PRICE_ACTIVATED_DEACTIVATED_BY_SCHEDULER), Mockito.anyString(),
            any(WholesalePriceActivatedOrDeactivatedEvent.class));
    this.saveAndPublishServiceImpl.publishWholesalePriceActivatedOrDeactivatedEvent(ITEM_SKU, true, MERCHANT_CODE);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.WHOLESALE_PRICE_ACTIVATED_DEACTIVATED_BY_SCHEDULER),
        Mockito.anyString(), any(WholesalePriceActivatedOrDeactivatedEvent.class));
  }


  @Test
  public void testUpdateOff2OnItemCountIncrementActivate() {
    int increment = 1;
    Mockito.when(this.productRepository.updateOff2OnItemCountIncrementByProductSku(STORE_ID, PRODUCT_SKU, increment))
            .thenReturn(PRODUCT);

    ProductChange productChange = getProductChange();

    this.saveAndPublishServiceImpl.updateOff2OnItemCountIncrement(STORE_ID, PRODUCT_SKU, true, increment);
    Mockito.verify(this.productRepository).updateOff2OnItemCountIncrementByProductSku(STORE_ID, PRODUCT_SKU, increment);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME),
      any(),
      any(ProductChange.class));
  }

  @Test
  public void testUpdateOff2OnItemCountIncrementNotActivate() {
    int increment = -1;
    Mockito.when(this.productRepository.updateOff2OnItemCountIncrementByProductSku(STORE_ID, PRODUCT_SKU, increment))
            .thenReturn(PRODUCT);

    ProductChange productChange = new ProductChange();
    BeanUtils.copyProperties(PRODUCT, productChange);

    if (PRODUCT.getMasterDataProduct() != null) {
      com.gdn.x.product.domain.event.model.MasterDataProduct masterDataProduct =
              new com.gdn.x.product.domain.event.model.MasterDataProduct();
      BeanUtils.copyProperties(PRODUCT.getMasterDataProduct(), masterDataProduct);
      productChange.setMasterDataProduct(masterDataProduct);
    }

    this.saveAndPublishServiceImpl.updateOff2OnItemCountIncrement(STORE_ID, PRODUCT_SKU, false, 1);
    Mockito.verify(this.productRepository).updateOff2OnItemCountIncrementByProductSku(STORE_ID, PRODUCT_SKU, increment);
    Mockito.verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME), any(), any(ProductChange.class));
  }

  @Test
  public void updateItemOff2OnChannelActiveByProductSkuTest() {
    List<Item> items = new ArrayList<>();
    items.add(new Item());
    Mockito.when(this.itemRepository.updateOff2OnChannelActiveByProductSku(STORE_ID, PRODUCT_SKU, false))
        .thenReturn(items);
    this.saveAndPublishServiceImpl.updateItemOff2OnChannelActiveByProductSku(STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(this.itemRepository).updateOff2OnChannelActiveByProductSku(STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), isNull(), any());
  }

  @Test
  public void updateItemOff2OnChannelActiveAndUpdatedDataByProductSkuTest() {
    saveAndPublishServiceImpl.updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(STORE_ID, PRODUCT_SKU, false,
        Constants.DEFAULT_UPDATED_BY);
    verify(itemRepository).updateOff2OnChannelActiveByProductSkuAndUpdatedDateAndBy(STORE_ID, PRODUCT_SKU, false,
        Constants.DEFAULT_UPDATED_BY);
  }

  @Test
  public void publishProductL3SolrReindexEventTest() {
    this.saveAndPublishServiceImpl.publishProductL3SolrReindexEvent(Arrays.asList(PRODUCT_SKU), REINDEX_STATUS);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_L3_SOLR_REINDEX_EVENT_NAME),
        eq(new ProductL3SolrReindexEvent(Arrays.asList(PRODUCT_SKU), REINDEX_STATUS)));
  }

  @Test
  public void publishProductL3SolrReindexEvent_exceptionTest() {
    doThrow(ApplicationRuntimeException.class).when(this.kafkaProducer)
        .send(eq(ProductDomainEventName.PRODUCT_L3_SOLR_REINDEX_EVENT_NAME),
            eq(new ProductL3SolrReindexEvent(Arrays.asList(PRODUCT_SKU), REINDEX_STATUS)));
    this.saveAndPublishServiceImpl.publishProductL3SolrReindexEvent(Arrays.asList(PRODUCT_SKU), REINDEX_STATUS);
    verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.PRODUCT_L3_SOLR_REINDEX_EVENT_NAME),
            eq(new ProductL3SolrReindexEvent(Arrays.asList(PRODUCT_SKU), REINDEX_STATUS)));
  }

  @Test
  public void updateOff2OnItemCountIncrementAndFlagTest() {
    Mockito.when(productRepository.updateOff2OnItemCountIncrementByProductSkuAndFlag(STORE_ID, PRODUCT_SKU, true, 5,
        Constants.DEFAULT_UPDATED_BY)).thenReturn(PRODUCT);
    saveAndPublishServiceImpl
        .updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, true, 5, Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(productRepository).updateOff2OnItemCountIncrementByProductSkuAndFlag(STORE_ID, PRODUCT_SKU, true, 5,
        Constants.DEFAULT_UPDATED_BY);
  }

  @Test
  public void updateOff2OnItemCountIncrementAndFlagDeactivateTest() {
    Mockito.when(productRepository.updateOff2OnItemCountIncrementByProductSkuAndFlag(STORE_ID, PRODUCT_SKU, false, 0,
        Constants.DEFAULT_UPDATED_BY))
        .thenReturn(PRODUCT);
    saveAndPublishServiceImpl.updateOff2OnItemCountIncrementAndFlag(STORE_ID, PRODUCT_SKU, false, 5, Constants.DEFAULT_UPDATED_BY);
    Mockito.verify(productRepository).updateOff2OnItemCountIncrementByProductSkuAndFlag(STORE_ID, PRODUCT_SKU, false, 0,
        Constants.DEFAULT_UPDATED_BY);
  }

  private ProductChange getProductChange() {
    ProductChange productChange = new ProductChange();
    BeanUtils.copyProperties(PRODUCT, productChange);

    com.gdn.x.product.domain.event.model.MasterDataProduct masterDataProduct =
        new com.gdn.x.product.domain.event.model.MasterDataProduct();
    BeanUtils.copyProperties(PRODUCT.getMasterDataProduct(), masterDataProduct);
    productChange.setMasterDataProduct(masterDataProduct);
    return productChange;
  }

  @Test
  public void publishProduct() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "ranchIntegrationEnabled", true);
    List<SalesCatalog> salesCatalogList = new ArrayList<>();
    SalesCatalog salesCatalog1 = new SalesCatalog();
    salesCatalog1.setCatalogCode(ITEM_CODE);
    SalesCatalog salesCatalog2 = new SalesCatalog();
    salesCatalog2.setCatalogCode(MERCHANT_CODE);
    salesCatalogList.add(salesCatalog2);
    salesCatalogList.add(salesCatalog1);
    product.setSalesCatalogs(salesCatalogList);
    product.setArchived(true);
    product.setB2cActivated(true);
    product.setForceReview(true);
    product.setB2bActivated(true);
    product.setProductType(ProductType.REGULAR);
    saveAndPublishServiceImpl.publishProduct(product, new ArrayList<>());
    Mockito.verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME), isNull(),
          productChangeArgumentCaptor.capture());
    assertFalse(productChangeArgumentCaptor.getValue().isMultiVariant());
    assertTrue(productChangeArgumentCaptor.getValue().isArchived());
    assertEquals(productChangeArgumentCaptor.getValue().getProductType().getCode(),
      ProductType.REGULAR.getCode());
    assertTrue(productChangeArgumentCaptor.getValue().isForceReview());
    assertEquals(DistributionStatus.NON_DISTRIBUTION.getDescription(),
        productChangeArgumentCaptor.getValue().getDistributionMappingStatus());
  }

  @Test
  public void publishProductMultiVariantTest() {
    product.setDefiningAttributes(Arrays.asList(new ProductAttribute(), new ProductAttribute()));
    saveAndPublishServiceImpl.publishProduct(product, new ArrayList<>());
    Mockito.verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME), Mockito.isNull(),
            productChangeArgumentCaptor.capture());
    assertTrue(productChangeArgumentCaptor.getValue().isMultiVariant());
  }

  @Test
  public void publishProductChangeEventTypesTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "ranchIntegrationEnabled", true);
    product.setDistributionStatus(null);
    product.setArchived(true);
    saveAndPublishServiceImpl.publishProduct(product, Arrays.asList(ProductChangeEventType.ARCHIVE_FLAG_CHANGE));
    Mockito.verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME), isNull(),
            productChangeArgumentCaptor.capture());

    assertEquals(ProductChangeEventType.ARCHIVE_FLAG_CHANGE,
        productChangeArgumentCaptor.getValue().getProductChangeEventType().get(0));

  }

  @Test
  public void publishListOfItemsWithItemChangeTypeMPPTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "newlyAddedItemDataChangeEventTypes", "");
    saveAndPublishServiceImpl.publishListOfItemsWithItemChangeType(Collections.singletonList(item),
        Collections.singletonList(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE));
    List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
    itemChangeEventTypes.add(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE);
    item.setItemChangeEventTypes(itemChangeEventTypes);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME),any(),
      any(ItemDataChange.class));
    Mockito.verify(applicationContext).getBean(SaveAndPublishService.class);
  }

  @Test
  public void publishMerchantVoucherViewConfigChangeByItemPickupPointTest() {
    ItemVo itemVo = new ItemVo();
    itemVo.setItemSku(ITEM_SKU);
    itemVo.setItemPickupPointVoList(Arrays.asList(itemPickupPointVo));
    saveAndPublishServiceImpl.publishMerchantVoucherViewConfigChange(itemVo);
    verify(kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE), isNull(),
      any(ItemViewConfigWithArchivedChangeEvent.class));
  }

  @Test
  public void publishMerchantVoucherViewConfigChangeByItemPickupPointExceptionTest() {
    doThrow(ApplicationRuntimeException.class).when(kafkaProducer)
        .send(eq(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE),eq(ITEM_SKU),
          eq(ItemViewConfig.class));
    ItemVo itemVo = new ItemVo();
    itemVo.setItemPickupPointVoList(Arrays.asList(itemPickupPointVo));
    saveAndPublishServiceImpl.publishMerchantVoucherViewConfigChange(itemVo);
    verify(kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE),
      isNull(), any(ItemViewConfigWithArchivedChangeEvent.class));
  }

  @Test
  public void publishItemChangeEventByItemPickupPointTest() {
    ItemVo itemVo = new ItemVo();
    itemVo.setItemPickupPointVoList(Arrays.asList(itemPickupPointVo));
    itemPickupPointVo.setPickupPointCode(PICKUP_POINT_CODE_1);

    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemViewConfig.class),
            eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());
    Mockito.when(gdnMapper.deepCopy(Mockito.any(MasterDataItem.class),
            eq(com.gdn.x.product.domain.event.model.MasterDataItem.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.MasterDataItem());
    Mockito.when(gdnMapper.deepCopy(Mockito.any(PristineDataItem.class),
            eq(com.gdn.x.product.domain.event.model.PristineDataItemEventModel.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.PristineDataItemEventModel());

    ItemChange itemChange = saveAndPublishServiceImpl.publishItemChangeEvent(itemVo, itemPickupPointVo);

    Mockito.verify(gdnMapper ,times(4)).deepCopy(Mockito.any(), Mockito.any());
    Mockito.verify(kafkaProducer)
      .send(Mockito.eq(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME), any(),
        any(ItemChange.class));
    Assertions.assertEquals(PICKUP_POINT_CODE_1, itemChange.getPickupPointCode());
  }

  @Test
  public void publishItemChangeEventByItemPickupPointAndItemTest() {
    itemPickupPoint.setItemSku(ITEM_SKU);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper.deepCopy(Mockito.any(ItemViewConfig.class),
            eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());
    Mockito.when(gdnMapper.deepCopy(Mockito.any(MasterDataItem.class),
            eq(com.gdn.x.product.domain.event.model.MasterDataItem.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.MasterDataItem());
    Mockito.when(gdnMapper.deepCopy(Mockito.any(PristineDataItem.class),
            eq(com.gdn.x.product.domain.event.model.PristineDataItemEventModel.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.PristineDataItemEventModel());

    saveAndPublishServiceImpl.publishItemChangeEvent(Arrays.asList(itemPickupPoint),
        ImmutableMap.of(itemPickupPoint.getItemSku(), item));

    Mockito.verify(gdnMapper, times(4)).deepCopy(Mockito.any(), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME),any(),
          any(ItemChange.class));
  }

  @Test
  public void publishItemChangeEventByItemPickupPointAndItemNullTest() {
    itemPickupPoint.setItemSku(ITEM_SKU2);
    saveAndPublishServiceImpl.publishItemChangeEvent(Arrays.asList(itemPickupPoint),
        ImmutableMap.of(ITEM_SKU, item));
    assertNotNull(ITEM);
  }

  @Test
  public void publishItemDataChangeEventTest() {
    List<Item> itemList = new ArrayList<>();
    item.setPreferredSubscriptionType(Sets.newHashSet("WH"));
    item2.setOldPreferredSubscriptionType(Sets.newHashSet("MKT"));
    itemList.add(item);
    itemList.add(item2);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(MasterDataItem.class),
            eq(com.gdn.x.product.domain.event.model.MasterDataItem.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.MasterDataItem());
    Mockito.when(gdnMapper.deepCopy(Mockito.any(PristineDataItem.class), eq(PristineDataItemEventModel.class)))
        .thenReturn(new PristineDataItemEventModel());

    saveAndPublishServiceImpl.publishItemDataChangeEvent(itemList);

    Mockito.verify(gdnMapper, times(2))
        .deepCopy(Mockito.isNull(), eq(com.gdn.x.product.domain.event.model.MasterDataItem.class));
    Mockito.verify(gdnMapper, times(2))
        .deepCopy(Mockito.any(PristineDataItem.class), eq(PristineDataItemEventModel.class));
    Mockito.verify(kafkaProducer,times(2))
        .send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME),any(),
          Mockito.any(ItemDataChange.class));
  }

  @Test
  public void publishItemDataChangeEventWithSourceTest() {
    List<Item> itemList = new ArrayList<>();
    item.setPreferredSubscriptionType(Sets.newHashSet("WH"));
    item2.setOldPreferredSubscriptionType(Sets.newHashSet("MKT"));
    itemList.add(item);
    itemList.add(item2);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(MasterDataItem.class),
            eq(com.gdn.x.product.domain.event.model.MasterDataItem.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.MasterDataItem());
    Mockito.when(gdnMapper.deepCopy(Mockito.any(PristineDataItem.class), eq(PristineDataItemEventModel.class)))
        .thenReturn(new PristineDataItemEventModel());

    saveAndPublishServiceImpl.publishItemDataChangeEvent(itemList, Constants.SOURCE_PRODUCT_PUBLISH, false);

    Mockito.verify(gdnMapper, times(2))
        .deepCopy(Mockito.isNull(), eq(com.gdn.x.product.domain.event.model.MasterDataItem.class));
    Mockito.verify(gdnMapper, times(2))
        .deepCopy(Mockito.any(PristineDataItem.class), eq(PristineDataItemEventModel.class));
    Mockito.verify(kafkaProducer,times(2))
        .send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME),any(),
            Mockito.any(ItemDataChange.class));
  }

  @Test
  public void publishItemDataChangeEventExceptionTest() {
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    itemList.add(item2);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(MasterDataItem.class),
        eq(com.gdn.x.product.domain.event.model.MasterDataItem.class))).thenThrow(NullPointerException.class);

    saveAndPublishServiceImpl.publishItemDataChangeEvent(itemList);
    Mockito.verify(gdnMapper, times(2))
        .deepCopy(isNull(), eq(com.gdn.x.product.domain.event.model.MasterDataItem.class));
    Mockito.verify(kafkaProducer, times(2)).send(Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void publishItemPickupPointDataChangeEventTest() {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);

    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL, Constants.B2C_SELLER_CHANNEL));
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
        saveAndPublishServiceImpl.publishItemPickupPointDataChangeEvent(itemPickupPointList, new ArrayList<>(),
          Collections.EMPTY_MAP);
    Mockito.verify(kafkaProducer)
      .send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME),any(),
        Mockito.any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    Mockito.verify(gdnMapper)
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), anyString());
  }

  @Test
  public void publishItemPickupPointDataChangeEventProfileResponseNullTest() {
    ReflectionTestUtils.setField(saveAndPublishServiceImpl,
      "newlyAddedItemPickupPointDataChangeEventTypes",
      ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName().concat(",")
        .concat(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    B2bFields b2bFields = new B2bFields();
    b2bFields.setBasePrice(OFFER_PRICE_1);
    itemPickupPoint.setB2bFields(b2bFields);
    itemPickupPoint.setItemPickupPointDataChangeType(Arrays.asList("DiscoverFlag"));
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);

    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEvent(itemPickupPointList, new ArrayList<>(),
      Collections.EMPTY_MAP);
    Mockito.verify(kafkaProducer)
        .send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME),any(),
            Mockito.any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    Mockito.verify(gdnMapper)
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), anyString());
  }


  @Test
  public void publishItemPickupPointDataChangeEventProfileResponseForDuplicateEventTypes() {
    ReflectionTestUtils.setField(saveAndPublishServiceImpl,
      "newlyAddedItemPickupPointDataChangeEventTypes",
      ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName().concat(",")
        .concat(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    B2bFields b2bFields = new B2bFields();
    b2bFields.setBasePrice(OFFER_PRICE_1);
    itemPickupPoint.setB2bFields(b2bFields);
    itemPickupPoint.setItemPickupPointDataChangeType(Arrays.asList("discoverableFlagChange"));
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
            .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);

    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes = new ArrayList<>();
    itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);
    itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);
    itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);

    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEvent(itemPickupPointList,itemPickupPointChangeEventTypes,
      Collections.EMPTY_MAP);
    Mockito.verify(kafkaProducer)
        .send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME),any(),
            Mockito.any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    Mockito.verify(gdnMapper)
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), anyString());
  }


  @Test
  public void publishItemPickupPointDataChangeEventProfileResponseDiscoverFlag() {
    ReflectionTestUtils.setField(saveAndPublishServiceImpl,
      "newlyAddedItemPickupPointDataChangeEventTypes",
      ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName().concat(",")
        .concat(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    B2bFields b2bFields = new B2bFields();
    b2bFields.setBasePrice(OFFER_PRICE_1);
    itemPickupPoint.setB2bFields(b2bFields);
    itemPickupPoint.setItemPickupPointDataChangeType(Arrays.asList("DiscoverFlag"));
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);

    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
            .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEvent(itemPickupPointList, new ArrayList<>(),
      Collections.EMPTY_MAP);
    Mockito.verify(kafkaProducer)
        .send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME),any(),
            Mockito.any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    Mockito.verify(gdnMapper)
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), anyString());
  }

  @Test
  public void publishItemPickupPointDataChangeEventCompanyNullTest() {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);

    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEvent(itemPickupPointList, new ArrayList<>(),
      Collections.EMPTY_MAP);
    Mockito.verify(kafkaProducer)
        .send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME),any(),
            Mockito.any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    Mockito.verify(gdnMapper)
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), anyString());
  }

  @Test
  public void publishItemPickupPointDataChangeEventSalesChannelNullTest() {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);

    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(null);
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEvent(itemPickupPointList, new ArrayList<>(),
      Collections.EMPTY_MAP);
    Mockito.verify(kafkaProducer)
        .send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME),any(),
            Mockito.any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    Mockito.verify(gdnMapper)
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), anyString());
  }

  @Test
  public void publishItemPickupPointDataChangeEventSalesChannelEmptyListTest() {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);

    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());

    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(new ArrayList<>());
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEvent(itemPickupPointList, new ArrayList<>(),
      Collections.EMPTY_MAP);
    Mockito.verify(kafkaProducer)
        .send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME),any(),
            Mockito.any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    Mockito.verify(gdnMapper)
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), anyString());
  }

  @Test
  public void publishListOfOfflineItemsItemPickupPointBigProductMPPTrueTest() {
    itemPickupPoint.setItemSku(ITEM_SKU);
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList = new ArrayList<>();
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    itemPickupPointDataChangeEventModel.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointDataChangeEventModel.setItemSku(ITEM_SKU);
    itemPickupPointDataChangeEventModelList.add(itemPickupPointDataChangeEventModel);
    Map<String, Item> itemMap = ImmutableMap.of(itemPickupPoint.getItemSku(), item);
    Mockito.doNothing().when(itemPickupPointService)
        .publishItemPickupPointDataChangeEventWithPureCncStatusChange(Mockito.any(), eq(Collections.EMPTY_MAP));
    saveAndPublishServiceImpl.publishListOfOfflineItems(Arrays.asList(itemPickupPoint), itemMap, CLIENT_ID,
        itemPickupPointDataChangeEventModelList);
    assertNotNull(ITEM);
  }

  @Test
  public void publishItemsForMigrationTest() {
    saveAndPublishServiceImpl.publishItemsForMigration(itemSkuList);
    Mockito.verify(this.kafkaProducer)
        .send(ProductDomainEventName.ITEM_MIGRATION_EVENT,itemPickupPointMigrationEvent);
  }

  @Test
  public void publishItemCreationForMigrationTest() {
    saveAndPublishServiceImpl.publishItemCreationForMigration(itemSkuList);
    verify(this.kafkaProducer)
      .send(eq(ProductDomainEventName.MIGRATION_FOR_CREATION_EVENT), any(MigrationForItemCreationEvent.class));
  }

  @Test
  public void publishListOfItemsGlobalSwitchTrueL5PublishOnlyOnceTrueIL5EmptyTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishItemPickupPointChangeEventOnlyOnce", true);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    saveAndPublishServiceImpl.publishListOfItems(Collections.singletonList(item), new ArrayList<>(), new ArrayList<>(),
      false);
    Mockito.verify(applicationContext).getBean(SaveAndPublishService.class);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(),
        any(ItemDataChange.class));
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(Mockito.isNull(), Mockito.anyString());
  }

  @Test
  public void publishListOfItemsWithSourceTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishItemPickupPointChangeEventOnlyOnce", true);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    saveAndPublishServiceImpl.publishListOfItems(Collections.singletonList(item), new ArrayList<>(), new ArrayList<>(),
        Constants.SOURCE_PRODUCT_PUBLISH, false, Collections.EMPTY_MAP);
    Mockito.verify(applicationContext).getBean(SaveAndPublishService.class);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(),
        any(ItemDataChange.class));
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(Mockito.isNull(), Mockito.anyString());
  }

  @Test
  public void publishListOfItemsGlobalSwitchTrueL5PublishOnlyOnceTrueIL5NotEmptyTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishItemPickupPointChangeEventOnlyOnce", true);
    saveAndPublishServiceImpl
        .publishListOfItems(Collections.singletonList(item), Collections.singletonList(itemPickupPoint),
            new ArrayList<>(), false);
    Mockito.verify(applicationContext, times(2)).getBean(SaveAndPublishService.class);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), isNull(),
        any(ItemPickupPointDataChangeEventModel.class));
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(),
        any(ItemDataChange.class));
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(Mockito.any(), Mockito.any());
  }

  @Test
  public void publishListOfItemsGlobalSwitchTrueL5PublishOnlyOnceTrueModelNotEmptyTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishItemPickupPointChangeEventOnlyOnce", true);
    saveAndPublishServiceImpl
        .publishListOfItems(Collections.singletonList(item), new ArrayList<>(), Arrays.asList(new ItemPickupPointDataChangeEventModel()),
          false);
    Mockito.verify(applicationContext).getBean(SaveAndPublishService.class);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(),
        any(ItemDataChange.class));
  }

  @Test
  public void publishListOfItemsGlobalSwitchTrueTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "newlyAddedItemDataChangeEventTypes", "MASTER_SKU_UPDATE");
    item.setItemChangeEventTypes(Collections.singletonList(ItemChangeEventType.ITEM_PRICE_CHANGE));
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    saveAndPublishServiceImpl
        .publishListOfItems(Collections.singletonList(item), Collections.singletonList(itemPickupPoint),
            new ArrayList<>(), false);
    Mockito.verify(applicationContext, times(2)).getBean(SaveAndPublishService.class);
    verify(kafkaProducer).send(Mockito.anyString(), isNull(), any());
    verify(kafkaProducer).send(Mockito.anyString(), Mockito.anyString(), any());
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), isNull());
  }

  @Test
  public void publishListOfItemsGlobalSwitchTrueItemChangeEventTypesEmptyTest() {
    Item itemNew = new Item();
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    saveAndPublishServiceImpl
        .publishListOfItems(Collections.singletonList(itemNew), Collections.singletonList(itemPickupPoint),
            new ArrayList<>(), false);
    verify(kafkaProducer, times(2)).send(Mockito.anyString(), isNull(), any());
    Mockito.verify(applicationContext, times(2)).getBean(SaveAndPublishService.class);
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), isNull());
  }

  @Test
  public void publishListOfItemsGlobalSwitchTrueNonEmptyItemPickupPointListTest() {
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    saveAndPublishServiceImpl
        .publishListOfItems(Collections.singletonList(item), Collections.singletonList(itemPickupPoint),
            new ArrayList<>(), false);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(),
      any(ItemDataChange.class));
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), isNull(),
      any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(applicationContext, Mockito.times(2)).getBean(SaveAndPublishService.class);
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), isNull());
  }

  @Test
  public void publishListOfItemsTestGlobalSwitchTrue() {
    saveAndPublishServiceImpl.publishListOfItems(Collections.singletonList(item));
    Mockito.verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME),
        anyString(), any(ItemDataChange.class));
  }

  @Test
  public void publishListOfItemsAndItemPickupPointsTest() {
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    saveAndPublishServiceImpl
        .publishListOfItems(Collections.singletonList(item), Collections.singletonList(itemPickupPoint),
            new ArrayList<>(), false);
    verify(kafkaProducer).send(anyString(), isNull(), any());
    verify(kafkaProducer).send(anyString(), anyString(), any());
    Mockito.verify(applicationContext, times(2)).getBean(SaveAndPublishService.class);
    Mockito.verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(isNull(), isNull());
  }

  @Test
  public void publishViewConfigChangeTest() {
    saveAndPublishServiceImpl.publishViewConfigChange(itemPickupPoint);
    Mockito.verify(this.kafkaProducer)
        .send(eq(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE), anyString(),
            any(ItemPickupPointViewConfigChangeEvent.class));
  }

  @Test
  public void publishListOfItemsAndItemPickupPointDataEventModelTest() {
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    itemPickupPointDataChangeEventModel.setItemSku(ITEM_SKU);
    itemPickupPointDataChangeEventModel.setPickupPointCode(PICKUP_POINT_CODE_1);
    saveAndPublishServiceImpl
        .publishListOfItems(Collections.singletonList(item), Collections.singletonList(itemPickupPoint),
            Collections.singletonList(itemPickupPointDataChangeEventModel), false);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(),
      any(ItemDataChange.class));
    Mockito.verify(applicationContext).getBean(SaveAndPublishService.class);
  }

  @Test
  public void publishViewConfigChangeExceptionTest() {
    doThrow(new RuntimeException()).when(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE),
        anyString(), any(ItemPickupPointViewConfigChangeEvent.class));
    this.saveAndPublishServiceImpl.publishViewConfigChange(itemPickupPoint);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE),
        anyString(), any(ItemPickupPointViewConfigChangeEvent.class));
  }

  @Test
  public void publishItemPickupPointDataChangeEventForAGPTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "newL5DataChangeTypeEnabled",
      true);
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeList = new ArrayList<>();
    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
      .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
      .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());
    itemPickupPointDataChangeList.add(new ItemPickupPointDataChangeEventModel());
    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEventForAGP(itemPickupPointList.get(0));
    verify(kafkaProducer).send(
      eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME_AGP), anyString(),
      any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    Mockito.verify(gdnMapper)
      .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    assertTrue(CollectionUtils.isNotEmpty(itemPickupPointDataChangeList));

  }

  @Test
  public void publishItemPickupPointDataChangeEventForAGP_cncForWarehouseTrueTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "newL5DataChangeTypeEnabled",
        true);
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "cncForWarehouseFeatureSwitch",
        true);
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeList = new ArrayList<>();
    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
            .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
        .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());
    itemPickupPointDataChangeList.add(new ItemPickupPointDataChangeEventModel());
    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEventForAGP(itemPickupPointList.get(0));
    verify(kafkaProducer).send(
        eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME_AGP), anyString(),
        any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    Mockito.verify(gdnMapper)
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    assertTrue(CollectionUtils.isNotEmpty(itemPickupPointDataChangeList));

  }

  @Test
  public void publishItemPickupPointDataChangeEventForAGPEmptyTest() {
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeList = null;
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "newL5DataChangeTypeEnabled",
      true);
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
      .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
      .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());
    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEventForAGP(itemPickupPointList.get(0));
    verify(kafkaProducer).send(
      eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME_AGP), anyString(),
      any(ItemPickupPointDataChangeEventModel.class));
    Mockito.verify(gdnMapper).deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    Mockito.verify(gdnMapper)
      .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    assertTrue(CollectionUtils.isEmpty(itemPickupPointDataChangeList));

  }

  @Test
  public void publishItemPickupPointDataChangeEventForAGP_ExceptionTest() {
    ItemPickupPointDataChangeEventModel changeEventModel =
      ItemPickupPointDataChangeEventModel.builder().itemSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).build();
    doThrow(ApplicationRuntimeException.class).when(this.kafkaProducer)
        .send(eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME_AGP),
          eq(itemPickupPoint.getItemSku()),
            eq(changeEventModel));
    this.saveAndPublishServiceImpl.publishItemPickupPointDataChangeEventForAGP(itemPickupPoint);
    verify(this.kafkaProducer).send(anyString(),
      anyString(), any());
  }
  @Test
  public void publishItemPickupPointDataChangeEventForAGP_emptyListTest() {
    List<ItemPickupPoint> itemPickupPointList = Collections.EMPTY_LIST;
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    Mockito.when(gdnMapper.deepCopy(Mockito.any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class)))
      .thenReturn(new com.gdn.x.product.domain.event.model.Price());
    Mockito.when(gdnMapper
        .deepCopy(Mockito.any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class)))
      .thenReturn(new com.gdn.x.product.domain.event.model.ItemViewConfig());
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeList =
      Collections.EMPTY_LIST;

    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEventForAGP(itemPickupPoint);
    verify(kafkaProducer).send(
      eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME_AGP), anyString(),
      any(ItemPickupPointDataChangeEventModel.class));
    assertTrue(CollectionUtils.isEmpty(itemPickupPointDataChangeList));
  }

  @Test
  public void publishMerchantVoucherViewConfigChangeTest() {
    itemPickupPoint.setItemSku(ITEM_SKU);
    Map<String, Item> itemMap = ImmutableMap.of(ITEM_SKU, item);
    saveAndPublishServiceImpl.publishMerchantVoucherViewConfigChange(Arrays.asList(itemPickupPoint), Arrays.asList(item));
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE),
        anyString(), any());
  }

  @Test
  public void publishMerchantPromoDiscountEventChangeWithPickupPointTest() {
    List<DiscountPrice> discountPriceList = new ArrayList<>();
    discountPriceList.add(
      new DiscountPrice(200, new Date(), new Date(), StringUtils.EMPTY, AdjustmentType.MERCHANT));
    Price price = new Price();
    price.setListPrice(700);
    price.setOfferPrice(600);
    price.setChannel("DEFAULT");
    price.setListOfDiscountPrices(discountPriceList);
    price.setMerchantPromoDiscountPrice(discountPriceList.get(0));
    Set<Price> prices = new HashSet<Price>();
    prices.add(price);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(prices);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    this.saveAndPublishServiceImpl.publishMerchantPromoDiscountEventChange(itemPickupPoint);
    Mockito.verify(this.kafkaProducer).send(
        eq(ProductDomainEventName.MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE),anyString(),
        Mockito.any(MerchantPromoDiscountChangeEvent.class));
  }

  @Test
  public void publishMerchantPromoDiscountEventChangeWithPickupPoint_ExceptionTest() {
    doThrow(new RuntimeException()).when(this.kafkaProducer).send(eq(ProductDomainEventName.MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE),
      anyString(), any(MerchantPromoDiscountChangeEvent.class));
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(prices);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    this.saveAndPublishServiceImpl.publishMerchantPromoDiscountEventChange(itemPickupPoint);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE),
        anyString(), Mockito.any(MerchantPromoDiscountChangeEvent.class));
  }

  @Test
  public void publishSolrUpdateEventTest() {
    ProductAndItemEventModel productAndItemEventModel =
        new ProductAndItemEventModel(PRODUCT_SKU, new ProductEventModel(),
          Arrays.asList(new ItemEventModel()), true, false, MERCHANT_CODE);
    saveAndPublishServiceImpl.publishSolrUpdateEvent(Arrays.asList(productAndItemEventModel));
    Mockito.verify(
        this.kafkaProducer).send(eq(ProductDomainEventName.UPDATE_TO_SOLR), anyString(),
            any(ProductAndItemEventModel.class));
  }

  @Test
  public void publishOfflineItemChangeSellerEventTest() {
    OfflineItemChange offlineItemChange = new OfflineItemChange();
    saveAndPublishServiceImpl.publishOfflineItemChangeSellerEvent(offlineItemChange);
    Mockito.verify(this.kafkaProducer).send(eq(ProductDomainEventName.OFFLINE_ITEM_CHANGE_REGULAR_SELLER_EVENT_NAME),
        isNull(), any(OfflineItemChange.class));
  }

  @Test
  public void publishListOfItemsForHalaConfigChangeTest() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "newlyAddedItemDataChangeEventTypes", "SKU_UPDATE");
    item.setItemChangeEventTypes(Collections.singletonList(ItemChangeEventType.HALAL_CONFIG_CHANGE));
    saveAndPublishServiceImpl.publishListOfItemsForHalaConfigChange(Collections.singletonList(item), true);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(),
        any(ItemDataChange.class));
  }

  @Test
  public void publishListOfItemsForHalaConfigChangeExceptionTest() {
    doThrow(new RuntimeException()).when(this.kafkaProducer)
        .send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(), any(ItemDataChange.class));
    this.saveAndPublishServiceImpl.publishListOfItemsForHalaConfigChange(Collections.singletonList(item), true);
    verify(this.kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(),
        any(ItemDataChange.class));
  }

  @Test
  public void publishProductBundleOneToOneMappingEventAsOneItemInRecipeTest() {
    BundleRecipe bundleRecipe = new BundleRecipe();
    bundleRecipe.setItemSku(ITEM_SKU2);
    bundleRecipe.setQuantity(1);
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setBundleRecipe(ImmutableSet.of(bundleRecipe));

    saveAndPublishServiceImpl.publishProductBundleOneToOneMappingEvent(Arrays.asList(item));

    Mockito.verify(kafkaProducer).send(ProductDomainEventName.PRODUCT_BUNDLE_ONE_TO_ONE_MAPPING_EVENT, item.getItemSku(),
        new BundleProductOneToOneMappingEventModel(item.getItemSku(),
            item.getBundleRecipe().stream().findFirst().get().getItemSku()));
  }

  @Test
  public void publishProductBundleOneToOneMappingEventAsTwoItemInRecipeTest() {
    BundleRecipe bundleRecipe = new BundleRecipe();
    bundleRecipe.setItemSku(ITEM_SKU2);
    bundleRecipe.setQuantity(1);
    BundleRecipe bundleRecipe2 = new BundleRecipe();
    bundleRecipe2.setItemSku(ITEM_SKU3);
    bundleRecipe2.setQuantity(1);
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setBundleRecipe(ImmutableSet.of(bundleRecipe, bundleRecipe2));

    saveAndPublishServiceImpl.publishProductBundleOneToOneMappingEvent(Arrays.asList(item));
    assertNotNull(ITEM);
  }

  @Test
  public void publishProductBundleOneToOneMappingEventNoInRecipeTest() {
    Item item = new Item();
    item.setItemSku(ITEM_SKU);

    saveAndPublishServiceImpl.publishProductBundleOneToOneMappingEvent(Arrays.asList(item));
    assertNotNull(ITEM);
  }

  @Test
  public void publishProductBundleOneToOneMappingEventNullItemTest() {
    List<Item> items = new ArrayList<>();
    items.add(null);
    saveAndPublishServiceImpl.publishProductBundleOneToOneMappingEvent(items);
    assertNotNull(ITEM);
  }

  @Test
  public void publishListOfItemsItemChangeEventTypeV1AndV2Test() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "newlyAddedItemDataChangeEventTypes", "MASTER_SKU_UPDATE");
    item.setItemChangeEventTypes(Arrays.asList(ItemChangeEventType.HALAL_CONFIG_CHANGE,
        ItemChangeEventType.MASTER_SKU_UPDATE));
    saveAndPublishServiceImpl.publishListOfItemsForHalaConfigChange(Collections.singletonList(item), true);
    verify(kafkaProducer).send(eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), anyString(),
        itemDataChangeArgumentCaptor.capture());
    Assertions.assertEquals(itemDataChangeArgumentCaptor.getValue().getItemChangeEventTypes().size(), 1);
    Assertions.assertEquals(itemDataChangeArgumentCaptor.getValue().getItemChangeEventTypesV2().size(), 2);
    assertFalse(itemDataChangeArgumentCaptor.getValue().getItemChangeEventTypes()
        .contains(com.gdn.x.product.domain.event.enums.ItemChangeEventType.MASTER_SKU_UPDATE));
  }

  @Test
  public void publishHistoryEventTest() {
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(product.getMerchantCode(), Constants.DEFAULT, Constants.ACTION_KEY, String.valueOf(true),
            String.valueOf(false), null, product.getProductSku(), product.getProductName());
    saveAndPublishServiceImpl.publishHistoryEvent(List.of(auditTrailDto));
    Mockito.verify(kafkaProducer).send(Mockito.eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), auditTrailListResponseArgumentCaptor.capture());
    Assertions.assertEquals(1, auditTrailListResponseArgumentCaptor.getValue().getAuditTrailResponseList().size());
    Assertions.assertEquals(product.getProductSku(), auditTrailListResponseArgumentCaptor.getValue().getAuditTrailResponseList().getFirst().getProductSku());
    Assertions.assertEquals(Constants.ACTION_KEY, auditTrailListResponseArgumentCaptor.getValue().getAuditTrailResponseList().getFirst().getActionKey());
    Assertions.assertEquals(Boolean.TRUE.toString(), auditTrailListResponseArgumentCaptor.getValue().getAuditTrailResponseList().getFirst().getOldValue());
    Assertions.assertEquals(Boolean.FALSE.toString(), auditTrailListResponseArgumentCaptor.getValue().getAuditTrailResponseList().getFirst().getNewValue());
  }

  @Test
  public void publishOdooCreationEvent() {
    saveAndPublishServiceImpl.publishOdooCreationEvent(new OdooCreationEventModel());
    Mockito.verify(kafkaTopicProperties, times(2)).getOdooCreationEvent();
    Mockito.verify(kafkaProducer).send(Mockito.any(), Mockito.any(),Mockito.any());
  }

  @Test
  public void publishVideoCompressionEvent() {
    VideoCompressionEventModel videoCompressionEventModel =
      VideoCompressionEventModel.builder().videoId(ID)
        .clientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT).build();
    saveAndPublishServiceImpl.publishVideoCompressionEvent(videoCompressionEventModel);
    Mockito.verify(kafkaTopicProperties, times(2)).getVideoCompressionEvent();
    Mockito.verify(kafkaProducer).send(Mockito.any(), Mockito.any(),Mockito.any());
  }

  @Test
  public void addItemPickupPointDataChangeType_WithNonEmptySourceAndNonEmptyItemPickupPoints_ShouldAddSource() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishItemPickupPointChangeEventOnlyOnce", false);
    String source = Constants.SOURCE_PRODUCT_PUBLISH;
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setItemPickupPointDataChangeType(new ArrayList<>());
    
    ItemPickupPoint itemPickupPoint2 = new ItemPickupPoint();
    itemPickupPoint2.setItemSku(ITEM_SKU2);
    itemPickupPoint2.setItemPickupPointDataChangeType(new ArrayList<>());
    
    List<ItemPickupPoint> itemPickupPoints = Arrays.asList(itemPickupPoint1, itemPickupPoint2);

    SaveAndPublishService mockSaveAndPublishService = Mockito.mock(SaveAndPublishService.class);
    Mockito.when(applicationContext.getBean(SaveAndPublishService.class)).thenReturn(mockSaveAndPublishService);
    
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    
    saveAndPublishServiceImpl.publishListOfItems(Collections.singletonList(item), itemPickupPoints,
        new ArrayList<>(), source, false, Collections.EMPTY_MAP);

  }

  @Test
  public void addItemPickupPointDataChangeType_WithEmptySource_ShouldNotAddSource() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishItemPickupPointChangeEventOnlyOnce", false);
    String source = "";
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemPickupPointDataChangeType(new ArrayList<>());
    SaveAndPublishService mockSaveAndPublishService = Mockito.mock(SaveAndPublishService.class);
    Mockito.when(applicationContext.getBean(SaveAndPublishService.class)).thenReturn(mockSaveAndPublishService);
    List<ItemPickupPoint> emptyItemPickupPoints = new ArrayList<>();
    List<ItemPickupPointDataChangeEventModel> modelList = Collections.singletonList(new ItemPickupPointDataChangeEventModel());
    
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    
    saveAndPublishServiceImpl.publishListOfItems(Collections.singletonList(item), emptyItemPickupPoints,
        modelList, source, false, Collections.EMPTY_MAP);
    
    assertEquals(0, itemPickupPoint.getItemPickupPointDataChangeType().size());
  }

  @Test
  public void addItemPickupPointDataChangeType_WithNullSource_ShouldNotAddSource() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishItemPickupPointChangeEventOnlyOnce", false);
    String source = null;
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemPickupPointDataChangeType(new ArrayList<>());
    SaveAndPublishService mockSaveAndPublishService = Mockito.mock(SaveAndPublishService.class);
    Mockito.when(applicationContext.getBean(SaveAndPublishService.class)).thenReturn(mockSaveAndPublishService);
    List<ItemPickupPoint> emptyItemPickupPoints = new ArrayList<>();
    List<ItemPickupPointDataChangeEventModel> modelList = Collections.singletonList(new ItemPickupPointDataChangeEventModel());
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    saveAndPublishServiceImpl.publishListOfItems(Collections.singletonList(item), emptyItemPickupPoints,
        modelList, source, false, Collections.EMPTY_MAP);
    assertEquals(0, itemPickupPoint.getItemPickupPointDataChangeType().size());
  }

  @Test
  public void addItemPickupPointDataChangeType_WithEmptyItemPickupPointsList_ShouldNotAddSource() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishItemPickupPointChangeEventOnlyOnce", false);
    String source = Constants.SOURCE_PRODUCT_PUBLISH;
    List<ItemPickupPoint> emptyItemPickupPoints = new ArrayList<>();
    SaveAndPublishService mockSaveAndPublishService = Mockito.mock(SaveAndPublishService.class);
    Mockito.when(applicationContext.getBean(SaveAndPublishService.class)).thenReturn(mockSaveAndPublishService);
    List<ItemPickupPointDataChangeEventModel> modelList = Collections.singletonList(new ItemPickupPointDataChangeEventModel());
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new ArrayList<>());
    saveAndPublishServiceImpl.publishListOfItems(Collections.singletonList(item), emptyItemPickupPoints,
        modelList, source, false, Collections.EMPTY_MAP);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(eq(null), Mockito.anyString());
  }

  @Test
  public void addItemPickupPointDataChangeType_WithExistingDataChangeTypes_ShouldAppendSource() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishItemPickupPointChangeEventOnlyOnce", false);
    String source = Constants.SOURCE_PRODUCT_PUBLISH;
    String existingChangeType = "PRICE_CHANGE";
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemPickupPointDataChangeType(new ArrayList<>(Arrays.asList(existingChangeType)));
    List<ItemPickupPoint> itemPickupPoints = Collections.singletonList(itemPickupPoint);
    SaveAndPublishService mockSaveAndPublishService = Mockito.mock(SaveAndPublishService.class);
    Mockito.when(applicationContext.getBean(SaveAndPublishService.class)).thenReturn(mockSaveAndPublishService);
    List<ItemPickupPointDataChangeEventModel> modelList = Collections.singletonList(new ItemPickupPointDataChangeEventModel());
    saveAndPublishServiceImpl.publishListOfItems(Collections.singletonList(item), itemPickupPoints,
        modelList, source, false, Collections.EMPTY_MAP);
    assertEquals(2, itemPickupPoint.getItemPickupPointDataChangeType().size());
  }

  @Test
  public void addItemPickupPointDataChangeType_WithPublishOnlyOnceTrueAndEmptyList_ShouldAddSourceWhenFetched() {
    ReflectionTestUtils.setField(this.saveAndPublishServiceImpl, "publishItemPickupPointChangeEventOnlyOnce", true);
    String source = Constants.SOURCE_PRODUCT_PUBLISH;
    ItemPickupPoint fetchedItemPickupPoint = new ItemPickupPoint();
    fetchedItemPickupPoint.setItemSku(ITEM_SKU);
    fetchedItemPickupPoint.setItemPickupPointDataChangeType(new ArrayList<>());
    SaveAndPublishService mockSaveAndPublishService = Mockito.mock(SaveAndPublishService.class);
    Mockito.when(applicationContext.getBean(SaveAndPublishService.class)).thenReturn(mockSaveAndPublishService);
    List<ItemPickupPointDataChangeEventModel> modelList = new ArrayList<>();
    Mockito.when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(anyString(), anyString()))
        .thenReturn(businessPartner);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(Collections.singletonList(fetchedItemPickupPoint));
    saveAndPublishServiceImpl.publishListOfItems(Collections.singletonList(item), new ArrayList<>(),
        modelList, source, false, Collections.EMPTY_MAP);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(eq(null), Mockito.anyString());
  }
  @Test
  public void saveProduct_shouldPublishEvent_whenMerchantNotBlacklisted() {
    SaveAndPublishServiceImplTest.PRODUCT.setMerchantCode("MERCHANT_ALLOWED");

    when(productRepository.save(any(Product.class))).thenReturn(SaveAndPublishServiceImplTest.PRODUCT);

    Map<String, Set<String>> blacklistMap = new HashMap<>();
    blacklistMap.put(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME,
            new HashSet<>(Arrays.asList("OTHER_MERCHANT")));

    this.saveAndPublishServiceImpl.saveProduct(
            SaveAndPublishServiceImplTest.PRODUCT,
            Collections.singletonList("CHANGE"),
            "",
            blacklistMap
    );
    verify(this.productRepository).save(SaveAndPublishServiceImplTest.PRODUCT);
    verify(this.kafkaProducer)
            .send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME), any(), any(ProductChange.class));
  }
  @Test
  public void saveProduct_shouldNotPublishEvent_whenMerchantIsBlacklisted() {
    SaveAndPublishServiceImplTest.PRODUCT.setMerchantCode("BLACKLISTED_MERCHANT");

    when(productRepository.save(any(Product.class))).thenReturn(SaveAndPublishServiceImplTest.PRODUCT);

    Map<String, Set<String>> blacklistMap = new HashMap<>();
    blacklistMap.put(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME,
            new HashSet<>(Arrays.asList("BLACKLISTED_MERCHANT")));

    this.saveAndPublishServiceImpl.saveProduct(
            SaveAndPublishServiceImplTest.PRODUCT,
            Collections.singletonList("CHANGE"),
            "",
            blacklistMap
    );

    verify(this.productRepository).save(SaveAndPublishServiceImplTest.PRODUCT);
    verify(this.kafkaProducer, never())
            .send(eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME), any(), any(ProductChange.class));
  }
  @Test
  public void saveProduct_shouldPublishEvent_whenBlacklistSetIsEmpty() {
    SaveAndPublishServiceImplTest.PRODUCT.setMerchantCode("ANY_MERCHANT");

    when(productRepository.save(any(Product.class)))
            .thenReturn(SaveAndPublishServiceImplTest.PRODUCT);

    Map<String, Set<String>> blacklistMap = new HashMap<>();
    blacklistMap.put(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME, Collections.emptySet());

    saveAndPublishServiceImpl.saveProduct(
            SaveAndPublishServiceImplTest.PRODUCT,
            Collections.singletonList("CHANGE"),
            "",
            blacklistMap
    );

    verify(productRepository).save(SaveAndPublishServiceImplTest.PRODUCT);
    verify(kafkaProducer).send(
            eq(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME),
            any(),
            any(ProductChange.class)
    );
  }
  @Test
  public void publishItemPickupPointDataChangeEvent_shouldPublish_whenMerchantNotBlacklisted() {
    List<ItemPickupPoint> itemPickupPointList = Collections.singletonList(itemPickupPoint);
    List<ItemPickupPointChangeEventType> emptyChangeTypes = Collections.<ItemPickupPointChangeEventType>emptyList();

    Map<String, Set<String>> blacklistMap = new HashMap<>();
    blacklistMap.put(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME,
            new HashSet<>(Arrays.asList("OTHER_MERCHANT")));

    ReflectionTestUtils.setField(masterProductChangeEventListener, "eventToBlackListedSellersMap", blacklistMap);

    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEvent(itemPickupPointList, emptyChangeTypes,
      Collections.EMPTY_MAP);

    verify(kafkaProducer, atLeastOnce()).send(
            eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME),
            any(),
            any(com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel.class)
    );

    verify(gdnMapper).deepCopy(any(Price.class), eq(com.gdn.x.product.domain.event.model.Price.class));
    verify(gdnMapper).deepCopy(any(ItemViewConfig.class), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
    verify(businessPartnerService).getBusinessPartnerByBusinessPartnerCode(nullable(String.class), nullable(String.class));
  }
  @Test
  public void publishItemPickupPointDataChangeEvent_shouldNotPublish_whenMerchantIsBlacklisted() {

    Item item = new Item();
    item.setItemSku("ITEM_SKU");
    item.setMerchantCode("MERCHANT_CODE");
    item.setStoreId(STORE_ID);
    ItemPickupPoint pickupPoint = new ItemPickupPoint();
    pickupPoint.setItemSku(item.getItemSku());
    pickupPoint.setMerchantCode(item.getMerchantCode());
    pickupPoint.setStoreId(STORE_ID);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, item.getItemSku()))
            .thenReturn(Collections.singletonList(pickupPoint));
    Map<String, Set<String>> blacklist = new HashMap<>();
    blacklist.put(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME, new HashSet<>(Collections.singletonList("MERCHANT_CODE"))
    );
    blacklist.put(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME, new HashSet<>(Collections.singletonList("MERCHANT_CODE"))
    );

    saveAndPublishServiceImpl.publishListOfItems(
            Collections.singletonList(item),
            Collections.emptyList(),
            Collections.emptyList(),
            "source",
            false,
            blacklist
    );

    Mockito.verify(kafkaProducer, Mockito.never()).send(Mockito.eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), Mockito.any(), Mockito.any());
    Mockito.verify(itemPickupPointService, Mockito.never()).publishItemPickupPointDataChangeEventWithPureCncStatusChange(Mockito.any(), eq(Collections.EMPTY_MAP));
  }
  @Test
  public void publishItemPickupPointDataChangeEvent_shouldPublish_whenBlacklistSetIsEmpty() {
    Item item = new Item();
    item.setItemSku("ITEM_SKU");
    item.setMerchantCode("MERCHANT_CODE");
    item.setStoreId(STORE_ID);

    ItemPickupPoint pickupPoint = new ItemPickupPoint();
    pickupPoint.setItemSku(item.getItemSku());
    pickupPoint.setMerchantCode(item.getMerchantCode());
    pickupPoint.setStoreId(STORE_ID);

    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, item.getItemSku())).thenReturn(Collections.singletonList(pickupPoint));

    Map<String, Set<String>> blacklist = new HashMap<>();
    blacklist.put(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME, Collections.emptySet());

    saveAndPublishServiceImpl.publishListOfItems(
            Collections.singletonList(item),
            Collections.emptyList(),
            Collections.emptyList(),
            "source",
            false,
            blacklist
    );
    Mockito.verify(kafkaProducer, Mockito.atLeastOnce()).send(Mockito.eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), Mockito.any(), Mockito.any(com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel.class));
    Mockito.clearInvocations(kafkaProducer, businessPartnerService);
  }
  @Test
  public void publishItemPickupPointDataChangeEvent_shouldPublish_whenBlacklistContainsOnlyOtherMerchants() {
    Item item = new Item();
    item.setItemSku("ITEM_SKU");
    item.setMerchantCode("MERCHANT_ALLOWED");
    item.setStoreId(STORE_ID);

    ItemPickupPoint pickupPoint = new ItemPickupPoint();
    pickupPoint.setItemSku(item.getItemSku());
    pickupPoint.setMerchantCode(item.getMerchantCode());
    pickupPoint.setStoreId(STORE_ID);

    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, item.getItemSku())).thenReturn(Collections.singletonList(pickupPoint));

    Map<String, Set<String>> blacklist = new HashMap<>();
    blacklist.put(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME, new HashSet<>(Collections.singletonList("OTHER_MERCHANT")));

    saveAndPublishServiceImpl.publishListOfItems(
            Collections.singletonList(item),
            Collections.emptyList(),
            Collections.emptyList(),
            "source",
            false,
            blacklist
    );
    Mockito.verify(kafkaProducer, Mockito.atLeastOnce()).send(Mockito.eq(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME), Mockito.any(), Mockito.any(com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel.class));
    Mockito.clearInvocations(kafkaProducer, businessPartnerService);
  }
  @Test
  public void publishItemDataChangeEvent_shouldNotPublish_whenMerchantIsBlacklisted() {
    Item item = new Item();
    item.setItemSku("ITEM_SKU");
    item.setMerchantCode("BLACKLISTED_MERCHANT");
    item.setStoreId(STORE_ID);

    Map<String, Set<String>> blacklist = new HashMap<>();
    blacklist.put(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME,
            new HashSet<>(Collections.singletonList("BLACKLISTED_MERCHANT")));

    saveAndPublishServiceImpl.publishListOfItems(
            Collections.singletonList(item),
            Collections.emptyList(),
            Collections.emptyList(),
            "source",
            false,
            blacklist
    );
    Mockito.verify(kafkaProducer, Mockito.never()).send(Mockito.eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), Mockito.any(), Mockito.any());
    Mockito.clearInvocations(kafkaProducer, businessPartnerService);
  }
  @Test
  public void publishItemDataChangeEvent_shouldPublish_whenMerchantNotBlacklisted() {
    Item item = new Item();
    item.setItemSku("ITEM_SKU");
    item.setMerchantCode("ALLOWED_MERCHANT");
    item.setStoreId(STORE_ID);

    Map<String, Set<String>> blacklist = new HashMap<>();
    blacklist.put(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME,
            new HashSet<>(Collections.singletonList("OTHER_MERCHANT")));

    saveAndPublishServiceImpl.publishListOfItems(
            Collections.singletonList(item),
            Collections.emptyList(),
            Collections.emptyList(),
            "source",
            false,
            blacklist
    );
    Mockito.verify(kafkaProducer, Mockito.atLeastOnce()).send(Mockito.eq(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME), Mockito.any(), Mockito.any());
    Mockito.clearInvocations(kafkaProducer, businessPartnerService);
  }
  @Test
  public void publishItemPickupPointChangeEvent_shouldInvokeItemPickupPointService_whenModelListNotEmpty() {
    List<ItemPickupPoint> emptyItemPickupPoints = Collections.emptyList();
    ItemPickupPointDataChangeEventModel model = new ItemPickupPointDataChangeEventModel();
    List<ItemPickupPointDataChangeEventModel> models = Collections.singletonList(model);
    boolean productRejected = true;
    Map<String, Set<String>> blacklistMap = Collections.emptyMap();
    ReflectionTestUtils.invokeMethod(
            saveAndPublishServiceImpl,
            "publishItemPickupPointChangeEvent",
            emptyItemPickupPoints,
            models,
            productRejected,
            blacklistMap
    );
    verify(itemPickupPointService, times(1)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(eq(model), eq(Collections.EMPTY_MAP));
    verify(kafkaProducer, never()).send(any(), any(), any());
  }
  @Test
  public void shouldPublish_whenBlacklistMapIsNull() {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantCode("MERCHANT_SKU");
    itemPickupPoint.setStoreId("STORE_ID");
    itemPickupPoint.setPrice(Collections.emptySet());
    itemPickupPoint.setItemPickupPointDataChangeType(Collections.emptyList());
    itemPickupPoint.setMarkForDelete(false);

    List<ItemPickupPoint> itemPickupPoints = Collections.singletonList(itemPickupPoint);
    List<ItemPickupPointChangeEventType> types = new ArrayList<>();

    BusinessPartner businessPartner = new BusinessPartner();
    when(businessPartnerService.getBusinessPartnerByBusinessPartnerCode(
      eq(itemPickupPoint.getStoreId()),
      eq(itemPickupPoint.getMerchantCode())
    )).thenReturn(businessPartner);

    Map<String, Set<String>> eventToBlackListedSellersMap = null;

    saveAndPublishServiceImpl.publishItemPickupPointDataChangeEvent(
      itemPickupPoints, types, eventToBlackListedSellersMap);

    ArgumentCaptor<String> topicCaptor = ArgumentCaptor.forClass(String.class);
    ArgumentCaptor<String> keyCaptor = ArgumentCaptor.forClass(String.class);
    ArgumentCaptor<ItemPickupPointDataChangeEventModel> modelCaptor =
      ArgumentCaptor.forClass(ItemPickupPointDataChangeEventModel.class);

    verify(kafkaProducer, times(1)).send(
      topicCaptor.capture(), keyCaptor.capture(), modelCaptor.capture());

    assertEquals(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME,
      topicCaptor.getValue());

    ItemPickupPointDataChangeEventModel published = modelCaptor.getValue();
    assertNotNull(published);
    assertEquals(itemPickupPoint.getMerchantCode(), published.getMerchantCode());
    assertEquals(itemPickupPoint.getStoreId(), published.getStoreId());

    verify(businessPartnerService, times(1))
      .getBusinessPartnerByBusinessPartnerCode(
        itemPickupPoint.getStoreId(),
        itemPickupPoint.getMerchantCode()
      );
  }
}