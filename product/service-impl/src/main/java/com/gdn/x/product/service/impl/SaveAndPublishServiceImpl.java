package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.x.product.domain.event.model.VideoCompressionEventModel;
import com.gdn.x.product.service.util.ValidationUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.partners.merchant.voucher.streaming.model.DomainEventName;
import com.gdn.partners.merchant.voucher.streaming.model.VoucherItemSkusEventModel;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.BundleProductOneToOneMappingEventModel;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemDataChange;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.ItemPickupPointMigrationEvent;
import com.gdn.x.product.domain.event.model.ItemViewConfig;
import com.gdn.x.product.domain.event.model.ItemViewConfigWithArchivedChangeEvent;
import com.gdn.x.product.domain.event.model.MasterDataItem;
import com.gdn.x.product.domain.event.model.MerchantPromoDiscountChangeEvent;
import com.gdn.x.product.domain.event.model.MigrationForItemCreationEvent;
import com.gdn.x.product.domain.event.model.OdooCreationEventModel;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.domain.event.model.OfflineItemPopulateEvent;
import com.gdn.x.product.domain.event.model.Price;
import com.gdn.x.product.domain.event.model.PristineDataItemEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.ProductChange;
import com.gdn.x.product.domain.event.model.ProductL3SolrReindexEvent;
import com.gdn.x.product.domain.event.model.ProductModel;
import com.gdn.x.product.domain.event.model.WholesalePriceActivatedOrDeactivatedEvent;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.executor.api.AsyncProcessor;
import com.gdn.x.product.service.properties.KafkaTopicProperties;
import com.gdn.x.product.service.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;


@Slf4j
@Service
public class SaveAndPublishServiceImpl implements SaveAndPublishService {
  private static final Logger LOG = LoggerFactory.getLogger(SaveAndPublishServiceImpl.class);

  private static final String SKU_DELIMITER = "-";

  private static final String COMMAND_DESC_PRODUCT = "publishStreamOfProducts";

  private static final String COMMAND_DESC_ITEM = "publishStreamOfItems";

  private static final String CLIENT_ID_UPSERT_OFFLINE_ITEM_PRICE = "Regular-Merchant-Client-Id";

  @Autowired
  private ItemRepository itemRepository;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private AsyncProcessor asyncProcessor;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  protected GdnMapper gdnMapper;

  @Autowired
  private ApplicationContext applicationContext;

  @Value("${publish.pristine.mapping.change.event}")
  private boolean publishPristineMappingChangeEvent;

  @Value("${skip.event.publish.take.down.products}")
  private boolean skipEventPublishForTakeDownProduct;

  @Value("${publish.item.pickup.point.change.event.only.once}")
  private boolean publishItemPickupPointChangeEventOnlyOnce;

  @Value("${newly.added.item.data.change.event.types}")
  private String newlyAddedItemDataChangeEventTypes;

  @Value("${newly.added.item.pickup.point.data.change.event.types}")
  private String newlyAddedItemPickupPointDataChangeEventTypes;

  @Value("${new.l5.data.change.event.types.enabled}")
  private boolean newL5DataChangeTypeEnabled;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  private SaveAndPublishService getSaveAndPublishServiceBean() {
    return applicationContext.getBean(SaveAndPublishService.class);
  }

  @Override
  public Item addActivePromoBundling(String storeId, String itemSku, String activePromoBundling) {
    return this.itemRepository.addActivePromoBundling(storeId, itemSku, activePromoBundling);
  }

  @Override
  public Item insertItem(Item item) {
    Item itemAfterInsert = this.itemRepository.insert(item);
    LOG.info("Publishing event:{} while inserting, item :{}", ProductDomainEventName.ITEM_CHANGE_EVENT_NAME, item);
    itemAfterInsert.setUniqueId(itemAfterInsert.getItemSku());
    ItemChange itemChange = setL5DetailsForPublishing(itemAfterInsert);
    kafkaPublisher.send(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME, itemChange.getItemSku(),
      itemChange);
    return itemAfterInsert;
  }

  @Override
  public List<Item> insertItems(List<Item> items) {
    List<Item> insertedItems = this.itemRepository.insert(items);
    this.publishListOfItems(insertedItems);
    return insertedItems;
  }

  private void publishProductChange(Product product, List<String> productChangeEventTypes,
      String productPublishSourceDeletePickupPoint) {
    ProductChange productChange = new ProductChange();
    productChange.setProductChangeEventType(productChangeEventTypes);
    Set<String> productChannel = new HashSet<>();
    BeanUtils.copyProperties(product, productChange);
    CommonUtil.populateProductType(product, productChange);
    productChange.setMultiVariant(product.getDefiningAttributes().size() > Constants.SINGLE_VARIANT_L4_COUNT);
    setDistributionMappingStatusInEvent(product, productChange);
    if (Objects.nonNull(product.getMasterDataProduct())) {
      com.gdn.x.product.domain.event.model.MasterDataProduct masterDataProduct =
              new com.gdn.x.product.domain.event.model.MasterDataProduct();
      BeanUtils.copyProperties(product.getMasterDataProduct(), masterDataProduct);
      productChange.setMasterDataProduct(masterDataProduct);
    }
    if (CollectionUtils.isNotEmpty(product.getAllSalesCatalogs())) {
      for (SalesCatalog salesCatalog : product.getAllSalesCatalogs()) {
        productChange.getSalesCatalogs()
            .add(gdnMapper.deepCopy(salesCatalog, com.gdn.x.product.domain.event.model.SalesCatalog.class));
      }
    }
    if (product.isB2bActivated()) {
      productChannel.add(Constants.B2B);
    }
    if (Boolean.TRUE.equals(product.getB2cActivated())) {
      productChannel.add(Constants.RETAIL);
    }
    productChange.setProductChannel(productChannel);
    CommonUtil.setBrandAndCategoryCodeAndPreOrderForOdoo(product, productChange);
    productChange.setSource(Optional.ofNullable(productPublishSourceDeletePickupPoint).orElse(StringUtils.EMPTY));
    LOG.info("Publishing event : {}, product : {}", ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME,
        productChange);
    this.kafkaPublisher.send(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME,
      productChange.getProductSku(), productChange);
  }

  private void setDistributionMappingStatusInEvent(Product result, ProductChange productChange) {
    if (ranchIntegrationEnabled) {
      if (Objects.nonNull(result.getDistributionStatus())) {
        productChange.setDistributionMappingStatus(result.getDistributionStatus().getDescription());
      }
    }
  }

  @Override
  public Item removeActivePromoBundling(String storeId, String itemSku,
      String activePromoBundling) {
    return this.itemRepository.removeActivePromoBundling(storeId, itemSku, activePromoBundling);
  }

  @Override
  public void publishListOfItems(List<Item> items) {
    for (Item item : items) {
      item.setUniqueId(item.getItemSku());
      LOG.info("Publishing L4 data event:{}, item : {} ", ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME, item);
      publishItemDataChangeEvent(Arrays.asList(item));
    }
  }

  @Override
  public void publishListOfItemsForHalaConfigChange(List<Item> items, boolean halalProduct){
    for (Item item : items) {
      item.setUniqueId(item.getItemSku());
      LOG.info("Publishing L4 data event:{}, item : {} ", ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME, item);
      publishItemDataChangeEventForHalalConfigChange(item, halalProduct);
    }
  }

  @Override
  public void publishListOfItems(List<Item> items, List<ItemPickupPoint> itemPickupPoints,
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList,
    boolean productRejected) {
    publishL4AndL5Events(items, itemPickupPoints, itemPickupPointDataChangeEventModelList,
      StringUtils.EMPTY, productRejected, Collections.EMPTY_MAP);
  }

  @Override
  public void publishListOfItems(List<Item> items, List<ItemPickupPoint> itemPickupPoints,
                                 List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList,
                                 String source, boolean productRejected,
                                 Map<String, Set<String>> eventToBlackListedSellersMap) {
    publishL4AndL5Events(items, itemPickupPoints, itemPickupPointDataChangeEventModelList, source,
      productRejected, eventToBlackListedSellersMap);
  }

  private void publishL4AndL5Events(List<Item> items, List<ItemPickupPoint> itemPickupPoints,
                                    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList,
                                    String source, boolean productRejected,
                                    Map<String, Set<String>> eventToBlackListedSellersMap) {
    for (Item item : items) {
      item.setUniqueId(item.getItemSku());
      publishItemDataChangeIfAllowed(item, source, productRejected, eventToBlackListedSellersMap);
      boolean isItemPickPointEmpty =
          CollectionUtils.isEmpty(itemPickupPoints) && CollectionUtils.isEmpty(itemPickupPointDataChangeEventModelList);
      if (publishItemPickupPointChangeEventOnlyOnce) {
        if (isItemPickPointEmpty) {
          itemPickupPoints = itemPickupPointService.findByStoreIdAndItemSku(item.getStoreId(), item.getItemSku());
          addItemPickupPointDataChangeType(itemPickupPoints, source);
          publishItemPickupPointChangeEvent(itemPickupPoints,
            itemPickupPointDataChangeEventModelList, productRejected, eventToBlackListedSellersMap);
          itemPickupPoints = new ArrayList<>();
        }
      } else {
        if (CollectionUtils.isEmpty(itemPickupPoints)) {
          itemPickupPoints = itemPickupPointService.findByStoreIdAndItemSku(item.getStoreId(), item.getItemSku());
        }
        addItemPickupPointDataChangeType(itemPickupPoints, source);
        publishItemPickupPointChangeEvent(itemPickupPoints,
          itemPickupPointDataChangeEventModelList, productRejected, eventToBlackListedSellersMap);
      }
    }

    if (publishItemPickupPointChangeEventOnlyOnce && (CollectionUtils.isNotEmpty(itemPickupPoints)
        || CollectionUtils.isNotEmpty(itemPickupPointDataChangeEventModelList))) {
      addItemPickupPointDataChangeType(itemPickupPoints, source);
      publishItemPickupPointChangeEvent(itemPickupPoints, itemPickupPointDataChangeEventModelList
        , productRejected, eventToBlackListedSellersMap);
    }
  }

  private void publishItemDataChangeIfAllowed(Item item, String source, boolean productRejected,
                                              Map<String, Set<String>> eventToBlackListedSellersMap) {
    if (!ValidationUtil.checkIfBlacklistedSellerForSpecificEvent(eventToBlackListedSellersMap,
      item.getMerchantCode(),
      ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME)) {
      getSaveAndPublishServiceBean().publishItemDataChangeEvent(Collections.singletonList(item),
              source, productRejected);
    }
  }

  private void publishItemPickupPointChangeEvent(List<ItemPickupPoint> itemPickupPoints,
                                                 List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList,
                                                 boolean productRejected,
                                                 Map<String, Set<String>> eventToBlackListedSellersMap) {
    if (CollectionUtils.isNotEmpty(itemPickupPointDataChangeEventModelList)) {
      CommonUtil.setRejectedChangeTypeForL5DataChangeEventModelList(
          itemPickupPointDataChangeEventModelList, productRejected);
      itemPickupPointDataChangeEventModelList.forEach(
          itemPickupPointDataChangeEventModel -> itemPickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(
              itemPickupPointDataChangeEventModel, eventToBlackListedSellersMap));
    } else if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypeList = new ArrayList<>();
        getSaveAndPublishServiceBean().publishItemPickupPointDataChangeEvent(
                itemPickupPoints, itemPickupPointChangeEventTypeList, eventToBlackListedSellersMap);
    }
  }

  private ItemChange setL5DetailsForPublishing(Item item) {
    ItemChange itemChange = new ItemChange();
    ItemPickupPoint itemPickupPoint =
        itemPickupPointService.findByStoreIdAndItemSkuAndDeliveryTrueAndMarkForDeleteFalse(item.getStoreId(), item.getItemSku());
    populateItemChange(item, itemChange, itemPickupPoint);
    return itemChange;
  }

  private void addItemPickupPointDataChangeType(List<ItemPickupPoint> itemPickupPoints, String source) {
    if (StringUtils.isNotEmpty(source) && CollectionUtils.isNotEmpty(itemPickupPoints)) {
      itemPickupPoints.forEach(itemPickupPoint -> {
        List<String> dataChangeType = Optional.ofNullable(itemPickupPoint.getItemPickupPointDataChangeType())
            .orElse(new ArrayList<>());
        dataChangeType.add(source);
        itemPickupPoint.setItemPickupPointDataChangeType(dataChangeType);
      });
    }
  }

  private void populateItemChange(Item item, ItemChange itemChange, ItemPickupPoint itemPickupPoint) {
    BeanUtils.copyProperties(item, itemChange, "masterDataItem", "price", "itemViewConfigs", "pristineDataItem",
        "itemChangeEventTypes");
    List<com.gdn.x.product.domain.event.enums.ItemChangeEventType> itemChangeEventTypes =
        Optional.ofNullable(item.getItemChangeEventTypes()).orElse(new ArrayList<>()).stream().map(
            itemChangeEventType -> com.gdn.x.product.domain.event.enums.ItemChangeEventType
                .valueOf(itemChangeEventType.name())).collect(Collectors.toList());
    if (Objects.nonNull(itemPickupPoint)) {
      itemChange.setPickupPointCode(itemPickupPoint.getPickupPointCode());
      Set<Price> prices =
        itemPickupPoint.getPrice().stream().map(price -> gdnMapper.deepCopy(price, Price.class))
          .collect(Collectors.toSet());
      Set<ItemViewConfig> itemViewConfigs = itemPickupPoint.getItemViewConfig().stream()
          .map(itemViewConfig -> gdnMapper.deepCopy(itemViewConfig, ItemViewConfig.class)).collect(Collectors.toSet());
      itemChange.setPrice(prices);
      itemChange.setItemViewConfigs(itemViewConfigs);
    }
    itemChange.setMasterDataItem(gdnMapper.deepCopy(item.getMasterDataItem(), MasterDataItem.class));
    itemChange.setPristineDataItem(gdnMapper.deepCopy(item.getPristineDataItem(), PristineDataItemEventModel.class));
    itemChange.setItemChangeEventTypes(itemChangeEventTypes);
  }

  @Override
  public void publishListOfItemsWithItemChangeType(List<Item> items,
      List<ItemChangeEventType> itemChangeEventTypeList) {
    for (Item item : items) {
      item.setUniqueId(item.getItemSku());
      item.setItemChangeEventTypes(itemChangeEventTypeList);
      LOG.info("Publishing event:{}, item :{}", ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME, item);
      getSaveAndPublishServiceBean().publishItemDataChangeEvent(Collections.singletonList(item));
    }
  }

  @Override
  public void publishProduct(Product product, List<String> productChangeEventTypes) {
    publishProductChange(product, productChangeEventTypes, StringUtils.EMPTY);
  }

  @Override
  public List<Item> saveItems(List<Item> listOfItems) {
    List<Item> savedItems = this.itemRepository.saveAll(listOfItems);
    this.publishListOfItems(savedItems);
    return savedItems;
  }

  @Override
  public void publishPristineItem(String storeId, Item item) {
    List<ItemChangeEventType> pristineItemChangeEventTypes = new ArrayList<>();
    pristineItemChangeEventTypes.add(ItemChangeEventType.PRISTINE_MAPPING_CHANGE);
    item.setItemChangeEventTypes(pristineItemChangeEventTypes);
    ItemChange itemChange = setL5DetailsForPublishing(item);
    if (publishPristineMappingChangeEvent) {
      LOG.info("Publishing pristine mapping change event:{}, item :{}",
          ProductDomainEventName.PRISTINE_ITEM_CHANGE_EVENT_NAME, item);
      kafkaPublisher.send(ProductDomainEventName.PRISTINE_ITEM_CHANGE_EVENT_NAME, itemChange.getItemSku(), itemChange);
    } else {
      item.setUniqueId(item.getItemSku());
      List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
      List<com.gdn.x.product.domain.event.enums.ItemChangeEventType> changeEventTypes = new ArrayList<>();
      itemChangeEventTypes.add(ItemChangeEventType.PRISTINE_MAPPING_CHANGE);
      changeEventTypes.add(com.gdn.x.product.domain.event.enums.ItemChangeEventType.PRISTINE_MAPPING_CHANGE);
      item.setItemChangeEventTypes(itemChangeEventTypes);
      itemChange.setItemChangeEventTypes(changeEventTypes);
      LOG.info("Publishing Pristine event:{}, item :{}", ProductDomainEventName.ITEM_CHANGE_EVENT_NAME, item);
      getSaveAndPublishServiceBean().publishItemDataChangeEvent(Arrays.asList(item));
    }
  }


  @Override
  public Product saveProduct(Product product, List<String> productChangeEventTypes,
    String productPublishSourceDeletePickupPoint,
    Map<String, Set<String>> eventToBlackListedSellersMap) {
    LOG.info("Publishing event:{}, product :{}", ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME,
      product);
    product = this.productRepository.save(product);
    if (!ValidationUtil.checkIfBlacklistedSellerForSpecificEvent(eventToBlackListedSellersMap,
      product.getMerchantCode(),
      ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME)) {
      this.publishProductChange(product, productChangeEventTypes,
        productPublishSourceDeletePickupPoint);
    }
    return product;
  }

  @Override
  public Product saveProductAndSKipPublishForMfdTrueProducts(Product product) {
    LOG.info("Publishing event:{}, product :{}", ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME, product);
    product = this.productRepository.save(product);
    if (!(skipEventPublishForTakeDownProduct && product.isMarkForDelete())) {
      this.publishProductChange(product, Collections.EMPTY_LIST, StringUtils.EMPTY);
    }
    return product;
  }

  @Override
  public List<Product> saveProducts(List<Product> productList) {
    LOG.info("Publishing event:{}, product :{}", ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME,
      productList);
    List<Product> updatedProductList = this.productRepository.saveAll(productList);
    updatedProductList.forEach(product -> publishProductChange(product, Collections.EMPTY_LIST, StringUtils.EMPTY));
    return updatedProductList;
  }

  @Override
  public Item updateItemFieldByItemSku(String storeId, String itemSku, String fieldName,
      Object fieldValue) {
    Item item = this.itemRepository.updateFieldByItemSku(storeId, itemSku, fieldName, fieldValue);
    item.setUniqueId(item.getItemSku());
    ItemChange itemChange = setL5DetailsForPublishing(item);
    LOG.info("Publishing event:{}, item :{}", ProductDomainEventName.ITEM_CHANGE_EVENT_NAME, item);
    kafkaPublisher.send(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME,itemChange.getItemSku(),
      itemChange);
    return item;
  }

  @Override
  public List<Item> updateItemFieldByItemSkus(String storeId, Collection<String> itemSkus, String fieldName,
      Object fieldValue) {
    List<Item> savedItems = this.itemRepository.updateFieldByItemSkus(storeId, itemSkus, fieldName, fieldValue);
    this.publishListOfItems(savedItems);
    return savedItems;
  }

  @Override
  public Item updateItemOff2OnChannelActiveByItemSku(String storeId, String itemSku,
      boolean active) {
    Item result = this.itemRepository.updateOff2OnChannelActiveByItemSku(storeId, itemSku, active);
    if (result != null) {
      result.setItemChangeEventTypes(
          Collections.singletonList(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE));
      result.setUniqueId(result.getItemSku());
     publishItemDataChangeEvent(Collections.singletonList(result));
    }
    return result;
  }

  @Override
  public List<Item> updateItemOff2OnChannelActiveByProductSku(String storeId, String productSku,
      boolean active) {
    List<Item> result =
        this.itemRepository.updateOff2OnChannelActiveByProductSku(storeId, productSku, active);
    result.stream().filter(Objects::nonNull).forEach(item->item.setUniqueId(item.getItemSku()));
    publishItemDataChangeEvent(result);
    return result;
  }

  @Override
  public List<Item> updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(String storeId, String productSku,
      boolean active, String userName) {
    return this.itemRepository
        .updateOff2OnChannelActiveByProductSkuAndUpdatedDateAndBy(storeId, productSku, active, userName);
  }

  @Override
  public Product updateOff2OnItemCountIncrement(String storeId, String productSku, boolean activate,
      int increment) {
    if (activate) {
      increment *= 1;
    } else {
      increment *= -1;
    }
    Product updatedProduct = this.productRepository
        .updateOff2OnItemCountIncrementByProductSku(storeId, productSku, increment);
    LOG.info("Publishing event:{}, product :{}", ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME,
        updatedProduct);
    this.publishProductChange(updatedProduct, Collections.EMPTY_LIST, StringUtils.EMPTY);
    return updatedProduct;

  }

  @Override
  public Product updateOff2OnItemCountIncrementAndFlag(String storeId, String productSku, boolean activate,
      int itemsCount, String userName) {
    if (!activate) {
      itemsCount = 0;
    }
    return this.productRepository
        .updateOff2OnItemCountIncrementByProductSkuAndFlag(storeId, productSku, activate, itemsCount, userName);
  }

  private String getProductSkuFromItemSku(String itemSku) {
    return itemSku.substring(0, itemSku.lastIndexOf(SKU_DELIMITER));
  }

  private OfflineItemChange convertToOfflineItemChange(OfflineItem offlineItem) {
    OfflineItemChange offlineItemChange = new OfflineItemChange();
    offlineItemChange.setExternalPickupPointCode(offlineItem.getExternalPickupPointCode());
    offlineItemChange.setItemSku(offlineItem.getItemSku());
    offlineItemChange.setProductSku(getProductSkuFromItemSku(offlineItem.getItemSku()));
    offlineItemChange.setMarkForDelete(offlineItem.isMarkForDelete());
    offlineItemChange.setMerchantCode(offlineItem.getMerchantCode());
    offlineItemChange.setMerchantSku(offlineItem.getMerchantSku());
    offlineItemChange.setListPrice(
        Optional.ofNullable(offlineItem.getListPrice()).orElse(offlineItem.getOfferPrice()));
    offlineItemChange.setOfferPrice(offlineItem.getOfferPrice());
    offlineItemChange.setPickupPointCode(offlineItem.getPickupPointCode());
    offlineItemChange.setStoreId(offlineItem.getStoreId());
    offlineItemChange.setUniqueId(offlineItem.getOfflineItemId());
    offlineItemChange.setNewData(Optional.ofNullable(offlineItem.getNewData()).orElse(false));
    offlineItemChange.setUpdatedDate(new Date());
    offlineItemChange.setUsername(offlineItem.getUpdatedBy());
    if (Objects.nonNull(offlineItem.getOfflineItemHistoryDetail())) {
      offlineItemChange.setOldListPrice(
          Optional.ofNullable(offlineItem.getOfflineItemHistoryDetail().getOldListPrice())
              .orElse(offlineItem.getOfflineItemHistoryDetail().getOldOfferPrice()));
      offlineItemChange.setOldOfferPrice(offlineItem.getOfflineItemHistoryDetail().getOldOfferPrice());
      offlineItemChange.setSyncPriceAction(offlineItem.getOfflineItemHistoryDetail().getSyncPriceAction());
      offlineItemChange.setRequestId(offlineItem.getOfflineItemHistoryDetail().getRequestId());
      offlineItemChange.setClientId(offlineItem.getOfflineItemHistoryDetail().getClientId());
    }

    return offlineItemChange;
  }

  private OfflineItemPopulateEvent convertToOfflineItemEvent(OfflineItem offlineItem, Item item) {
    OfflineItemPopulateEvent offlineItemPopulateEvent = new OfflineItemPopulateEvent();
    BeanUtils.copyProperties(offlineItem, offlineItemPopulateEvent);
    offlineItemPopulateEvent.setProductSku(getProductSkuFromItemSku(offlineItem.getItemSku()));
    offlineItemPopulateEvent.setMarkForDelete(offlineItem.isMarkForDelete());
    offlineItemPopulateEvent.setListPrice(
        Optional.ofNullable(offlineItem.getListPrice()).orElse(offlineItem.getOfferPrice()));
    offlineItemPopulateEvent.setUniqueId(offlineItem.getOfflineItemId());
    offlineItemPopulateEvent.setNewData(Optional.ofNullable(offlineItem.getNewData()).orElse(false));
    offlineItemPopulateEvent.setUpdatedDate(new Date());
    offlineItemPopulateEvent.setUsername(offlineItem.getUpdatedBy());
    if (Objects.nonNull(offlineItem.getOfflineItemHistoryDetail())) {
      offlineItemPopulateEvent.setOldListPrice(
          Optional.ofNullable(offlineItem.getOfflineItemHistoryDetail().getOldListPrice())
              .orElse(offlineItem.getOfflineItemHistoryDetail().getOldOfferPrice()));
      offlineItemPopulateEvent
          .setOldOfferPrice(offlineItem.getOfflineItemHistoryDetail().getOldOfferPrice());
      offlineItemPopulateEvent
          .setSyncPriceAction(offlineItem.getOfflineItemHistoryDetail().getSyncPriceAction());
      offlineItemPopulateEvent.setRequestId(offlineItem.getOfflineItemHistoryDetail().getRequestId());
      offlineItemPopulateEvent.setClientId(offlineItem.getOfflineItemHistoryDetail().getClientId());
    }
    if (Objects.nonNull(item)) {
      if (Objects.nonNull(item.getPristineDataItem())){
        offlineItemPopulateEvent.setPristineId(item.getPristineDataItem().getPristineId());
      }
      offlineItemPopulateEvent.setItemCode(item.getItemCode());
    }

    return offlineItemPopulateEvent;
  }

  @Override
  public void publishListOfOfflineItems(List<OfflineItem> offlineItems, MandatoryRequestParam mandatoryRequestParam) {
    if (CollectionUtils.isNotEmpty(offlineItems)) {
      Map<String, String> itemCodeByItemSku = getItemCodeByItemSku(offlineItems);
      for (OfflineItem offlineItem : offlineItems) {
        try {
          OfflineItemChange offlineItemChange = convertToOfflineItemChange(offlineItem);
          offlineItemChange.setItemCode(itemCodeByItemSku.get(offlineItemChange.getItemSku()));

          LOG.info("Publishing event:{}, Publish offline item change : {}",
              ProductDomainEventName.OFFLINE_ITEM_CHANGE_EVENT_NAME, offlineItemChange);
          this.kafkaPublisher.send(ProductDomainEventName.OFFLINE_ITEM_CHANGE_EVENT_NAME,
            offlineItemChange.getUniqueId(), offlineItemChange);
          if (Objects.nonNull(mandatoryRequestParam) && mandatoryRequestParam.getClientId()
              .equals(CLIENT_ID_UPSERT_OFFLINE_ITEM_PRICE)) {
            LOG.info("Publishing event:{}, Publish offline item change regular seller : {}",
                ProductDomainEventName.OFFLINE_ITEM_CHANGE_REGULAR_SELLER_EVENT_NAME,
                offlineItemChange);
            this.kafkaPublisher.send(
              ProductDomainEventName.OFFLINE_ITEM_CHANGE_REGULAR_SELLER_EVENT_NAME,
              offlineItemChange.getUniqueId(), offlineItemChange);
          } else {
            LOG.info("Publishing event:{}, Publish offline item change big seller : {}",
                ProductDomainEventName.OFFLINE_ITEM_CHANGE_BIG_SELLER_EVENT_NAME,
                offlineItemChange);
            this.kafkaPublisher.send(
              ProductDomainEventName.OFFLINE_ITEM_CHANGE_BIG_SELLER_EVENT_NAME,
              offlineItemChange.getUniqueId(), offlineItemChange);
          }

        } catch (Exception e) {
          LOG.error("failed to publishListOfOfflineItems with offlineItem: {}", offlineItem, e);
        }
      }
    }
  }

  @Override
  public void publishListOfItemsPickupPoints(List<ItemPickupPoint> pickupPointList,
      MandatoryRequestParam mandatoryRequestParam) {
    if (CollectionUtils.isNotEmpty(pickupPointList)) {
      publishItemPickupPointDataChangeEvent(pickupPointList, new ArrayList<>(),
        Collections.EMPTY_MAP);
    }
  }

  private Map<String, Item> getItemsByItemSku(List<OfflineItem> offlineItems) {

    String storeId = offlineItems.stream().map(OfflineItem::getStoreId).findFirst().orElse(null);
    Set<String> itemSkus = offlineItems.stream().map(OfflineItem::getItemSku).collect(Collectors.toSet());
    return itemService.getItemsByItemSkuIn(storeId, itemSkus);
  }

  private Map<String, String> getItemCodeByItemSku(List<OfflineItem> offlineItems) {

    String storeId = offlineItems.stream().map(OfflineItem::getStoreId).findFirst().orElse(null);
    Set<String> itemSkus = offlineItems.stream().map(OfflineItem::getItemSku).collect(Collectors.toSet());
    return itemService.getItemCodesByItemSkuIn(storeId, itemSkus);
  }

  @Override
  public void publishListOfOfflineItemsWithoutFailsafe(List<OfflineItem> offlineItems) throws Exception {

    Map<String, Item> itemsByItemSku = getItemsByItemSku(offlineItems);
    for (OfflineItem offlineItem : offlineItems) {
      try {
        Item item = itemsByItemSku.get(offlineItem.getItemSku());
        OfflineItemPopulateEvent
            offlineItemPopulateEvent = convertToOfflineItemEvent(offlineItem, item);
        LOG.info("Publishing event:{}, Publish offline item populate event : {}",
            ProductDomainEventName.POPULATE_OFFLINE_ITEM_EVENT_NAME, offlineItemPopulateEvent);
        this.kafkaPublisher.send(ProductDomainEventName.POPULATE_OFFLINE_ITEM_EVENT_NAME,
          offlineItemPopulateEvent.getUniqueId(), offlineItemPopulateEvent);
      } catch (Exception e) {
        LOG.error("failed to publishListOfOfflineItemsWithoutFailsafe with offlineItem: {}", offlineItem, e);
        throw new Exception(e.getMessage());
      }
    }
  }

  @Override
  public void publishMerchantPromoDiscountEventChange(Item item) {
    MerchantPromoDiscountChangeEvent merchantPromoDiscountChangeEvent = null;
    try {
      merchantPromoDiscountChangeEvent = CommonUtil.toMerchantPromoDiscountChangeEvent(item);
      LOG.info("Publishing event:{}, merchantPromoDiscountChangeEvent :{}",
          ProductDomainEventName.MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE, merchantPromoDiscountChangeEvent);
      this.kafkaPublisher.send(ProductDomainEventName.MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE,
        merchantPromoDiscountChangeEvent.getMerchantCode(), merchantPromoDiscountChangeEvent);
    } catch (Exception e) {
      LOG.error("failed to publish Promo discount change event. Price: {}, itemSku:{}", item.getPrice(), item.getItemSku(), e);
    }
  }

  @Override
  public void publishItemSkuListForVoucher(VoucherItemSkusEventModel voucherItemSkusEventModel) {
    try {
      LOG.info("Publishing event:{}, voucherItemSkusEventModel :{}",
          DomainEventName.PRODUCT_DETAIL_EVENT, voucherItemSkusEventModel);
      this.kafkaPublisher.send(DomainEventName.PRODUCT_DETAIL_EVENT,
        voucherItemSkusEventModel.getVoucherCode(), voucherItemSkusEventModel);
    } catch (Exception ex) {
      LOG.error("fail to publish event to merchant voucher. Merchant code : {}",
          voucherItemSkusEventModel.getMerchantCode(), ex);
    }
  }

  @Override
  public void publishMerchantVoucherViewConfigChange(ItemVo itemVo) {
    try {
      LOG.info("Publishing event:{}, view config change event to merchant voucher. ItemPickUpPoint :{}",
          ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE, itemVo);
      for (ItemPickupPointVo itemPickupPointVo : itemVo.getItemPickupPointVoList()) {
        ItemViewConfigWithArchivedChangeEvent itemViewConfigChange =
            CommonUtil.toItemViewConfigWithArchivedChangeEvent(itemVo, itemPickupPointVo,
                cncForWarehouseFeatureSwitch);
        this.kafkaPublisher.send(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE,
          itemViewConfigChange.getItemSku(), itemViewConfigChange);
      }
    } catch (Exception ex) {
      LOG.error("fail to publish view config change event to merchant voucher. Item sku : {}", itemVo.getItemSku(), ex);
    }
  }

  @Override
  public void publishMerchantVoucherViewConfigChange(List<ItemPickupPoint> itemPickupPoints, List<Item> items) {
    LOG.info("Publishing event:{}, view config change event to merchant voucher. ItemPickUpPoint :{}",
        ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE, itemPickupPoints);
    Map<String, Item> itemMap = Optional.ofNullable(items).orElse(new ArrayList<>()).stream()
        .collect(Collectors.toMap(Item::getItemSku, Function.identity(), (v1, v2) -> v2));
    for (ItemPickupPoint itemPickupPoint : Optional.ofNullable(itemPickupPoints).orElse(new ArrayList<>())) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      if (Objects.nonNull(item)) {
        ItemViewConfigWithArchivedChangeEvent itemViewConfigChangeEvent =
            CommonUtil.toItemViewConfigWithArchivedChangeEvent(item, itemPickupPoint,
                cncForWarehouseFeatureSwitch);
        this.kafkaPublisher.send(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE,
            itemViewConfigChangeEvent.getItemSku(), itemViewConfigChangeEvent);
      }
    }
  }

  @Override
  public void publishStreamOfProducts(Stream<Product> productStream){
    Stream<ProductModel> productModelStream =
        productStream.map(product -> gdnMapper.deepCopy(product, ProductModel.class));
    productModelStream.forEach(product ->
      asyncProcessor.submitWithBackoff(COMMAND_DESC_PRODUCT, () -> {
        this.kafkaPublisher.send(ProductDomainEventName.PRODUCT_ALL_EVENT_NAME,
          product.getProductSku(), product);
        LOG.info("Success to publish product with productSku '{}' to topic '{}'",
            product.getProductSku(),ProductDomainEventName.PRODUCT_ALL_EVENT_NAME);
      }
    ));
  }

  @Override
  public void publishStreamOfItems(Stream<Item> itemStream){
    itemStream.forEach(item ->
      asyncProcessor.submitWithBackoff(COMMAND_DESC_ITEM, () -> {
        this.kafkaPublisher.send(ProductDomainEventName.ITEM_ALL_EVENT_NAME, item.getItemSku(),
          item);
        LOG.info("Success to publish item with itemSku '{}' to topic '{}'",
            item.getItemSku(),ProductDomainEventName.ITEM_ALL_EVENT_NAME);
      }
    ));
  }

  @Override
  public void publishWholesalePriceActivatedOrDeactivatedEvent(String itemSku, boolean wholesalePriceActivated,
      String merchantCode) {
    try {
      this.kafkaPublisher.send(
        ProductDomainEventName.WHOLESALE_PRICE_ACTIVATED_DEACTIVATED_BY_SCHEDULER, itemSku,
        new WholesalePriceActivatedOrDeactivatedEvent(itemSku, wholesalePriceActivated,
          merchantCode));
      LOG.info("Success to publish wholesale price activated or deactivated event with itemSku '{}' to topic '{}' and "
              + "activated :{} ", itemSku, ProductDomainEventName.WHOLESALE_PRICE_ACTIVATED_DEACTIVATED_BY_SCHEDULER,
          wholesalePriceActivated);
    } catch (Exception e) {
      LOG.error("fail to publish wholesale price activated or deactivated event with itemSku : {}",
          itemSku, e);
    }
  }

  @Override
  public void publishProductL3SolrReindexEvent(List<String> productSkuList, String status) {
    try {
      this.kafkaPublisher.send(ProductDomainEventName.PRODUCT_L3_SOLR_REINDEX_EVENT_NAME,
        new ProductL3SolrReindexEvent(productSkuList, status));
      LOG.info("Published Product L3 solr event to populate new fields for : {} ", productSkuList);
    } catch (Exception e) {
      LOG.error("Failed to publish event for product L3 solr reindex with input : {}",
          productSkuList, e);
    }
  }

  @Override
  public void publishItemsForMigration(List<String> itemSkuList) {
    log.info("Publishing L5 migration event for items : {}", itemSkuList);
    this.kafkaPublisher.send(ProductDomainEventName.ITEM_MIGRATION_EVENT,
      ItemPickupPointMigrationEvent.builder().itemSkuList(itemSkuList).build());
  }

  @Override
  public void publishItemCreationForMigration(List<String> itemSkuList) {
    log.info("Publishing Migration on creation event for items : {}", itemSkuList);
    this.kafkaPublisher.send(ProductDomainEventName.MIGRATION_FOR_CREATION_EVENT,
      MigrationForItemCreationEvent.builder().itemSkuList(itemSkuList).build());
  }

  @Override
  public void publishViewConfigChange(ItemPickupPoint itemPickupPoint) {
    try {
      log.info("Publishing event:{}, view config change event to merchant voucher. Item "
          + "pickupPoint :{} ", ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE,
        itemPickupPoint);
      this.kafkaPublisher.send(ProductDomainEventName.PRODUCT_ITEM_VIEW_CONFIG_CHANGE,
        itemPickupPoint.getItemSku(),
        CommonUtil.toItemPickupPointViewConfigChangeEvent(itemPickupPoint, cncForWarehouseFeatureSwitch));
    } catch (Exception ex) {
      log.info("failed to publish view config change event to merchant voucher. Item sku : {} ",
        itemPickupPoint.getItemSku(), ex);
    }
  }

  @Override
  public ItemPickupPointDataChangeEventModel publishItemPickupPointDataChangeEventForAGP(ItemPickupPoint itemPickupPoint) {
    ItemPickupPointDataChangeEventModel itemPickupPointDataChange = new ItemPickupPointDataChangeEventModel();
    BeanUtils.copyProperties(itemPickupPoint, itemPickupPointDataChange, "price",
      "itemViewConfigs");
    Set<Price> prices =
      itemPickupPoint.getPrice().stream().map(price -> gdnMapper.deepCopy(price, Price.class))
        .collect(Collectors.toSet());
    Set<com.gdn.x.product.model.entity.ItemViewConfig> itemViewConfigSet =
        cncForWarehouseFeatureSwitch ?
            itemPickupPoint.getAllItemViewConfigs() :
            itemPickupPoint.getItemViewConfig();
    Set<ItemViewConfig> itemViewConfigs = itemViewConfigSet.stream()
      .map(itemViewConfig -> gdnMapper.deepCopy(itemViewConfig, ItemViewConfig.class))
      .collect(Collectors.toSet());
    itemPickupPointDataChange.setPrice(prices);
    itemPickupPointDataChange.setItemViewConfigs(itemViewConfigs);
    setV2ChangeTypeAndRemoveNewlyAddedChangeTypes(itemPickupPointDataChange);
    LOG.info("Publishing item pickup point data change event. itemPickupPointDataChange : {} ",
        itemPickupPointDataChange);
    kafkaPublisher.send(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME_AGP,
      itemPickupPoint.getItemSku(), itemPickupPointDataChange);
    return itemPickupPointDataChange;
  }

  @Override
  public void publishMerchantPromoDiscountEventChange(ItemPickupPoint itemPickupPoint) {
    MerchantPromoDiscountChangeEvent merchantPromoDiscountChangeEvent = null;
    try {
      merchantPromoDiscountChangeEvent =
        CommonUtil.toMerchantPromoDiscountChangeEventWithPickupPoint(itemPickupPoint);
      LOG.info("Publishing event:{}, merchantPromoDiscountChangeEvent :{}",
        ProductDomainEventName.MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE,
        merchantPromoDiscountChangeEvent);
      this.kafkaPublisher.send(ProductDomainEventName.MERCHANT_PROMO_DISCOUNT_PRICE_CHANGE,
        merchantPromoDiscountChangeEvent.getMerchantCode(), merchantPromoDiscountChangeEvent);
    } catch (Exception e) {
      LOG.error("failed to publish Promo discount change event. Price: {}, itemSku:{} "
          + "PickupPointCode : {}", itemPickupPoint.getPrice(), itemPickupPoint.getItemSku(),
        itemPickupPoint.getPickupPointCode(), e);
    }
  }

  @Override
  public ItemChange publishItemChangeEvent(ItemVo itemVo, ItemPickupPointVo itemPickupPointVo) {
    ItemChange itemChange = new ItemChange();
    BeanUtils.copyProperties(itemVo, itemChange, "masterDataItem", "price", "itemViewConfigs", "pristineDataItem",
        "itemChangeEventTypes");
    List<com.gdn.x.product.domain.event.enums.ItemChangeEventType> itemChangeEventTypes =
        Optional.ofNullable(itemVo.getItemChangeEventTypes()).orElse(new ArrayList<>()).stream().map(
            itemChangeEventType -> com.gdn.x.product.domain.event.enums.ItemChangeEventType.valueOf(
                itemChangeEventType.name())).collect(Collectors.toList());

    Set<Price> prices = itemPickupPointVo.getPrice().stream()
        .map(price -> gdnMapper.deepCopy(price, Price.class)).collect(Collectors.toSet());
    Set<ItemViewConfig> itemViewConfigs = itemPickupPointVo.getItemViewConfig().stream()
            .map(itemViewConfig -> gdnMapper.deepCopy(itemViewConfig, ItemViewConfig.class))
            .collect(Collectors.toSet());
    itemChange.setMasterDataItem(gdnMapper.deepCopy(itemVo.getMasterDataItem(), MasterDataItem.class));
    itemChange.setPristineDataItem(gdnMapper.deepCopy(itemVo.getPristineDataItem(), PristineDataItemEventModel.class));
    itemChange.setItemChangeEventTypes(itemChangeEventTypes);
    itemChange.setPrice(prices);
    itemChange.setItemViewConfigs(itemViewConfigs);
    itemChange.setUniqueId(itemVo.getItemSku());
    itemChange.setPickupPointCode(itemPickupPointVo.getPickupPointCode());
    LOG.info("Publishing item change event. eventPayload : {} ", itemChange);
    this.kafkaPublisher.send(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME,
      itemChange.getItemSku(), itemChange);
    return itemChange;
  }

  @Override
  public void publishItemChangeEvent(List<ItemPickupPoint> itemPickupPointList, Map<String, Item> itemMap) {
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Item item = itemMap.get(itemPickupPoint.getItemSku());
      if (Objects.nonNull(item)) {
        ItemChange itemChange = new ItemChange();
        BeanUtils.copyProperties(item, itemChange, "masterDataItem", "price", "itemViewConfigs",
          "pristineDataItem", "itemChangeEventTypes");
        List<com.gdn.x.product.domain.event.enums.ItemChangeEventType> itemChangeEventTypes =
          Optional.ofNullable(item.getItemChangeEventTypes()).orElse(new ArrayList<>()).stream().map(
            itemChangeEventType -> com.gdn.x.product.domain.event.enums.ItemChangeEventType.valueOf(
              itemChangeEventType.name())).collect(Collectors.toList());
        Set<Price> prices =
          itemPickupPoint.getPrice().stream().map(price -> gdnMapper.deepCopy(price, Price.class))
            .collect(Collectors.toSet());
        Set<ItemViewConfig> itemViewConfigs = itemPickupPoint.getItemViewConfig().stream().map(itemViewConfig -> gdnMapper.deepCopy(itemViewConfig, ItemViewConfig.class))
          .collect(Collectors.toSet());
        itemChange.setMasterDataItem(gdnMapper.deepCopy(item.getMasterDataItem(), MasterDataItem.class));
        itemChange.setPristineDataItem(gdnMapper.deepCopy(item.getPristineDataItem(), PristineDataItemEventModel.class));
        itemChange.setItemChangeEventTypes(itemChangeEventTypes);
        itemChange.setPrice(prices);
        itemChange.setItemViewConfigs(itemViewConfigs);
        itemChange.setUniqueId(item.getItemSku());
        itemChange.setPickupPointCode(itemPickupPoint.getPickupPointCode());
        LOG.info("Publishing item change event. eventPayload : {} ", itemChange);
        kafkaPublisher.send(ProductDomainEventName.ITEM_CHANGE_EVENT_NAME, itemChange.getItemSku(),
          itemChange);
      }
    }
  }

  @Override
  public void publishItemDataChangeEvent(List<Item> items) {
    publishItems(items, StringUtils.EMPTY, false);
  }

  private void publishItems(List<Item> items, String source, boolean productRejected) {
    for (Item item : items) {
      try {
        ItemDataChange itemDataChange = createBasicItemDataChangeEventMessage(item);
        itemDataChange.setSource(source);
        CommonUtil.setRejectedChangeTypeForItemDataChangeEvent(itemDataChange, productRejected);
        LOG.info("Publishing item data change event. itemDataChange : {} ", itemDataChange);
        this.kafkaPublisher.send(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME, itemDataChange.getItemSku(),
            itemDataChange);
      } catch (Exception e) {
        LOG.error("Failed to publish item data change event. Item : {} , Exception : ", item, e);
      }
    }
  }

  @Override
  public void publishItemDataChangeEvent(List<Item> items, String source, boolean productRejected) {
    publishItems(items, source, productRejected);
  }

  @Override
  public void publishItemDataChangeEventForHalalConfigChange(Item item, boolean halalProduct) {
    try {
      ItemDataChange itemDataChange = createBasicItemDataChangeEventMessage(item);
      itemDataChange.setHalalProduct(halalProduct);

      LOG.info("Publishing item data change event. itemDataChange : {} ", itemDataChange);
      this.kafkaPublisher.send(ProductDomainEventName.ITEM_DATA_CHANGE_EVENT_NAME, itemDataChange.getItemSku(),
          itemDataChange);
    } catch (Exception e) {
      LOG.error("Failed to publish item data change event. Item : {} , Exception : ", item, e);
    }
  }

  private ItemDataChange createBasicItemDataChangeEventMessage(Item item) {
    ItemDataChange itemDataChange = new ItemDataChange();
    BeanUtils.copyProperties(item, itemDataChange, "masterDataItem", "pristineDataItem", "itemChangeEventTypes");
    List<com.gdn.x.product.domain.event.enums.ItemChangeEventType> itemChangeEventTypes =
        Optional.ofNullable(item.getItemChangeEventTypes()).orElse(new ArrayList<>()).stream().map(
            itemChangeEventType -> com.gdn.x.product.domain.event.enums.ItemChangeEventType.valueOf(
                itemChangeEventType.name())).collect(Collectors.toList());
    itemDataChange.setMasterDataItem(gdnMapper.deepCopy(item.getMasterDataItem(), MasterDataItem.class));
    itemDataChange.setNewData(item.isNewData());
    itemDataChange.setPristineDataItem(
        gdnMapper.deepCopy(item.getPristineDataItem(), PristineDataItemEventModel.class));
    itemDataChange.setItemChangeEventTypesV2(itemChangeEventTypes.stream()
        .map(Objects::toString).collect(Collectors.toList()));
    itemDataChange.setItemChangeEventTypes(CommonUtil.removeNewlyAddedEventTypes(
        itemChangeEventTypes, newlyAddedItemDataChangeEventTypes));
    itemDataChange.setUpdatedFields(item.getUpdatedFields());
    itemDataChange.setUpcCode(item.getUpcCode());
    return itemDataChange;
  }

  @Override
  public void publishItemPickupPointDataChangeEvent(List<ItemPickupPoint> itemPickupPoints,
      List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes,
    Map<String, Set<String>> eventToBlackListedSellersMap) {
    Map<String, BusinessPartner> businessPartnerMap = new HashMap<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
      if (ValidationUtil.checkIfBlacklistedSellerForSpecificEvent(eventToBlackListedSellersMap,
        itemPickupPoint.getMerchantCode(),
        ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME)) {
        LOG.info("Skipping publish. Merchant {} is blacklisted for event {}",
          itemPickupPoint.getMerchantCode(),
          ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME);
        continue;
      }

      ItemPickupPointDataChangeEventModel itemPickupPointDataChange = new ItemPickupPointDataChangeEventModel();
      try {
        BeanUtils.copyProperties(itemPickupPoint, itemPickupPointDataChange, "price", "itemViewConfigs");
        Set<Price> prices = itemPickupPoint.getPrice().stream().map(price -> gdnMapper.deepCopy(price, Price.class))
            .collect(Collectors.toSet());
        Set<ItemViewConfig> itemViewConfigs = itemPickupPoint.getAllItemViewConfigs().stream()
            .map(itemViewConfig -> gdnMapper.deepCopy(itemViewConfig, ItemViewConfig.class))
            .collect(Collectors.toSet());
        BusinessPartner businessPartner =
            businessPartnerMap.getOrDefault(itemPickupPoint.getMerchantCode(), businessPartnerService.getBusinessPartnerByBusinessPartnerCode(itemPickupPoint.getStoreId(),
                itemPickupPoint.getMerchantCode()));
        itemPickupPointDataChange.setPrice(prices);
        itemPickupPointDataChange.setItemViewConfigs(itemViewConfigs);
        itemPickupPointDataChange.setPureCNCStatusChange(false);
        itemPickupPointDataChange.setSellerChannel(
            CommonUtil.generateSellerChannelFromBusinessPartner(businessPartner));
        itemPickupPointChangeEventTypes.addAll(itemPickupPoint.getItemPickupPointDataChangeType().stream()
            .map(ItemPickupPointChangeEventType::findEnumByValue).collect(Collectors.toSet()));
        itemPickupPointChangeEventTypes =
            itemPickupPointChangeEventTypes.stream().distinct().collect(Collectors.toList());
        itemPickupPointDataChange.setItemPickupPointChangeEventTypes(itemPickupPointChangeEventTypes);
        itemPickupPointDataChange.setStoreId(itemPickupPoint.getStoreId());
        itemPickupPointDataChange.setB2bFields(
            Objects.nonNull(itemPickupPoint.getB2bFields()) ? itemPickupPoint.getB2bFields() : new B2bFields());
        setV2ChangeTypeAndRemoveNewlyAddedChangeTypes(itemPickupPointDataChange);
        CommonUtil.setRejectedChangeTypeForL5DataChangeEventModelList(
          Collections.singletonList(itemPickupPointDataChange), itemPickupPoint.isMarkForDelete());
        LOG.info("Publishing item pickup point data change event. itemPickupPointDataChange : {} ",
            itemPickupPointDataChange);
        kafkaPublisher.send(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME,
          itemPickupPointDataChange.getOfflineItemId(), itemPickupPointDataChange);
        businessPartnerMap.put(itemPickupPoint.getMerchantCode(), businessPartner);
      } catch (Exception e) {
        LOG.error(
            "Failed to publish item pickup point data change event. itemPickupPointDataChange : {} , Exception : ",
            itemPickupPointDataChange, e);
      }
    }
  }

  private void setV2ChangeTypeAndRemoveNewlyAddedChangeTypes(ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel) {
    if (newL5DataChangeTypeEnabled) {
      Optional.ofNullable(itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypes())
        .ifPresent(types -> {
          List<String> eventTypesV2 = types.stream().map(Object::toString).collect(Collectors.toList());
          itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypesV2(eventTypesV2);
          List<ItemPickupPointChangeEventType> updatedEventTypes =
            CommonUtil.removeNewlyAddedEventTypesForL5DataChange(types, newlyAddedItemPickupPointDataChangeEventTypes);
          itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(updatedEventTypes);
        });
    }
  }

  @Override
  public void publishOfflineItemChangeSellerEvent(OfflineItemChange offlineItemChange) {
    LOG.info("Publishing event:{}, Publish offline item change regular seller : {}",
        ProductDomainEventName.OFFLINE_ITEM_CHANGE_REGULAR_SELLER_EVENT_NAME, offlineItemChange);
    this.kafkaPublisher.send(ProductDomainEventName.OFFLINE_ITEM_CHANGE_REGULAR_SELLER_EVENT_NAME,
      offlineItemChange.getUniqueId(), offlineItemChange);
  }

  @Override
  public void publishListOfOfflineItems(List<ItemPickupPoint> itemPickupPoints, Map<String, Item> itemMap,
      String clientId, List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList) {
    itemPickupPointDataChangeEventModelList.forEach(
        itemPickupPointDataChangeEventModel -> itemPickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(
            itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP));
  }

  @Override
  public void publishSolrUpdateEvent(List<ProductAndItemEventModel> productAndItemEventModels) {
    for (ProductAndItemEventModel productAndItemEventModel : productAndItemEventModels) {
      log.info("Publishing solr update event for productAndItemEventModel : {} ", productAndItemEventModel);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel.getProductSku(), productAndItemEventModel);
    }
  }

  @Override
  public void publishProductBundleOneToOneMappingEvent(Collection<Item> items) {
    for (Item item : Optional.ofNullable(items).orElse(new ArrayList<>())) {
      if (hasOneChildProduct(item)) {
        BundleProductOneToOneMappingEventModel bundleProductOneToOneMappingEventModel =
            new BundleProductOneToOneMappingEventModel(item.getItemSku(),
                item.getBundleRecipe().stream().findFirst().get().getItemSku());
        log.info("publishing event : {} , payload : {} ",
            ProductDomainEventName.PRODUCT_BUNDLE_ONE_TO_ONE_MAPPING_EVENT, bundleProductOneToOneMappingEventModel);
        kafkaPublisher.send(ProductDomainEventName.PRODUCT_BUNDLE_ONE_TO_ONE_MAPPING_EVENT, item.getItemSku(),
            bundleProductOneToOneMappingEventModel);
      }
    }
  }

  @Override
  public void publishHistoryEvent(List<AuditTrailDto> auditTrailDtoList) {
    AuditTrailListResponse auditTrailListResponse =
        new AuditTrailListResponse(auditTrailDtoList, GdnMandatoryRequestParameterUtil.getChannelId(),
            GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), true);
    log.info("Publishing the event : {} , payload : {} ", ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY,
        auditTrailListResponse);
    kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
  }

  private boolean hasOneChildProduct(Item item) {
    if (Objects.nonNull(item) && CollectionUtils.isNotEmpty(item.getBundleRecipe())
        && Constants.ONE == item.getBundleRecipe().size()) {
      return true;
    } else {
      return false;
    }
  }

  @Override
  public void publishOdooCreationEvent(OdooCreationEventModel odooCreationEventModel) {
    log.info("Publishing the odoo creation event : {} , payload : {} ", kafkaTopicProperties.getOdooCreationEvent(),
        odooCreationEventModel);
    kafkaPublisher.send(kafkaTopicProperties.getOdooCreationEvent(), odooCreationEventModel.getProductCode(),
        odooCreationEventModel);
  }

  @Override
  public void publishVideoCompressionEvent(VideoCompressionEventModel videoCompressionEventModel) {
    log.info("Publishing the video compression event : {} , payload : {} ",
      kafkaTopicProperties.getVideoCompressionEvent(), videoCompressionEventModel);
    kafkaPublisher.send(kafkaTopicProperties.getVideoCompressionEvent(),
      videoCompressionEventModel.getVideoId(), videoCompressionEventModel);
  }
}