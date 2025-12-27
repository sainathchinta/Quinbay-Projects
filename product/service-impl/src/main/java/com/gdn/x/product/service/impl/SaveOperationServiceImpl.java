package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.ItemPickupPointRepository;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.VideoCompressionEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.enums.ReindexType;
import com.gdn.x.product.model.entity.DeferredSolrReindexItem;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.Video;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.DeferredSolrReindexItemService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductL3SolrReindexStatusService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SolrIndexService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.CommonUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

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

@Service
public class SaveOperationServiceImpl implements SaveOperationService {

  private static final String PRODUCT_MUST_NOT_BE_NULL = "product must not be null";

  private static final String ITEM_MUST_NOT_BE_NULL = "item must not be null";

  private static final Logger LOG = LoggerFactory.getLogger(SaveOperationServiceImpl.class);

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private SolrIndexService solrIndexService;

  @Autowired
  private ItemRepository itemRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private DeferredSolrReindexItemService deferredSolrReindexItemService;

  @Autowired
  private MasterDataCacheService masterDataCacheService;

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductL3SolrReindexStatusService productL3SolrReindexStatusService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private ItemPickupPointRepository itemPickupPointRepository;

  @Autowired
  private GdnMapper gdnMapper;

  @Autowired
  private ProductCacheableService productCacheableService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ItemPriceService itemPriceService;

  @Value("${skip.event.publish.take.down.products}")
  private boolean skipEventPublishForTakeDownProduct;

  @Value("${populate.new.data.flag.item.change.event.creation}")
  private boolean populateNewDataFlagInItemChangeEventForCreation;

  @Value("${client-id.video.compression.event}")
  private String clientIdForVideoCompression;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  @Override
  public Item addActivePromoBundling(String storeId, String itemSku, String promoBundlingType) {
    Item item =
        this.saveAndPublishService.addActivePromoBundling(storeId, itemSku, promoBundlingType);

    this.cacheEvictHelperService.evictItemData(storeId, item);

    return item;
  }

  @Override
  public Item updateActivePromoBundling(String storeId, String itemSku, String promoBundlingType) {
    Item item = itemRepository.findItemByStoreIdAndItemSku(storeId, itemSku, false);
    if (CollectionUtils.isNotEmpty(item.getActivePromoBundlings())) {
      if (Constants.WHOLESALE_PRICE.equalsIgnoreCase(promoBundlingType)) {
        item.getActivePromoBundlings().remove(Constants.WHOLESALE);
      } else if (Constants.WHOLESALE.equalsIgnoreCase(promoBundlingType)) {
        item.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
      }
      item.getActivePromoBundlings().add(promoBundlingType);
    } else {
      item.setActivePromoBundlings(new HashSet<>());
      item.getActivePromoBundlings().add(promoBundlingType);
    }
    item.setUpdatedDate(new Date());
    item.setUpdatedBy(Constants.PROMO_SCHEDULER);
    Item saveItem = this.itemRepository.save(item);
    this.cacheEvictHelperService.evictItemData(storeId, saveItem);
    return saveItem;
  }

  @Override
  public Item insertItem(Item item) {
    checkArgument(item != null, SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    Item insertItem = this.saveAndPublishService.insertItem(item);
    this.cacheEvictHelperService.evictItemData(insertItem.getStoreId(), insertItem);
    ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
    productAndItemEventModel.setProductSku(item.getProductSku());
    productAndItemEventModel.setItems(Arrays.asList(objectConverterService.convertToItemEventModel(insertItem)));
    productAndItemEventModel.setMerchantCode(item.getMerchantCode());
    LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      productAndItemEventModel.getProductSku(), productAndItemEventModel);
    return insertItem;
  }

  @Override
  public List<Item> insertItems(List<Item> items) {
    checkArgument(items != null && !items.isEmpty(),
        SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    List<Item> insertItems = this.saveAndPublishService.insertItems(items);
    for (Item item : items) {
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
    }
    updateItemsToSolr(insertItems);
    return insertItems;
  }

  @Override
  public Item removeActivePromoBundling(String storeId, String itemSku,
      String promoBundlingType) {
    Item item =
        this.saveAndPublishService.removeActivePromoBundling(storeId, itemSku, promoBundlingType);

    this.cacheEvictHelperService.evictItemData(storeId, item);

    return item;
  }

  @Override
  public Item saveItem(Item item, List<ItemPickupPoint> itemPickupPointList,
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList) {
    checkArgument(item != null, SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    Item saveItem = this.itemRepository.save(item);
    this.cacheEvictHelperService.evictItemData(saveItem.getStoreId(), saveItem);
    this.cacheItemHelperService.setItemCacheByStoreIdAndItemSku(saveItem.getStoreId(), saveItem.getItemSku(), saveItem);
    saveAndPublishService.publishListOfItems(Collections.singletonList(saveItem), itemPickupPointList,
        itemPickupPointDataChangeEventModelList, false);
    ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
    productAndItemEventModel.setProductSku(item.getProductSku());
    productAndItemEventModel.setItems(Arrays.asList(objectConverterService.convertToItemEventModel(saveItem)));
    productAndItemEventModel.setMerchantCode(saveItem.getMerchantCode());
    LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      productAndItemEventModel.getProductSku(), productAndItemEventModel);
    return saveItem;
  }

  @Override
  public Item saveItemWithoutUpdatingSolr(Item item, List<ItemPickupPoint> savedItemPickupPoints,
    boolean productRejected, String source, Map<String, Set<String>> eventToBlackListedSellersMap) {
    checkArgument(item != null, SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    Item saveItem = this.itemRepository.save(item);
    this.cacheEvictHelperService.evictItemData(saveItem.getStoreId(), saveItem);
    saveAndPublishService.publishListOfItems(Collections.singletonList(saveItem),
      savedItemPickupPoints, new ArrayList<>(), source, productRejected,
      eventToBlackListedSellersMap);
    return saveItem;
  }

  @Override
  public List<Item> saveItems(List<Item> listOfItems, List<ItemPickupPoint> itemPickupPointList) {
    checkArgument(CollectionUtils.isNotEmpty(listOfItems), SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    List<Item> saveItems = itemRepository.saveAll(listOfItems);
    for (Item item : saveItems) {
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      saveAndPublishService.publishListOfItems(Collections.singletonList(item), itemPickupPointList, new ArrayList<>(),
        false);
    }
    updateItemsToSolr(saveItems);
    return saveItems;
  }

  private void updateItemsToSolr(List<Item> saveItems) {
    Map<String, List<Item>> productSkuMap = saveItems.stream().collect(Collectors.groupingBy(Item::getProductSku));
    for (String productSku : productSkuMap.keySet()) {
      List<Item> itemList = productSkuMap.get(productSku);
      ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
      productAndItemEventModel.setProductSku(productSku);
      productAndItemEventModel.setMerchantCode(productAndItemEventModel.getMerchantCode());
      productAndItemEventModel.setItems(objectConverterService.convertToListItemEventModel(itemList));
      LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
          productAndItemEventModel);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel.getProductSku(), productAndItemEventModel);
    }
  }

  @Override
  public List<Item> saveNewlyAddedItems(List<Item> items) {
    checkArgument(CollectionUtils.isNotEmpty(items), SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    List<Item> saveItems = itemRepository.saveAll(items);
    for (Item item : saveItems) {
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      saveAndPublishService.publishItemDataChangeEvent(Collections.singletonList(item));
    }
    updateItemsToSolr(saveItems);
    return saveItems;
  }

  @Override
  public List<Item> saveItemsWithoutUpdatingSolr(List<Item> listOfItems) {
    checkArgument(CollectionUtils.isNotEmpty(listOfItems), SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    List<Item> saveItems = itemRepository.saveAll(listOfItems);
    for (Item item : saveItems) {
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      saveAndPublishService.publishListOfItems(Collections.singletonList(item));
    }
    return saveItems;
  }

  @Override
  public List<Item> saveItemsAndClearCacheWithoutUpdatingSolr(List<Item> listOfItems,
      List<ItemPickupPoint> itemPickupPointList, String source) {
    checkArgument(CollectionUtils.isNotEmpty(listOfItems), SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    List<Item> saveItems = itemRepository.saveAll(listOfItems);
    for (Item item : saveItems) {
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      if (CollectionUtils.isNotEmpty(itemPickupPointList)) {
        itemPickupPointList = itemPickupPointList.stream()
            .filter(itemPickupPoint -> StringUtils.equals(itemPickupPoint.getItemSku(), (item.getItemSku())))
            .collect(Collectors.toList());
      }
      saveAndPublishService.publishListOfItems(Collections.singletonList(item), itemPickupPointList, new ArrayList<>(),
        source, false, Collections.EMPTY_MAP);
    }
    return saveItems;
  }

  @Override
  public List<Item> saveItemsAndClearCacheWithoutUpdatingSolrWithoutPublishing(List<Item> listOfItems) {
    checkArgument(CollectionUtils.isNotEmpty(listOfItems), SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    List<Item> saveItems = itemRepository.saveAll(listOfItems);
    for (Item item : saveItems) {
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
    }
    return saveItems;
  }

  @Override
  public List<Item> saveItemsWithDeferredReindexing(List<Item> items, List<ItemPickupPoint> itemPickupPointList) {
    checkArgument(CollectionUtils.isNotEmpty(items), SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    List<DeferredSolrReindexItem> deferredSolrReindexItems = new ArrayList<>();
    List<Item> savedItems = itemRepository.saveAll(items);
    for (Item item : savedItems) {
      cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      saveAndPublishService.publishListOfItems(Collections.singletonList(item), itemPickupPointList, new ArrayList<>(),
        false);
    }
    this.deferredReindexItems(items.get(0).getStoreId(), items, true, ReindexType.ITEM_REINDEX,
      false);
    return savedItems;
  }

  private DeferredSolrReindexItem toDeferredSolrReindexItem(Item item, String reindexType) {
    DeferredSolrReindexItem deferredSolrReindexItem = new DeferredSolrReindexItem();
    deferredSolrReindexItem.setItemSku(item.getItemSku());
    deferredSolrReindexItem.setReindexType(reindexType);
    deferredSolrReindexItem.setProductReindexStatus(ProductReindexStatus.REINDEX_PENDING);
    deferredSolrReindexItem.setCreatedBy(Constants.DEFAULT_UPDATED_BY);
    deferredSolrReindexItem.setCreatedDate(new Date());
    deferredSolrReindexItem.setUpdatedDate(new Date());
    deferredSolrReindexItem.setStoreId(item.getStoreId());
    return deferredSolrReindexItem;
  }

  @Override
  public Product saveProduct(Product product) {
    checkArgument(product != null, SaveOperationServiceImpl.PRODUCT_MUST_NOT_BE_NULL);
    Product saveProduct = this.saveAndPublishService.saveProduct(product, Collections.EMPTY_LIST, StringUtils.EMPTY, Collections.EMPTY_MAP);
    this.cacheEvictHelperService.evictProductData(saveProduct.getStoreId(), saveProduct);
    ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
    productAndItemEventModel.setProductSku(saveProduct.getProductSku());
    productAndItemEventModel.setProduct(objectConverterService.convertToProductEventModel(product));
    productAndItemEventModel.setMerchantCode(product.getMerchantCode());
    LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      productAndItemEventModel.getProductSku(), productAndItemEventModel);
    return saveProduct;
  }

  @Override
  public Product saveProductWithoutUpdatingSolr(Product product,
    List<String> productChangeEventTypes, String productPublishSourceDeletePickupPoint,
    Map<String, Set<String>> eventToBlackListedSellersMap) {
    checkArgument(product != null, SaveOperationServiceImpl.PRODUCT_MUST_NOT_BE_NULL);
    Product saveProduct =
        this.saveAndPublishService.saveProduct(product, productChangeEventTypes, productPublishSourceDeletePickupPoint, eventToBlackListedSellersMap);
    this.cacheEvictHelperService.evictProductData(saveProduct.getStoreId(), saveProduct);
    return saveProduct;
  }

  @Override
  public ProductAndItemsVO saveProductAndItems(ProductAndItemsVO productAndItems, List<String> productChangeTypes) {
    ProductAndItemsVO result = new ProductAndItemsVO();
    if (productAndItems.getProduct() != null) {
      result.setProduct(this.saveAndPublishService.saveProduct(productAndItems.getProduct(), productChangeTypes,
          StringUtils.EMPTY, Collections.EMPTY_MAP));
      this.cacheEvictHelperService.evictProductData(result.getProduct().getStoreId(),
          result.getProduct());
    }
    if (CollectionUtils.isNotEmpty(productAndItems.getItems())) {
      LOG.debug("updated items details: {}", productAndItems.getItems());
      result.setItems(itemRepository.saveAll(productAndItems.getItems()));
      for (Item item : result.getItems()) {
        this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
        saveAndPublishService.publishListOfItems(Collections.singletonList(item));
      }
    }
    ProductAndItemEventModel productAndItemEventModel = objectConverterService.convertToProductAndItemEventModel(result);
    LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      productAndItemEventModel.getProductSku(), productAndItemEventModel);
    return result;
  }

  @Override
  public ProductAndItemsVO saveProductAndItemsAndPickupPoint(Product product, List<ItemVo> itemVos) {
    ProductAndItemsVO result = new ProductAndItemsVO();
    Product entity = gdnMapper.deepCopy(product, Product.class);
    if (Objects.nonNull(entity)) {
      entity.setSalesCatalogs(product.getAllSalesCatalogs());
    }
    List<Item> items = itemVos.stream().map(itemVo -> gdnMapper.deepCopy(itemVo, Item.class)).filter(Objects::nonNull)
        .collect(Collectors.toList());
    List<ItemPickupPointVo> itemPickupPointVos = itemVos.stream()
        .flatMap(itemVo -> itemVo.getItemPickupPointVoList().stream()).collect(Collectors.toList());
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    for (ItemPickupPointVo itemPickupPointVo : itemPickupPointVos) {
      ItemPickupPoint itemPickupPoint = gdnMapper.deepCopy(itemPickupPointVo, ItemPickupPoint.class);
      Set<ItemViewConfig> itemViewConfigs = itemPickupPointVo.getAllItemViewConfigs().stream()
          .map(itemViewConfig -> gdnMapper.deepCopy(itemViewConfig, ItemViewConfig.class)).collect(Collectors.toSet());
      itemPickupPoint.setItemViewConfig(itemViewConfigs);
      itemPickupPoints.add(itemPickupPoint);
    }
    setItemPickupPointMerchantCode(product, itemPickupPoints);
    Optional.ofNullable(entity)
      .ifPresent(savedProduct -> setPickupPointCodesInProduct(savedProduct, itemPickupPoints));
    setPickupPointCode(itemPickupPoints,itemVos);
    if (Objects.nonNull(entity)) {
      result.setProduct(this.saveAndPublishService.saveProductAndSKipPublishForMfdTrueProducts(entity));
      this.cacheEvictHelperService.evictProductData(result.getProduct().getStoreId(), result.getProduct());
    }
    CommonUtil.computeDistributionFlag(itemVos, distributionSellerList, product.getMerchantCode(),
        product);
    if (CollectionUtils.isNotEmpty(items)) {
      LOG.debug("updated items details: {}", items);
      result.setItems(itemRepository.saveAll(items));
      if (populateNewDataFlagInItemChangeEventForCreation) {
        // we can keep new data as true always since this is creation
        items.forEach(item -> item.setNewData(true));
      }
      for (Item item : result.getItems()) {
        this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      }
      saveAndPublishService.publishProductBundleOneToOneMappingEvent(items);
    }
    List<ItemPickupPoint> savedItemPickupPoint;
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      savedItemPickupPoint = itemPickupPointRepository.saveAll(itemPickupPoints);
      savedItemPickupPoint.forEach(
          itemPickupPoint -> cacheEvictHelperService.evictItemPickupPointData(itemPickupPoint.getStoreId(),
              itemPickupPoint, itemPickupPoint.getPickupPointCode()));
    }
    publishProductAndItemAndItemPickupPointEventsMultiPickupPointEnabled(product, items, itemPickupPoints);
    publishAddToSolrEvent(result, itemVos);
    return result;
  }

  private void setPickupPointCode(List<ItemPickupPoint> itemPickupPointList, List<ItemVo> itemVos) {
    Map<String, String> itemSkuAndPickupPointMap =
      itemPickupPointList.stream().filter(ItemPickupPoint::isDelivery).collect(Collectors
        .toMap(ItemPickupPoint::getItemSku, ItemPickupPoint::getPickupPointCode, (a, b) -> a));
    for (ItemVo itemVo : itemVos) {
      itemVo.setPickupPointCode(
        itemSkuAndPickupPointMap.getOrDefault(itemVo.getItemSku(), StringUtils.EMPTY));
    }
  }

  private void publishAddToSolrEvent(ProductAndItemsVO result, List<ItemVo> itemVos) {
    ProductAndItemEventModel productAndItemEventModel =
        objectConverterService.convertToProductAndItemEventModel(result, itemVos);
    LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      Optional.ofNullable(productAndItemEventModel).map(ProductAndItemEventModel::getProductSku)
        .orElse(null), productAndItemEventModel);
  }

  private void publishProductAndItemAndItemPickupPointEventsMultiPickupPointEnabled(Product product, List<Item> items,
      List<ItemPickupPoint> itemPickupPoints) {
    Map<String, Item> itemMap = items.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
    if(!(skipEventPublishForTakeDownProduct && product.isMarkForDelete())) {
      saveAndPublishService.publishItemDataChangeEvent(items);
      saveAndPublishService.publishItemPickupPointDataChangeEvent(itemPickupPoints, new ArrayList<>(),
        Collections.EMPTY_MAP);
    }
    saveAndPublishService.publishMerchantVoucherViewConfigChange(itemPickupPoints, items);
    publishEventForVideoUploads(product);
    itemPriceService.publishItemPriceChangeEvent(product, itemPickupPoints, itemMap);
  }

  private void publishEventForVideoUploads(Product product) {
    String videoId = Optional.ofNullable(product).map(Product::getVideo).map(Video::getVideoId)
      .orElse(StringUtils.EMPTY);
    if (StringUtils.isNotBlank(videoId)) {
      saveAndPublishService.publishVideoCompressionEvent(
        VideoCompressionEventModel.builder().videoId(videoId).clientId(clientIdForVideoCompression)
          .ownerId(product.getMerchantCode()).additionalFields(
            Map.of(Constants.PRODUCT_CODE, product.getProductCode(), Constants.PRODUCT_SKU,
              product.getProductSku())).build());
    }
  }

  private void setItemPickupPointMerchantCode(Product product, List<ItemPickupPoint> itemPickupPoints) {
    itemPickupPoints.forEach(
      itemPickupPoint -> itemPickupPoint.setMerchantCode(product.getMerchantCode()));
  }

  private void setPickupPointCodesInProduct(Product entity, List<ItemPickupPoint> itemPickupPoints) {
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      entity.setPickupPointCodes(
          itemPickupPoints.stream().map(ItemPickupPoint::getPickupPointCode).collect(Collectors.toSet()));
    } else {
        entity.setArchived(true);
    }
  }

  @Override
  public ProductAndItemsVO saveProductAndItemsWithoutPublishingEvent(ProductAndItemsVO productAndItems) {
    ProductAndItemsVO result = new ProductAndItemsVO();
    if (Objects.nonNull(productAndItems.getProduct())) {
      result.setProduct(productRepository.save(productAndItems.getProduct()));
      this.cacheEvictHelperService.evictProductData(result.getProduct().getStoreId(), result.getProduct());
    }
    if (CollectionUtils.isNotEmpty(productAndItems.getItems())) {
      LOG.debug("updated items details: {}", productAndItems.getItems());
      result.setItems(itemRepository.saveAll(productAndItems.getItems()));
      for (Item item : result.getItems()) {
        this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      }
    }
    ProductAndItemEventModel productAndItemEventModel = objectConverterService.convertToProductAndItemEventModel(result);
    LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    this.kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      productAndItemEventModel.getProductSku(), productAndItemEventModel);
    return result;
  }

  @Override
  public ProductAndItemsVO saveProductAndItemsWithoutPublishingEventAndWithoutReindexing(
      ProductAndItemsVO productAndItems) {
    ProductAndItemsVO result = new ProductAndItemsVO();
    if (Objects.nonNull(productAndItems.getProduct())) {
      result.setProduct(productRepository.save(productAndItems.getProduct()));
      this.cacheEvictHelperService.evictProductData(result.getProduct().getStoreId(), result.getProduct());
    }
    if (CollectionUtils.isNotEmpty(productAndItems.getItems())) {
      LOG.debug("updated items details: {}", productAndItems.getItems());
      result.setItems(itemRepository.saveAll(productAndItems.getItems()));
      for (Item item : result.getItems()) {
        this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      }
    }
    return result;
  }

  @Override
  public ProductAndItemsVO saveProductAndItemsWithoutPublishingItemChange(ProductAndItemsVO productAndItems) {
    ProductAndItemsVO result = new ProductAndItemsVO();
    if (Objects.nonNull(productAndItems.getProduct())) {
      result.setProduct(this.saveAndPublishService.saveProduct(productAndItems.getProduct(), Collections.EMPTY_LIST,
          StringUtils.EMPTY, Collections.EMPTY_MAP));
      this.cacheEvictHelperService.evictProductData(result.getProduct().getStoreId(), result.getProduct());
    }
    if (CollectionUtils.isNotEmpty(productAndItems.getItems())) {
      LOG.debug("updated items details: {}", productAndItems.getItems());
      result.setItems(itemRepository.saveAll(productAndItems.getItems()));
      for (Item item : result.getItems()) {
        this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      }
    }
    ProductAndItemEventModel productAndItemEventModel = objectConverterService.convertToProductAndItemEventModel(result);
    LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      productAndItemEventModel.getProductSku(), productAndItemEventModel);
    return result;
  }

  @Override
  public ProductAndItemsVO saveProductAndItemsWithoutUpdatingSolr(ProductAndItemsVO productAndItems) {
    ProductAndItemsVO result = new ProductAndItemsVO();
    if (productAndItems.getProduct() != null) {
      result.setProduct(this.saveAndPublishService.saveProduct(productAndItems.getProduct(), Collections.EMPTY_LIST,
          StringUtils.EMPTY, Collections.EMPTY_MAP));
      this.cacheEvictHelperService.evictProductData(result.getProduct().getStoreId(),
          result.getProduct());
    }
    if (CollectionUtils.isNotEmpty(productAndItems.getItems())) {
      result.setItems(itemRepository.saveAll(productAndItems.getItems()));
      for (Item item : result.getItems()) {
        this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
        if (Objects.nonNull(item.getMasterDataItem())) {
          item.getMasterDataItem().setProductCode(productAndItems.getProduct().getProductCode());
        }
        saveAndPublishService.publishListOfItems(Collections.singletonList(item));
      }
    }
    return result;
  }

  @Override
  public ProductAndItemsVO saveProductAndItemsWithDeferredReindex(ProductAndItemsVO productAndItems) {
    ProductAndItemsVO result = new ProductAndItemsVO();
    if (Objects.nonNull(productAndItems.getProduct())) {
      result.setProduct(this.saveAndPublishService.saveProduct(productAndItems.getProduct(), Collections.EMPTY_LIST,
          StringUtils.EMPTY, Collections.EMPTY_MAP));
      this.cacheEvictHelperService.evictProductData(result.getProduct().getStoreId(), result.getProduct());
    }
    if (CollectionUtils.isNotEmpty(productAndItems.getItems())) {
      result.setItems(itemRepository.saveAll(productAndItems.getItems()));
      for (Item item : result.getItems()) {
        this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
        item.getMasterDataItem().setProductCode(productAndItems.getProduct().getProductCode());
        saveAndPublishService.publishListOfItems(Collections.singletonList(item));
      }
    }
    if (CollectionUtils.isNotEmpty(result.getItems())) {
      this.deferredReindexItems(result.getItems().get(0).getStoreId(), result.getItems(), true,
          ReindexType.ITEM_REINDEX, false);
    }
    return result;
  }

  @Override
  public Product updateAndEvictOff2OnItemCountByProductSku(String storeId, String productSku,
      boolean active, int increment) {
    Product saveProduct = this.saveAndPublishService.updateOff2OnItemCountIncrement(storeId,
        productSku, active, increment);
    this.cacheEvictHelperService.evictProductData(storeId, saveProduct);
    return saveProduct;
  }

  @Override
  public List<Item> updateItemDGLevel(String storeId, Set<String> itemSku, Integer dangerousLevel) {
    List<Item> result = itemRepository.updateDangerousGoodsByItemSku(storeId, itemSku, dangerousLevel);
    for (Item item : result) {
      solrIndexService.updateSolrAndClearCache(storeId, item);
      saveAndPublishService.publishListOfItems(Collections.singletonList(item));
    }
    return result;
  }

  @Override
  public Item updateItemFieldByItemSku(String storeId, String itemSku, String fieldName,
      Object fieldValue) {
    Item saveItem = itemRepository.updateFieldByItemSku(storeId, itemSku, fieldName, fieldValue);
    this.cacheEvictHelperService.evictItemData(saveItem.getStoreId(), saveItem);
    saveAndPublishService.publishListOfItems(Collections.singletonList(saveItem));
    return saveItem;
  }

  @Override
  public ItemPickupPoint updateItemFieldByItemSkuAndPickupPoint(String storeId, String itemSku, String fieldName,
      Object fieldValue, Item item) {
    ItemPickupPoint savedItemPickupPoint =
        itemPickupPointService.updateFieldByItemSku(storeId, itemSku, fieldName, fieldValue);
    this.cacheEvictHelperService.evictItemData(savedItemPickupPoint.getStoreId(), item);
    saveAndPublishService
        .publishListOfItems(Collections.singletonList(item), Collections.singletonList(savedItemPickupPoint),
            new ArrayList<>(), false);
    return savedItemPickupPoint;
  }

  @Override
  public List<ItemPickupPoint> updateItemPickupPointFieldsByItemSku(String storeId, String itemSku, String fieldName,
      Object fieldValue, Item item) {
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes = new ArrayList<>();
    List<ItemPickupPoint> savedItemPickupPoints =
        itemPickupPointService.updateFieldsByItemSku(storeId, itemSku, fieldName, fieldValue);
    cacheEvictHelperService.evictItemPickupPointCache(storeId, Arrays.asList(item), savedItemPickupPoints);
    setItemPickupPointChangeTypeBasedOnFieldName(fieldName, itemPickupPointChangeEventTypes);
    saveAndPublishService.publishItemPickupPointDataChangeEvent(savedItemPickupPoints, itemPickupPointChangeEventTypes,
      Collections.EMPTY_MAP);
    return savedItemPickupPoints;
  }

  private void setItemPickupPointChangeTypeBasedOnFieldName(String fieldName,
    List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes) {
    if (fieldName.equals(ProductFieldNames.ITEM_PICKUP_POINT_DISPLAYABLE_SCHEDULES)) {
      itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE);
    } else if (fieldName.equals(ProductFieldNames.ITEM_PICKUP_POINT_DISCOVERABLE_SCHEDULES)) {
      itemPickupPointChangeEventTypes.add(
        ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE);
    }
  }

  @Override
  public List<ItemPickupPoint> updateMerchantSkuForItemPickupPoints(String storeId, String itemSku, Item item,
      String newMerchantSku) {
    List<ItemPickupPoint> itemPickupPoints = itemPickupPointService
        .findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, Collections.singletonList(itemSku));
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
      itemPickupPoint.setMerchantSku(newMerchantSku);
    }
    List<ItemPickupPoint> savedItemPickupPoints = itemPickupPointService.saveItemPickupPoint(itemPickupPoints);
    cacheEvictHelperService.evictItemPickupPointCache(storeId, Arrays.asList(item), savedItemPickupPoints);
    saveAndPublishService.publishItemPickupPointDataChangeEvent(savedItemPickupPoints, new ArrayList<>(),
      Collections.EMPTY_MAP);
    return savedItemPickupPoints;
  }

  @Override
  public List<Item> updateItemFieldByItemSkus(String storeId, Collection<String> itemSkus, String fieldName,
      Object fieldValue) {
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    List<Item> items = itemRepository.updateFieldByItemSkus(storeId, itemSkus, fieldName, fieldValue);
    for (Item item: items) {
      cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      saveAndPublishService.publishListOfItems(Collections.singletonList(item));
    }
    return items;
  }

  @Override
  public ProductAndItemsVO updateOff2OnChannelActiveByItemSku(String storeId, String itemSku,
      boolean active) {
    ProductAndItemsVO productAndItemsVO = null;
    Item saveItem =
        this.saveAndPublishService.updateItemOff2OnChannelActiveByItemSku(storeId, itemSku, active);
    if (Objects.nonNull(saveItem)) {
      Product saveProduct =
          this.updateAndEvictOff2OnItemCountByProductSku(storeId, saveItem.getProductSku(), active,
              1);
      List<Item> items = new ArrayList<Item>();
      items.add(saveItem);
      productAndItemsVO = new ProductAndItemsVO(saveProduct, items);
      this.cacheEvictHelperService.evictItemData(storeId, saveItem);
      ProductAndItemEventModel productAndItemEventModel =
          objectConverterService.convertToProductAndItemEventModel(productAndItemsVO);
      LOG.info("Publishing event : {}, productAndItemEventModel : {}",
          ProductDomainEventName.UPDATE_TO_SOLR, productAndItemEventModel);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
          productAndItemEventModel.getProductSku(), productAndItemEventModel);
    }
    return productAndItemsVO;
  }

  @Override
  public ProductAndItemsVO updateOff2OnChannelActiveByProductSku(String storeId, String productSku,
      boolean active) {
    ProductAndItemsVO productAndItemsVO = null;
    List<Item> saveItems = this.saveAndPublishService
        .updateItemOff2OnChannelActiveByProductSku(storeId, productSku, active);
    if (!saveItems.isEmpty()) {
      Product saveProduct = this.updateAndEvictOff2OnItemCountByProductSku(storeId, productSku,
          active, saveItems.size());
      productAndItemsVO = new ProductAndItemsVO(saveProduct, saveItems);
      for (Item item : saveItems) {
        this.cacheEvictHelperService.evictItemData(storeId, item);
      }
    }
    ProductAndItemEventModel productAndItemEventModel = objectConverterService.convertToProductAndItemEventModel(productAndItemsVO);
    LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      productAndItemEventModel.getProductSku(), productAndItemEventModel);
    return productAndItemsVO;
  }

  @Override
  public Item saveAndEvictItem(Item item) {
    checkArgument(Objects.nonNull(item), SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    Item saveItem = this.itemRepository.save(item);
    this.cacheEvictHelperService.evictItemCache(saveItem.getStoreId(), saveItem);
    ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
    productAndItemEventModel.setProductSku(item.getProductSku());
    productAndItemEventModel.setItems(Arrays.asList(objectConverterService.convertToItemEventModel(saveItem)));
    productAndItemEventModel.setMerchantCode(item.getMerchantCode());
    LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      productAndItemEventModel.getProductSku(), productAndItemEventModel);
    return saveItem;
  }

  @Override
  public List<Item> saveAndEvictItems(List<Item> listOfItems) {
    checkArgument(CollectionUtils.isNotEmpty(listOfItems),
        SaveOperationServiceImpl.ITEM_MUST_NOT_BE_NULL);
    List<Item> saveItems = this.itemRepository.saveAll(listOfItems);
    for (Item item : saveItems) {
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
    }
    updateItemsToSolr(saveItems);
    return saveItems;
  }

  @Override
  public void deferredReindexItems(String storeId, List<Item> items, boolean updatedOfflineItem,
    ReindexType reindexType, boolean updateL3PickupPointCodes) {
    List<DeferredSolrReindexItem> deferredSolrReindexItems =
        deferredSolrReindexItemService.findByStoreIdAndItemSkuIn(storeId,
            items.stream().map(Item::getItemSku).collect(Collectors.toList()));
    List<DeferredSolrReindexItem> updatedDefferedReindexItems = new ArrayList<>();
    Map<String, DeferredSolrReindexItem> deferredSolrReindexMap = deferredSolrReindexItems.stream()
        .collect(Collectors.toMap(DeferredSolrReindexItem::getItemSku, Function.identity()));
    for (Item item : items) {
      getDefferedSolrReindexItems(updatedDefferedReindexItems, deferredSolrReindexMap, item, reindexType);
      if (!updatedOfflineItem) {
        cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      }
    }
    if (CollectionUtils.isNotEmpty(updatedDefferedReindexItems)) {
      deferredSolrReindexItemService.save(updatedDefferedReindexItems);
      Set<String> distinctProductSkus = items.stream().map(Item::getProductSku).collect(Collectors.toSet());
      if (updateL3PickupPointCodes) {
        productL3SolrReindexStatusService.insertProductSkusToReindexStatusCollection(storeId,
          CommonUtil.toProductL3SolrReindexStatuses(distinctProductSkus, storeId,
            ProductReindexStatus.REINDEX_PENDING_L3_SOLR_DB));
      } else if (ReindexType.ITEM_REINDEX.equals(reindexType)) {
        productL3SolrReindexStatusService.insertProductSkusToReindexStatusCollection(storeId,
          CommonUtil.toProductL3SolrReindexStatuses(distinctProductSkus, storeId,
            ProductReindexStatus.REINDEX_PENDING_L3));
      }
    }
  }

  private void getDefferedSolrReindexItems(List<DeferredSolrReindexItem> updatedItems,
      Map<String, DeferredSolrReindexItem> deferredSolrReindexMap, Item item, ReindexType reindexType) {
    if (deferredSolrReindexMap.containsKey(item.getItemSku())) {
      DeferredSolrReindexItem deferredSolrReindexItem = deferredSolrReindexMap.get(item.getItemSku());
      deferredSolrReindexItem.setReindexType(reindexType.getDescription());
      deferredSolrReindexItem.setProductReindexStatus(ProductReindexStatus.REINDEX_PENDING);
      deferredSolrReindexItem.setMarkForDelete(false);
      updatedItems.add(deferredSolrReindexItem);
    } else {
      DeferredSolrReindexItem deferredSolrReindexItem = toDeferredSolrReindexItem(item, reindexType.getDescription());
      updatedItems.add(deferredSolrReindexItem);
    }
  }

  @Override
  public ProductAndItemsVO changeOff2OnChannelActiveByProductSkus(String storeId, String productSku, boolean active,
      String userName, List<AuditTrailDto> auditTrailResponseList, String requestId) throws Exception{
    ProductAndItemsVO productAndItemsVO = null;
    List<Item> items = this.saveAndPublishService
        .updateItemOff2OnChannelActiveAndUpdatedDataByProductSku(storeId, productSku, active, userName);
    if (CollectionUtils.isNotEmpty(items)) {
      for (Item item : items) {
        this.cacheEvictHelperService.evictItemData(storeId, item);
      }
    }
    Product oldProduct = this.productService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
        Constants.DEFAULT_STORE_ID, productSku);
    Product product =
        this.updateAndEvictOff2OnItemCountAndFlagByProductSku(storeId, productSku, active, items.size(), userName);
    String productName = null;
    if(Objects.nonNull(product.getMasterDataProduct()) && Objects.nonNull(product.getMasterDataProduct().getProductName())){
      productName = product.getMasterDataProduct().getProductName();
    }else {
      MasterDataProductAndItemsVO masterDataProductAndItemsVO = masterDataCacheService.
          getMasterDataProductAndItems(userName, requestId, product.getProductCode(), Boolean.FALSE);
      productName = masterDataProductAndItemsVO.getMasterDataProduct().getProductName();
    }
    if(oldProduct.isOff2OnChannelActive() != active){
    auditTrailResponseList.add(new AuditTrailDto(product.getMerchantCode(), Constants.DEFAULT, Constants.ACTION_KEY,
        String.valueOf(!active), String.valueOf(active), null, productSku, productName));
    }
    this.saveAndPublishService.publishProduct(product, Collections.EMPTY_LIST);
    if (CollectionUtils.isNotEmpty(items)) {
      this.saveAndPublishService.publishListOfItemsWithItemChangeType(items,
          Collections.singletonList(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE));
    }
    productAndItemsVO = new ProductAndItemsVO(product, items);
    ProductAndItemEventModel productAndItemEventModel = objectConverterService.convertToProductAndItemEventModel(productAndItemsVO);
    LOG.info("Publishing event : {}, productAndItemEventModel : {}", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR,
      productAndItemEventModel.getProductSku(), productAndItemEventModel);
    return productAndItemsVO;
  }

  @Override
  public List<Product> updateCncActivatedFlagAtProductAndItemAndItemPickupPoint(String storeId, String clientId,
      Map<String, List<ItemPickupPoint>> pickupPointItemMap, Map<String, Item> itemMap, Set<Item> itemsToReindex,
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList,
      Map<String, Set<String>> productSkuAndPickupPointCodeMap, List<String> existingL4MadeCncFalse,
      List<String> l3MadeCncFalse) {
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, Product> productMap = new HashMap<>();
    for (Map.Entry<String,List<ItemPickupPoint>> entry : pickupPointItemMap.entrySet()) {
      String itemSku = entry.getKey();
      List<ItemPickupPoint> itemPickupPointList = entry.getValue();
      boolean cncActivated = false, online = false, fbbActivated = false;
      List<String> newPickupPoints = new ArrayList<>();
      for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
        if (itemPickupPoint.isCncActive()) {
          cncActivated = true;
        }
        if (itemPickupPoint.isFbbActivated()) {
          fbbActivated = true;
        }
        ItemViewConfig itemViewConfig = itemPickupPoint.getItemViewConfig().stream().findFirst().get();
        if (itemViewConfig.isBuyable() && itemViewConfig.isDiscoverable()) {
          online = true;
        }
        itemPickupPoints.add(itemPickupPoint);
        if (Objects.nonNull(itemPickupPoint.getNewData()) && itemPickupPoint.getNewData()) {
          newPickupPoints.add(itemPickupPoint.getPickupPointCode());
        }
      }
      Item item = itemMap.get(itemSku);
      if (existingL4MadeCncFalse.contains(itemSku)) {
        item.setCncActivated(false);
        itemsToReindex.add(item);
      } else if (cncActivated && !item.isCncActivated()) {
        item.setCncActivated(true);
        itemsToReindex.add(item);
      }
      Product product = null;
      if (productMap.containsKey(item.getProductSku())) {
        product = productMap.get(item.getProductSku());
      } else {
        product = productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId,
            item.getProductSku());
      }
      boolean isProductChanged = false;
      if (cncActivated && !product.isCncActivated()) {
        product.setCncActivated(true);
        isProductChanged = true;
      }
      if (online && !product.isOnline()) {
        product.setOnline(true);
        isProductChanged = true;
      }
      if (CollectionUtils.isNotEmpty(newPickupPoints)) {
        Set<String> pickupPointCodes = product.getPickupPointCodes();
        for (String pickupPointCode : newPickupPoints) {
          if (!pickupPointCodes.contains(pickupPointCode)) {
            pickupPointCodes.add(pickupPointCode);
            isProductChanged = true;
          }
        }
        product.setPickupPointCodes(pickupPointCodes);
      }
      if (fbbActivated && !product.isFbbActivated()) {
        product.setFbbActivated(true);
      }
      if (l3MadeCncFalse.contains(product.getProductSku())) {
        product.setCncActivated(false);
        isProductChanged = true;
      }
      if (isProductChanged) {
        productMap.put(item.getProductSku(), product);
      }
    }

    if (CollectionUtils.isNotEmpty(itemsToReindex)) {
      itemRepository.saveAll(itemsToReindex);
      for (Item item : itemsToReindex) {
        cacheEvictHelperService.evictItemData(storeId, item);
      }
    }

    List<ItemPickupPoint> fbbUpdateItemPickupPoints = itemPickupPointService.updateFbbFlag(itemPickupPoints);
    Map<String, List<ItemPickupPoint>> fbbDeletedSkuAndPPCodeMap =
        fbbUpdateItemPickupPoints.stream().collect(Collectors.groupingBy(ItemPickupPoint::getProductSku));

    List<Product> updatedProductList = new ArrayList<>();
    if (MapUtils.isNotEmpty(productMap)) {
      List<Product> productList = new ArrayList<>(productMap.values());
        for (Product product : productList) {
          Set<String> pickupPointCodes = productSkuAndPickupPointCodeMap.get(product.getProductSku());
          if (pickupPointCodes.stream()
              .anyMatch(pickupPointCode -> !product.getPickupPointCodes().contains(pickupPointCode))) {
            product.getPickupPointCodes().addAll(pickupPointCodes);
          }
          Set<String> ppCodesAddedToOtherVariants =
              itemPickupPointService.pickupPointsAddedToOtherVariants(product.getProductSku(),
                  Optional.ofNullable(fbbDeletedSkuAndPPCodeMap.get(product.getProductSku())).orElse(new ArrayList<>())
                      .stream().map(ItemPickupPoint::getOfflineItemId).collect(Collectors.toSet()));
          Set<String> deletedPPCodes =
              Optional.ofNullable(fbbDeletedSkuAndPPCodeMap.get(product.getProductSku())).orElse(new ArrayList<>())
                  .stream().map(ItemPickupPoint::getPickupPointCode).collect(Collectors.toSet());
          deletedPPCodes.removeAll(ppCodesAddedToOtherVariants);
          product.getPickupPointCodes().removeAll(deletedPPCodes);
        }
      updatedProductList = productRepository.saveAll(productList);
      productList.forEach(product -> cacheEvictHelperService.evictProductData(storeId, product));
    }

    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      List<ItemPickupPointDataChangeEventModel> fbbItemPickupPointDataChangeEventModel =
          fbbUpdateItemPickupPoints.stream().map(
              itemPickupPoint -> objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint,
                  false)).collect(Collectors.toList());
      itemPickupPoints.addAll(fbbUpdateItemPickupPoints);
      itemPickupPointDataChangeEventModelList.addAll(fbbItemPickupPointDataChangeEventModel);

      itemPickupPoints = itemPickupPointRepository.saveAll(itemPickupPoints);
      itemPickupPoints.forEach(
          itemPickupPoint -> cacheEvictHelperService.evictItemPickupPointData(itemPickupPoint.getStoreId(),
              itemPickupPoint, itemPickupPoint.getPickupPointCode()));
      saveAndPublishService
          .publishListOfOfflineItems(itemPickupPoints, itemMap, clientId, itemPickupPointDataChangeEventModelList);
        List<ProductAndItemEventModel> productAndItemEventModelList = new ArrayList<>();
        for (Product product : updatedProductList) {
          ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
          productAndItemEventModel.setProduct(objectConverterService.convertToProductEventModel(product));
          productAndItemEventModelList.add(productAndItemEventModel);
        }
        saveAndPublishService.publishSolrUpdateEvent(productAndItemEventModelList);
    }
    return updatedProductList;
  }

  @Override
  public List<ItemPickupPoint> saveItemPickupPointsAndClearCacheWithoutUpdatingSolrWithoutPublishing(
    List<ItemPickupPoint> listOfItems) {
    checkArgument(CollectionUtils.isNotEmpty(listOfItems),
      ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
    List<ItemPickupPoint> saveItems = itemPickupPointRepository.saveAll(listOfItems);
    for (ItemPickupPoint item : saveItems) {
      this.cacheEvictHelperService
        .evictItemPickupPointData(item.getStoreId(), item, item.getPickupPointCode());
    }
    return saveItems;
  }

  private Product updateAndEvictOff2OnItemCountAndFlagByProductSku(String storeId, String productSku, boolean active,
      int itemsCount, String userName) {
    Product saveProduct = this.saveAndPublishService
        .updateOff2OnItemCountIncrementAndFlag(storeId, productSku, active, itemsCount, userName);
    this.cacheEvictHelperService.evictProductData(storeId, saveProduct);
    saveProduct.setOff2OnChannelActive(active);
    return saveProduct;
  }
}
