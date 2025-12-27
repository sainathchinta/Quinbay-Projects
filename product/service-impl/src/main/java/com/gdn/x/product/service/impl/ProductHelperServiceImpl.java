package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.x.businesspartner.dto.ProductCounterResponse;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.outbound.api.XbpOutbound;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.FormulaUtil;
import com.gdn.x.product.service.util.MasterDataUtil;
import com.gdn.x.product.service.util.ProductUtil;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.function.Function;

@Service
public class ProductHelperServiceImpl implements ProductHelperService {

  private static final Logger LOG = LoggerFactory.getLogger(ProductHelperServiceImpl.class);
  private static final String PRICE_SET_MUST_NOT_BE_NULL = "price set must not be null";
  private static final String CATEGORY_CODE_MUST_NOT_BE_NULL = "Category code must not be null";
  private static final String CHANNEL_MUST_NOT_BE_BLANK = "Channel must not be blank";
  private static final long PAGE = 0;
  private static final long SIZE = 0;
  private static final String ITEMS_MUST_NOT_BE_NULL = "Items must not be null";
  private static final String NEW_PRODUCT_NAME_MUST_NOT_BE_BLANK =
      "newProductName must not be blank";
  private static final String OLD_PRODUCT_NAME_MUST_NOT_BE_BLANK =
      "oldProductName must not be blank";
  private static final String ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL =
      "Item view config must not be null";
  private static final String PRODUCT_AND_ITEMS_REQUEST_VO_MUST_NOT_BE_NULL =
      "ProductAndItemRequestVO must not be null";
  private static final String ITEM_REQUEST_MUST_NOT_BE_NULL = "itemRequest must not be null";

  private static final int SKU_DIGITS = 5;

  @Autowired
  private CachedService cachedService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseClient;

  @Autowired
  private ChannelService channelService;

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private FormulaUtil formulaUtil;

  @Autowired
  private GdnMapper gdnMapper;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private XbpOutbound xbpOutbound;

  @Value("${category.hierarchy.cache.switch.enabled}")
  private boolean categoryHierarchyCacheSwitchEnabled;

  @Value("${throw.exception.on.missing.categoryId}")
  private boolean throwExceptionOnMissingCategoryId;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Override
  public Product addItemAttributeToProductAttribute(Product product, String itemSku,
      List<MasterDataItemAttributeValue> itemAttributeValues) {
    return MasterDataUtil.addItemAttributeToProductAttribute(product, itemSku, itemAttributeValues);
  }


  @Override
  public List<List<CategoryResponse>> constructListOfCategoriesListOfProduct(String requestId,
    String username, List<String> categoryCodes) {
    checkArgument(categoryCodes != null, ProductHelperServiceImpl.CATEGORY_CODE_MUST_NOT_BE_NULL);
    List<List<CategoryResponse>> listOfCategoriesList = new ArrayList<List<CategoryResponse>>();
    if (categoryHierarchyCacheSwitchEnabled) {
      Set<String> categoryCodesForHierarchy = new HashSet<>(categoryCodes);
      Map<String, List<CategoryResponse>> categoryHierarchyMap = this.cachedService.getParentCategoriesFromDbAndCache(
              requestId, username, categoryCodesForHierarchy);
      for (String categoryCode : categoryCodes) {
        if (categoryHierarchyMap.containsKey(categoryCode)) {
          listOfCategoriesList.add(categoryHierarchyMap.get(categoryCode));
        }
      }
    } else {
      for (String categoryCode : categoryCodes) {
        listOfCategoriesList.add(
                this.cachedService.getParentCategoriesFromMasterData(requestId, username, categoryCode));
      }
    }
    return listOfCategoriesList;
  }

  @Override
  public Map<String, List<CategoryResponse>> constructMapOfCategoriesAndCategoryCode(
          String requestId, String username, List<String> categoryCodes) {
    checkArgument(CollectionUtils.isNotEmpty(categoryCodes),
            ProductHelperServiceImpl.CATEGORY_CODE_MUST_NOT_BE_NULL);
    if (categoryHierarchyCacheSwitchEnabled) {
      Set<String> categoryCodesForHierarchy = new HashSet<>(categoryCodes);
      return this.cachedService.getParentCategoriesFromDbAndCache(requestId, username, categoryCodesForHierarchy);
    } else {
      return categoryCodes.stream().collect(Collectors.toMap(Function.identity(),
              categoryCode -> this.cachedService
                      .getParentCategoriesFromMasterData(requestId, username, categoryCode)));
    }
  }

  @Override
  public Map<String, List<ProductAndItemsVO>> constructProductWithItemAndMasterData(
      List<Product> productAvailables, Map<String, List<Item>> itemAvailableCandidate,
      Map<String, MasterDataProductAndItemsVO> masterDataProducts) {
    return ProductUtil.constructProductWithItemAndMasterData(productAvailables, itemAvailableCandidate, masterDataProducts);
  }

  @Override
  public boolean containDefaultChannelPrice(Item itemRequestVO) {
    String defaultChannel = this.channelService.getDefaultChannel();
    for (Price price : itemRequestVO.getPrice()) {
      if (defaultChannel.equals(price.getChannel())) {
        return true;
      }
    }
    return false;
  }

  @Override
  public boolean containDefaultChannelPriceForListOfItemRequestVO(List<Item> listOfItemRequestVO) {
    String defaultChannel = this.channelService.getDefaultChannel();
    for (Item itemRequestVO : listOfItemRequestVO) {
      for (Price price : itemRequestVO.getPrice()) {
        if (defaultChannel.equals(price.getChannel())) {
          return true;
        }
      }
    }
    return false;
  }

  @Override
  public Product deleteItemAttributeFromProductAttribute(Product product, String itemSku) {
    return ProductUtil.deleteItemAttributeFromProductAttribute(product, itemSku);
  }

  @Override
  public String generateSpecificationDetail(Product product) {
    return ProductUtil.generateSpecificationDetail(product);
  }

  private boolean getBuyableStatusByChannel(Set<ItemViewConfig> itemViewConfigs, String channel, boolean isArchived)
      throws Exception {
    return ProductUtil.getBuyableStatusByChannel(itemViewConfigs, channel, isArchived);
  }

  private boolean getDiscoverableStatusByChannel(Set<ItemViewConfig> itemViewConfigs, String channel, boolean isArchived)
      throws Exception {
    return ProductUtil.getDiscoverableStatusByChannel(itemViewConfigs, channel, isArchived);
  }

  @Override
  public boolean getCurrentBuyableStatusForItem(Set<ItemViewConfig> itemViewConfigs, String channel, boolean isArchived)
      throws Exception {
    checkArgument(Objects.nonNull(itemViewConfigs), ProductHelperServiceImpl.ITEMS_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(itemViewConfigs), ProductHelperServiceImpl.ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(channel), ProductHelperServiceImpl.CHANNEL_MUST_NOT_BE_BLANK);
    try {
      return this.getBuyableStatusByChannel(itemViewConfigs, channel, isArchived);
    } catch (Exception e) {
      return this.getBuyableStatusByChannel(itemViewConfigs, this.channelService.getDefaultChannel(), isArchived);
    }
  }

  @Override
  public boolean getCurrentDiscoverableStatusForItem(Set<ItemViewConfig> itemViewConfigs, String channel,
      boolean isArchived)
      throws Exception {
    checkArgument(Objects.nonNull(itemViewConfigs), ProductHelperServiceImpl.ITEMS_MUST_NOT_BE_NULL);
    checkArgument(Objects.nonNull(itemViewConfigs),
        ProductHelperServiceImpl.ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(channel), ProductHelperServiceImpl.CHANNEL_MUST_NOT_BE_BLANK);
    try {
      return this.getDiscoverableStatusByChannel(itemViewConfigs, channel, isArchived);
    } catch (Exception e) {
      return this.getDiscoverableStatusByChannel(itemViewConfigs, this.channelService.getDefaultChannel(),
          isArchived);
    }
  }

  private boolean getOriginalBuyableStatusByChannel(Set<ItemViewConfig> itemViewConfigs, String channel,
      boolean isArchived) throws Exception {
    return ProductUtil.getOriginalBuyableStatusByChannel(itemViewConfigs, channel, isArchived);
  }

  private boolean getOriginalDiscoverableStatusByChannel(Set<ItemViewConfig> itemViewConfigs, String channel,
      boolean isArchived) throws Exception {
    return ProductUtil.getOriginalDiscoverableStatusByChannel(itemViewConfigs, channel, isArchived);
  }

  @Override
  public boolean getOriginalBuyableStatusForItem(Set<ItemViewConfig> itemViewConfigs, boolean isArchived)
      throws Exception {
    checkArgument(Objects.nonNull(itemViewConfigs), ProductHelperServiceImpl.ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL);
    return this.getOriginalBuyableStatusByChannel(itemViewConfigs, this.channelService.getDefaultChannel(), isArchived);
  }

  @Override
  public boolean getOriginalDiscoverableStatusForItem(Set<ItemViewConfig> itemViewConfigs, boolean isArchived)
      throws Exception {
    checkArgument(Objects.nonNull(itemViewConfigs), ProductHelperServiceImpl.ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL);
    return this.getOriginalDiscoverableStatusByChannel(itemViewConfigs, this.channelService.getDefaultChannel(), isArchived);
  }

  @Override
  public Map<String, MasterDataItem> getMasterDataItemsByItems(String storeId, String requestId,
      String username, List<Item> items) throws Exception {
    GdnRestListRequest listRequest =
        new GdnRestListRequest(ProductHelperServiceImpl.PAGE, ProductHelperServiceImpl.SIZE);
    listRequest.setRequestId(requestId);
    Set<String> itemCodes = new HashSet<String>();
    for (Item item : items) {
      itemCodes.add(item.getItemCode());
    }
    return this.masterDataService.getMasterDataItems(storeId, requestId, username, itemCodes);
  }

  private Price getPriceByChannel(Set<Price> prices, String channel) throws Exception {
    return ProductUtil.getPriceByChannel(prices, channel);
  }

  @Override
  public Price getRelevantItemPrice(Set<Price> prices, String channel) throws Exception {
    checkArgument(prices != null, ProductHelperServiceImpl.PRICE_SET_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(channel),
        ProductHelperServiceImpl.CHANNEL_MUST_NOT_BE_BLANK);
    try {
      return this.getPriceByChannel(prices, channel);
    } catch (Exception e) {
      return this.getPriceByChannel(prices, this.channelService.getDefaultChannel());
    }
  }

  @Override
  public String getSettlementType(Product product, Item item) {
    return ProductUtil.getSettlementType(product, item);
  }

  @Override
  public List<Item> modifyItemNames(List<Item> items, String oldProductName,
      String newProductName) {
    checkArgument(items != null, ProductHelperServiceImpl.ITEMS_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(oldProductName),
        ProductHelperServiceImpl.OLD_PRODUCT_NAME_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(newProductName),
        ProductHelperServiceImpl.NEW_PRODUCT_NAME_MUST_NOT_BE_BLANK);

    for (Item item : items) {
      String oldItemName = item.getMasterDataItem().getGeneratedItemName();
      if (oldItemName.length() <= oldProductName.length()) {
        item.getMasterDataItem().setGeneratedItemName(newProductName);
      } else {
        String additional = oldItemName.substring(oldProductName.length(), oldItemName.length());
        item.getMasterDataItem().setGeneratedItemName(newProductName.concat(additional));
      }
    }
    return items;
  }

  @Override
  public Item setItemDetail(String storeId, String productSku, String merchantCode,
      int sizeOfProductAttributes,
      Item item) {
    checkArgument(item != null, ProductHelperServiceImpl.ITEM_REQUEST_MUST_NOT_BE_NULL);
    if (StringUtils.isBlank(item.getItemSku())) {
      item.setItemSku(this.formulaUtil.appendWithSerial(productSku, sizeOfProductAttributes + 1,
          ProductHelperServiceImpl.SKU_DIGITS));
    }
    item.setProductSku(productSku);
    item.setStoreId(storeId);
    item.setSynchronized(true);
    item.setItemCatentryId(item.getItemSku());
    item.setMerchantCode(merchantCode);
    return item;
  }

  @Override
  public ItemVo setItemDetail(String storeId, String productSku, String merchantCode,
      int sizeOfProductAttributes,
      ItemVo itemVo) {
    checkArgument(itemVo != null, ProductHelperServiceImpl.ITEM_REQUEST_MUST_NOT_BE_NULL);
    if (StringUtils.isBlank(itemVo.getItemSku())) {
      itemVo.setItemSku(this.formulaUtil.appendWithSerial(productSku, sizeOfProductAttributes + 1,
          ProductHelperServiceImpl.SKU_DIGITS));
    }
    itemVo.setProductSku(productSku);
    itemVo.setStoreId(storeId);
    itemVo.setSynchronized(true);
    itemVo.setItemCatentryId(itemVo.getItemSku());
    itemVo.setMerchantCode(merchantCode);
    return itemVo;
  }

  @Override
  public Item setMasterDataItemFromMasterData(String storeId, String requestId, String username,
      Item item) throws Exception {
    GdnRestListRequest listRequest =
        new GdnRestListRequest(ProductHelperServiceImpl.PAGE, ProductHelperServiceImpl.SIZE);
    listRequest.setRequestId(requestId);
    Set<String> itemCodes = new HashSet<String>();
    itemCodes.add(item.getItemCode());
    Map<String, MasterDataItem> masterDataItems =
        this.masterDataService.getMasterDataItems(storeId, requestId, username, itemCodes);
    item.setMasterDataItem(masterDataItems.get(item.getItemCode()));
    return item;
  }

  @Override
  public Product setMasterDataProductFromMasterData(String storeId, String requestId,
      String username, Product product) throws Exception {
    GdnRestListRequest listRequest =
        new GdnRestListRequest(ProductHelperServiceImpl.PAGE, ProductHelperServiceImpl.SIZE);
    listRequest.setRequestId(requestId);
    Set<String> productCodes = new HashSet<String>();
    productCodes.add(product.getProductCode());
    product.setMasterDataProduct(
        this.masterDataService.getMasterDataProducts(storeId, username, requestId, productCodes)
            .get(product.getProductCode()));
    return product;
  }

  @Override
  public List<Item> setMultipleMasterDataItemsFromMasterData(String storeId, String requestId,
      String username, List<Item> items) throws Exception {
    Map<String, MasterDataItem> masterDataItems =
        this.getMasterDataItemsByItems(storeId, requestId, username, items);
    for (Item item : items) {
      item.setMasterDataItem(masterDataItems.get(item.getItemCode()));
    }
    return items;
  }

  @Override
  public Product setProductDetail(String requestId, String username, String storeId,
      Product product, ProductDetailResponse productDetail) throws Exception {
    checkArgument(product != null,
        ProductHelperServiceImpl.PRODUCT_AND_ITEMS_REQUEST_VO_MUST_NOT_BE_NULL);
    if (StringUtils.isBlank(product.getProductSku())) {
      ProductCounterResponse productCounterResponse =
              xbpOutbound.productCounterIncrementAndGet(storeId, requestId, username,
                      product.getMerchantCode());
      Long counter = productCounterResponse.getCounter();
      product.setProductSku(this.formulaUtil.appendWithSerial(
          product.getMerchantCode().toUpperCase(), counter, ProductHelperServiceImpl.SKU_DIGITS));
    }
    product.setStoreId(storeId);
    CategoryDetailResponse categoryDetailResponse =
        this.productCategoryBaseClient.getCategoryDetail(requestId, username,
            fetchCategoryIdFromProductDetailResponse(productDetail, product.getProductSku()));
    List<CategoryReferenceResponse> salesCategoryReferences = new ArrayList<>();
    List<CategoryReferenceResponse> b2bSalesCategoryReferences = new ArrayList<>();
    if(Objects.nonNull(categoryDetailResponse)) {
      if (CollectionUtils.isNotEmpty(categoryDetailResponse.getSalesCategoryReferences())) {
        salesCategoryReferences.addAll(categoryDetailResponse.getSalesCategoryReferences());
      }
      if (CollectionUtils.isNotEmpty(categoryDetailResponse.getB2bSalesCategoryReferences())) {
        b2bSalesCategoryReferences.addAll(categoryDetailResponse.getB2bSalesCategoryReferences());
      }
    }
    product.setSalesCatalogs(this.objectConverterService
        .convertToSalesCatalogsFromDirectParentCategory(requestId, username, salesCategoryReferences, b2bSalesCategoryReferences,
            businessPartnerService.isBusinessPartnerUmkmMerchant(storeId, product.getMerchantCode())));
    if (Optional.ofNullable(categoryDetailResponse).map(CategoryResponse::isHalalCategory).orElse(false)) {
      product.setCurationStatus(CurationStatus.NEED_CURATION);
    } else {
      product.setCurationStatus(CurationStatus.NONE);
    }
    product.setSynchronized(true);
    product.setProductCatentryId(product.getProductSku());
    product.setBrand(productDetail.getBrand());
    product.setProductName(productDetail.getName());
    product.setUrl(productDetail.getUrl());
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetail, product);
    product.setCategoryCode(productDetail.getProductCategoryResponses().get(0).getCategory().getCategoryCode());
    return product;
  }

  @Override
  public Item updateItemViewConfigForExistingChannel(Item item, ItemViewConfig itemViewConfig) {
    checkArgument(item != null, ProductHelperServiceImpl.ITEMS_MUST_NOT_BE_NULL);
    checkArgument(itemViewConfig != null,
        ProductHelperServiceImpl.ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL);
    if (StringUtils.isBlank(itemViewConfig.getChannel())) {
      itemViewConfig.setChannel(this.channelService.getDefaultChannel());
    }
    for (ItemViewConfig currItemViewConfig : item.getItemViewConfigs()) {
      if (currItemViewConfig.getChannel().equals(itemViewConfig.getChannel())) {
        currItemViewConfig.setBuyable(itemViewConfig.isBuyable());
        currItemViewConfig.setDiscoverable(itemViewConfig.isDiscoverable());
        currItemViewConfig
            .setItemBuyableScheduleIfNotNull(itemViewConfig.getItemBuyableSchedules());
        currItemViewConfig
            .setItemDiscoverableScheduleIfNotNull(itemViewConfig.getItemDiscoverableSchedules());
        return item;
      }
    }
    return null;
  }

  @Override
  public boolean updateItemViewConfigForExistingChannelItemPickupPoint(ItemPickupPoint itemPickupPoint, ItemViewConfig itemViewConfig) {
    if (StringUtils.isBlank(itemViewConfig.getChannel())) {
      itemViewConfig.setChannel(this.channelService.getDefaultChannel());
    }
    boolean updatedItemViewConfig = false;
    for (ItemViewConfig currItemViewConfig : itemPickupPoint.getAllItemViewConfigs()) {
      if (currItemViewConfig.getChannel().equals(itemViewConfig.getChannel())) {
        updatedItemViewConfig = true;
        currItemViewConfig.setBuyable(itemViewConfig.isBuyable());
        if (currItemViewConfig.isDiscoverable() != itemViewConfig.isDiscoverable()) {
          currItemViewConfig.setDiscoverable(itemViewConfig.isDiscoverable());
          if (cncForWarehouseFeatureSwitch && StringUtils.equals(currItemViewConfig.getChannel(),
              Constants.CNC)) {
            itemPickupPoint.setItemPickupPointDataChangeType(Arrays.asList(
                ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE.getName()));
          } else {
            itemPickupPoint.setItemPickupPointDataChangeType(
                Arrays.asList(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName()));
          }
        }
        currItemViewConfig.setItemBuyableScheduleIfNotNull(itemViewConfig.getItemBuyableSchedules());
        currItemViewConfig.setItemDiscoverableScheduleIfNotNull(itemViewConfig.getItemDiscoverableSchedules());
      }
    }
    return updatedItemViewConfig;
  }

  @Override
  public void updateItemViewConfigForExistingChannel(Item item,
      Set<ItemViewConfig> itemViewConfigs) {
    for (ItemViewConfig itemViewConfig : itemViewConfigs) {
      this.updateItemViewConfigForExistingChannel(item, itemViewConfig);
    }
  }

  @Override
  public List<List<CategoryResponse>> getCategoryResponseListByCategoryCodesForProducts(
          String requestId, String username, List<String> categoryCodes) {
    checkArgument(categoryCodes != null, ProductHelperServiceImpl.CATEGORY_CODE_MUST_NOT_BE_NULL);
    List<List<CategoryResponse>> listOfCategoriesList = new ArrayList<List<CategoryResponse>>();
    if (categoryHierarchyCacheSwitchEnabled) {
      Set<String> categoryCodesForHierarchy = new HashSet<>(categoryCodes);
      Map<String, List<CategoryResponse>> categoryHierarchyMap = this.cachedService.getParentCategoriesFromDbAndCache
       (requestId, username, categoryCodesForHierarchy);
      for (String categoryCode : categoryCodes) {
        if(Objects.nonNull(categoryHierarchyMap.get(categoryCode))) {
          listOfCategoriesList.add(categoryHierarchyMap.get(categoryCode));
        }
      }
    } else {
      for (String categoryCode : categoryCodes) {
        List<CategoryResponse> response = new ArrayList<>();
        try {
          response =
                  this.cachedService.getParentCategoriesFromMasterData(requestId, username, categoryCode);
          listOfCategoriesList.add(response);
        } catch (ApplicationRuntimeException e) {
          LOG.error("category sequence is not found for given category code : {}", categoryCode, e);
        }
      }
    }
    return listOfCategoriesList;
  }

  @Override
  public void constructOfflineItem(Item item, OfflineItem offlineItem) {
    if (item == null || offlineItem == null) {
      return;
    }

    OfflineItemDetailVo offlineItemDetailVo = new OfflineItemDetailVo();
    offlineItemDetailVo.setUniqueId(offlineItem.getOfflineItemId());
    offlineItemDetailVo.setItemSku(offlineItem.getItemSku());
    offlineItemDetailVo.setPickupPointCode(offlineItem.getPickupPointCode());

    Set<Price> prices = new HashSet<>();
    if (!CollectionUtils.isEmpty(item.getPrice())) {
      for (Price price : item.getPrice()) {
        if (price == null) {
          continue;
        }
        prices.add(gdnMapper.deepCopy(price, Price.class));
      }
    }
    offlineItemDetailVo.setPrices(prices);

    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    if (!CollectionUtils.isEmpty(item.getItemViewConfigs())) {
      for (ItemViewConfig itemViewConfig : item.getItemViewConfigs()) {
        if (itemViewConfig == null) {
          continue;
        }
        itemViewConfigs.add(gdnMapper.deepCopy(itemViewConfig, ItemViewConfig.class));
      }
    }
    offlineItemDetailVo.setItemViewConfigs(itemViewConfigs);

    this.overwriteItemPriceWithOfflinePrice(offlineItemDetailVo.getPrices(), offlineItem);
    this.overwriteItemViewConfigsForOfflineItem(offlineItemDetailVo.getItemViewConfigs());

    if (item.getOfflineItems() == null) {
      item.setOfflineItems(new ArrayList<>());
    }
    item.getOfflineItems().add(offlineItemDetailVo);
  }

  @Override
  public void constructOfflineItemForTransaction(Item item, ItemPickupPoint itemPickupPoint) {
    if (Objects.isNull(item) || Objects.isNull(itemPickupPoint)) {
      return;
    }
    item.setPrice(itemPickupPoint.getPrice());
    item.setItemViewConfigsIfNotNullOrEmpty(itemPickupPoint.getItemViewConfig());
    if (CollectionUtils.isNotEmpty(item.getItemViewConfigs())) {
      item.getItemViewConfigs().stream().findFirst().get().setBuyable(true);
      item.getItemViewConfigs().stream().findFirst().get().setDiscoverable(true);
    }
    item.setPickupPointCode(itemPickupPoint.getPickupPointCode());
  }

  @Override
  public void findAndConstructOfflineItems(String storeId, List<Item> items) {
    if (CollectionUtils.isEmpty(items)) {
      return;
    }

    Set<String> itemSkus = items.stream()
        .filter(Item::isCncActivated)
        .map(Item::getItemSku)
        .collect(Collectors.toSet());

    Map<String, List<OfflineItem>> offlineItemsMap = new HashMap<>();
    if (!CollectionUtils.isEmpty(itemSkus)) {
      SystemParameter limitOfflineItems = this.systemParameterService
          .findValueByStoreIdAndVariable(storeId, SystemParameterNames.LIMIT_OFFLINE_ITEMS);

      if (Objects.nonNull(limitOfflineItems.getValue()) &&
          Integer.parseInt(limitOfflineItems.getValue()) >= 0) {
        collectOfflineItemsWithLimit(storeId, itemSkus, offlineItemsMap, Integer.parseInt(limitOfflineItems.getValue()));
      } else {
        collectOfflineItemsWithoutLimit(storeId, itemSkus, offlineItemsMap);
      }
    }

    for (Item item : items) {
      item.setUniqueId(item.getItemSku());

      List<OfflineItem> offlineItemsByItemSku = offlineItemsMap.get(item.getItemSku());
      if (CollectionUtils.isEmpty(offlineItemsByItemSku)) {
        continue;
      }

      for (OfflineItem offlineItem : offlineItemsByItemSku) {
        this.constructOfflineItem(item, offlineItem);
      }
    }
  }

  @Override
  public void findAndConstructOfflineItemsByPickupPointCode(String storeId, List<Item> items,
      String pickupPointCode) {
    Set<String> itemSkus = items.stream()
        .filter(Item::isCncActivated)
        .map(Item::getItemSku)
        .collect(Collectors.toSet());

    if(!CollectionUtils.isEmpty(itemSkus)) {
      if (StringUtils.isNotBlank(pickupPointCode)) {
        List<ItemPickupPoint> itemPickupPointList = itemPickupPointService
            .findItemPickupPointByStoreIdAndItemSkusAndPickupPointCodeAndCncActivatedAndMarkForDeleteFalse(storeId,
                itemSkus, true, pickupPointCode);
        List<OfflineItem> offlineItems = CommonUtil.getOfflineItemsByItemPickupPoint(itemPickupPointList, false, null);
        if (!CollectionUtils.isEmpty(offlineItems)) {
          Map<String, Item> itemByItemSkuMap = items.stream()
              .collect(Collectors.toMap(Item::getItemSku, Function.identity(), (o1, o2) -> o1));
          Map<String, OfflineItem> offlineItemByItemSkuMap = offlineItems.stream()
              .collect(Collectors.toMap(OfflineItem::getItemSku, Function.identity(), (o1, o2) -> o1));
          for (String itemSku : itemSkus) {
            Item item = itemByItemSkuMap.get(itemSku);
            OfflineItem offlineItem = offlineItemByItemSkuMap.get(itemSku);
            if (Objects.isNull(item) || Objects.isNull(offlineItem)) {
              continue;
            }
            this.constructOfflineItem(item, offlineItem);
          }
        }
      } else {
        findAndConstructOfflineItems(storeId, items);
      }
    }
  }

  private void collectOfflineItemsWithLimit(
      String storeId, Set<String> itemSkus, Map<String, List<OfflineItem>> offlineItemsMap, int limit) {
    for (String itemSku : itemSkus) {
      Page<ItemPickupPoint> itemPickupPointPage = itemPickupPointService
          .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(storeId, itemSku, true, false,
              PageRequest.of(0, limit));
      List<OfflineItem> offlineItems =
          CommonUtil.getOfflineItemsByItemPickupPoint(itemPickupPointPage.getContent(), false, null);
      convertToOfflineItemMap(offlineItemsMap, offlineItems);
    }
  }

  private void collectOfflineItemsWithoutLimit(
      String storeId, Set<String> itemSkus, Map<String, List<OfflineItem>> offlineItemsMap) {
    List<ItemPickupPoint> itemPickupPoints = itemPickupPointService
        .findItemPickupPointByStoreIdAndItemSkusAndCncActiveAndMarkForDeleteFalse(storeId, itemSkus, true);
    List<OfflineItem> offlineItems =
        CommonUtil.getOfflineItemsByItemPickupPoint(itemPickupPoints, false, null);
    convertToOfflineItemMap(offlineItemsMap, offlineItems);
  }

  private void convertToOfflineItemMap(Map<String, List<OfflineItem>> offlineItemsMap, List<OfflineItem> offlineItems) {
    ProductUtil.convertToOfflineItemMap(offlineItemsMap, offlineItems);
  }

  @Override
  public void overwriteItemPriceWithOfflinePrice(Set<Price> prices, OfflineItem offlineItem) {
    ProductUtil.overwriteItemPriceWithOfflinePrice(prices, offlineItem);
  }

  @Override
  public void overwriteItemViewConfigsForOfflineItem(Set<ItemViewConfig> itemViewConfigs) {
    ProductUtil.overwriteItemViewConfigsForOfflineItem(itemViewConfigs);
  }

  @Override
  public CategoryResponse getCategoryResponseByCategoryCode(String requestId, String username, String categoryCode) {
    if (StringUtils.isNotBlank(categoryCode)) {
      List<CategoryResponse> parentCategoriesFromMasterData =
          cachedService.getParentCategoriesFromMasterData(requestId, username, categoryCode);
      return CollectionUtils.isEmpty(parentCategoriesFromMasterData) ? null : parentCategoriesFromMasterData.get(0);
    }
    return null;
  }

  @Override
  public List<Item> getCachedItemsByProductSkuWithoutOverridingL5Data(String storeId, String productSku, List<String> itemSkus) {
    List<Item> items =
        cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
    List<Item> cachedItem = new ArrayList<>(items);
    // removes the item as data is fetched from slave and can be stale
    cachedItem.removeIf(item -> itemSkus.contains(item.getItemSku()));
    return cachedItem;
  }

  @Override
  public void updateItemPickupPointViewConfigForExistingChannel(ItemPickupPoint pickupPointToSet,
    Set<ItemViewConfig> itemViewConfigs) {
    checkArgument(Objects.nonNull(pickupPointToSet), ProductHelperServiceImpl.ITEMS_MUST_NOT_BE_NULL);
    for (ItemViewConfig itemViewConfig : itemViewConfigs) {
      ItemViewConfig singleItemViewConfigByChannel =
          pickupPointToSet.getSingleItemViewConfigByChannel(itemViewConfig.getChannel());
      if (Objects.isNull(singleItemViewConfigByChannel)) {
        pickupPointToSet.getAllItemViewConfigs().add(itemViewConfig);
      } else {
        this.updateItemPickupPointViewConfigForExistingChannel(singleItemViewConfigByChannel, itemViewConfig);
      }
    }
  }

  @Override
  public ItemViewConfig updateItemPickupPointViewConfigForExistingChannel(ItemViewConfig currItemViewConfig,
      ItemViewConfig itemViewConfig) {
    checkArgument(Objects.nonNull(currItemViewConfig), ProductHelperServiceImpl.ITEM_VIEW_CONFIG_MUST_NOT_BE_NULL);
    if (StringUtils.isBlank(itemViewConfig.getChannel())) {
      itemViewConfig.setChannel(this.channelService.getDefaultChannel());
    }
    if (currItemViewConfig.getChannel().equals(itemViewConfig.getChannel())) {
      currItemViewConfig.setBuyable(itemViewConfig.isBuyable());
      currItemViewConfig.setDiscoverable(itemViewConfig.isDiscoverable());
      currItemViewConfig.setItemBuyableScheduleIfNotNull(itemViewConfig.getItemBuyableSchedules());
      currItemViewConfig.setItemDiscoverableScheduleIfNotNull(itemViewConfig.getItemDiscoverableSchedules());
      return currItemViewConfig;
    }
    return null;
  }

  @Override
  public List<Item> getCachedItemsByProductSku(String storeId, String productSku, List<String> itemSkus) {
    List<Item> items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
    List<Item> cachedItem = new ArrayList<>(items);
    // removes the item as data is fetched from slave and can be stale
    cachedItem.removeIf(item -> itemSkus.contains(item.getItemSku()));
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(cachedItem)) {
      itemPickupPoints = itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId,
          cachedItem.stream().map(Item::getItemSku).collect(Collectors.toList()));
    }
    objectConverterService.overrideL4DetailsFromL5(cachedItem, itemPickupPoints);
    return cachedItem;
  }

  public String fetchCategoryIdFromProductDetailResponse(
    ProductDetailResponse productDetailResponse, String productSku) {
    return ProductUtil.fetchCategoryIdFromProductDetailResponse(productDetailResponse, productSku, throwExceptionOnMissingCategoryId);
  }
}
