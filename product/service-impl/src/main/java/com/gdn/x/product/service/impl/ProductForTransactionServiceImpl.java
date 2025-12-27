package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
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

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.ItemSkuVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductForTransactionService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.promotion.enums.PromoBundlingType;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductForTransactionServiceImpl implements ProductForTransactionService {

  private static final String ITEM_SKU_MUST_NOT_BE_BLANK = "itemSku must not be blank";

  private static final String REQUEST_ID_MUST_NOT_BE_BLANK = "requestId must not be blank";

  private static final String USERNAME_MUST_NOT_BE_BLANK = "username must not be blank";

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";

  private static final String ITEM_SKU_MUST_NOT_BE_NULL = "itemSku must not be null";

  private static final String ITEM_SKU_MUST_NOT_BE_EMPTY = "itemSku must not be empty";

  @Autowired
  private ObjectConverterService objectConverterService;
  @Autowired
  private ProductHelperService productHelperService;
  @Autowired
  private ItemService itemService;
  @Autowired
  private ProductService productService;
  @Autowired
  private ChannelService channelService;
  @Autowired
  private CatalogService catalogService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private MasterDataConstructorService masterDataConstructorService;

  @Autowired
  private ItemPriceService itemPriceService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private PromotionOutbound promotionOutbound;

  @Value("${max.items.for.transaction}")
  private int maxItemsForTransaction;

  @Value("${fetch.pcb.master.data}")
  private boolean fetchPcbMasterData;

  @Override
  @Deprecated
  //Please refer to findProductForTransactionByItemSkus
  public ProductForTransactionVO findProductForTransactionForAllItems(String storeId, String requestId,
    String username, String itemSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductForTransactionServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku),
        ProductForTransactionServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId),
        ProductForTransactionServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    ProductAndItemsVO productAndItemsVO =
        this.productService.getProductDetailAndSingleItemByItemSku(storeId, requestId, username, itemSku, false, false, null);
    Product product = productAndItemsVO.getProduct();
    Item item = productAndItemsVO.getItems().get(0);
    List<ItemCatalogVO> itemCategoryVOListMap =
        this.catalogService.getItemCatalogsWithCategoryHierarchy(username, requestId, product);
    return this.objectConverterService.convertToProductForTransaction(product, item,
        itemCategoryVOListMap);
  }

  @Override
  public List<ItemPriceVO> getProductPriceForTransaction(String storeId, List<String> itemSkuList,
    String channel) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductForTransactionServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(itemSkuList != null, ProductForTransactionServiceImpl.ITEM_SKU_MUST_NOT_BE_NULL);
    checkArgument(!itemSkuList.isEmpty(),
        ProductForTransactionServiceImpl.ITEM_SKU_MUST_NOT_BE_EMPTY);
    String channelToUse = channel;
    if (StringUtils.isBlank(channelToUse)) {
      channelToUse = this.channelService.getDefaultChannel();
    }
    List<ItemPriceVO> result = new ArrayList<>();
    List<Item> items = this.itemService.getItemPriceAndViewConfigs(storeId, itemSkuList);
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.findByItemSkuInAndDelivery(storeId, itemSkuList, true);
    Map<String, ItemPickupPoint> itemPickupPointMap =
        itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity()));
    for (Item item : items) {
      ItemPickupPoint itemPickupPoint = itemPickupPointMap.get(item.getItemSku());
      Price price = this.productHelperService.getRelevantItemPrice(itemPickupPoint.getPrice(), channelToUse);
      result.add(new ItemPriceVO.ItemPriceBuilder().setBuyable(this.productHelperService
          .getCurrentBuyableStatusForItem(itemPickupPoint.getItemViewConfig(), channelToUse, item.isArchived()))
          .setDiscoverable(this.productHelperService
              .getCurrentDiscoverableStatusForItem(itemPickupPoint.getItemViewConfig(), channelToUse,
                  item.isArchived())).setItemSku(item.getItemSku()).setOfferPrice(price.getOfferPrice())
          .setListPrice(price.getListPrice()).build());
    }
    return result;
  }

  @Override
  public List<ItemPriceVO> findProductPriceForOnlineAndOfflineTransaction(String storeId, List<ItemSkuVO> itemSkuVOs,
    String channel, String requestId, String username) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
        ProductForTransactionServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkuVOs),
        ProductForTransactionServiceImpl.ITEM_SKU_MUST_NOT_BE_EMPTY);
    checkArgument(StringUtils.isNotBlank(requestId),
        ProductForTransactionServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);

    return this.getProductPriceBySkus(storeId, itemSkuVOs,
        StringUtils.isBlank(channel) ? this.channelService.getDefaultChannel() : channel, requestId, username);
  }

  @Override
  public List<ProductForTransactionVO> findProductForTransactionByItemSkus(String storeId,
    String requestId, String username, List<String> itemSkus) throws Exception {
    checkArgument(StringUtils.isNotEmpty(storeId),
      ProductForTransactionServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotEmpty(requestId),
      ProductForTransactionServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotEmpty(username),
      ProductForTransactionServiceImpl.USERNAME_MUST_NOT_BE_BLANK);
    checkArgument(itemSkus.size() < maxItemsForTransaction,
      ErrorMessages.ITEM_COUNT_LIMIT_FOR_TRANSACTION_API);
    List<Item> itemList = Optional.ofNullable(
        this.itemService.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, new HashSet<>(itemSkus)))
      .orElse(new ArrayList<>());
    if (CollectionUtils.isNotEmpty(itemList)) {
      setOnlineItemViewConfig(storeId, itemList);
    }
    List<String> itemSkusForOfflineItemSearch =
      CommonUtil.getItemSkusNotFoundInFetch(itemList, itemSkus);
    List<Item> itemListForOfflineItemSearch = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemSkusForOfflineItemSearch)) {
      itemListForOfflineItemSearch =
        this.itemService.getItemsByOfflineItemIds(storeId, itemSkusForOfflineItemSearch, false);
    }
    itemList = Stream.concat(itemList.stream(), itemListForOfflineItemSearch.stream())
      .collect(Collectors.toList());
    List<String> productSkuList =
      itemList.stream().map(Item::getProductSku).distinct().collect(Collectors.toList());
    List<Product> productList =
      this.productService.findByStoreIdAndProductSkuIn(storeId, productSkuList);
    if (CollectionUtils.isEmpty(productList)) {
      return Collections.emptyList();
    }
    itemList.forEach(item -> item.setUniqueId(item.getItemSku()));
    objectConverterService.overrideDefiningAttributeDetailsFromL3ToL4(productList, itemList);
    List<ProductAndItemsVO> productAndItemsVOList = generateProductAndItemVos(productList, itemList);
    setWholesaleRules(MandatoryRequestParam.generateMandatoryRequestParam(storeId, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, requestId, username, null), productAndItemsVOList);
    return generateProductTransactionResponse(storeId, requestId, username, itemList, productList, productAndItemsVOList);
  }

  private List<ProductForTransactionVO> generateProductTransactionResponse(String storeId, String requestId,
      String username, List<Item> itemList, List<Product> productList, List<ProductAndItemsVO> productAndItemsVOList) {
    List<Product> productWithMasterDataDetails =
        ProductAndItemsUtil.filterProductWithMasterDataDetails(productList, fetchPcbMasterData);
    List<Product> productWithoutMasterDataDetails =
        ProductAndItemsUtil.filterProductWithoutMasterDataDetails(productList, fetchPcbMasterData);
    Set<String> withoutMasterDataProductSkus =
        ProductAndItemsUtil.getProductSkuFromProduct(productWithoutMasterDataDetails);
    Map<String, ProductMasterDataResponse> productMasterDataResponseMap =
        checkAndGetMasterDataDetails(storeId, productWithoutMasterDataDetails,
            ProductAndItemsUtil.filterItemWithoutMasterDataDetails(itemList, withoutMasterDataProductSkus),
            ProductAndItemsUtil.filterProductAndItemsVOWithoutMasterDataDetails(productAndItemsVOList,
                withoutMasterDataProductSkus));
    setAndFetchItemCatalogs(productWithMasterDataDetails, productWithoutMasterDataDetails, username, requestId);
    return objectConverterService.generateProductForTransactionVO(withoutMasterDataProductSkus, productAndItemsVOList,
        productMasterDataResponseMap);
  }

  private Map<String, ProductMasterDataResponse> checkAndGetMasterDataDetails(String storeId,
      List<Product> productWithoutMasterDataDetails, List<Item> filteredItemList,
      List<ProductAndItemsVO> filteredProductAndItemsVOList) {
    Map<String, ProductMasterDataResponse> productMasterDataResponseMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(productWithoutMasterDataDetails)) {
      productMasterDataResponseMap = this.masterDataConstructorService.fetchMasterDataMapForTransaction(storeId,
          ProductAndItemsUtil.getItemCodeFromItem(filteredItemList));
      setMasterDataProductCategory(filteredProductAndItemsVOList, productMasterDataResponseMap);
    }
    return productMasterDataResponseMap;
  }

  private void setOnlineItemViewConfig(String storeId, List<Item> itemList) throws Exception {
    Map<String, ItemPickupPoint> itemPickupPointMap = Optional.ofNullable(
        this.itemPickupPointService.findByItemSkusAndDelivery(storeId,
          Optional.ofNullable(itemList).orElse(new ArrayList<>()).stream().map(Item::getItemSku)
            .collect(Collectors.toList()), true)).orElse(new ArrayList<>()).stream()
      .collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity()));
    if (CollectionUtils.isNotEmpty(itemList)) {
      for (Item item : itemList) {
        item.setItemViewConfigs(itemPickupPointMap.get(item.getItemSku()).getItemViewConfig());
        item.getItemViewConfigs().stream().findFirst().get().setBuyable(
          this.productHelperService.getCurrentBuyableStatusForItem(item.getItemViewConfigs(),
            Constants.DEFAULT, item.isArchived()));
        item.getItemViewConfigs().stream().findFirst().get().setDiscoverable(
          this.productHelperService.getCurrentDiscoverableStatusForItem(item.getItemViewConfigs(),
            Constants.DEFAULT, item.isArchived()));
        item.setPrice(itemPickupPointMap.get(item.getItemSku()).getPrice());
        item.setPickupPointCode(itemPickupPointMap.get(item.getItemSku()).getPickupPointCode());
        item.setMerchantSku(itemPickupPointMap.get(item.getItemSku()).getMerchantSku());
        item.setActivePromoBundlings(itemPickupPointMap.get(item.getItemSku()).getActivePromoBundlings());
      }
    }
  }

  private void setMasterDataProductCategory(List<ProductAndItemsVO> productAndItemsVOList,
    Map<String, ProductMasterDataResponse> productMasterDataResponseMap) {
    for (ProductAndItemsVO productAndItemsVO : productAndItemsVOList) {
      ProductMasterDataResponse productMasterDataResponse =
        productMasterDataResponseMap.get(productAndItemsVO.getItems().get(0).getItemCode());
      if (Objects.nonNull(productMasterDataResponse)) {
        CategoryResponse categoryResponse =
          productMasterDataResponse.getProductCategoryResponses().get(0).getCategory();
        if (Objects.isNull(productAndItemsVO.getProduct().getMasterDataProduct())) {
          productAndItemsVO.getProduct().setMasterDataProduct(new MasterDataProduct());
        }
        MasterCatalog masterCatalog = new MasterCatalog();
        masterCatalog.setCatalogCode(categoryResponse.getCatalog().getCatalogCode());
        masterCatalog.setCategory(
          new Category(categoryResponse.getCategoryCode(), categoryResponse.getCategoryCode()));
        productAndItemsVO.getProduct().getMasterDataProduct().setMasterCatalog(masterCatalog);
      }
    }
  }

  private List<ProductAndItemsVO> generateProductAndItemVos(List<Product> productList,
    List<Item> itemList) {
    List<ProductAndItemsVO> productAndItemsVOList = new ArrayList<>();
    Map<String, Product> productMap =
      productList.stream().collect(Collectors.toMap(Product::getProductSku, Function.identity()));
    itemList.forEach(item -> productAndItemsVOList.add(
      new ProductAndItemsVO(productMap.get(item.getProductSku()), Arrays.asList(item))));
    return productAndItemsVOList;
  }

  private void setWholesaleRules(MandatoryRequestParam mandatoryRequestParam,
    List<ProductAndItemsVO> productAndItemsList) {
    Set<String> itemSkus = productAndItemsList.stream()
      .flatMap(productAndItemsVO -> productAndItemsVO.getItems().stream()).filter(
        item -> CollectionUtils.isNotEmpty(item.getActivePromoBundlings()) && (
          item.getActivePromoBundlings().contains(Constants.WHOLESALE)
            || item.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE)))
      .map(Item::getItemSku).collect(Collectors.toSet());
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      ActivePromoBundlingResponseVO activePromoBundlingResponseVO =
        promotionOutbound.getActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam,
          PromoBundlingType.WHOLESALE, itemSkus);
      Map<String, List<WholesaleRuleVO>> promoDetailResponseMap =
        activePromoBundlingResponseVO.getPromoBundlingDetailResponseVOList().stream().collect(
          Collectors.toMap(PromoBundlingDetailResponseVO::getItemSku,
            PromoBundlingDetailResponseVO::getWholesaleRules, (oldValue, newValue) -> newValue));
      double minimumPrice = Double.parseDouble(
        systemParameterService.findValueByStoreIdAndVariable(mandatoryRequestParam.getStoreId(),
          SystemParameterNames.MINIMUM_PRICE).getValue());
      productAndItemsList.forEach(productAndItemsVO -> productAndItemsVO.getItems()
        .forEach(item -> constructWholesaleRules(promoDetailResponseMap, item, minimumPrice)));
    }
  }

  private void constructWholesaleRules(Map<String, List<WholesaleRuleVO>> promoDetailResponseMap,
    Item item, Double minimumPrice) {
    if (promoDetailResponseMap.containsKey(item.getItemSku())) {
      List<WholesaleRuleVO> wholesaleRuleVOS = promoDetailResponseMap.get(item.getItemSku());
      wholesaleRuleVOS.forEach(wholesaleRuleVO -> wholesaleRuleVO.setFinalPrice(
        this.itemPriceService.getFinalPriceWithMinimumPriceParameter(
          item.getPrice().stream().findFirst().orElse(new Price()),
          wholesaleRuleVO.getDiscountPercentage(), minimumPrice)));
      item.setWholesaleRules(wholesaleRuleVOS);
    } else {
      item.setWholesaleRules(new ArrayList<>());
    }
  }

  private void setAndFetchItemCatalogs(List<Product> productWithMasterDataDetails,
      List<Product> productWithoutMasterDataDetails, String username, String requestId) {
    Set<String> categoryCodeSet = new HashSet<>();
    Map<String, List<String>> mapOfProductToCategoryCodeList = new HashMap<>();
    if (CollectionUtils.isNotEmpty(productWithMasterDataDetails)) {
      productWithMasterDataDetails.forEach(
          product -> CommonUtil.fetchCategoryCodeListFromProduct(product, categoryCodeSet,
              mapOfProductToCategoryCodeList));
    }
    if (CollectionUtils.isNotEmpty(productWithoutMasterDataDetails)) {
      productWithoutMasterDataDetails.forEach(
          product -> CommonUtil.fetchCategoryCodeList(product, categoryCodeSet, mapOfProductToCategoryCodeList,
              Constants.ALL));
    }
    if (CollectionUtils.isNotEmpty(categoryCodeSet)) {
      Map<String, List<ItemCatalogVO>> itemCategoryVOListMap =
          this.catalogService.getCategoryCodeToItemCatalogsMap(username, requestId, new ArrayList<>(categoryCodeSet));
      productWithMasterDataDetails.forEach(product -> CommonUtil.setItemCatalogs(product, itemCategoryVOListMap,
          mapOfProductToCategoryCodeList.get(product.getProductSku()), false));
      productWithoutMasterDataDetails.forEach(product -> CommonUtil.setItemCatalogs(product, itemCategoryVOListMap,
          mapOfProductToCategoryCodeList.get(product.getProductSku()), false));
    }
  }

  private List<ItemPriceVO> getProductPriceBySkus(String storeId, List<ItemSkuVO> itemSkuVOList, String channelToUse,
    String requestId, String username) throws Exception {
    List<ItemPriceVO> result = new ArrayList<>();

    List<String> onlineItemSkus = itemSkuVOList
        .stream()
        .filter(itemSkuVO -> !itemSkuVO.isInstantPickup())
        .map(ItemSkuVO::getItemSku)
        .collect(Collectors.toList());

    List<ItemSkuVO> offlineItemSkuVOList = itemSkuVOList
        .stream()
        .filter(itemSkuVO -> itemSkuVO.isInstantPickup())
        .collect(Collectors.toList());

    result.addAll(this.getProductPriceByOfflineSkus(storeId, offlineItemSkuVOList, requestId, username));
    result.addAll(this.getProductPriceByOnlineSkus(storeId, onlineItemSkus, channelToUse));

    return result;
  }

  private List<ItemPriceVO> getProductPriceByOfflineSkus(String storeId, List<ItemSkuVO> offlineItemSkuVOList,
      String requestId, String username) throws Exception {
    List<ItemPriceVO> result = new ArrayList<>();
    Map<String, Boolean> itemMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(offlineItemSkuVOList)) {
      List<String> offlineItemSkus =
          offlineItemSkuVOList.stream().map(ItemSkuVO::getItemSku).collect(Collectors.toList());
      List<Item> items = this.itemService.getItemPriceAndViewConfigs(storeId, offlineItemSkus);
      itemMap = items.stream().collect(Collectors.toMap(Item::getItemSku, Item::isArchived));
    }

    List<ItemPickupPoint> offlineItemList =
        this.validateOfflineItemMapping(storeId, requestId, username, offlineItemSkuVOList);
    if (CollectionUtils.isNotEmpty(offlineItemList)) {
      for (ItemPickupPoint offlineItem : offlineItemList) {
        Price price = offlineItem.getPrice().iterator().next();
        result.add(new ItemPriceVO.ItemPriceBuilder().setBuyable(!itemMap.get(offlineItem.getItemSku()))
            .setItemSku(offlineItem.getItemSku())
            .setListPrice(Optional.ofNullable(price.getListPrice()).orElse(price.getOfferPrice()))
            .setOfferPrice(price.getOfferPrice()).setPickupPointCode(offlineItem.getPickupPointCode()).build());
      }
    }
    return result;
  }

  private List<ItemPriceVO> getProductPriceByOnlineSkus(String storeId, List<String> itemSkuUsingOnlineData,
    String channelToUse) throws Exception {
    List<ItemPriceVO> result = new ArrayList<>();

    List<Item> items = this.itemService.getItemPriceAndViewConfigs(storeId, itemSkuUsingOnlineData);
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.findByItemSkuInAndDelivery(storeId, itemSkuUsingOnlineData, true);
    Map<String, ItemPickupPoint> itemPickupPointMap =
        itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity()));

    if (CollectionUtils.isNotEmpty(items)) {
      for (Item item : items) {
        ItemPickupPoint itemPickupPoint = itemPickupPointMap.get(item.getItemSku());
        Price price = this.productHelperService.getRelevantItemPrice(itemPickupPoint.getPrice(), channelToUse);
        result.add(new ItemPriceVO.ItemPriceBuilder().setBuyable(this.productHelperService
            .getCurrentBuyableStatusForItem(itemPickupPoint.getItemViewConfig(), channelToUse, item.isArchived()))
            .setItemSku(item.getItemSku()).setOfferPrice(price.getOfferPrice()).setListPrice(price.getListPrice())
            .build());
      }
    }
    return result;
  }

  private List<ItemPickupPoint> validateOfflineItemMapping(String storeId, String requestId, String username,
    List<ItemSkuVO> offlineItemSkuVos) throws Exception {

    return offlineItemSkuVos.stream().map(offlineItemSkuVo -> this.itemPickupPointService
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId, offlineItemSkuVo.getItemSku(),
            offlineItemSkuVo.getPickupPointCode())).filter(Objects::nonNull).collect(Collectors.toList());
  }

}
