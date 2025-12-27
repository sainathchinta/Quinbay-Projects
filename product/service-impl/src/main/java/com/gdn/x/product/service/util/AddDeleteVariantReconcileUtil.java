package com.gdn.x.product.service.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.springframework.beans.BeanUtils;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ProductCollectionsVo;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

public class AddDeleteVariantReconcileUtil {

  public static List<ItemPickupPoint> deleteExtraItemsAndItemPickupPoints(ProductDetailResponse productDetailResponse,
      ProductCollectionsVo productCollectionsVo) {
    Set<String> nonDeleteItemsFromPCB = getNonDeletedItemsFromPCB(productDetailResponse);
    List<ItemPickupPoint> deletedItemPickupPoints = new ArrayList<>();

    if (CollectionUtils.isNotEmpty(nonDeleteItemsFromPCB) && CollectionUtils.isNotEmpty(
        productCollectionsVo.getItems())) {
      Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointMap =
          groupItemPickupPointByItemSku(productCollectionsVo);
      for (Item item : productCollectionsVo.getItems()) {
        if (!nonDeleteItemsFromPCB.contains(item.getItemCode())) {
          List<ItemPickupPoint> itemPickupPointOfDeletedItem = deleteItem(item, itemSkuAndItemPickupPointMap);
          deletedItemPickupPoints.addAll(itemPickupPointOfDeletedItem);
        }
      }
    }

    return deletedItemPickupPoints;
  }

  public static List<ItemPickupPoint> createMissingItemsAndItemPickupPoints(ProductDetailResponse productDetailResponse,
      ProductCollectionsVo productCollectionsVo, AddDeleteVariantRetryRequest addDeleteVariantRetryRequest) {
    Map<String, ItemActivationRequest> itemCodeAndItemActivationRequestMap =
        getItemCodeAndItemActivationRequestMapping(addDeleteVariantRetryRequest);
    Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointMap =
        groupItemPickupPointByItemSku(productCollectionsVo);
    Map<String, Item> itemCodeAndItemMapOfXProduct =
        getNonPermanentDeletedItemsFromXProduct(productCollectionsVo.getItems());
    List<ProductItemResponse> nonDeleteItemsFromPCB = getNonDeletedItemListFromPCB(productDetailResponse);

    List<ItemPickupPoint> newLyAddedItemPickupPoint = new ArrayList<>();

    if (CollectionUtils.isNotEmpty(nonDeleteItemsFromPCB) && MapUtils.isNotEmpty(itemCodeAndItemActivationRequestMap)) {
      for (ProductItemResponse productItemResponse : nonDeleteItemsFromPCB) {
        List<ItemPickupPoint> newItemPickupPoints = new ArrayList<>();
        if (itemCodeAndItemMapOfXProduct.containsKey(productItemResponse.getSkuCode())) {
          //check and create item pickup points
          newItemPickupPoints = createItemPickupPints(productCollectionsVo, itemCodeAndItemActivationRequestMap,
              itemSkuAndItemPickupPointMap, productItemResponse,
              itemCodeAndItemMapOfXProduct.get(productItemResponse.getSkuCode()));
        } else if (itemCodeAndItemActivationRequestMap.containsKey(productItemResponse.getSkuCode())) {
          newItemPickupPoints =
              createNewMissingVariantItemsAndPickupPoint(productCollectionsVo, itemCodeAndItemActivationRequestMap,
                  itemSkuAndItemPickupPointMap, productItemResponse);
        }
        productCollectionsVo.getItemPickupPoints().addAll(newItemPickupPoints);
        newLyAddedItemPickupPoint.addAll(newItemPickupPoints);
      }
    }

    return newLyAddedItemPickupPoint;
  }

  public static void setCncAndOnlineAndB2cAndB2bActivatedAndPickupPointCodesFlag(
      ProductCollectionsVo productCollectionsVo) {
    Map<String, List<ItemPickupPoint>> itemPickupPointMap = groupItemPickupPointByItemSku(productCollectionsVo);
    Product product = productCollectionsVo.getProducts().stream().findFirst().get();
    for (Item item : productCollectionsVo.getItems()) {
      for (ItemPickupPoint itemPickupPoint : Optional.ofNullable(itemPickupPointMap.get(item.getItemSku()))
          .orElse(new ArrayList<>())) {
        updateItemAndProductWhenItemPickupPointIsCnc(product, item, itemPickupPoint);
        updateItemAndProductWhenItemPickupPointIsOnline(product, itemPickupPoint);
        updateProductWhenItemPickupPointIsB2BChannel(product, itemPickupPoint);
        product.getPickupPointCodes().add(itemPickupPoint.getPickupPointCode());
      }
    }
  }

  private static List<ItemPickupPoint> createNewMissingVariantItemsAndPickupPoint(ProductCollectionsVo productCollectionsVo,
      Map<String, ItemActivationRequest> itemCodeAndItemActivationRequestMap,
      Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointMap, ProductItemResponse productItemResponse) {
    //create new items
    Item newItem = createItem(itemCodeAndItemActivationRequestMap.get(productItemResponse.getSkuCode()),
        productCollectionsVo.getProducts().stream().findFirst().get(), productItemResponse);
    productCollectionsVo.getItems().add(newItem);

    //check and create item pickup points
    List<ItemPickupPoint> newItemPickupPoints =
        createItemPickupPints(productCollectionsVo, itemCodeAndItemActivationRequestMap, itemSkuAndItemPickupPointMap,
            productItemResponse, newItem);

    return newItemPickupPoints;
  }

  private static void updateProductWhenItemPickupPointIsB2BChannel(Product product, ItemPickupPoint itemPickupPoint) {
    ItemViewConfig b2bItemViewConfig = itemPickupPoint.getAllItemViewConfigs().stream()
        .filter(itemViewConfig -> Constants.B2B.equals(itemViewConfig.getChannel())).findFirst().orElse(null);
    if (Objects.nonNull(b2bItemViewConfig) && (b2bItemViewConfig.isBuyable() || b2bItemViewConfig.isDiscoverable())) {
      product.setB2bActivated(true);
    }
  }

  private static void updateItemAndProductWhenItemPickupPointIsOnline(Product product,
      ItemPickupPoint itemPickupPoint) {
    ItemViewConfig itemViewConfig =
        itemPickupPoint.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig());
    if (itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable()) {
      product.setOnline(true);
      product.setB2cActivated(true);
    }
  }

  private static void updateItemAndProductWhenItemPickupPointIsCnc(Product product, Item item,
      ItemPickupPoint itemPickupPoint) {
    if (itemPickupPoint.isCncActive()) {
      product.setCncActivated(true);
      item.setCncActivated(true);
      product.setB2cActivated(true);
    }
  }

  private static List<ItemPickupPoint> createItemPickupPints(ProductCollectionsVo productCollectionsVo,
      Map<String, ItemActivationRequest> itemCodeAndItemActivationRequestMap,
      Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointMap, ProductItemResponse productItemResponse,
      Item newItem) {
    if (!itemSkuAndItemPickupPointMap.containsKey(newItem.getItemSku())) {
      List<ItemPickupPoint> newItemPickupPoints =
          createItemPickupPoints(itemCodeAndItemActivationRequestMap.get(productItemResponse.getSkuCode()),
              productCollectionsVo.getProducts().stream().findFirst().get());
      return newItemPickupPoints;
    }
    return new ArrayList<>();
  }

  private static Set<String> getNonDeletedItemsFromPCB(ProductDetailResponse productDetailResponse) {
    return Optional.ofNullable(productDetailResponse.getProductItemResponses()).orElse(new HashSet<>()).stream()
        .filter(Predicate.not(ProductItemResponse::isMarkForDelete)).map(ProductItemResponse::getSkuCode)
        .collect(Collectors.toSet());
  }

  private static List<ProductItemResponse> getNonDeletedItemListFromPCB(ProductDetailResponse productDetailResponse) {
    return Optional.ofNullable(productDetailResponse.getProductItemResponses()).orElse(new HashSet<>()).stream()
        .filter(Predicate.not(ProductItemResponse::isMarkForDelete)).collect(Collectors.toList());
  }

  private static Map<String, Item> getNonPermanentDeletedItemsFromXProduct(List<Item> items) {
    return items.stream().filter(Predicate.not(Item::isPermanentDelete))
        .collect(Collectors.toMap(Item::getItemCode, Function.identity(), (v1, v2) -> v2));
  }

  private static Map<String, List<ItemPickupPoint>> groupItemPickupPointByItemSku(
      ProductCollectionsVo productCollectionsVo) {
    return Optional.ofNullable(productCollectionsVo.getItemPickupPoints()).orElse(new ArrayList<>()).stream()
        .collect(Collectors.groupingBy(ItemPickupPoint::getItemSku));
  }

  private static Map<String, ItemActivationRequest> getItemCodeAndItemActivationRequestMapping(
      AddDeleteVariantRetryRequest addDeleteVariantRetryRequest) {
    return Optional.ofNullable(addDeleteVariantRetryRequest.getItemActivationRequestList()).orElse(new ArrayList<>())
        .stream().filter(Predicate.not(ItemActivationRequest::isMarkForDelete))
        .collect(Collectors.toMap(ItemActivationRequest::getItemCode, Function.identity(), (v1, v2) -> v2));
  }

  private static List<ItemPickupPoint> deleteItem(Item item,
      Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointMap) {
    item.setMarkForDelete(true);
    item.setPermanentDelete(true);
    item.getItemChangeEventTypes().add(ItemChangeEventType.ITEM_DELETED);
    return deleteAllItemsPickupPointsOfAItem(item.getItemSku(), itemSkuAndItemPickupPointMap);
  }

  private static List<ItemPickupPoint> deleteAllItemsPickupPointsOfAItem(String itemSku,
      Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointMap) {
    if (itemSkuAndItemPickupPointMap.containsKey(itemSku)) {
      List<ItemPickupPoint> itemPickupPoints = itemSkuAndItemPickupPointMap.get(itemSku);
      CommonUtil.deletePickupPointsPromoAndDiscountDetails(itemPickupPoints);
      deleteItemPickupPoint(itemPickupPoints);
      return itemPickupPoints;
    } else {
      return new ArrayList<>();
    }
  }

  private static void deleteItemPickupPoint(List<ItemPickupPoint> itemPickupPoints) {
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
      itemPickupPoint.setMarkForDelete(true);
      itemPickupPoint.setCncActive(false);
      itemPickupPoint.setFbbActivated(false);
    }
  }

  private static Item createItem(ItemActivationRequest itemActivationRequest, Product product,
      ProductItemResponse productItemResponse) {
    Item item = new Item();
    BeanUtils.copyProperties(itemActivationRequest, item);
    item.setProductSku(product.getProductSku());
    item.setStoreId(product.getStoreId());
    item.setSynchronized(true);
    item.setItemCatentryId(item.getItemSku());
    item.setMerchantCode(product.getMerchantCode());
    item.setItemCode(productItemResponse.getSkuCode());
    item.setSourceItemCode(productItemResponse.getSourceItemCode());
    item.setContentChanged(productItemResponse.isContentChanged());
    return item;
  }

  private static List<ItemPickupPoint> createItemPickupPoints(ItemActivationRequest itemActivationRequest,
      Product product) {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    for (ItemPickupPointActivationRequest itemPickupPointActivationRequest : itemActivationRequest.getItemPickupPoints()) {
      ItemPickupPoint itemPickupPoint = ItemPickupPoint.builder().offlineItemId(
              CommonUtil.generateOfflineItemId(itemActivationRequest.getItemSku(),
                  itemPickupPointActivationRequest.getPickupPointCode())).itemSku(itemActivationRequest.getItemSku())
          .merchantSku(itemActivationRequest.getMerchantSku())
          .pickupPointCode(itemPickupPointActivationRequest.getPickupPointCode())
          .merchantCode(product.getMerchantCode()).cncActive(itemPickupPointActivationRequest.isCncActive())
          .productSku(product.getProductSku()).fbbActivated(itemPickupPointActivationRequest.isFbbActivated()).build();

      //update price, item view config, b2b fields
      itemPickupPoint.setStoreId(product.getStoreId());
      itemPickupPoint.setPrice(getPriceFromRequest(itemPickupPointActivationRequest));
      itemPickupPoint.setItemViewConfig(
          getItemViewConfigsFromRequest(itemActivationRequest, itemPickupPointActivationRequest));
      itemPickupPoint.setB2bFields(getB2bFieldsFromRequest(itemPickupPointActivationRequest));

      itemPickupPointList.add(itemPickupPoint);
    }
    return itemPickupPointList;
  }

  private static B2bFields getB2bFieldsFromRequest(ItemPickupPointActivationRequest itemPickupPointActivationRequest) {
    if (Objects.nonNull(itemPickupPointActivationRequest.getB2bFields())) {
      B2bFields b2bFields = new B2bFields();
      BeanUtils.copyProperties(itemPickupPointActivationRequest.getB2bFields(), b2bFields);
      return b2bFields;
    } else {
      return null;
    }
  }

  private static Set<ItemViewConfig> getItemViewConfigsFromRequest(ItemActivationRequest itemActivationRequest,
      ItemPickupPointActivationRequest itemPickupPointActivationRequest) {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    for (ItemViewConfigRequest itemViewConfigRequest : itemPickupPointActivationRequest.getItemViewConfigs()) {
      ItemViewConfig itemViewConfig = new ItemViewConfig();
      BeanUtils.copyProperties(itemViewConfigRequest, itemViewConfig);
      itemViewConfigs.add(itemViewConfig);
    }
    return itemViewConfigs;
  }

  private static Set<Price> getPriceFromRequest(ItemPickupPointActivationRequest itemPickupPointActivationRequest) {
    Set<Price> prices = new HashSet<>();
    for (PriceRequest priceRequest : itemPickupPointActivationRequest.getPrice()) {
      Price price = new Price();
      BeanUtils.copyProperties(priceRequest, price);
      prices.add(price);
    }
    return prices;
  }

}
