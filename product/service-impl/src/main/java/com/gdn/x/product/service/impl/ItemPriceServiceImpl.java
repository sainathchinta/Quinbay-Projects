package com.gdn.x.product.service.impl;

import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemPriceChangeEventModel;
import com.gdn.x.product.domain.event.model.ParentCategory;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.AdjustmentType;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.response.AdjustmentProductResponse;
import com.gdn.x.product.model.vo.ItemDiscountPriceVO;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.outbound.api.PromotionOutbound;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
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
public class ItemPriceServiceImpl implements ItemPriceService {

  private static final String STORE_ID = "10001";
  private static final double PERCENT = 100.0;
  private static final Logger LOG = LoggerFactory.getLogger(ItemPriceServiceImpl.class);

  @Autowired
  private PromotionOutbound promotionOutbound;

  @Autowired
  private ItemHelperService itemHelperService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private CachedService cachedService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  private Map<String, Set<Price>> convertToMapOfPrice(List<Item> items) {
    Map<String, Set<Price>> mapOfPrices = new HashMap<String, Set<Price>>();
    for (Item item : items) {
      mapOfPrices.put(item.getItemSku(), item.getPrice());
    }
    return mapOfPrices;
  }

  private List<Item> getAndSetDiscountPrice(List<Item> items) {
    items.forEach(item -> {
      item.getPrice().iterator().next().setListOfDiscountPrices(
          item.getPrice().stream().findFirst().map(Price::getListOfDiscountPrices)
              .map(this::validDiscountPrice).orElse(Collections.emptyList()));
      ItemDiscountPriceVO itemDiscountPriceVO = new ItemDiscountPriceVO();
      if (item.isMerchantPromoDiscount()) {
        itemDiscountPriceVO.setDiscountPrice(
            item.getPrice().stream().findFirst().map(Price::getMerchantPromoDiscountPrice).orElse(null));
      }
      itemDiscountPriceVO.setMerchantPromoDiscount(item.isMerchantPromoDiscount());
      setMerchantPromoDiscountPrice(item, itemDiscountPriceVO);
    });
    return items;
  }

  private List<DiscountPrice> validDiscountPrice(List<DiscountPrice> discountPrices) {
    DiscountPrice discountPrice = itemHelperService.processDiscountPricesByPriority(discountPrices);
    if (Objects.nonNull(discountPrice)) {
      return new ArrayList<>(Arrays.asList(discountPrice));
    }
    return new ArrayList<>();
  }

  private void setMerchantPromoDiscountPrice(Item item, ItemDiscountPriceVO itemDiscountPriceVO) {
    if (Objects.nonNull(itemDiscountPriceVO)) {
      if (item.isMerchantPromoDiscount() && Objects.nonNull(itemDiscountPriceVO.getDiscountPrice())) {
        item.getPrice().forEach(price -> {
          boolean isValidDiscountPrice = validMerchantPromoDiscountPrice(price.getMerchantPromoDiscountPrice());
          if (isValidDiscountPrice) {
            price.setOfferPrice(price.getMerchantPromoDiscountPrice().getDiscountPrice());
            item.setMerchantPromoDiscountActive(true);
            price.setMerchantPromoDiscountPrice(itemDiscountPriceVO.getDiscountPrice());
          } else {
            item.setMerchantPromoDiscountActive(false);
            price.setMerchantPromoDiscountPrice(null);
          }
        });
      }
      item.setMerchantPromoDiscount(itemDiscountPriceVO.isMerchantPromoDiscount());
    }
  }

  @Override
  public boolean validMerchantPromoDiscountPrice(DiscountPrice merchantPromoDiscountPrice) {
    Date now = new Date();
    return (merchantPromoDiscountPrice.getStartDateTime().before(now) && merchantPromoDiscountPrice.getEndDateTime()
        .after(now));
  }

  @Override
  public Map<String, Set<Price>> getDiscountItemPickupPoint(Collection<ItemPickupPoint> itemPickupPoints) {
    getAndSetDiscountPriceForItemPickupPoint(itemPickupPoints);
    Map<String, Set<Price>> mapOfPrices = new HashMap<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
      mapOfPrices.put(itemPickupPoint.getItemSku(), itemPickupPoint.getPrice());
    }
    return mapOfPrices;
  }

  private void getAndSetDiscountPriceForItemPickupPoint(Collection<ItemPickupPoint> itemPickupPoints) {
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
      itemPickupPoint.getPrice().iterator().next().setListOfDiscountPrices(
          itemPickupPoint.getPrice().stream().findFirst().map(Price::getListOfDiscountPrices)
              .map(this::validDiscountPrice).orElse(Collections.emptyList()));
      Optional.ofNullable(itemPickupPoint.getPrice()).orElse(Collections.emptySet()).stream()
          .findFirst().ifPresent(price -> price.setOriginalSellingPrice(price.getOfferPrice()));
      if (itemPickupPoint.isMerchantPromoDiscount()) {
        DiscountPrice discountPrice =
            itemPickupPoint.getPrice().stream().findFirst().map(Price::getMerchantPromoDiscountPrice).orElse(null);
        if (Objects.nonNull(discountPrice)) {
          for (Price price : itemPickupPoint.getPrice()) {
            boolean isValidDiscountPrice = validMerchantPromoDiscountPrice(price.getMerchantPromoDiscountPrice());
            if (isValidDiscountPrice) {
              price.setOfferPrice(price.getMerchantPromoDiscountPrice().getDiscountPrice());
              price.setMerchantPromoDiscountPrice(discountPrice);
            } else {
              price.setMerchantPromoDiscountPrice(null);
            }
          }
        }
      }
    }
  }

  @Override
  public double getFinalPrice(Price price, double discountPercentage){
    double offerPrice = price.getOfferPrice();
    double discountPrice = itemHelperService.getDiscountPrice(price);
    double finalPrice =
        offerPrice - Math.round(offerPrice * discountPercentage / PERCENT) - discountPrice;
    double minFinalPrice = Double.parseDouble(systemParameterService
        .findValueByStoreIdAndVariable(ItemPriceServiceImpl.STORE_ID,
            SystemParameterNames.MINIMUM_PRICE).getValue());

    return Math.max(finalPrice, minFinalPrice);
  }

  @Override
  public double getFinalPriceWithMinimumPriceParameter(Price price, double discountPercentage,
    double minimumPrice) {
    double offerPrice = price.getOfferPrice();
    double discountPrice = itemHelperService.getDiscountPrice(price);
    double finalPrice =
      offerPrice - Math.round(offerPrice * discountPercentage / PERCENT) - discountPrice;
    return Math.max(finalPrice, minimumPrice);
  }

  @Async
  @Override
  public void publishItemPriceChangeEvent(String username, String requestId, Product product, Item item) {
    List<List<CategoryResponse>> categoryResponses = getParentCategoryResponse(requestId, username, product);
    if (CollectionUtils.isNotEmpty(categoryResponses)) {
      categoryResponses.stream().findFirst()
          .map(categoryResponseList -> categoryResponseList.get(categoryResponseList.size() - 1))
          .map(categoryResponse -> new ParentCategory(categoryResponse.getCategoryCode(), categoryResponse.getName()))
          .map(parentCategory -> convertToItemPriceChangeEventModel(product.getProductCode(), item, parentCategory))
          .map(itemPriceChangeEventModel -> {
            LOG.info("Publishing event : {}, Publish item price change : {}",
                ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME, itemPriceChangeEventModel);
            this.kafkaPublisher.send(ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME,
              itemPriceChangeEventModel.getItemSku(), itemPriceChangeEventModel);
            return itemPriceChangeEventModel;
          });
    }
  }

  @Override
  @Async
  public void publishItemPriceChangeEvent(String username, String requestId, Product product, ItemVo itemVo) {
    List<List<CategoryResponse>> categoryResponses = getParentCategoryResponse(requestId, username, product);
    if (CollectionUtils.isNotEmpty(categoryResponses)) {
      CategoryResponse categoryResponse = categoryResponses.get(0).get(categoryResponses.get(0).size() - 1);
      ParentCategory parentCategory =
          new ParentCategory(categoryResponse.getCategoryCode(), categoryResponse.getName());
      List<ItemPriceChangeEventModel> itemPriceChangeEventModelList =
          convertToItemPriceChangeEventModel(product.getProductCode(), product, parentCategory, itemVo);
      for (ItemPriceChangeEventModel itemPriceChangeEventModel : itemPriceChangeEventModelList) {
        LOG.info("Publishing event : {}, Publish item price change : {}",
            ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME, itemPriceChangeEventModel);
        this.kafkaPublisher.send(ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME,
          itemPriceChangeEventModel.getItemSku(), itemPriceChangeEventModel);
      }
    }
  }

  @Override
  @Async
  public void publishItemPriceChangeEvent(Product product, List<ItemPickupPoint> itemPickupPoints,
    Map<String, Item> itemMap) {
    CategoryResponse categoryResponse;
    List<List<CategoryResponse>> categoryResponses =
      getParentCategoryResponse(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, product);
    Optional<CategoryResponse> optionalCategoryResponse =
      Optional.ofNullable(categoryResponses).filter(list -> !list.isEmpty())
        .map(list -> list.get(0)).filter(innerList -> !innerList.isEmpty())
        .map(innerList -> innerList.get(innerList.size() - 1));
    if (optionalCategoryResponse.isPresent()) {
      categoryResponse = optionalCategoryResponse.get();
      List<ItemPriceChangeEventModel> itemPriceChangeEventModels =
        CommonUtil.convertToItemPriceChangeEventModel(product.getProductCode(),
          new ParentCategory(categoryResponse.getCategoryCode(), categoryResponse.getName()),
          product, itemPickupPoints, itemMap);
      for (ItemPriceChangeEventModel itemPriceChangeEventModel : itemPriceChangeEventModels) {
        LOG.info("Publishing event : {}, Publish item price change : {}",
          ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME, itemPriceChangeEventModel);
        this.kafkaPublisher.send(ProductDomainEventName.ITEM_PRICE_CHANGE_EVENT_NAME,
          itemPriceChangeEventModel.getItemSku(), itemPriceChangeEventModel);
      }
    } else {
      LOG.error("Category response fetch failed , skipping item price change for ; {} ",
        product.getProductSku());
    }
  }

  private List<List<CategoryResponse>> getParentCategoryResponse(String requestId, String username, Product product) {
    List<List<CategoryResponse>> categoryResponses = null;
    try {
      if (Objects.nonNull(product.getMasterCatalog()) && Objects.nonNull(product.getMasterCatalog().getCategory())) {
        categoryResponses = productHelperService.getCategoryResponseListByCategoryCodesForProducts(requestId, username,
            Arrays.asList(product.getMasterCatalog().getCategory().getCategoryCode()));
      } else {
        if (StringUtils.isNotBlank(product.getProductCode())) {
          List<CategoryResponse> response =
              cachedService.getMasterParentCategoryResponseByProductCode(requestId, username, product.getProductCode());
          categoryResponses = new ArrayList();
          categoryResponses.add(response);
        }
      }
    } catch (Exception e) {
      LOG.error("Error in getting parentCategory for productCode :{}, MasterCatalog: {}", product.getProductCode(),
          product.getMasterCatalog(), e);
    }
    return categoryResponses;
  }

  private ItemPriceChangeEventModel convertToItemPriceChangeEventModel(String productCode,
      Item item, ParentCategory parentCategory) {
    ItemPriceChangeEventModel itemPriceChangeEventModel = new ItemPriceChangeEventModel();
    itemPriceChangeEventModel.setParentCategory(parentCategory);
    itemPriceChangeEventModel.setProductCode(productCode);
    itemPriceChangeEventModel.setMerchantCode(item.getMerchantCode());
    itemPriceChangeEventModel.setItemCode(item.getItemCode());
    itemPriceChangeEventModel.setPristineId(
        Objects.nonNull(item.getPristineDataItem()) ? item.getPristineDataItem().getPristineId() : StringUtils.EMPTY);
    itemPriceChangeEventModel.setItemSku(item.getItemSku());
    itemPriceChangeEventModel.setArchived(item.isArchived());
    itemPriceChangeEventModel.setMarkForDelete(item.isMarkForDelete());
    itemPriceChangeEventModel.setStoreId(item.getStoreId());
    Set<com.gdn.x.product.domain.event.model.Price> eventPriceSet = new HashSet<>();
    for (Price price : item.getPrice()) {
      com.gdn.x.product.domain.event.model.Price eventPrice =
        new com.gdn.x.product.domain.event.model.Price();
        setPriceForPriceChangeEvent(price, eventPrice);
      eventPriceSet.add(eventPrice);
    }
    itemPriceChangeEventModel.setPrice(eventPriceSet);
    return itemPriceChangeEventModel;
  }

  private void setPriceForPriceChangeEvent(Price price,
    com.gdn.x.product.domain.event.model.Price eventPrice) {
    org.springframework.beans.BeanUtils.copyProperties(price, eventPrice, "listOfDiscountPrices",
      "merchantPromoDiscountPrice");
    List<com.gdn.x.product.domain.event.model.DiscountPrice> discountPriceList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(price.getListOfDiscountPrices())) {
      for (DiscountPrice itemDiscountPrice : price.getListOfDiscountPrices()) {
        com.gdn.x.product.domain.event.model.DiscountPrice eventDiscountPrice =
          new com.gdn.x.product.domain.event.model.DiscountPrice();
        org.springframework.beans.BeanUtils.copyProperties(itemDiscountPrice, eventDiscountPrice);
        discountPriceList.add(eventDiscountPrice);
      }
    }
    if (Objects.nonNull(price.getMerchantPromoDiscountPrice())) {
      com.gdn.x.product.domain.event.model.DiscountPrice merchantPromoDiscount =
        new com.gdn.x.product.domain.event.model.DiscountPrice();
      org.springframework.beans.BeanUtils.copyProperties(price.getMerchantPromoDiscountPrice(),
        merchantPromoDiscount);
      eventPrice.setMerchantPromoDiscountPrice(merchantPromoDiscount);
    }
    LOG.info("Discount Price list for ItemPriceChangeEventModel is {} ", discountPriceList);
    eventPrice.setListOfDiscountPrices(discountPriceList);
  }

  private List<ItemPriceChangeEventModel> convertToItemPriceChangeEventModel(String productCode, Product product,
      ParentCategory parentCategory, ItemVo itemVo) {
    List<ItemPriceChangeEventModel> itemPriceChangeEventModels = new ArrayList();
    for (ItemPickupPointVo itemPickupPointVo : itemVo.getItemPickupPointVoList()) {
      ItemPriceChangeEventModel itemPriceChangeEventModel = new ItemPriceChangeEventModel();
      itemPriceChangeEventModel.setParentCategory(parentCategory);
      itemPriceChangeEventModel.setProductCode(productCode);
      itemPriceChangeEventModel.setMerchantCode(itemVo.getMerchantCode());
      itemPriceChangeEventModel.setItemCode(itemVo.getItemCode());
      itemPriceChangeEventModel.setPristineId(Objects.nonNull(itemVo.getPristineDataItem()) ?
          itemVo.getPristineDataItem().getPristineId() :
          StringUtils.EMPTY);
      itemPriceChangeEventModel.setItemSku(itemPickupPointVo.getItemSku());
      itemPriceChangeEventModel.setArchived(product.isArchived());
      itemPriceChangeEventModel.setMarkForDelete(itemVo.isMarkForDelete());
      itemPriceChangeEventModel.setStoreId(itemVo.getStoreId());
      Set<com.gdn.x.product.domain.event.model.Price> prices = new HashSet<>();
      for (Price itemPrice : itemPickupPointVo.getPrice()) {
        com.gdn.x.product.domain.event.model.Price price = new com.gdn.x.product.domain.event.model.Price();
        org.springframework.beans.BeanUtils.copyProperties(itemPrice, price);
        prices.add(price);
      }
      itemPriceChangeEventModel.setPrice(prices);
      itemPriceChangeEventModels.add(itemPriceChangeEventModel);
    }
    return itemPriceChangeEventModels;
  }

  @Override
  public void getAndSetPromotionPrice(String requestId, String username, List<Item> items) {
    Map<String, Item> mapOfItems =
        items.stream().collect(Collectors.toMap(item -> item.getItemSku(), Function.identity()));
    List<AdjustmentProductResponse> adjustmentProductResponses =
        this.promotionOutbound.getAdjustmentProduct(requestId, username, new ArrayList<>(mapOfItems.keySet()));
    Map<String, List<AdjustmentProductResponse>> adjustmentMap =
        adjustmentProductResponses.stream().filter(AdjustmentProductResponse::isActivated)
            .collect(Collectors.groupingBy(AdjustmentProductResponse::getProductSku));
    adjustmentMap.keySet()
        .forEach(itemSku -> {
          setItemDiscountPrice(mapOfItems, adjustmentMap.get(itemSku), itemSku);
          LOG.info("promotion price of item with Sku : {} updated", itemSku);
        });
  }

  @Override
  public void validateAndSetDiscountPrice(List<Item> items) {
    items.forEach(item -> {
      item.getPrice().iterator().next().setListOfDiscountPrices(
        item.getPrice().stream().findFirst().map(Price::getListOfDiscountPrices)
          .map(this::validDiscountPrice).orElse(Collections.emptyList()));
    });
  }

  /**
   * set item discount prices
   *
   * @param mapOfItems map of items
   * @param adjustmentProductResponse discount price response from x promotion
   */
  private void setItemDiscountPrice(Map<String, Item> mapOfItems,
      List<AdjustmentProductResponse> adjustmentProductResponse, String itemSku) {
    Item item = mapOfItems.get(itemSku);
    if (Objects.nonNull(item)) {
      item.getPrice().forEach(price -> getSetListOfDiscountPrices(adjustmentProductResponse, price));
    }
  }

  private void getSetListOfDiscountPrices(List<AdjustmentProductResponse> adjustmentProductResponse, Price price) {
    DiscountPrice discountPrice = itemHelperService.processDiscountPricesByPriority(
        adjustmentProductResponse.stream().map(this::getDiscountPrice).collect(Collectors.toList()));
    if (Objects.nonNull(discountPrice)) {
      price.setListOfDiscountPrices(new ArrayList<>(Arrays.asList(discountPrice)));
    }
  }

  private DiscountPrice getDiscountPrice(AdjustmentProductResponse adjustmentProductResponse) {
    return new DiscountPrice(adjustmentProductResponse.getValue(), adjustmentProductResponse.getStartDate(),
        adjustmentProductResponse.getEndDate(), adjustmentProductResponse.getAdjustmentName(),
        AdjustmentType.BLIBLI, adjustmentProductResponse.getPriority());
  }

  //creating map for mpp products with key as itemSku + ppcode
  @Override
  public Map<String, Set<Price>> getDiscountItemPickupPoints(
    Collection<ItemPickupPoint> itemPickupPoints) {
    getAndSetDiscountPriceForItemPickupPoint(itemPickupPoints);
    Map<String, Set<Price>> mapOfPrices = new HashMap<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
      mapOfPrices.put(itemPickupPoint.getItemSku() + Constants.DASH + itemPickupPoint.getPickupPointCode(),
          itemPickupPoint.getPrice());
    }
    return mapOfPrices;
  }

  @Override
  public Map<String, Double> getOriginalSellingPrice(List<ItemPickupPoint> itemPickupPoints) {
    return Optional.ofNullable(itemPickupPoints).orElse(new ArrayList<>()).stream()
        .collect(Collectors.toMap(itemPickupPoint -> CommonUtil.toOfflineItemId(itemPickupPoint.getItemSku(),
                itemPickupPoint.getPickupPointCode()), itemPickupPoint -> itemPickupPoint.getPrice()
                .stream().findFirst().orElse(new Price()).getOfferPrice(), (a, b) -> a));
  }

}