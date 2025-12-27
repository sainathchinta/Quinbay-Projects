package com.gdn.x.product.service.api;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ItemVo;

public interface ItemPriceService {

  Map<String, Set<Price>> getDiscountItemPickupPoint(Collection<ItemPickupPoint> itemPickupPoints);

  /**
   * Validate merchantPromoDiscountPrice based on start and end dates
   * @param merchantPromoDiscountPrice
   * @return
   */
  boolean validMerchantPromoDiscountPrice(DiscountPrice merchantPromoDiscountPrice);

  /**
   * @param price must not be null
   * @param discountPercentage must not be blank
   * @return final price
   */
  double getFinalPrice(Price price, double discountPercentage);

  /**
   * Get final price with minimum price parameter
   *
   * @param price
   * @param discountPercentage
   * @param minimumPrice
   * @return
   */
  double getFinalPriceWithMinimumPriceParameter(Price price, double discountPercentage,
    double minimumPrice);

  /**
   * Publish an event on price change
   * @param username
   * @param product
   * @param item
   */
  void publishItemPriceChangeEvent(String username, String requestId, Product product, Item item);

  /**
   * Publish an event on price change
   * @param username
   * @param product
   * @param itemVo
   */
  void publishItemPriceChangeEvent(String username, String requestId, Product product, ItemVo itemVo);

  /**
   * Publish an event on price change
   * @param product
   * @param itemPickupPoints
   * @param itemMap
   */
  void publishItemPriceChangeEvent(Product product, List<ItemPickupPoint> itemPickupPoints, Map<String, Item> itemMap);

  /**
   * set discount price from x promotion api
   * @param username
   * @param requestId
   * @param items
   */
  void getAndSetPromotionPrice(String username, String requestId, List<Item> items);

  /**
   * validate and set discount price for list of items
   * @param items
   */
  void validateAndSetDiscountPrice(List<Item> items);

  /**
   *
   * @param itemPickupPoints
   * @return
   */
  Map<String, Set<Price>> getDiscountItemPickupPoints(Collection<ItemPickupPoint> itemPickupPoints);

  /**
   * get original selling price
   * @param itemPickupPoints
   * @return
   */
  Map<String, Double> getOriginalSellingPrice(List<ItemPickupPoint> itemPickupPoints);
}

