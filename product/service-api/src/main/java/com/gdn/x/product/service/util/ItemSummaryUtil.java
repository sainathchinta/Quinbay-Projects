package com.gdn.x.product.service.util;

import java.util.Map;
import java.util.Set;

import org.springframework.data.domain.Page;

import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;

/**
 * @author nitinmathew - created on 31/01/2020
 **/
public interface ItemSummaryUtil {

  /**
   * Construct Item Summary Response VO for promo items
   *
   * @param requestId
   * @param username
   * @param resultFromSolr
   * @param mapOfPrices
   * @param mapOfOriginalPrice
   * @param itemViewConfigMap
   * @return
   */
  ItemSummaryPageResponseVo constructItemSummaryResponseForPromoItems(String requestId, String username,
      Page<ProductAndItemSolr> resultFromSolr, Map<String, Set<Price>> mapOfPrices,
      Map<String, Double> mapOfOriginalPrice, Map<String, Set<ItemViewConfig>> itemViewConfigMap);

  /**
   * Extract Product Code from ItemCode
    * @param itemCode
   * @return
   */
  String getProductCodeFromItemCode(String itemCode);
}