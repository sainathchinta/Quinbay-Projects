package com.gdn.x.product.service.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.gdn.x.product.service.impl.ItemSummaryServiceImpl;

/**
 * @author nitinmathew - created on 31/01/2020
 **/
@Service
public class ItemSummaryUtilImpl implements ItemSummaryUtil {
  private static final Logger LOGGER = LoggerFactory.getLogger(ItemSummaryServiceImpl.class);

  @Autowired
  private ProductAndItemSolrConstructorService solrDataConstructor;

  @Autowired
  private ItemHelperService itemHelperService;

  @Override
  public ItemSummaryPageResponseVo constructItemSummaryResponseForPromoItems(String requestId, String username,
      Page<ProductAndItemSolr> resultFromSolr, Map<String, Set<Price>> mapOfPrices,
      Map<String, Double> mapOfOriginalPrice, Map<String, Set<ItemViewConfig>> itemViewConfigMap) {
    List<ItemSummaryResponseVO> itemSummaryResponses = new ArrayList<>();
    for (ProductAndItemSolr productAndItem : resultFromSolr.getContent()) {
      try {
        String pickupPointKey = productAndItem.getItemSku();
        Set<Price> prices = mapOfPrices.get(pickupPointKey);
        Set<ItemViewConfig> itemViewConfigs = itemViewConfigMap.get(pickupPointKey);
        Double originalPrice = mapOfOriginalPrice.get(pickupPointKey);

        ItemSummaryResponseVO itemSummaryResponse = new ItemSummaryResponseVO();
        itemSummaryResponse.setStoreId(productAndItem.getStoreId());
        itemSummaryResponse.setGeneratedItemName(productAndItem.getItemName());
        itemSummaryResponse.setItemCode(productAndItem.getItemCode());
        itemSummaryResponse.setItemSku(productAndItem.getItemSku());
        itemSummaryResponse.setMasterCatalog(this.solrDataConstructor
            .constructMasterCatalog(productAndItem.getMasterCatalog()));
        itemSummaryResponse.setCategoryName((Objects.nonNull(itemSummaryResponse.getMasterCatalog()) && Objects
            .nonNull(itemSummaryResponse.getMasterCatalog().getCategory())) ?
            itemHelperService.getCategoryNameByCategoryCode(requestId, username,
                itemSummaryResponse.getMasterCatalog().getCategory().getCategoryCode()) :
            Constants.HYPHEN);
        itemSummaryResponse.setMasterDataItemImages(this.solrDataConstructor
            .constructMasterDataItemMainImages(productAndItem.getItemImages()));
        itemSummaryResponse.setMerchantCode(productAndItem.getMerchantCode());
        itemSummaryResponse.setPrice(prices);
        itemSummaryResponse.setProductSku(productAndItem.getProductSku());
        itemSummaryResponse.setArchived(productAndItem.isArchived());
        itemSummaryResponse.setBrand(productAndItem.getBrand());
        itemSummaryResponse.setProductCode(productAndItem.getProductCode());
        itemSummaryResponse.setPromoBundling(productAndItem.isPromoBundling());
        itemSummaryResponse
            .setMerchantPromoDiscountActivated(productAndItem.isMerchantPromoDiscountActivated());
        itemSummaryResponse.setMerchantPromoDiscount(productAndItem.isMerchantPromoDiscount());
        itemSummaryResponse.setProductName(productAndItem.getProductName());
        itemSummaryResponse.setWholesalePriceActivated(Objects.nonNull(productAndItem.getWholesalePriceActivated()) ?
            productAndItem.getWholesalePriceActivated() : false);
        itemSummaryResponse.setOff2OnChannelActive(productAndItem.isOff2OnChannelActive());
        if (Objects.nonNull(originalPrice)) {
          itemSummaryResponse.setOriginalSellingPrice(originalPrice);
        }
        itemSummaryResponse.setItemViewConfigs(itemViewConfigs);
        try {
          itemSummaryResponse.setProductType(ProductType.valueOf(productAndItem.getProductType()));
        } catch (Exception e) {
          ItemSummaryUtilImpl.LOGGER.warn(
              "Product Type is not recognize for : {} , for itemSku : {}",
              productAndItem.getProductType(), productAndItem.getItemSku(), e);
        }
        itemSummaryResponses.add(itemSummaryResponse);
      } catch (Exception e) {
        ItemSummaryUtilImpl.LOGGER.warn(
            "Skipping generating item summary response. ItemSku : {}", productAndItem.getItemSku(),
            e);
      }
    }
    return new ItemSummaryPageResponseVo(itemSummaryResponses, resultFromSolr.getTotalElements(),
        resultFromSolr.getTotalPages());
  }

  @Override
  public String getProductCodeFromItemCode(String itemCode) {
    int firstHyphenIndex = itemCode.indexOf(Constants.HYPHEN);
    int secondHyphenIndex = itemCode.indexOf(Constants.HYPHEN, firstHyphenIndex + 1);
    return itemCode.substring(0, secondHyphenIndex);
  }
}
