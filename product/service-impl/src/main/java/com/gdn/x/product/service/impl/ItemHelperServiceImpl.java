package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.AddVariantRequestVo;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.OfflineItemPriceVO;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PriceHistoryService;
import com.gdn.x.product.service.util.ItemHelperUtil;
import com.gdn.x.product.service.util.PriceUtil;
import com.gdn.x.product.service.util.ProductUtil;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ItemHelperServiceImpl implements ItemHelperService {

  private static final String COMMA_DELIMITER = ",";
  private static final String STORE_ID_MUST_NOT_BE_BLANK = "Store id must not be blank";
  private static final String PRISTINE_ID_MUST_NOT_BE_BLANK = "pristineId must not be blank";
  private static final String ITEM_CODE_MUST_NOT_BE_BLANK = "itemCode must not be blank";

  @Autowired private ChannelService channelService;

  @Autowired private PriceHistoryService priceHistoryService;

  @Autowired private ObjectConverterService objectConverterService;

  @Autowired
  private ItemRepository itemRepository;

  @Autowired
  private CachedService cachedService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Value("${solr.string.delimiter:#_#}")
  private String solrStringDelimiter;

  @Override
  public ItemViewConfig getItemViewConfigs(Set<ItemViewConfig> itemViewConfigs, String channelParam)
      throws ApplicationRuntimeException {
    String channel = channelParam;
    if (StringUtils.isBlank(channel)) {
      channel = this.channelService.getDefaultChannel();
    }
    for (ItemViewConfig itemViewConfig : itemViewConfigs) {
      if (channel.equals(itemViewConfig.getChannel())) {
        return itemViewConfig;
      }
    }
    throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        String.format("itemViewConfig is not found for channel %s", channelParam));
  }

  @Override public Item setItemPriceByChannel(Item item, Set<Price> prices, String username) {
    Date updateDate = new Date();
    for (Price updatePrice : prices) {
      boolean channelFound = false;
      updatePrice.setLastUpdatedBy(username);
      updatePrice.setLastUpdatedDate(updateDate);

      for (Price currentPrice : item.getPrice()) {
        if (currentPrice.getChannel().equals(updatePrice.getChannel())) {
          channelFound = true;
          if (currentPrice.getListPrice() != updatePrice.getListPrice()
              || currentPrice.getOfferPrice() != updatePrice.getOfferPrice()) {
            this.priceHistoryService.savePriceHistory(
                this.objectConverterService.convertToPriceHistory(currentPrice, item.getItemSku()));
            updatePrice.setListOfDiscountPrices(currentPrice.getListOfDiscountPrices());
            updatePrice.setMerchantPromoDiscountPrice(currentPrice.getMerchantPromoDiscountPrice());
            BeanUtils.copyProperties(updatePrice, currentPrice);
          }
        }
      }
      if (!channelFound) {
        item.getPrice().add(updatePrice);
      }
    }
    return item;
  }

  @Override public double getDiscountPrice(Price price) {
    return PriceUtil.getDiscountPrice(price);
  }

  @Override
  public DiscountPrice processDiscountPricesByPriority(List<DiscountPrice> prices) {
    return PriceUtil.processDiscountPricesByPriority(prices);
  }

  @Override
  public String getCategoryNameByCategoryCode(String requestId, String username, String categoryCode) {
    try {
      return (StringUtils.isBlank(categoryCode)) ?
          Constants.HYPHEN :
          cachedService.getParentCategoriesFromMasterData(requestId, username, categoryCode).stream()
              .filter(categoryResponse -> (StringUtils.isNotBlank(categoryResponse.getCategoryCode()) && categoryResponse
                  .getCategoryCode().equals(categoryCode)))
              .map(CategoryResponse::getName)
              .findFirst()
              .orElse(Constants.HYPHEN);
    } catch (Exception e) {
      log.warn("getCategoryNameByCategoryCode error for categoryCode: {}, message: {}", categoryCode, e.getMessage(),
          e);
      return Constants.HYPHEN;
    }
  }

  @Override
  public List<OfflineItemPriceVO> convertOfflineItemPrices(String pickupPointCodeFilter,
      ProductAndItemSolr productAndItem) {
    return this
        .convertOfflineItemPrices(pickupPointCodeFilter, new ArrayList<>(), productAndItem);
  }

  @Override
  public List<OfflineItemPriceVO> convertOfflineItemPrices(String pickupPointCodeFilter,
      List<String> pickupPointCodesFilter, ProductAndItemSolr productAndItem) {
    List<String> filter = combinePickupPointCodeAndPickupPointCodesFilter(pickupPointCodeFilter,
        pickupPointCodesFilter);
    List<OfflineItemPriceVO> offlineItemPriceVOS = new ArrayList<>();
    for (String offlinePrice : productAndItem.getOfflinePrices()) {
      try {
        String[] offlinePriceArray = offlinePrice.split(solrStringDelimiter);
        String pickupPointCode = offlinePriceArray[0];
        if (CollectionUtils.isNotEmpty(filter) && !filter.contains(pickupPointCode)) {
          continue;
        }
        double listPrice, offerPrice;
        if (offlinePriceArray.length > 2) {
          listPrice = Double.parseDouble(offlinePriceArray[1]);
          offerPrice = Double.parseDouble(offlinePriceArray[2]);
          if (listPrice < offerPrice) {
            listPrice = offerPrice;
          }
        } else {
          offerPrice = Double.parseDouble(offlinePriceArray[1]);
          listPrice = offerPrice;
        }
        OfflineItemPriceVO offlineItemPriceVO = new OfflineItemPriceVO();
        offlineItemPriceVO.setPickupPointCode(pickupPointCode);
        offlineItemPriceVO.setListPrice(listPrice);
        offlineItemPriceVO.setOfferPrice(offerPrice);
        offlineItemPriceVOS.add(offlineItemPriceVO);
      } catch (Exception e) {
        log.error("convert offline item prices from solr error: ", e);
      }
    }
    return offlineItemPriceVOS;
  }

  private List<String> combinePickupPointCodeAndPickupPointCodesFilter(String pickupPointCodeFilter,
      List<String> pickupPointCodesFilter) {
    return ItemHelperUtil.combinePickupPointCodeAndPickupPointCodesFilter(pickupPointCodeFilter, pickupPointCodesFilter);
  }

  @Override
  public Item convertToItem(AddVariantRequestVo addVariantRequestVo, Product product,
      MandatoryRequestParam mandatoryRequestParam) {
    return ProductUtil.convertToItem(addVariantRequestVo, product, mandatoryRequestParam);
  }


  @Override
  public Set<String> findActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(String storeId,
      PristineDataItem pristineDataItem, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ItemHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(pristineDataItem != null, ItemHelperServiceImpl.PRISTINE_ID_MUST_NOT_BE_BLANK);
    Set<String> itemSkus = this.itemRepository.findItemSkusByPristine(storeId, pristineDataItem);
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      return itemPickupPointService
          .getActivePromoBundlingByItemSkusAndPickupPointCode(storeId, new ArrayList<>(itemSkus), pickupPointCode);
    }
    return new HashSet<>();
  }

  @Override
  public Set<String> findActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(String storeId, String itemCode,
      String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ItemHelperServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemCode), ItemHelperServiceImpl.ITEM_CODE_MUST_NOT_BE_BLANK);
    Set<String> itemSkus = itemRepository.findItemSkusByStoreIdAndItemCode(storeId, itemCode);
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      return itemPickupPointService
          .getActivePromoBundlingByItemSkusAndPickupPointCode(storeId, new ArrayList<>(itemSkus), pickupPointCode);
    }
    return new HashSet<>();
  }

  @Override
  public List<Item> getBasicItemDetailsByItemCodes(String storeId, Set<String> itemCodes){
    return itemRepository.findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(storeId, itemCodes);
  }
}
