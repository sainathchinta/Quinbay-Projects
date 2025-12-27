package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.vo.Off2OnPriceVO;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.response.Off2OnPriceResponseV2;
import com.gdn.x.product.service.api.BusinessPartnerPromoService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.Off2OnHelperService;
import com.gdn.x.product.service.api.Off2OnService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.gdn.x.product.service.util.ResponseHelper;
import com.google.common.collect.ImmutableSet;

@Service
public class Off2OnServiceImpl implements Off2OnService {

  private static final Logger LOG = LoggerFactory.getLogger(Off2OnServiceImpl.class);

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";

  private static final String ITEM_SKUS_MUST_NOT_BE_NULL = "itemSkus must not be null";

  private static final String ITEM_SKUS_MUST_NOT_BE_EMPTY = "itemSkus must not be empty";

  @Autowired
  private ChannelService channelService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private Off2OnHelperService off2OnHelperService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private BusinessPartnerPromoService businessPartnerPromoService;

  @Override
  public List<String> activateOff2OnChannelByItemSku(String storeId, List<String> itemSkus) {
    return this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(storeId, itemSkus, true);
  }

  @Override
  public boolean activateOff2OnChannelByItemSku(String storeId, String itemSku) {
    return this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(storeId, itemSku, true);
  }

  @Override
  public List<String> activateOff2OnChannelByMerchantCode(String storeId, String merchantCode) {
    return this.off2OnHelperService.changeOff2OnChannelActiveByMerchantCode(storeId, merchantCode,
        true);
  }

  @Override
  public List<String> activateOff2OnChannelByProductSku(String storeId, List<String> productSkus) {
    return this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(storeId, productSkus,
        true);
  }

  @Override
  public boolean activateOff2OnChannelByProductSku(String storeId, String productSku) {
    return this.off2OnHelperService
        .changeOff2OnChannelActiveByProductSku(storeId, productSku, true);
  }

  @Override
  public List<String> deactivateOff2OnChannelByItemSku(String storeId, List<String> itemSkus) {
    return this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(storeId, itemSkus, false);
  }

  @Override
  public boolean deactivateOff2OnChannelByItemSku(String storeId, String itemSku) {
    return this.off2OnHelperService.changeOff2OnChannelActiveByItemSku(storeId, itemSku, false);
  }

  @Override
  public List<String> deactivateOff2OnChannelByMerchantCode(String storeId, String merchantCode) {
    return this.off2OnHelperService.changeOff2OnChannelActiveByMerchantCode(storeId, merchantCode,
        false);
  }

  @Override
  public List<String> deactivateOff2OnChannelByProductSku(String storeId, List<String> productSkus) {
    return this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(storeId, productSkus,
        false);
  }

  @Override
  public boolean deactivateOff2OnChannelByProductSku(String storeId, String productSku) {
    return this.off2OnHelperService.changeOff2OnChannelActiveByProductSku(storeId, productSku,
        false);
  }

  @Override
  public List<Off2OnPriceVO> getProductPriceForOff2On(String storeId, List<String> itemSkuList,
      String channel) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), Off2OnServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(itemSkuList != null, Off2OnServiceImpl.ITEM_SKUS_MUST_NOT_BE_NULL);
    checkArgument(!itemSkuList.isEmpty(), Off2OnServiceImpl.ITEM_SKUS_MUST_NOT_BE_EMPTY);
    String channelToUse = channel;
    if (StringUtils.isBlank(channelToUse)) {
      channelToUse = this.channelService.getDefaultChannel();
    }
    List<Off2OnPriceVO> result = new ArrayList<Off2OnPriceVO>();
    List<Item> items = this.itemService.getItemPriceAndOff2OnChannelActive(storeId, itemSkuList);
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.findByItemSkusAndDelivery(storeId, itemSkuList, true);
    Map<String, ItemPickupPoint> itemPickupPointMap = itemPickupPoints.stream()
        .collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (v1, v2) -> v2));
    for (Item item : items) {
      ItemPickupPoint itemPickupPoint =
          Optional.ofNullable(itemPickupPointMap.get(item.getItemSku())).orElse(new ItemPickupPoint());
      Price price = this.productHelperService.getRelevantItemPrice(itemPickupPoint.getPrice(), channelToUse);
      result.add(new Off2OnPriceVO(item.getItemSku(), price.getOfferPrice(), price.getListPrice(),
          item.isOff2OnChannelActive()));
    }
    return result;
  }

  @Override
  public List<Off2OnPriceResponseV2> getProductPriceForOff2OnV2(String storeId,
      List<ItemPickupPointRequest> itemPickupPointRequestList, String channel, boolean needActivePromoBundlings) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemPickupPointRequestList),
        ErrorMessages.ITEM_PICKUP_POINT_LIST_MUST_NOT_BE_EMPTY);
    ProductAndItemsUtil.validateItemPickupPointRequest(itemPickupPointRequestList);
    String channelToUse = StringUtils.isBlank(channel) ? channelService.getDefaultChannel() : channel;
    List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointService.fetchItemPickupPointsByItemSkuAndPickupPointCode(storeId,
            itemPickupPointRequestList, false);
    Map<String, Item> itemMap = itemService.fetchItemMapByItemSkus(storeId,
        ProductAndItemsUtil.getItemSkuFromItemPickupPoints(itemPickupPointList));
    setRelevantPrice(itemPickupPointList, channelToUse);
    Map<String, BusinessPartnerPromo> businessPartnerPromoMap = new HashMap<>();
    if (needActivePromoBundlings) {
      businessPartnerPromoMap = getStringBusinessPartnerPromoMap(storeId, itemPickupPointList);
    }
    return ResponseHelper.toOff2OnPriceResponseV2(itemPickupPointList, itemMap, businessPartnerPromoMap);
  }

  private Map<String, BusinessPartnerPromo> getStringBusinessPartnerPromoMap(String storeId,
      List<ItemPickupPoint> itemPickupPointList) {
    List<String> merchantCodes =
        itemPickupPointList.stream().map(ItemPickupPoint::getMerchantCode).distinct().collect(Collectors.toList());
    List<BusinessPartnerPromo> businessPartnerPromoList =
        businessPartnerPromoService.findByStoreIdAndBusinessPartnerList(storeId, merchantCodes);
    return Optional.ofNullable(businessPartnerPromoList).orElse(new ArrayList<>()).stream().collect(
            Collectors.toMap(BusinessPartnerPromo::getBusinessPartnerCode, Function.identity(), (v1, v2) -> v2));
  }

  private void setRelevantPrice(List<ItemPickupPoint> itemPickupPointList, String channelToUse) throws Exception {
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      Price price = productHelperService.getRelevantItemPrice(itemPickupPoint.getPrice(), channelToUse);
      itemPickupPoint.setPrice(ImmutableSet.of(price));
    }
  }


}
