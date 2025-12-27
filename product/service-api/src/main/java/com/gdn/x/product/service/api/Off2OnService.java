package com.gdn.x.product.service.api;

import java.util.List;

import com.gdn.x.product.model.vo.Off2OnPriceVO;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.response.Off2OnPriceResponseV2;

public interface Off2OnService {

  List<String> activateOff2OnChannelByItemSku(String storeId, List<String> itemSkus);

  boolean activateOff2OnChannelByItemSku(String storeId, String itemSku);

  List<String> activateOff2OnChannelByMerchantCode(String storeId, String merchantCode);

  List<String> activateOff2OnChannelByProductSku(String storeId, List<String> productSkus);

  boolean activateOff2OnChannelByProductSku(String storeId, String productSku);

  List<String> deactivateOff2OnChannelByItemSku(String storeId, List<String> itemSkus);

  boolean deactivateOff2OnChannelByItemSku(String storeId, String itemSku);

  List<String> deactivateOff2OnChannelByMerchantCode(String storeId, String merchantCode);

  List<String> deactivateOff2OnChannelByProductSku(String storeId, List<String> productSkus);

  boolean deactivateOff2OnChannelByProductSku(String storeId, String productSku);

  List<Off2OnPriceVO> getProductPriceForOff2On(String storeId, List<String> itemSkuList,
      String channel) throws Exception;

  /**
   * fetch off2On details using itemsku and pickupPointCode
   *
   * @param storeId
   * @param itemPickupPointRequestList
   * @param channel
   * @param needActivePromoBundlings
   * @return
   * @throws Exception
   */
  List<Off2OnPriceResponseV2> getProductPriceForOff2OnV2(String storeId,
      List<ItemPickupPointRequest> itemPickupPointRequestList, String channel, boolean needActivePromoBundlings) throws Exception;

}
