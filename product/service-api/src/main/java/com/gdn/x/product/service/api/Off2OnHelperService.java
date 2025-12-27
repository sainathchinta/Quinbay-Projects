package com.gdn.x.product.service.api;

import java.util.List;

public interface Off2OnHelperService {

  List<String> changeOff2OnChannelActiveByItemSku(String storeId, List<String> itemSkus,
      boolean active);

  boolean changeOff2OnChannelActiveByItemSku(String storeId, String itemSku, boolean active);

  List<String> changeOff2OnChannelActiveByMerchantCode(String storeId, String merchantCode,
      boolean active);

  List<String> changeOff2OnChannelActiveByProductSku(String storeId, List<String> productSkus,
      boolean active);

  boolean changeOff2OnChannelActiveByProductSku(String storeId, String productSku, boolean active);

}
