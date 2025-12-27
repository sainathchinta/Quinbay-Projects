package com.gdn.x.product.dao.api;

import java.util.List;

public interface ItemPickupPointArchiveCustomRepository {

  void deleteByProductSku(List<String> productSkus);

}
