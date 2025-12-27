package com.gdn.x.product.dao.api;

import java.util.List;

public interface ItemArchiveCustomRepository {

  void deleteByProductSku(List<String> productSkus);

}
