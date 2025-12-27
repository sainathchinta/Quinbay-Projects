package com.gdn.x.product.dao.api;

import java.util.Date;
import java.util.List;

import com.gdn.x.product.model.entity.ProductArchive;

public interface ProductArchiveCustomRepository {

  List<ProductArchive> fetchArchivedListOfProduct(Date date, int batchSize);

  void deleteByProductSku(List<String> productSkus);

}
