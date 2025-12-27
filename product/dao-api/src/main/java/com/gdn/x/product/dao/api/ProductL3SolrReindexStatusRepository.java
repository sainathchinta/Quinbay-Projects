package com.gdn.x.product.dao.api;

import java.util.List;

import com.gdn.x.product.model.entity.ProductL3SolrReindexStatus;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface ProductL3SolrReindexStatusRepository
    extends MongoRepository<ProductL3SolrReindexStatus, String>,
    ProductL3SolrReindexStatusRepositoryCustom {

  ProductL3SolrReindexStatus findByStoreIdAndProductSku(String storeId, String productSku);

  List<ProductL3SolrReindexStatus> findByStoreIdAndProductSkuIn(String storeId, List<String> productSku);
}
