package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.AutoApprovedProducts;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface AutoApprovedRepository
  extends MongoRepository<AutoApprovedProducts, String>, AutoApprovedRepositoryCustom {

  /**
   * Return auto approved product
   * @param productCode Non null product code
   * @return Auto approved product with the product code
   */
  AutoApprovedProducts findByProductCodeAndMarkForDeleteFalse(String productCode);

  /**
   * Delete product by product code
   * @param productCode Product code to delete
   */
  void deleteByProductCode(String productCode);
}
