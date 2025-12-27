package com.gdn.partners.product.analytics.repository;

import java.util.Set;

import com.gdn.partners.product.analytics.entity.TerminatedSellerDeletion;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface TerminatedSellerDeletionRepository extends
    MongoRepository<TerminatedSellerDeletion, String>, TerminatedSellerDeletionRepositoryCustom {

  /**
   *
   * @param productCode Non null product code
   * @param sellerCode Non null seller code
   * @return terminated seller
   */
  TerminatedSellerDeletion findByProductCodeAndSellerCodeAndFinalResultNotIn(String productCode, String sellerCode,
      Set<String> finalStatuses);
}
