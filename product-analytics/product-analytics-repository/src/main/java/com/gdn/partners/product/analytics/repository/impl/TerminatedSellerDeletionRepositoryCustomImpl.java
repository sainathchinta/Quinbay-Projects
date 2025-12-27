package com.gdn.partners.product.analytics.repository.impl;

import com.gdn.partners.product.analytics.entity.TerminatedSellerDeletion;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.TerminatedSellerDeletionFields;
import com.gdn.partners.product.analytics.repository.TerminatedSellerDeletionRepositoryCustom;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Set;

import static org.springframework.data.mongodb.core.query.Criteria.where;

@Repository
@RequiredArgsConstructor
public class TerminatedSellerDeletionRepositoryCustomImpl implements TerminatedSellerDeletionRepositoryCustom {

  private final MongoTemplate mongoTemplate;

  @Value("${terminated.seller.deletion.publish.batch.size}")
  private int terminatedSellerPublishBatchSize;

  @Value("${terminated.seller.deletion.retry.count.limit}")
  private int retryCountLimit;

  @Override
  public List<TerminatedSellerDeletion> fetchTerminatedSellerProductsForDeletion(String storeId,
      Set<String> status) {
    Query query = new Query(
        where(TerminatedSellerDeletionFields.FINAL_RESULT).in(status).and(Constants.STORE_ID)
            .is(storeId).and(TerminatedSellerDeletionFields.RETRY_COUNT).lt(retryCountLimit));
    query.limit(terminatedSellerPublishBatchSize);
    return mongoTemplate.find(query, TerminatedSellerDeletion.class);
  }
}
