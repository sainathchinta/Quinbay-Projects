package com.gdn.partners.product.analytics.repository.impl;

import com.gdn.partners.product.analytics.entity.ImageDeletion;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.model.ImageDeletionFields;
import com.gdn.partners.product.analytics.repository.ImageDeletionRepositoryCustom;
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
public class ImageDeletionRepositoryCustomImpl implements ImageDeletionRepositoryCustom {

  private final MongoTemplate mongoTemplate;
  @Value("${image.deletion.fetch.batch.size}")
  private int imageDeleteFetchBatchSize;

  @Value("${image.deletion.retry.count.limit}")
  private int imageDeletionRetryCount;

  @Override
  public List<ImageDeletion> fetchProductCodeForImageDeletion(String storeId, Set<String> status) {
    Query query = new Query(
      where(ImageDeletionFields.RESULT).in(status).and(Constants.STORE_ID).is(storeId)
        .and(ImageDeletionFields.RETRY_COUNT).lt(imageDeletionRetryCount));
    query.limit(imageDeleteFetchBatchSize);
    return mongoTemplate.find(query, ImageDeletion.class);
  }
}
