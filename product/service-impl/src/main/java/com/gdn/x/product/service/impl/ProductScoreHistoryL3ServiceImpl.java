package com.gdn.x.product.service.impl;

import java.util.Date;
import java.util.Objects;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.dao.api.ProductScoreHistoryL3Repository;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.ProductScoreHistoryL3;
import com.gdn.x.product.service.api.ProductScoreHistoryL3Service;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductScoreHistoryL3ServiceImpl implements ProductScoreHistoryL3Service {

  private static final Logger LOG = LoggerFactory.getLogger(ProductScoreHistoryL3ServiceImpl.class);

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductScoreHistoryL3Repository productScoreHistoryL3Repository;

  @Async
  @Override
  public void saveProductScoreHistoryL3(String storeId, String productSku, ProductScore existingProductScore,
      ProductScore updatedProductScore) throws Exception {

    if (!GdnObjects.equals(existingProductScore, updatedProductScore)) {
      ProductScoreHistoryL3 productScoreHistoryL3 = getProductScoreHistoryL3(storeId, productSku);
      productScoreHistoryL3.setNewValue(this.objectMapper
          .writeValueAsString(Objects.nonNull(updatedProductScore) ? updatedProductScore : StringUtils.EMPTY));
      productScoreHistoryL3.setOldValue(this.objectMapper
          .writeValueAsString(Objects.nonNull(existingProductScore) ? existingProductScore : StringUtils.EMPTY));
      LOG.info("Saving the history on score update for :{} with oldValues : {}, newValues : {}", productSku,
          existingProductScore, updatedProductScore);
      this.productScoreHistoryL3Repository.save(productScoreHistoryL3);
    }
  }

  private ProductScoreHistoryL3 getProductScoreHistoryL3(String storeId, String productSku) {
    ProductScoreHistoryL3 productScoreHistoryL3 = new ProductScoreHistoryL3();
    productScoreHistoryL3.setProductSku(productSku);
    productScoreHistoryL3.setStoreId(storeId);
    productScoreHistoryL3.setCreatedDate(new Date());
    productScoreHistoryL3.setUpdatedDate(new Date());
    productScoreHistoryL3.setMarkForDelete(false);
    return productScoreHistoryL3;
  }
}
