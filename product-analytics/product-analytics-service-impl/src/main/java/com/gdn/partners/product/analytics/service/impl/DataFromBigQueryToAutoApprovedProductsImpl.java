package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.AutoApprovedProducts;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.AutoApprovedRepository;
import com.gdn.partners.product.analytics.service.UpdateDataFromBigQueryToDB;
import com.gdn.partners.product.analytics.service.impl.helper.ProcessorUtils;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.google.common.collect.Lists;
import com.mongodb.bulk.BulkWriteResult;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

@Slf4j
@Service("AUTO_APPROVED_PRODUCTS_JOB")
public class DataFromBigQueryToAutoApprovedProductsImpl implements UpdateDataFromBigQueryToDB {

  @Autowired
  private GCPProperties gcpProperties;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private AutoApprovedRepository autoApprovedRepository;

  @Override
  public List<SellerFieldsChangeResponse> writeJsonDataFromFileToDB(String filePath, int batchSize)
    throws IOException {
    List<AutoApprovedProducts> autoApprovedProductsList =
      ProcessorUtils.getDataListFromJsonFile(filePath, AutoApprovedProducts.class, objectMapper);
    Lists.partition(autoApprovedProductsList, batchSize).forEach(autoApprovedProducts -> {
      BulkWriteResult bulkWriteResult =
        autoApprovedRepository.bulkWriteAutoApprovedProducts(autoApprovedProducts);
      log.info("Auto approved products : {}, upserted: {}, updated: {}",
        bulkWriteResult.getMatchedCount() + bulkWriteResult.getUpserts().size(),
        bulkWriteResult.getUpserts().size(), bulkWriteResult.getModifiedCount());
    });
    return Collections.emptyList();
  }

  @Override
  public String getResultFileNameLocal() {
    return gcpProperties.getAutoApprovedResultFileNameLocal();
  }

  @Override
  public String getResultFileNamePrefix() {
    return gcpProperties.getAutoApprovedResultFileNamePrefix();
  }

  @Override
  public String getResultDataSet() {
    return gcpProperties.getAutoApprovedResultDataSet();
  }

  @Override
  public String getResultTablePrefix() {
    return gcpProperties.getAutoApprovedResultTablePrefix();
  }
}
