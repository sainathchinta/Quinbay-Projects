package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.SellerAnalytics;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.SellerAnalyticsRepository;
import com.gdn.partners.product.analytics.service.UpdateDataFromBigQueryToDB;
import com.gdn.partners.product.analytics.service.impl.helper.ProcessorUtils;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.google.common.collect.Lists;
import com.mongodb.bulk.BulkWriteResult;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service("SELLER_ANALYTICS_INFO_BQ_JOB")
public class DataFromBigQueryToSellerAnalyticsDBImpl implements UpdateDataFromBigQueryToDB {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private SellerAnalyticsRepository sellerAnalyticsRepository;

  @Autowired
  private GCPProperties gcpProperties;

  @Override
  public List<SellerFieldsChangeResponse> writeJsonDataFromFileToDB(String filePath, int batchSize)
    throws IOException {
    List<SellerAnalytics> sellerAnalyticsList =
      ProcessorUtils.getDataListFromJsonFile(filePath, SellerAnalytics.class, objectMapper);
    Lists.partition(sellerAnalyticsList, batchSize).forEach(sellerAnalyticsList1 -> {
      BulkWriteResult bulkWriteResult =
        sellerAnalyticsRepository.bulkWriteSellerAnalyticsDetail(sellerAnalyticsList1);
      log.info("Seller Analytics detail Documents total : {}, upserted: {}, updated: {}",
        bulkWriteResult.getMatchedCount() + bulkWriteResult.getUpserts().size(),
        bulkWriteResult.getUpserts().size(), bulkWriteResult.getModifiedCount());
    });
    return new ArrayList<>();
  }

  @Override
  public String getResultFileNameLocal() {
    return gcpProperties.getSellerAnalyticsResultFileNameLocal();
  }

  @Override
  public String getResultFileNamePrefix() {
    return gcpProperties.getSellerAnalyticsResultFileNamePrefix();
  }

  @Override
  public String getResultDataSet() {
    return gcpProperties.getSellerAnalyticsResultDataSet();
  }

  @Override
  public String getResultTablePrefix() {
    return gcpProperties.getSellerAnalyticsResultTablePrefix();
  }
}
