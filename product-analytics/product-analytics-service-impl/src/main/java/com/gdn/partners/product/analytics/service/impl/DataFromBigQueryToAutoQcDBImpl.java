package com.gdn.partners.product.analytics.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.model.Constants;
import com.gdn.partners.product.analytics.properties.ApplicationProperties;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.AutoQCRepository;
import com.gdn.partners.product.analytics.service.UpdateDataFromBigQueryToDB;
import com.gdn.partners.product.analytics.service.impl.helper.ProcessorUtils;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.google.common.collect.Lists;
import com.mongodb.bulk.BulkWriteResult;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service("SELLER_INFO_BQ_JOB")
public class DataFromBigQueryToAutoQcDBImpl implements UpdateDataFromBigQueryToDB {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private AutoQCRepository autoQCRepository;

  @Autowired
  private GCPProperties gcpProperties;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Override
  public List<SellerFieldsChangeResponse> writeJsonDataFromFileToDB(String filePath, int batchSize) throws IOException {
    List<AutoQCDetail> autoQCDetailList =
        ProcessorUtils.getDataListFromJsonFile(filePath, AutoQCDetail.class, objectMapper);
    List<SellerFieldsChangeResponse> sellerFieldsChangeResponseList = new ArrayList<>();
    Lists.partition(autoQCDetailList, batchSize).forEach(autoQCDetailList1 -> {
      BulkWriteResult bulkWriteResult = autoQCRepository
          .bulkWriteAutoQcDetail(autoQCDetailList1, sellerFieldsChangeResponseList,
              Arrays.asList(applicationProperties.getChangeFieldList().split(Constants.COMMA)));
      log.info("Documents total : {}, upserted: {}, updated: {}",
          bulkWriteResult.getMatchedCount() + bulkWriteResult.getUpserts().size(), bulkWriteResult.getUpserts().size(),
          bulkWriteResult.getModifiedCount());
    });
    return sellerFieldsChangeResponseList;
  }

  @Override
  public String getResultFileNameLocal() {
    return gcpProperties.getResultFileNameLocal();
  }

  @Override
  public String getResultFileNamePrefix() {
    return gcpProperties.getResultFileNamePrefix();
  }

  @Override
  public String getResultDataSet() {
    return gcpProperties.getResultDataSet();
  }

  @Override
  public String getResultTablePrefix() {
    return gcpProperties.getResultTablePrefix();
  }
}
