package com.gdn.partners.product.analytics.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.SellerQCDetail;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.SellerQCDetailRepository;
import com.gdn.partners.product.analytics.service.UpdateDataFromBigQueryToDB;
import com.gdn.partners.product.analytics.service.impl.helper.ProcessorUtils;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.google.common.collect.Lists;
import com.mongodb.bulk.BulkWriteResult;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service("SELLER_SPECIFIC_INFO_BQ_JOB")
public class DataFromBigQueryToSellerSpecificDBImpl implements UpdateDataFromBigQueryToDB {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private SellerQCDetailRepository sellerQCDetailRepository;

  @Autowired
  private GCPProperties gcpProperties;

  @Override
  public List<SellerFieldsChangeResponse> writeJsonDataFromFileToDB(String filePath, int batchSize) throws IOException {
    List<SellerQCDetail> sellerQCDetailList = ProcessorUtils.getDataListFromJsonFile(filePath, SellerQCDetail.class, objectMapper);
    Lists.partition(sellerQCDetailList, batchSize).forEach(sellerQCDetailList1 -> {
      BulkWriteResult bulkWriteResult = sellerQCDetailRepository.bulkWriteSellerQcDetail(sellerQCDetailList1);
      log.info("Seller QC detail Documents total : {}, upserted: {}, updated: {}",
          bulkWriteResult.getMatchedCount() + bulkWriteResult.getUpserts().size(), bulkWriteResult.getUpserts().size(),
          bulkWriteResult.getModifiedCount());
    });
    return null;
  }

  @Override
  public String getResultFileNameLocal() {
    return gcpProperties.getSellerSpecificResultFileNameLocal();
  }

  @Override
  public String getResultFileNamePrefix() {
    return gcpProperties.getSellerSpecificResultFileNamePrefix();
  }

  @Override
  public String getResultDataSet() {
    return gcpProperties.getSellerSpecificResultDataSet();
  }

  @Override
  public String getResultTablePrefix() {
    return gcpProperties.getSellerSpecificResultTablePrefix();
  }
}
