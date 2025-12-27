package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.ProductAttributeExtractions;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.ProductAttributeExtractionsRepository;
import com.gdn.partners.product.analytics.service.UpdateDataFromBigQueryToDB;
import com.gdn.partners.product.analytics.service.impl.helper.ProcessorUtils;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.google.common.collect.Lists;
import com.mongodb.bulk.BulkWriteResult;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

@Slf4j
@Service("PRODUCT_ATTRIBUTE_EXTRACTIONS_BQ_JOB")
@RequiredArgsConstructor
public class DataFromBigQueryToProductAttributeExtractionsImpl
    implements UpdateDataFromBigQueryToDB {

  private final GCPProperties gcpProperties;

  private final ObjectMapper objectMapper;

  private final ProductAttributeExtractionsRepository productAttributeExtractionsRepository;

  @Override
  public List<SellerFieldsChangeResponse> writeJsonDataFromFileToDB(String filePath, int batchSize)
      throws IOException {
    List<ProductAttributeExtractions> productAttributeExtractionsList =
        ProcessorUtils.getDataProductAttributeExtractionsListFromJsonFile(filePath, objectMapper);
    Lists.partition(productAttributeExtractionsList, batchSize)
        .forEach(productAttributeExtractions -> {
          BulkWriteResult bulkWriteResult =
              productAttributeExtractionsRepository.bulkWriteProductAttributeExtractions(
                  productAttributeExtractions);
          if (Objects.nonNull(bulkWriteResult)) {
            log.info("Product Attribute extractions : {}, upserted: {}, updated: {}",
                bulkWriteResult.getMatchedCount() + bulkWriteResult.getUpserts().size(),
                bulkWriteResult.getUpserts().size(), bulkWriteResult.getModifiedCount());
          }
        });
    return Collections.emptyList();
  }

  @Override
  public String getResultFileNameLocal() {
    return gcpProperties.getProductAttributeExtractionsResultFileNameLocal();
  }

  @Override
  public String getResultFileNamePrefix() {
    return gcpProperties.getProductAttributeExtractionsResultFileNamePrefix();
  }

  @Override
  public String getResultDataSet() {
    return gcpProperties.getProductAttributeExtractionsResultDataSet();
  }

  @Override
  public String getResultTablePrefix() {
    return gcpProperties.getProductAttributeExtractionsResultTablePrefix();
  }
}