package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.UpdateDataFromBigQueryToDB;
import com.gdn.partners.product.analytics.service.impl.helper.KafkaPublisher;
import com.gdn.partners.product.analytics.service.impl.helper.ProcessorUtils;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.ProductOptimisationEventModel;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Slf4j
@Service("PRODUCT_OPTIMISATION_JOB")
@RequiredArgsConstructor
public class DataFromBigQueryToProductOptimisationImpl implements UpdateDataFromBigQueryToDB {

  private final GCPProperties gcpProperties;

  private final ObjectMapper objectMapper;

  private final KafkaPublisher kafkaPublisher;

  private final KafkaTopicProperties kafkaTopicProperties;

  @Override
  public List<SellerFieldsChangeResponse> writeJsonDataFromFileToDB(String filePath, int batchSize)
    throws IOException {
    List<ProductOptimisationEventModel> productOptimisationDetails =
      ProcessorUtils.getDataListFromJsonFile(filePath, ProductOptimisationEventModel.class,
        objectMapper);
    Set<String> sellerCodeSet = new HashSet<>();
    productOptimisationDetails.forEach(productOptimised -> {
      log.info("Sending Product Optimisation Event to Kafka. Event: {}, Event Body {} ",
        kafkaTopicProperties.getProductOptimisationEventName(), productOptimisationDetails);
      kafkaPublisher.send(kafkaTopicProperties.getProductOptimisationEventName(),
        productOptimised.getProductSku(), productOptimised);
      sellerCodeSet.add(productOptimised.getSellerCode());
    });
    sellerCodeSet.forEach(sellerCode -> {
      log.info("Sending cache clear. Event: {}, Seller Code {} ",
        kafkaTopicProperties.getSellerCacheClearEventName(), sellerCode);
      kafkaPublisher.send(kafkaTopicProperties.getSellerCacheClearEventName(), sellerCode,
        sellerCode);
    });
    return new ArrayList<>();
  }

  @Override
  public String getResultFileNameLocal() {
    return gcpProperties.getProductOptimisationResultFileNameLocal();
  }

  @Override
  public String getResultFileNamePrefix() {
    return gcpProperties.getProductOptimisationResultFileNamePrefix();
  }

  @Override
  public String getResultDataSet() {
    return gcpProperties.getProductOptimisationResultDataSet();
  }

  @Override
  public String getResultTablePrefix() {
    return gcpProperties.getProductOptimisationResultTablePrefix();
  }
}
