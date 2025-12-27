package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.model.IPRProduct;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.UpdateDataFromBigQueryToDB;
import com.gdn.partners.product.analytics.service.impl.helper.KafkaPublisher;
import com.gdn.partners.product.analytics.service.impl.helper.ProcessorUtils;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.IPRProductsEventModel;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service("IPR_PRODUCTS_JOB")
@RequiredArgsConstructor
public class DataFromBigQueryToIPRProductsImpl implements UpdateDataFromBigQueryToDB {

  private final GCPProperties gcpProperties;

  private final ObjectMapper objectMapper;

  private final KafkaPublisher kafkaPublisher;

  private final KafkaTopicProperties kafkaTopicProperties;

  @Override
  public List<SellerFieldsChangeResponse> writeJsonDataFromFileToDB(String filePath, int batchSize)
      throws IOException {
    List<IPRProduct> iprProductList =
        ProcessorUtils.getDataListFromJsonFile(filePath, IPRProduct.class, objectMapper);
    iprProductList.forEach(
        iprProduct -> kafkaPublisher.send(kafkaTopicProperties.getIprProductsEventName(),
            iprProduct.getProductSku(), convertToEventModel(iprProduct)));
    return new ArrayList<>();
  }

  private IPRProductsEventModel convertToEventModel(IPRProduct iprProduct) {
    return IPRProductsEventModel.builder().productCode(iprProduct.getProductCode())
        .productSku(iprProduct.getProductSku()).source(iprProduct.getSource())
        .addedDate(iprProduct.getAddedDate()).build();
  }

  @Override
  public String getResultFileNameLocal() {
    return gcpProperties.getIprProductsResultFileNameLocal();
  }

  @Override
  public String getResultFileNamePrefix() {
    return gcpProperties.getIprProductsResultFileNamePrefix();
  }

  @Override
  public String getResultDataSet() {
    return gcpProperties.getIprProductsResultDataSet();
  }

  @Override
  public String getResultTablePrefix() {
    return gcpProperties.getIprProductsResultTablePrefix();
  }
}