package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.ProductOptimisationDetails;
import com.gdn.partners.product.analytics.model.IPRProduct;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.impl.helper.KafkaPublisher;
import model.ProductOptimisationEventModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.io.File;
import java.io.IOException;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class DataFromBigQueryToProductOptimisationImplTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String JSON_FILE = "jsonFile";
  private static final String TOPIC_NAME = "topicName";
  private static File jsonFile;
  @InjectMocks
  private DataFromBigQueryToProductOptimisationImpl bigQueryProductOptimisation;
  @Mock
  private GCPProperties gcpProperties;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  @Mock
  private KafkaPublisher kafkaPublisher;

  @BeforeEach
  public void setup() {
    jsonFile = new File(JSON_FILE);
  }

  @AfterEach
  public void afterTest() {
    Mockito.verifyNoMoreInteractions(gcpProperties);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  void writeJsonDataFromFileToDB() throws IOException {
    ProductOptimisationEventModel productOptimisationDetails =
      ProductOptimisationEventModel.builder().productSku(PRODUCT_CODE).build();
    ObjectMapper mapper = new ObjectMapper();
    mapper.writeValue(jsonFile, productOptimisationDetails);
    String jsonInString = mapper.writeValueAsString(productOptimisationDetails);
    when(objectMapper.readValue(jsonInString, ProductOptimisationEventModel.class)).thenReturn(
      productOptimisationDetails);
    when(kafkaTopicProperties.getProductOptimisationEventName()).thenReturn(TOPIC_NAME);
    bigQueryProductOptimisation.writeJsonDataFromFileToDB(JSON_FILE, 1);
    verify(objectMapper).readValue(jsonInString, ProductOptimisationEventModel.class);
    verify(kafkaTopicProperties, times(2)).getProductOptimisationEventName();
  }


  @Test
  void getResultFileNameLocalTest() {
    bigQueryProductOptimisation.getResultFileNameLocal();
    Mockito.verify(gcpProperties).getProductOptimisationResultFileNameLocal();
  }

  @Test
  void getResultFileNamePrefixTest() {
    bigQueryProductOptimisation.getResultFileNamePrefix();
    Mockito.verify(gcpProperties).getProductOptimisationResultFileNamePrefix();
  }

  @Test
  void getResultDataSetTest() {
    bigQueryProductOptimisation.getResultDataSet();
    Mockito.verify(gcpProperties).getProductOptimisationResultDataSet();
  }

  @Test
  void getResultTablePrefixTest() {
    bigQueryProductOptimisation.getResultTablePrefix();
    Mockito.verify(gcpProperties).getProductOptimisationResultTablePrefix();
  }
}
