package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.model.IPRProduct;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.impl.helper.KafkaPublisher;
import model.IPRProductsEventModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.io.File;
import java.io.IOException;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class DataFromBigQueryToIPRProductImplTest {

  @InjectMocks
  private DataFromBigQueryToIPRProductsImpl bigQueryToIPRProducts;
  @Mock
  private GCPProperties gcpProperties;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  @Mock
  private KafkaPublisher kafkaPublisher;

  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String JSON_FILE = "jsonFile";
  private static final String TOPIC_NAME = "topicName";
  private static File jsonFile;

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    jsonFile = new File(JSON_FILE);
  }

  @AfterEach
  public void afterTest() {
    Mockito.verifyNoMoreInteractions(gcpProperties);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  void writeJsonDataFromFileToDB() throws IOException {
    IPRProduct iprProduct =
        IPRProduct.builder().productCode(PRODUCT_CODE).productSku(PRODUCT_SKU).build();
    ObjectMapper mapper = new ObjectMapper();
    mapper.writeValue(jsonFile, iprProduct);
    String jsonInString = mapper.writeValueAsString(iprProduct);
    when(objectMapper.readValue(jsonInString, IPRProduct.class)).thenReturn(iprProduct);
    when(kafkaTopicProperties.getIprProductsEventName()).thenReturn(TOPIC_NAME);
    bigQueryToIPRProducts.writeJsonDataFromFileToDB(JSON_FILE, 1);
    verify(objectMapper).readValue(jsonInString, IPRProduct.class);
    verify(kafkaTopicProperties).getIprProductsEventName();
    verify(kafkaPublisher).send(eq(TOPIC_NAME), eq(PRODUCT_SKU),
        Mockito.any(IPRProductsEventModel.class));
  }

  @Test
  void getResultFileNameLocalTest() {
    bigQueryToIPRProducts.getResultFileNameLocal();
    Mockito.verify(gcpProperties).getIprProductsResultFileNameLocal();
  }

  @Test
  void getResultFileNamePrefixTest() {
    bigQueryToIPRProducts.getResultFileNamePrefix();
    Mockito.verify(gcpProperties).getIprProductsResultFileNamePrefix();
  }

  @Test
  void getResultDataSetTest() {
    bigQueryToIPRProducts.getResultDataSet();
    Mockito.verify(gcpProperties).getIprProductsResultDataSet();
  }

  @Test
  void getResultTablePrefixTest() {
    bigQueryToIPRProducts.getResultTablePrefix();
    Mockito.verify(gcpProperties).getIprProductsResultTablePrefix();
  }
}
