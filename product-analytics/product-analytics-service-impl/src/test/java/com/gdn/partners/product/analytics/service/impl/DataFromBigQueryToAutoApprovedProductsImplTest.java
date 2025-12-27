package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.AutoApprovedProducts;
import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.AutoApprovedRepository;
import com.mongodb.bulk.BulkWriteResult;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class DataFromBigQueryToAutoApprovedProductsImplTest {

  @InjectMocks
  private DataFromBigQueryToAutoApprovedProductsImpl bigQueryToAutoApprovedProducts;

  @Mock
  private GCPProperties gcpProperties;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private AutoApprovedRepository autoApprovedRepository;

  private static final String PRODUCT_CODE = "productCode";
  private static final String JSON_FILE = "jsonFile";
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
    Mockito.verifyNoMoreInteractions(autoApprovedRepository);
  }

  @Test
  void writeJsonDataFromFileToDB() throws IOException {
    AutoApprovedProducts autoApprovedProducts =
      AutoApprovedProducts.builder().productCode(PRODUCT_CODE).build();
    ObjectMapper mapper = new ObjectMapper();
    mapper.writeValue(jsonFile, autoApprovedProducts);
    String jsonInString = mapper.writeValueAsString(autoApprovedProducts);
    when(objectMapper.readValue(jsonInString, AutoApprovedProducts.class)).thenReturn(
      autoApprovedProducts);
    when(autoApprovedRepository.bulkWriteAutoApprovedProducts(Mockito.anyList())).thenReturn(
      mock(BulkWriteResult.class));
    bigQueryToAutoApprovedProducts.writeJsonDataFromFileToDB(JSON_FILE, 1);
    verify(autoApprovedRepository).bulkWriteAutoApprovedProducts(Mockito.anyList());
    verify(objectMapper).readValue(jsonInString, AutoApprovedProducts.class);
  }


  @Test
  void getResultFileNameLocalTest() {
    bigQueryToAutoApprovedProducts.getResultFileNameLocal();
    Mockito.verify(gcpProperties).getAutoApprovedResultFileNameLocal();
  }

  @Test
  void getResultFileNamePrefixTest() {
    bigQueryToAutoApprovedProducts.getResultFileNamePrefix();
    Mockito.verify(gcpProperties).getAutoApprovedResultFileNamePrefix();
  }

  @Test
  void getResultDataSetTest() {
    bigQueryToAutoApprovedProducts.getResultDataSet();
    Mockito.verify(gcpProperties).getAutoApprovedResultDataSet();
  }

  @Test
  void getResultTablePrefixTest() {
    bigQueryToAutoApprovedProducts.getResultTablePrefix();
    Mockito.verify(gcpProperties).getAutoApprovedResultTablePrefix();
  }

}
