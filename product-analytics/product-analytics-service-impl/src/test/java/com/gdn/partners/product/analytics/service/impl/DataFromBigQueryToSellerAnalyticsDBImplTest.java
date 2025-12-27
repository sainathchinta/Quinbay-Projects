package com.gdn.partners.product.analytics.service.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.SellerAnalytics;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.SellerAnalyticsRepository;
import com.mongodb.bulk.BulkWriteResult;
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

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
class DataFromBigQueryToSellerAnalyticsDBImplTest {

  private static File jsonFile;
  private static final String JSON_FILE = "jsonFile";
  private static final String SELLER_CODE = "sellerCode";

  @InjectMocks
  private DataFromBigQueryToSellerAnalyticsDBImpl dataFromBigQueryToSellerAnalyticsDB;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private SellerAnalyticsRepository sellerAnalyticsRepository;

  @Mock
  private GCPProperties gcpProperties;

  @BeforeEach
  void setUp() {
    jsonFile = new File(JSON_FILE);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(sellerAnalyticsRepository);
    Mockito.verifyNoMoreInteractions(gcpProperties);
    jsonFile.delete();
  }

  @Test
  public void writeJsonDataFromFileToSellerAnalyticsDBTest() throws IOException {
    SellerAnalytics sellerAnalytics = SellerAnalytics.builder().sellerCode(SELLER_CODE).build();
    ObjectMapper mapper = new ObjectMapper();
    mapper.writeValue(jsonFile, sellerAnalytics);
    String jsonInString = mapper.writeValueAsString(sellerAnalytics);
    when(objectMapper.readValue(jsonInString, SellerAnalytics.class)).thenReturn(sellerAnalytics);
    when(sellerAnalyticsRepository.bulkWriteSellerAnalyticsDetail(Mockito.anyList())).thenReturn(
      mock(BulkWriteResult.class));
    dataFromBigQueryToSellerAnalyticsDB.writeJsonDataFromFileToDB(JSON_FILE, 1);
    verify(objectMapper).readValue(jsonInString, SellerAnalytics.class);
    verify(sellerAnalyticsRepository).bulkWriteSellerAnalyticsDetail(Mockito.anyList());
  }

  @Test
  public void getResultFileNameLocalTest() {
    dataFromBigQueryToSellerAnalyticsDB.getResultFileNameLocal();
    Mockito.verify(gcpProperties).getSellerAnalyticsResultFileNameLocal();
  }

  @Test
  public void getResultFileNamePrefixTest() {
    dataFromBigQueryToSellerAnalyticsDB.getResultFileNamePrefix();
    Mockito.verify(gcpProperties).getSellerAnalyticsResultFileNamePrefix();
  }

  @Test
  public void getResultDataSetTest() {
    dataFromBigQueryToSellerAnalyticsDB.getResultDataSet();
    Mockito.verify(gcpProperties).getSellerAnalyticsResultDataSet();
  }

  @Test
  public void getResultTablePrefixTest() {
    dataFromBigQueryToSellerAnalyticsDB.getResultTablePrefix();
    Mockito.verify(gcpProperties).getSellerAnalyticsResultTablePrefix();
  }
}
