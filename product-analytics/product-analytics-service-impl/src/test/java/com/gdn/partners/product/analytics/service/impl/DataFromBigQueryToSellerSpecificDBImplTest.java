package com.gdn.partners.product.analytics.service.impl;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.entity.SellerQCDetail;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.SellerQCDetailRepository;
import com.mongodb.bulk.BulkWriteResult;


public class DataFromBigQueryToSellerSpecificDBImplTest {

  private static File jsonFile;
  private static final String JSON_FILE = "jsonFile";
  private static final String BUCKET_NAME = "bucketName";

  @InjectMocks
  private DataFromBigQueryToSellerSpecificDBImpl dataFromBigQueryToSellerSpecificDB;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private SellerQCDetailRepository sellerQCDetailRepository;

  @Mock
  private GCPProperties gcpProperties;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    jsonFile = new File(JSON_FILE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(sellerQCDetailRepository);
    Mockito.verifyNoMoreInteractions(gcpProperties);
    jsonFile.delete();
  }

  @Test
  void writeAutoQCDataFromFileToDBIOException() throws IOException {
    SellerQCDetail sellerQCDetail = SellerQCDetail.builder().businessPartnerCode(BUCKET_NAME).build();
    ObjectMapper mapper = new ObjectMapper();
    mapper.writeValue(jsonFile, sellerQCDetail);
    String jsonInString = mapper.writeValueAsString(sellerQCDetail);
    when(objectMapper.readValue(jsonInString, AutoQCDetail.class)).thenThrow(RuntimeException.class);
    when(sellerQCDetailRepository.bulkWriteSellerQcDetail(Mockito.anyList())).thenReturn(mock(BulkWriteResult.class));
    dataFromBigQueryToSellerSpecificDB.writeJsonDataFromFileToDB(JSON_FILE, 1);
    Mockito.verify(objectMapper).readValue(jsonInString, SellerQCDetail.class);
  }

  @Test
  void writeJsonDataFromFileToSellerQCDetailDBTest() throws IOException {
    SellerQCDetail sellerQCDetail = SellerQCDetail.builder().businessPartnerCode(BUCKET_NAME).build();
    ObjectMapper mapper = new ObjectMapper();
    mapper.writeValue(jsonFile, sellerQCDetail);
    String jsonInString = mapper.writeValueAsString(sellerQCDetail);
    when(objectMapper.readValue(jsonInString, SellerQCDetail.class)).thenReturn(sellerQCDetail);
    when(sellerQCDetailRepository.bulkWriteSellerQcDetail(Mockito.anyList())).thenReturn(mock(BulkWriteResult.class));
    dataFromBigQueryToSellerSpecificDB.writeJsonDataFromFileToDB(JSON_FILE, 1);
    verify(objectMapper).readValue(jsonInString, SellerQCDetail.class);
    verify(sellerQCDetailRepository).bulkWriteSellerQcDetail(Mockito.anyList());
  }

  @Test
  void getResultFileNameLocalTest() {
    dataFromBigQueryToSellerSpecificDB.getResultFileNameLocal();
    Mockito.verify(gcpProperties).getSellerSpecificResultFileNameLocal();
  }

  @Test
  void getResultFileNamePrefixTest() {
    dataFromBigQueryToSellerSpecificDB.getResultFileNamePrefix();
    Mockito.verify(gcpProperties).getSellerSpecificResultFileNamePrefix();
  }

  @Test
  void getResultDataSetTest() {
    dataFromBigQueryToSellerSpecificDB.getResultDataSet();
    Mockito.verify(gcpProperties).getSellerSpecificResultDataSet();
  }

  @Test
  void getResultTablePrefixTest() {
    dataFromBigQueryToSellerSpecificDB.getResultTablePrefix();
    Mockito.verify(gcpProperties).getSellerSpecificResultTablePrefix();
  }
}