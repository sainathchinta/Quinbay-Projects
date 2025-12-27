package com.gdn.partners.product.analytics.service.impl;


import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.entity.AutoQCDetail;
import com.gdn.partners.product.analytics.properties.ApplicationProperties;
import com.gdn.partners.product.analytics.properties.GCPProperties;
import com.gdn.partners.product.analytics.repository.AutoQCRepository;
import com.mongodb.bulk.BulkWriteResult;


public class UpdateDataFromBigQueryToAutoQcDBImplTest {

  private static File jsonFile;
  private static final String JSON_FILE = "jsonFile";
  private static final String BUCKET_NAME = "bucketName";
  private static final String PROPERTY = "prop";

  @InjectMocks
  private DataFromBigQueryToAutoQcDBImpl autoQCService;

  @Mock
  private AutoQCRepository autoQCRepository;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private GCPProperties gcpProperties;

  @Mock
  private ApplicationProperties applicationProperties;

  @Mock
  private KafkaProducerServiceImpl kafkaProducerService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    jsonFile = new File(JSON_FILE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(autoQCRepository);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(gcpProperties);
    Mockito.verifyNoMoreInteractions(applicationProperties);
    Mockito.verifyNoMoreInteractions(kafkaProducerService);
    jsonFile.delete();
  }

  @Test
  public void writeAutoQCDataFromFileToDBIOException() throws IOException {
    AutoQCDetail autoQCDetail = AutoQCDetail.builder().businessPartnerCode(BUCKET_NAME).build();
    ObjectMapper mapper = new ObjectMapper();
    mapper.writeValue(jsonFile, autoQCDetail);
    String jsonInString = mapper.writeValueAsString(autoQCDetail);
    Mockito.doThrow(JsonProcessingException.class).when(objectMapper).readValue(jsonInString, AutoQCDetail.class);
    when(applicationProperties.getChangeFieldList()).thenReturn(PROPERTY);
    List<String> propertyList = new ArrayList<>();
    propertyList.add(PROPERTY);
    when(autoQCRepository
        .bulkWriteAutoQcDetail(Mockito.anyList(), Mockito.eq(new ArrayList<>()), Mockito.eq(propertyList)))
        .thenReturn(mock(BulkWriteResult.class));
    autoQCService.writeJsonDataFromFileToDB(JSON_FILE, 1);
    Mockito.verify(objectMapper).readValue(jsonInString, AutoQCDetail.class);
  }

  @Test
  public void writeJsonDataFromFileToDB() throws IOException {
    AutoQCDetail autoQCDetail = AutoQCDetail.builder().businessPartnerCode(BUCKET_NAME).build();
    ObjectMapper mapper = new ObjectMapper();
    mapper.writeValue(jsonFile, autoQCDetail);
    String jsonInString = mapper.writeValueAsString(autoQCDetail);
    when(objectMapper.readValue(jsonInString, AutoQCDetail.class)).thenReturn(autoQCDetail);
    when(applicationProperties.getChangeFieldList()).thenReturn(PROPERTY);
    List<String> propertyList = new ArrayList<>();
    propertyList.add(PROPERTY);
    when(autoQCRepository
        .bulkWriteAutoQcDetail(Mockito.anyList(), Mockito.eq(new ArrayList<>()), Mockito.eq(propertyList)))
        .thenReturn(mock(BulkWriteResult.class));
    autoQCService.writeJsonDataFromFileToDB(JSON_FILE, 1);
    verify(autoQCRepository)
        .bulkWriteAutoQcDetail(Mockito.anyList(), Mockito.eq(new ArrayList<>()), Mockito.eq(propertyList));
    verify(objectMapper).readValue(jsonInString, AutoQCDetail.class);
    verify(applicationProperties).getChangeFieldList();
  }

  @Test
  public void writeJsonDataFromFileToDBKafkaPublish() throws IOException {
    AutoQCDetail autoQCDetail = AutoQCDetail.builder().businessPartnerCode(BUCKET_NAME).build();
    ObjectMapper mapper = new ObjectMapper();
    mapper.writeValue(jsonFile, autoQCDetail);
    String jsonInString = mapper.writeValueAsString(autoQCDetail);
    when(objectMapper.readValue(jsonInString, AutoQCDetail.class)).thenReturn(autoQCDetail);
    when(applicationProperties.getChangeFieldList()).thenReturn(PROPERTY);
    List<String> propertyList = new ArrayList<>();
    propertyList.add(PROPERTY);
    when(autoQCRepository
        .bulkWriteAutoQcDetail(Mockito.anyList(), Mockito.eq(new ArrayList<>()), Mockito.eq(propertyList)))
        .thenReturn(mock(BulkWriteResult.class));
    autoQCService.writeJsonDataFromFileToDB(JSON_FILE, 1);
    verify(autoQCRepository)
        .bulkWriteAutoQcDetail(Mockito.anyList(), Mockito.eq(new ArrayList<>()), Mockito.eq(propertyList));
    verify(objectMapper).readValue(jsonInString, AutoQCDetail.class);
    verify(applicationProperties).getChangeFieldList();
  }

  @Test
  public void getResultFileNameLocalTest() {
    autoQCService.getResultFileNameLocal();
    Mockito.verify(gcpProperties).getResultFileNameLocal();
  }

  @Test
  public void getResultFileNamePrefixTest() {
    autoQCService.getResultFileNamePrefix();
    Mockito.verify(gcpProperties).getResultFileNamePrefix();
  }

  @Test
  public void getResultDataSetTest() {
    autoQCService.getResultDataSet();
    Mockito.verify(gcpProperties).getResultDataSet();
  }

  @Test
  public void getResultTablePrefixTest() {
    autoQCService.getResultTablePrefix();
    Mockito.verify(gcpProperties).getResultTablePrefix();
  }
}