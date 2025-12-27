package com.gdn.x.mta.distributiontask.service.impl;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pdt.dto.configuration.distribution.IPRActionResponseDto;
import com.gdn.x.mta.distributiontask.dao.exception.ValidationException;
import com.gdn.x.mta.distributiontask.domain.event.model.IPRHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.ErrorCategory;
import com.gdn.x.mta.distributiontask.model.ProductIPR;
import com.gdn.x.mta.distributiontask.model.dto.IPRUpdateAssigneeRequest;
import com.gdn.x.mta.distributiontask.model.dto.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.dto.SubmitEvidenceRequest;
import com.gdn.x.mta.distributiontask.model.dto.AddingIprProductDTO;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import com.gdn.x.mta.distributiontask.service.api.IprService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Collections;

@ExtendWith(MockitoExtension.class)
class IprWrapperServiceImplTest {

  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String EVIDENCE_FILE_PATH = "EVIDENCE_FILE_PATH";
  private static final String EVIDENCE_NOTES = "EVIDENCE_NOTES";
  private static final String EVENT = "EVENT";
  private static final String STORE_ID = "10001";
  private static final String SOURCE = "CUSTOMER_REPORT";
  private static final String UPDATED_BY = "UPDATED_BY";
  private static final String ASSIGNEE = "ASSIGNEE";

  @InjectMocks
  private IprWrapperServiceImpl iprWrapperService;

  @Mock
  private IprService iprService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ObjectMapper objectMapper;

  private ProductChange productChange;
  private ProductIPR productIPR;
  private SubmitEvidenceRequest submitEvidenceRequest;
  private IprActionRequest iprActionRequest;
  private AddingIprProductDTO addingIprProductDTO;
  private IPRUpdateAssigneeRequest iprUpdateAssigneeRequest;
  private IPRHistoryEventModel iprHistoryEventModel;

  @BeforeEach
  void setup() {
    productChange = new ProductChange();
    productChange.setProductSku(PRODUCT_SKU);
    productIPR = new ProductIPR();
    productIPR.setProductSku(PRODUCT_SKU);
    addingIprProductDTO = new AddingIprProductDTO();

    submitEvidenceRequest = new SubmitEvidenceRequest();
    submitEvidenceRequest.setProductSku(PRODUCT_SKU);
    submitEvidenceRequest.setEvidenceFilePath(Collections.singletonList(EVIDENCE_FILE_PATH));
    submitEvidenceRequest.setEvidenceSubmittedNotes(EVIDENCE_NOTES);

    iprUpdateAssigneeRequest = IPRUpdateAssigneeRequest.builder().productSku(
        Collections.singletonList(PRODUCT_SKU)).build();

    iprActionRequest = new IprActionRequest();
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setAction(ProductStateIPR.SUSPENDED.name());
    iprHistoryEventModel=IPRHistoryEventModel.builder().productSku(PRODUCT_SKU).build();
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(iprService, kafkaProducer, kafkaTopicPropertiesConsumer);
  }

  @Test
  void testAddProductToIPR_withNonNullIprProductSolr() throws Exception {
    IPRProductSolr iprProductSolr = new IPRProductSolr();
    addingIprProductDTO.setIprProductSolr(iprProductSolr);
    addingIprProductDTO.setErrorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE);
    Mockito.when(iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null)).thenReturn(addingIprProductDTO);
    iprWrapperService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(iprService, Mockito.times(1))
        .addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaTopicPropertiesConsumer).getPublishHistoryForIprEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2)).send(Mockito.any(), Mockito.any(),
        Mockito.any());
  }

  @Test
  void testAddProductToIPR_onlyAssigneeUpdated() throws Exception {
    IPRProductSolr iprProductSolr = new IPRProductSolr();
    iprProductSolr.setAssignedTo(ASSIGNEE);
    addingIprProductDTO.setIprProductSolr(iprProductSolr);
    addingIprProductDTO.setErrorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE);
    addingIprProductDTO.setOnlyAssigneeUpdated(true);
    Mockito.when(iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, ASSIGNEE, null)).thenReturn(addingIprProductDTO);
    Mockito.when(objectMapper.writeValueAsString(Mockito.any())).thenReturn("{\"previous\":\"null\", \"current\":\"ASSIGNEE\"}");
    iprWrapperService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, ASSIGNEE, null);
    Mockito.verify(iprService, Mockito.times(1))
        .addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, ASSIGNEE, null);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaTopicPropertiesConsumer).getPublishHistoryForIprEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2)).send(Mockito.any(), Mockito.any(),
        Mockito.any());
  }

  @Test
  void testAddProductToIPR_withNullIprProductSolr() throws Exception {
    addingIprProductDTO.setIprProductSolr(null);
    addingIprProductDTO.setErrorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE);
    Mockito.when(iprService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null)).thenReturn(addingIprProductDTO);
    iprWrapperService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
    Mockito.verify(iprService, Mockito.times(1)).addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, null);
  }

  @Test
  void updateProductOnStateChangeTest() throws Exception {
    Mockito.when(iprService.updateProductOnStateChange(productChange))
        .thenReturn(Pair.of(productIPR, iprHistoryEventModel));
    Mockito.when(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent()).thenReturn(EVENT);
    Mockito.when(kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent()).thenReturn(EVENT);
    iprWrapperService.updateProductOnStateChange(productChange);
    Mockito.verify(iprService).updateProductOnStateChange(productChange);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2))
        .send(Mockito.eq(EVENT), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  void updateProductOnStateChangeNotPublishingToSolrTest() throws Exception {
    Mockito.when(iprService.updateProductOnStateChange(productChange))
        .thenReturn(Pair.of(null, null));
    iprWrapperService.updateProductOnStateChange(productChange);
    Mockito.verify(iprService).updateProductOnStateChange(productChange);
  }

  @Test
  void submitEvidenceForProductTest() throws JsonProcessingException {
    Mockito.when(iprService.submitEvidenceForProduct(submitEvidenceRequest))
        .thenReturn(Pair.of(productIPR, iprHistoryEventModel));
    Mockito.when(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent()).thenReturn(EVENT);
    Mockito.when(kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent()).thenReturn(EVENT);
    iprWrapperService.submitEvidenceForProduct(submitEvidenceRequest);
    Mockito.verify(iprService).submitEvidenceForProduct(submitEvidenceRequest);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaTopicPropertiesConsumer).getPublishHistoryForIprEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2))
        .send(Mockito.eq(EVENT), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  void submitEvidenceForProductEmptyEvidenceFileTest() throws JsonProcessingException {
    submitEvidenceRequest.setEvidenceFilePath(new ArrayList<>());
    submitEvidenceRequest.setEvidenceUrl(Collections.singletonList(EVIDENCE_FILE_PATH));
    Mockito.when(iprService.submitEvidenceForProduct(submitEvidenceRequest))
      .thenReturn(Pair.of(productIPR, iprHistoryEventModel));
    Mockito.when(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent()).thenReturn(EVENT);
    Mockito.when(kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent()).thenReturn(EVENT);
    iprWrapperService.submitEvidenceForProduct(submitEvidenceRequest);
    Mockito.verify(iprService).submitEvidenceForProduct(submitEvidenceRequest);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaTopicPropertiesConsumer).getPublishHistoryForIprEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2))
      .send(Mockito.eq(EVENT), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  void submitEvidenceForProductEmptyEvidenceFileAndUrlTest() {
    submitEvidenceRequest.setEvidenceFilePath(new ArrayList<>());
    Assertions.assertThrows(ValidationException.class,
      () -> iprWrapperService.submitEvidenceForProduct(submitEvidenceRequest));
  }

  @Test
  void submitEvidenceForProductTest_noHistory() throws JsonProcessingException {
    Mockito.when(iprService.submitEvidenceForProduct(submitEvidenceRequest))
        .thenReturn(Pair.of(productIPR, null));
    Mockito.when(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent()).thenReturn(EVENT);
    iprWrapperService.submitEvidenceForProduct(submitEvidenceRequest);
    Mockito.verify(iprService).submitEvidenceForProduct(submitEvidenceRequest);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(EVENT), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  void submitEvidenceForProductNullTest() throws JsonProcessingException {
    Mockito.when(iprService.submitEvidenceForProduct(submitEvidenceRequest))
        .thenReturn(Pair.of(null, null));
    iprWrapperService.submitEvidenceForProduct(submitEvidenceRequest);
    Mockito.verify(iprService).submitEvidenceForProduct(submitEvidenceRequest);
  }

  @Test
  void updateAssigneeTest_success() throws Exception {
    Mockito.when(iprService.updateAssignee(iprUpdateAssigneeRequest)).thenReturn(
        Pair.of(Collections.singletonList(productIPR),
            Collections.singletonList(IPRHistoryEventModel.builder().build())));
    Mockito.when(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent()).thenReturn(EVENT);
    iprWrapperService.updateAssignee(iprUpdateAssigneeRequest);
    Mockito.verify(iprService).updateAssignee(iprUpdateAssigneeRequest);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaTopicPropertiesConsumer).getPublishHistoryForIprEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2)).send(Mockito.any(), Mockito.any(),
        Mockito.any());
  }

  @Test
  void updateAssigneeTest_successNoHistory() throws Exception {
    Mockito.when(iprService.updateAssignee(iprUpdateAssigneeRequest)).thenReturn(
        Pair.of(Collections.singletonList(productIPR), Collections.singletonList(null)));
    Mockito.when(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent()).thenReturn(EVENT);
    iprWrapperService.updateAssignee(iprUpdateAssigneeRequest);
    Mockito.verify(iprService).updateAssignee(iprUpdateAssigneeRequest);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaProducer).send(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  void updateAssigneeTest_emptyProduct() throws Exception {
    Mockito.when(iprService.updateAssignee(iprUpdateAssigneeRequest))
        .thenReturn(Pair.of(null, null));
    iprWrapperService.updateAssignee(iprUpdateAssigneeRequest);
    Mockito.verify(iprService).updateAssignee(iprUpdateAssigneeRequest);
  }

  @Test
  void performIprActionForEmptyResultTest() throws Exception {
    iprActionRequest.setSellerNotes(EVIDENCE_NOTES);
    iprActionRequest.setReasons(EVIDENCE_NOTES);
    iprActionRequest.setViolationType(EVIDENCE_NOTES);
    Mockito.when(iprService.performIprActionForProduct(iprActionRequest, STORE_ID)).thenReturn(
      IPRActionResponseDto.builder().errorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE)
        .iprHistoryEventModels(Collections.singletonList(IPRHistoryEventModel.builder().build())).build());
    Mockito.when(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent()).thenReturn(EVENT);
    Mockito.when(kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent()).thenReturn(EVENT);
    iprWrapperService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Mockito.verify(iprService).performIprActionForProduct(iprActionRequest, STORE_ID);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaTopicPropertiesConsumer).getPublishHistoryForIprEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2))
        .send(Mockito.eq(EVENT), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  void performIprActionForEmptyResultTest_noHistory() throws Exception {
    iprActionRequest.setSellerNotes(EVIDENCE_NOTES);
    iprActionRequest.setReasons(EVIDENCE_NOTES);
    iprActionRequest.setViolationType(EVIDENCE_NOTES);
    Mockito.when(iprService.performIprActionForProduct(iprActionRequest, STORE_ID)).thenReturn(
      IPRActionResponseDto.builder().errorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE)
        .iprHistoryEventModels(null).build());
    Mockito.when(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent()).thenReturn(EVENT);
    iprWrapperService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Mockito.verify(iprService).performIprActionForProduct(iprActionRequest, STORE_ID);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaProducer).send(Mockito.eq(EVENT), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  void performIprActionForSameStateTest() throws Exception {
    iprActionRequest.setSellerNotes(EVIDENCE_NOTES);
    iprActionRequest.setReasons(EVIDENCE_NOTES);
    iprActionRequest.setViolationType(EVIDENCE_NOTES);
    Mockito.when(iprService.performIprActionForProduct(iprActionRequest, STORE_ID)).thenReturn(
      IPRActionResponseDto.builder().errorCategory(ErrorCategory.SAME_IPR_STATE)
        .iprHistoryEventModels(null).build());
    iprWrapperService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Mockito.verify(iprService).performIprActionForProduct(iprActionRequest, STORE_ID);
    Mockito.verify(kafkaTopicPropertiesConsumer, Mockito.never()).getAddIprProductSolrEvent();
    Mockito.verify(kafkaProducer, Mockito.never())
        .send(Mockito.eq(EVENT), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  void suspendEvidenceRequestedProductTest() throws Exception {
    productIPR.setState(ProductStateIPR.EVIDENCE_REQUESTED.name());
    productIPR.setSellerNotes(EVIDENCE_NOTES);
    productIPR.setViolationType(EVIDENCE_NOTES);
    productIPR.setReasons(EVIDENCE_NOTES);
    productIPR.setEvidenceRequestedBy(UPDATED_BY);
    productIPR.setStoreId(STORE_ID);
    iprActionRequest.setSellerNotes(EVIDENCE_NOTES);
    iprActionRequest.setViolationType(EVIDENCE_NOTES);
    iprActionRequest.setReasons(EVIDENCE_NOTES);
    iprActionRequest.setBulkAction(true);
    iprActionRequest.setUpdatedBy(UPDATED_BY);
    iprActionRequest.setStoreId(STORE_ID);
    Mockito.when(iprService.findByProductSku(PRODUCT_SKU)).thenReturn(productIPR);
    Mockito.when(iprService.performIprActionForProduct(iprActionRequest, STORE_ID)).thenReturn(
      IPRActionResponseDto.builder().errorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE)
        .iprHistoryEventModels(Collections.singletonList(IPRHistoryEventModel.builder().build())).build());
    iprWrapperService.suspendEvidenceRequestedProduct(PRODUCT_SKU);
    Mockito.verify(iprService).findByProductSku(PRODUCT_SKU);
    Mockito.verify(kafkaTopicPropertiesConsumer).getPublishHistoryForIprEvent();
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2))
      .send(Mockito.any(), Mockito.any(), Mockito.any());
  }

  @Test
  void performIprActionFor() throws Exception {
    iprActionRequest.setSellerNotes(EVIDENCE_NOTES);
    iprActionRequest.setReasons(EVIDENCE_NOTES);
    iprActionRequest.setViolationType(EVIDENCE_NOTES);
    Mockito.when(iprService.performIprActionForProduct(iprActionRequest, STORE_ID)).thenReturn(
      IPRActionResponseDto.builder().errorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE)
        .iprHistoryEventModels(null).productEmailEventModel(ProductEmailEventModel.builder().build())
        .build());
    Mockito.when(kafkaTopicPropertiesConsumer.getAddIprProductSolrEvent()).thenReturn(EVENT);
    Mockito.when(kafkaTopicPropertiesConsumer.getAddProductMailEvent()).thenReturn(EVENT);
    iprWrapperService.performIprActionForProduct(iprActionRequest, STORE_ID);
    Mockito.verify(iprService).performIprActionForProduct(iprActionRequest, STORE_ID);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2))
      .send(Mockito.eq(EVENT), Mockito.eq(PRODUCT_SKU), Mockito.any());
  }

  @Test
  void testAddDSProductToIPR_withNonNullIprProductSolr() {
    IPRProductSolr iprProductSolr = new IPRProductSolr();
    addingIprProductDTO.setIprProductSolr(iprProductSolr);
    addingIprProductDTO.setErrorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE);
    addingIprProductDTO.setOnlyAssigneeUpdated(true);
    Mockito.when(iprService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE))
        .thenReturn(addingIprProductDTO);
    iprWrapperService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaProducer, Mockito.times(1))
        .send(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(iprService, Mockito.times(1))
        .addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
  }

  @Test
  void testAddDSProductToIPR_withNullIprProductSolr() {
    addingIprProductDTO.setIprProductSolr(null);
    addingIprProductDTO.setErrorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE);
    Mockito.when(iprService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE))
        .thenReturn(addingIprProductDTO);
    iprWrapperService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
    Mockito.verify(iprService, Mockito.times(1))
        .addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
  }

  @Test
  void testAddDSProductToIPR_withNonNullIprProductSolrHistoryUpdate() {
    IPRProductSolr iprProductSolr = new IPRProductSolr();
    addingIprProductDTO.setIprProductSolr(iprProductSolr);
    addingIprProductDTO.setErrorCategory(ErrorCategory.EMPTY_ERROR_MESSAGE);
    addingIprProductDTO.setOnlyAssigneeUpdated(false);
    Mockito.when(iprService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE))
        .thenReturn(addingIprProductDTO);
    iprWrapperService.addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddIprProductSolrEvent();
    Mockito.verify(kafkaTopicPropertiesConsumer).getPublishHistoryForIprEvent();
    Mockito.verify(kafkaProducer, Mockito.times(2))
        .send(Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(iprService, Mockito.times(1))
        .addDSModelProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE);
  }
}
