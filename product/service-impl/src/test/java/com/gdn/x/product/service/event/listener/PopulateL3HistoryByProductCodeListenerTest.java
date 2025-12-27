package com.gdn.x.product.service.event.listener;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Arrays;
import java.util.Collections;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.AuditTrailDtoEventModel;
import com.gdn.x.product.domain.event.model.AuditTrailListByProductCodeEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.config.KafkaPublisher;

class PopulateL3HistoryByProductCodeListenerTest {

  private static final String MESSAGE = "message";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String PRODUCT_NAME = "PRODUCT_NAME";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String GDN_SKU_MATCH = "ITEM_CODE_MATCH";
  private static final String GDN_SKU_NOMATCH = "ITEM_CODE_NOMATCH";
  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String GENERATED_ITEM_NAME = "GENERATED_ITEM_NAME";
  private static final String CHANGED_BY = "user";

  @Mock
  private ProductService productService;

  @Mock
  private ItemHelperService itemHelperService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaPublisher kafkaPublisher;

  @InjectMocks
  private PopulateL3HistoryByProductCodeListener listener;

  @Captor
  private ArgumentCaptor<AuditTrailListResponse> responseCaptor;

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(itemHelperService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaPublisher);
  }

  @Test
  void onDomainEventConsumed_nonEmptyList_sendsResponse() throws Exception {
    AuditTrailListByProductCodeEventModel eventModel =
        AuditTrailListByProductCodeEventModel.builder().productCode(PRODUCT_CODE).changedBy(CHANGED_BY)
            .requestId(Constants.DEFAULT_REQUEST_ID).clientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT)
            .auditTrailResponseList(Arrays.asList(AuditTrailDtoEventModel.builder().gdnSku(GDN_SKU_MATCH).build(),
                AuditTrailDtoEventModel.builder().gdnSku(GDN_SKU_NOMATCH).build())).build();
    Mockito.when(objectMapper.readValue(MESSAGE, AuditTrailListByProductCodeEventModel.class)).thenReturn(eventModel);
    Product product = new Product();
    product.setProductName(PRODUCT_NAME);
    product.setProductSku(PRODUCT_SKU);
    Mockito.when(productService.findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE))
        .thenReturn(Collections.singletonList(product));
    Item item = new Item();
    item.setItemCode(GDN_SKU_MATCH);
    item.setItemSku(ITEM_SKU);
    item.setGeneratedItemName(GENERATED_ITEM_NAME);
    Mockito.when(itemHelperService.getBasicItemDetailsByItemCodes(Constants.DEFAULT_STORE_ID,
        eventModel.getAuditTrailResponseList().stream().map(AuditTrailDtoEventModel::getGdnSku)
            .collect(Collectors.toSet()))).thenReturn(Collections.singletonList(item));
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, AuditTrailListByProductCodeEventModel.class);
    Mockito.verify(productService).findByStoreIdAndProductCode(Constants.DEFAULT_STORE_ID, PRODUCT_CODE);
    Mockito.verify(itemHelperService)
        .getBasicItemDetailsByItemCodes(Mockito.eq(Constants.DEFAULT_STORE_ID), Mockito.anySet());
    Mockito.verify(kafkaPublisher)
        .send(Mockito.eq(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY), responseCaptor.capture());
    AuditTrailListResponse response = responseCaptor.getValue();
    assertNotNull(response);
    assertEquals(2, response.getAuditTrailResponseList().size());
    assertEquals(PRODUCT_SKU, response.getAuditTrailResponseList().get(0).getProductSku());
    assertEquals(GENERATED_ITEM_NAME, response.getAuditTrailResponseList().get(0).getName());
    assertEquals(PRODUCT_SKU, response.getAuditTrailResponseList().get(1).getProductSku());
    assertEquals(PRODUCT_NAME, response.getAuditTrailResponseList().get(1).getName());
    assertEquals(CHANGED_BY, response.getChangedBy());
    assertEquals(Constants.DEFAULT_CLIENT_ID_X_PRODUCT, response.getAccessChannel());
    assertEquals(Constants.DEFAULT_CLIENT_ID_X_PRODUCT, response.getClientId());
    assertEquals(Constants.DEFAULT_REQUEST_ID, response.getRequestId());
  }

  @Test
  void onDomainEventConsumed_emptyList_doesNothing() throws Exception {
    AuditTrailListByProductCodeEventModel eventModel = AuditTrailListByProductCodeEventModel.builder()
      .productCode(PRODUCT_CODE)
      .auditTrailResponseList(Collections.emptyList()).build();
    Mockito.when(objectMapper.readValue(MESSAGE, AuditTrailListByProductCodeEventModel.class)).thenReturn(eventModel);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, AuditTrailListByProductCodeEventModel.class);
  }

  @Test
  void onDomainEventConsumed_exceptionHandled() throws Exception {
    Mockito.when(objectMapper.readValue(MESSAGE, AuditTrailListByProductCodeEventModel.class))
      .thenThrow(new RuntimeException("error"));
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, AuditTrailListByProductCodeEventModel.class);
  }
}