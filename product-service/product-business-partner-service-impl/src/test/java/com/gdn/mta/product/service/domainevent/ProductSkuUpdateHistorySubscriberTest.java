package com.gdn.mta.product.service.domainevent;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.UpdatedProductHistoryService;
import com.gdn.pbp.property.MandatoryParameterHelper;

public class ProductSkuUpdateHistorySubscriberTest {

  private static final String MERCHANT_CODE = "merchant code";
  private static final String ITEM_SKU = "item sku";
  private static final String DEFAULT_OLD_VALUE = "old value";
  private static final String DEFAULT_NEW_VALUE = "new value";
  private static final String DEFAULT_ACCESS_CHANNEL = "Access channel";
  private static final String DEFAULT_ACCESS_KEY = "PICK_POINT_CODE";
  private static final String DEFAULT_CLIENT_ID = "clientId";
  private static final String DEFAULT_CHANGE_BY = "changeBy";
  private static final String ATTRIBUTE_NAME = "attribute name";
  private static final String PRODUCT_SKU = "product sku";
  private static final String DEFAULT_REQUEST_ID = "request id";
  private static final String NAME = "name";


  private AuditTrailListRequest auditTrailListRequest;
  private AuditTrailDto auditTrailRequest;
  private ObjectMapper mapper;

  @InjectMocks
  ProductSkuUpdateHistorySubscriber productSkuUpdateHistorySubscriber;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    auditTrailListRequest = new AuditTrailListRequest();
    auditTrailRequest = new AuditTrailDto();
    auditTrailRequest.setBusinessPartnerCode(MERCHANT_CODE);
    auditTrailRequest.setGdnSku(ITEM_SKU);
    auditTrailRequest.setActionKey(DEFAULT_ACCESS_KEY);
    auditTrailRequest.setOldValue(DEFAULT_OLD_VALUE);
    auditTrailRequest.setNewValue(DEFAULT_NEW_VALUE);
    auditTrailRequest.setAttributeName(ATTRIBUTE_NAME);
    auditTrailRequest.setProductSku(PRODUCT_SKU);
    auditTrailRequest.setName(NAME);
    List<AuditTrailDto> auditTrailDtos = new ArrayList<>();
    auditTrailDtos.add(auditTrailRequest);
    auditTrailListRequest.setAccessChannel(DEFAULT_ACCESS_CHANNEL);
    auditTrailListRequest.setChangedBy(DEFAULT_CHANGE_BY);
    auditTrailListRequest.setClientId(DEFAULT_CLIENT_ID);
    auditTrailListRequest.setRequestId(DEFAULT_REQUEST_ID);
    auditTrailListRequest.setAuditTrailResponseList(auditTrailDtos);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productServiceWrapper, objectMapper, updatedProductHistoryService);
  }

  @Test
  public void onDomainEventConsumedDirectlyUpdateInDbTest() throws Exception {
    ReflectionTestUtils.setField(productSkuUpdateHistorySubscriber, "historySolrUpdateNewEvent", false);
    auditTrailListRequest.setUpdateDirectlyToDB(true);
    String message = mapper.writeValueAsString(auditTrailListRequest);
    Mockito.when(objectMapper.readValue(message, AuditTrailListRequest.class)).thenReturn(this.auditTrailListRequest);
    this.productSkuUpdateHistorySubscriber.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, AuditTrailListRequest.class);
    Mockito.verify(updatedProductHistoryService).createAudit(Mockito.anyList(), Mockito.eq(false));
  }

  @Test
  public void onDomainEventConsumedDirectlyUpdateInDbAndSolrPublishTest() throws Exception {
    ReflectionTestUtils.setField(productSkuUpdateHistorySubscriber, "historySolrUpdateNewEvent", true);
    auditTrailListRequest.setUpdateDirectlyToDB(true);
    String message = mapper.writeValueAsString(auditTrailListRequest);
    Mockito.when(objectMapper.readValue(message, AuditTrailListRequest.class)).thenReturn(this.auditTrailListRequest);
    this.productSkuUpdateHistorySubscriber.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, AuditTrailListRequest.class);
    Mockito.verify(updatedProductHistoryService).createAudit(Mockito.anyList(), Mockito.eq(true));
    Mockito.verify(productServiceWrapper).publishSolrHistoryUpdateEvent(Mockito.anyList());
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = mapper.writeValueAsString(auditTrailListRequest);
    Mockito.when(objectMapper.readValue(message, AuditTrailListRequest.class)).thenReturn(this.auditTrailListRequest);
    this.productSkuUpdateHistorySubscriber.onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, AuditTrailListRequest.class);
    Mockito.verify(productServiceWrapper)
        .updateProductHistoryLevel3Audit(auditTrailListRequest.getAuditTrailResponseList(),
            auditTrailListRequest.getAccessChannel(), false, false);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    String message = mapper.writeValueAsString(auditTrailListRequest);
    try {
      Assertions.assertThrows(Exception.class, () -> {
        this.productSkuUpdateHistorySubscriber.onDomainEventConsumed(message);
      });
    } finally {
      Mockito.verify(objectMapper).readValue(message, AuditTrailListRequest.class);
    }
  }
}
