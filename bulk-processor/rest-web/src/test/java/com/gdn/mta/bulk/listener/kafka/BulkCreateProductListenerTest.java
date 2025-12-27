package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.service.GeneralProcessorService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.mta.rest.web.request.BulkListProductCreateRequest;
import com.gdn.x.mta.rest.web.request.BulkProcessMandatoryRequest;
import com.gdn.x.mta.rest.web.request.ProductApiRequest;

/**
 * Created by hardikbohra on 21/06/18.
 */
public class BulkCreateProductListenerTest {

  private static final String DEFAULT_REQUEST_ID = "request-id";
  private static final String CREATE_PRODUCT_ACTION = "createProduct";

  @InjectMocks
  private BulkCreateProductListener listener;

  @Mock
  private GeneralProcessorService<ProductApiRequest, Void, Void> productLevel3ApiProcessorServiceBean;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productLevel3ApiProcessorServiceBean);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void testCreateProductListen() throws Exception {
    BulkProcessMandatoryRequest bulkProcessMandatoryRequest = new BulkProcessMandatoryRequest();
    bulkProcessMandatoryRequest.setAction(CREATE_PRODUCT_ACTION);
    BulkListProductCreateRequest bulkListProductCreateRequest = new BulkListProductCreateRequest();
    bulkListProductCreateRequest.setAction(CREATE_PRODUCT_ACTION);
    bulkListProductCreateRequest.setRequestId(DEFAULT_REQUEST_ID);
    ProductApiRequest productApiRequest = new ProductApiRequest();
    List<ProductApiRequest> productApiRequestList = new ArrayList<ProductApiRequest>();
    productApiRequestList.add(productApiRequest);
    bulkListProductCreateRequest.setProducts(productApiRequestList);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkListProductCreateRequest.class))
        .thenReturn(bulkListProductCreateRequest);
    Mockito.doReturn(null).when(productLevel3ApiProcessorServiceBean).preProcess((ProductApiRequest) Mockito
        .any(), Mockito.anyMap());
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(productLevel3ApiProcessorServiceBean).preProcess(Mockito.any(), Mockito
        .any());
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkListProductCreateRequest.class);
    Mockito.verify(kafkaTopicProperties).getBulkApiCreateProductEvent();
  }

  @Test
  public void listenWhenError() throws Exception {
    BulkListProductCreateRequest bulkListProductCreateRequest = new BulkListProductCreateRequest();
    ProductApiRequest productApiRequest = new ProductApiRequest();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkListProductCreateRequest.class))
        .thenReturn(bulkListProductCreateRequest);
    Mockito.doThrow(new Exception()).when(productLevel3ApiProcessorServiceBean).process(
        Mockito.eq(productApiRequest));
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkListProductCreateRequest.class);
    Mockito.verify(kafkaTopicProperties).getBulkApiCreateProductEvent();
  }
}
