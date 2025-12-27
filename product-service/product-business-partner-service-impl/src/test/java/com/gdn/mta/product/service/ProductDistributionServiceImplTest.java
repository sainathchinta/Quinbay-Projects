package com.gdn.mta.product.service;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.mta.distributiontask.request.RemoveProductRequest;

public class ProductDistributionServiceImplTest {
  public static final String PRODUCT_CODE = "PRODUCT_CODE";
  public static final String REQUEST_ID = "REQUEST_ID";
  public static final String USERNAME = "USERNAME";

  @InjectMocks
  private ProductDistributionServiceImpl productDistributionServiceImpl;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  private RemoveProductRequest removeProductRequest;
  private ProductActionRetryEvent productActionRetryEvent;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    removeProductRequest = new RemoveProductRequest();
    removeProductRequest.setProductCode(PRODUCT_CODE);
    productActionRetryEvent =
        new ProductActionRetryEvent(Constants.DEFAULT_STORE_ID, PRODUCT_CODE, Constants.PDT_RETRY_DELETE,
            StringUtils.EMPTY);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productDistributionTaskRepository);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
  public void removeProductFromPDT_Test() throws Exception {
    this.productDistributionServiceImpl.removeProductFromPDT(REQUEST_ID, USERNAME, removeProductRequest);
    Mockito.verify(productDistributionTaskRepository).removeProductFromPDT(REQUEST_ID, USERNAME, removeProductRequest);
  }

  @Test
  public void removeProductFromPDT_ExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationException.class).when(productDistributionTaskRepository)
        .removeProductFromPDT(REQUEST_ID, USERNAME, removeProductRequest);
    this.productDistributionServiceImpl.removeProductFromPDT(REQUEST_ID, USERNAME, removeProductRequest);
    Mockito.verify(productDistributionTaskRepository).removeProductFromPDT(REQUEST_ID, USERNAME, removeProductRequest);
    Mockito.verify(kafkaProducer)
        .send(DomainEventName.ADD_PRODUCT_TO_PDT_RETRY, removeProductRequest.getProductCode(), productActionRetryEvent);
  }
}

