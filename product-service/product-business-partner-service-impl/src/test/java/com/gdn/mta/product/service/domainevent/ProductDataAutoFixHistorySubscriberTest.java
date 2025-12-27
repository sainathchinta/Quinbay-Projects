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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gda.mta.product.dto.response.ProductDataAutoFixHistoryListRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.service.ProductDataAutoFixHistoryService;

public class ProductDataAutoFixHistorySubscriberTest {

  private static final String PRODUCT_CODE = "ABC123";
  private static final String ADDITIONAL_INFO = "product Images was missisng";
  private static final String TYPE = "AUTO_HEAL";


  private ProductDataAutoFixHistoryListRequest productDataAutoFixHistoryListRequest;
  private ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto;
  private ObjectMapper mapper;

  @InjectMocks
  ProductDataAutoFixHistorySubscriber productDataAutoFixHistorySubscriber;

  @Mock
  private ProductDataAutoFixHistoryService productDataAutoFixHistoryService;

  @Mock
  private ObjectMapper objectMapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productDataAutoFixHistoryListRequest = new ProductDataAutoFixHistoryListRequest();
    productDataAutoFixHistoryDto = new ProductDataAutoFixHistoryDto();
    productDataAutoFixHistoryDto.setProductCode(PRODUCT_CODE);
    productDataAutoFixHistoryDto.setType(TYPE);
    productDataAutoFixHistoryDto.setAdditionalInfo(ADDITIONAL_INFO);
    List<ProductDataAutoFixHistoryDto> productDataAutoFixHistoryDtoList = new ArrayList<>();
    productDataAutoFixHistoryDtoList.add(productDataAutoFixHistoryDto);
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productDataAutoFixHistoryService, objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    String message = mapper.writeValueAsString(productDataAutoFixHistoryListRequest);
    Mockito.when(objectMapper.readValue(message, ProductDataAutoFixHistoryListRequest.class))
        .thenReturn(this.productDataAutoFixHistoryListRequest);
    this.productDataAutoFixHistorySubscriber.onDomainEventConsumed(message);
    Mockito.verify(objectMapper, Mockito.times(1)).readValue(message, ProductDataAutoFixHistoryListRequest.class);
    Mockito.verify(productDataAutoFixHistoryService, Mockito.times(1))
        .saveHistory(productDataAutoFixHistoryListRequest.getProductDataAutoFixHistoryDtoList());
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    String message = mapper.writeValueAsString(productDataAutoFixHistoryListRequest);
    Mockito.when(objectMapper.readValue(message, ProductDataAutoFixHistoryListRequest.class))
        .thenThrow(NullPointerException.class);
    this.productDataAutoFixHistorySubscriber.onDomainEventConsumed(message);
    Mockito.verify(objectMapper, Mockito.times(1)).readValue(message, ProductDataAutoFixHistoryListRequest.class);

  }

  @Test
  public void onDomainEventConsumedNullMessageTest() throws Exception {
    String message = mapper.writeValueAsString(productDataAutoFixHistoryListRequest);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      this.productDataAutoFixHistorySubscriber.onDomainEventConsumed(null);
    });

  }
}
