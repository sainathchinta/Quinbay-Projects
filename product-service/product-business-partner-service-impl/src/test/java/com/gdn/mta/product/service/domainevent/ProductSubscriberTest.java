package com.gdn.mta.product.service.domainevent;

import static org.mockito.Mockito.verify;

import java.text.ParseException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.service.ProductService;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;

public class ProductSubscriberTest {
  
  @InjectMocks
  ProductSubscriber productSubscriber;
  @Mock
  private ProductService productService;

  @Mock
  private ObjectMapper objectMapper;
  
  private ProductDomainEventModel productDomainEventModel;

  private static final String PRODUCT_NAME="product-name";
  private static String PRODUCT_CODE = "PRO-1234";
  private static String PRODUCT_ID = "123af-kdjfh234-qwe234";
  private static String STORE_ID = "10001";
  private ObjectMapper mapper;
  
  
  @Test
  public void testConsumeProduct() throws Exception{
    String message = mapper.writeValueAsString(this.productDomainEventModel);
    Mockito.when(objectMapper.readValue(message, ProductDomainEventModel.class))
        .thenReturn(this.productDomainEventModel);
    this.productSubscriber.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ProductDomainEventModel.class);
    Mockito.verify(productService, Mockito.times(1)).retryDelete(PRODUCT_ID, STORE_ID, PRODUCT_CODE, PRODUCT_NAME, true);
  }
  
  @Test
  public void testConsumeProductMarkForDeleteFalse() throws Exception{
    productDomainEventModel.setproductMarkForDelete(false);
    String message = mapper.writeValueAsString(this.productDomainEventModel);
    Mockito.when(objectMapper.readValue(message, ProductDomainEventModel.class))
        .thenReturn(this.productDomainEventModel);
    this.productSubscriber.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ProductDomainEventModel.class);
    Mockito.verify(productService, Mockito.never()).retryDelete(PRODUCT_ID, STORE_ID, PRODUCT_CODE, PRODUCT_NAME, true);
  }
  
  @Test
  public void testConsumeProductWhenError() throws Exception{
    String message = mapper.writeValueAsString(this.productDomainEventModel);
    Mockito.when(objectMapper.readValue(message, ProductDomainEventModel.class))
        .thenReturn(this.productDomainEventModel);
    this.productSubscriber.onDomainEventConsumed(message);
    verify(objectMapper).readValue(message, ProductDomainEventModel.class);
  }
  
  @BeforeEach
  public void initializeTest() throws ParseException{
    MockitoAnnotations.initMocks(this);
    
    productDomainEventModel = new ProductDomainEventModel();
    productDomainEventModel.setId(PRODUCT_ID);
    productDomainEventModel.setStoreId(STORE_ID);
    productDomainEventModel.setProductCode(PRODUCT_CODE);
    productDomainEventModel.setName(PRODUCT_NAME);
    productDomainEventModel.setproductMarkForDelete(true);
    productDomainEventModel.setproductMarkForDelete(true);
    mapper = new ObjectMapper();
  }
  
}
