package com.gdn.mta.product.service.util;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;

public class MapperUtilBeanTest {

  private static final String WHOLESALE_RULE_1 = "[{\"quantity\":3,\"wholesaleDiscount\":30.0,"
      + "\"wholesalePrice\":100000.0}]";

  @InjectMocks
  private MapperUtilBean mapperUtilBean;

  @Mock
  private ObjectMapper objectMapper;
  private ProductItemWholesalePriceResponse productItemWholesalePriceResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    productItemWholesalePriceResponse = new ProductItemWholesalePriceResponse();
    productItemWholesalePriceResponse.setQuantity(3);
    productItemWholesalePriceResponse.setWholesaleDiscount(30.0);
  }

  @Test
  public void mapRequestToString() throws Exception {
    Mockito.when(objectMapper.writeValueAsString(productItemWholesalePriceResponse)).thenReturn(WHOLESALE_RULE_1);
    String response = mapperUtilBean.mapRequestToString(productItemWholesalePriceResponse);
    Mockito.verify(objectMapper).writeValueAsString(Mockito.any(ProductItemWholesalePriceResponse.class));
    Assertions.assertEquals(response, WHOLESALE_RULE_1);
  }

  @Test
  public void mapStringToResponse() throws IOException {
    ProductItemWholesalePriceResponse productItemWholesalePriceResponse = new ProductItemWholesalePriceResponse();
    productItemWholesalePriceResponse.setQuantity(3);
    productItemWholesalePriceResponse.setWholesaleDiscount(30.0);
    Mockito.when(objectMapper.readValue(Mockito.eq(WHOLESALE_RULE_1), Mockito.any(TypeReference.class)))
        .thenReturn(Arrays.asList(productItemWholesalePriceResponse));
    List<ProductItemWholesalePriceResponse> productItemWholesalePriceResponses =
        mapperUtilBean.mapStringToResponse(WHOLESALE_RULE_1);
    Mockito.verify(objectMapper).readValue(Mockito.eq(WHOLESALE_RULE_1), Mockito.any(TypeReference.class));
    Assertions.assertNotNull(productItemWholesalePriceResponses);
    Assertions.assertEquals(1, productItemWholesalePriceResponses.size());
    Assertions.assertEquals(3, productItemWholesalePriceResponses.get(0).getQuantity());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(objectMapper);
  }
}