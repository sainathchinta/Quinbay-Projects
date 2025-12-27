package com.gdn.x.productcategorybase.controller.util;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.dto.response.ShippingResponse;
import com.gdn.x.productcategorybase.entity.CategoryShipping;

public class MapperUtilTest {

  private class DummyObject {
    private String name;

    public DummyObject(String name) {
      this.name = name;
    }

    public String toJSON() {
      return "{\"name\":\"" + this.name + "\"}";
    }
  }

  private static final String CONTENT = "{\"deliveredByMerchant\":true,\"specialHandling\":true,\"directFlight\":true,\"ageLimit\":true, \"sizeChartRequired\":true}";

  @Mock
  private ObjectMapper mapper;

  @InjectMocks
  private MapperUtilImpl mapperUtilImpl;

  private ShippingResponse shippingResponse;
  private List<CategoryShipping> categoryShippings;
  private CategoryShipping categoryShipping;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    shippingResponse = new ShippingResponse();
    shippingResponse.setSpecialHandling(true);
    shippingResponse.setDirectFlight(true);
    shippingResponse.setDeliveredByMerchant(true);
    shippingResponse.setSizeChartRequired(true);
    categoryShipping = new CategoryShipping();
    categoryShipping.setShippingCode(CONTENT);
    categoryShippings = new ArrayList<>();
    categoryShippings.add(categoryShipping);
  }

  @AfterEach
  public void postTest() {
    verifyNoMoreInteractions(this.mapper);
  }

  @Test
  public void testMapRequestToString() throws Exception {
    DummyObject object = new DummyObject("test");
    when(this.mapper.writeValueAsString(object)).thenReturn("{\"name\":\"test\"}");
    Assertions.assertEquals(this.mapperUtilImpl.mapRequestToString(object), (object.toJSON()));
    verify(this.mapper).writeValueAsString(object);
  }

  @Test
  public void testMapStringToShippingCodeResponse() throws Exception {
    ShippingResponse response = new ShippingResponse();
    response.setDeliveredByMerchant(true);
    response.setSpecialHandling(true);
    response.setDirectFlight(true);
    String content = "{\"deliveredByMerchant\":true,\"specialHandling\":true,\"directFlight\":true}";
    when(this.mapper.readValue(content, ShippingResponse.class)).thenReturn(response);
    Assertions.assertEquals(this.mapperUtilImpl.mapStringToShippingCodeResponse(content).toString(), (response.toString()));
    verify(this.mapper).readValue(content, ShippingResponse.class);
  }

  @Test
  public void testGetShippingResponseList() throws Exception {
    when(this.mapper.readValue(CONTENT, ShippingResponse.class)).thenReturn(shippingResponse);
    List<ShippingResponse> responses = this.mapperUtilImpl.getShippingResponseList(categoryShippings);
    verify(this.mapper).readValue(CONTENT, ShippingResponse.class);
    Assertions.assertTrue(responses.get(0).isDeliveredByMerchant());
    Assertions.assertTrue(responses.get(0).isSpecialHandling());
    Assertions.assertTrue(responses.get(0).isDirectFlight());
    Assertions.assertTrue(responses.get(0).isSizeChartRequired());
  }

  @Test
  public void testGetShippingResponseListWithException() throws Exception {
    when(this.mapper.readValue(CONTENT, ShippingResponse.class)).thenThrow(RuntimeException.class);
    List<ShippingResponse> responses = this.mapperUtilImpl.getShippingResponseList(categoryShippings);
    verify(this.mapper).readValue(CONTENT, ShippingResponse.class);
    Assertions.assertFalse(responses.get(0).isDeliveredByMerchant());
    Assertions.assertFalse(responses.get(0).isSpecialHandling());
    Assertions.assertFalse(responses.get(0).isDirectFlight());
  }
}
