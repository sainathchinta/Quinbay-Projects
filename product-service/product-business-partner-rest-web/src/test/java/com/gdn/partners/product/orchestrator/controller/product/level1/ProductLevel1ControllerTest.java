package com.gdn.partners.product.orchestrator.controller.product.level1;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.orchestrator.dto.product.level1.ProductLevel1FilterRequest;
import com.gdn.partners.product.orchestrator.dto.product.level1.ProductLevel1FilterResponse;
import com.gdn.partners.product.orchestrator.model.product.level1.ProductLevel1Filter;
import com.gdn.partners.product.orchestrator.service.product.level1.ProductLevel1Service;
import org.apache.http.client.utils.URIBuilder;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.net.URI;
import java.util.Arrays;

public class ProductLevel1ControllerTest {

  private static final String DEFAULT_STORE_ID = "STORE-ID";
  private static final String DEFAULT_CHANNEL_ID = "CHANNEL-ID";
  private static final String DEFAULT_CLIENT_ID = "CLIENT-ID";
  private static final String DEFAULT_REQUEST_ID = "REQUEST-ID";
  private static final String DEFAULT_USERNAME = "USERNAME";

  @Mock
  private ProductLevel1Service productLevel1Service;

  @InjectMocks
  private ProductLevel1Controller productLevel1Controller;
  private MockMvc mockMvc;
  private ObjectMapper objectMapper;

  private Page<ProductLevel1FilterResponse> generateProductLevel1s() throws Exception {
    return new PageImpl<>(Arrays.asList(new ProductLevel1FilterResponse()));
  }

  @BeforeEach
  public void setup() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.productLevel1Controller)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build();
    this.objectMapper = new ObjectMapper(new JsonFactory());
    Mockito.when(this.productLevel1Service
        .findByFilter(Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class)))
        .thenReturn(this.generateProductLevel1s());
  }

  @AfterEach
  public void teardown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel1Service);
  }

  @Test
  public void filter_Valid_Success() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel1ControllerPath.BASE_PATH + ProductLevel1ControllerPath.FILTER)
        .addParameter("storeId", ProductLevel1ControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel1ControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel1ControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel1ControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel1ControllerTest.DEFAULT_USERNAME).build();
    ProductLevel1FilterRequest request =
        ProductLevel1FilterRequest.builder().sortDirection("ASC").build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).content(this.objectMapper.writeValueAsString(request))
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.productLevel1Service)
        .findByFilter(Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class));
  }

  @Test
  public void filter_SortDirectionIsNull_Success() throws Exception {
    URI uri = new URIBuilder()
        .setPath(ProductLevel1ControllerPath.BASE_PATH + ProductLevel1ControllerPath.FILTER)
        .addParameter("storeId", ProductLevel1ControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", ProductLevel1ControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", ProductLevel1ControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", ProductLevel1ControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", ProductLevel1ControllerTest.DEFAULT_USERNAME).build();
    ProductLevel1FilterRequest request = ProductLevel1FilterRequest.builder().build();
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(uri).content(this.objectMapper.writeValueAsString(request))
            .contentType(MediaType.APPLICATION_JSON_VALUE).accept(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.productLevel1Service)
        .findByFilter(Mockito.any(ProductLevel1Filter.class), Mockito.any(Pageable.class));
  }

}
